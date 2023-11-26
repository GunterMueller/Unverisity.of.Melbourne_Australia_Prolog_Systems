/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include <ctype.h>
#include "mu.h"
#include <setjmp.h>

extern jmp_buf re_entry_point, interrupt_return_point;

Stream *streams;
int nstreams;		/* Current number of possible streams */
static int nofiles;	/* Maximum number of open files */

/*
 * Prolog current input and output
 */
Stream *inputStream, *outputStream;

#ifndef BSD4
int
setlinebuf(fp)
FILE *fp;
{
	return 1;
	/* BUG!  Do we really want to malloc() new buffers? */
	return setvbuf(fp, malloc(BUFSIZ), _IOLBF, BUFSIZ);
}
#endif

/*
 * Get a character from a stream.
 * Assumes that the stream is already validated and that eof
 * will be handled by the caller.
 */
static int
getcStream(st)
register Stream *st;
{
	register int c;
	register Stream *counts;

	if(st->s_npushed > 0) {
		c = st->s_pushed[--(st->s_npushed)];
	} else
		c = getc(st->s_fp);
	if(c == EOF)
		st->s_iseof = StarToAtom(&SymEOF);
	else {
		counts = st->s_counts;
		IncrementSmallInt(counts->s_chars, 1);
		if(c == '\n') {
			IncrementSmallInt(counts->s_lines, 1);
			counts->s_linepos = MakeSmallInt(0);
		} else
			IncrementSmallInt(counts->s_linepos, 1);
	}
	return c;
}

/*
 * Push a character back on an input stream.
 * WARNING!  No error checking.
 * BUG!  Pushing back the newline character stuffs the linepos count.
 */
static int
ungetcStream(c, st)
register int c;
register Stream *st;
{
	register Stream *counts;

	if(st->s_npushed >= MAXPUSHEDCHAR)
		panic("Too many characters pushed back on input stream.");
	st->s_pushed[st->s_npushed++] = c;

	if(c == EOF)
		st->s_iseof = StarToAtom(&SymFail);
	else {
		counts = st->s_counts;
		IncrementSmallInt(counts->s_chars, -1);
		if(c == '\n') {
			IncrementSmallInt(counts->s_lines, -1);
			counts->s_linepos = MakeSmallInt(0);	/* BUG! */
		} else
			IncrementSmallInt(counts->s_linepos, -1);
	}
	return c;
}

/*
 * Put a character on a stream.
 * Assumes that the stream is already validated.
 */
static int
putcStream(c, st)
register int c;
register Stream *st;
{
	register Stream *counts;

	counts = st->s_counts;
	IncrementSmallInt(counts->s_chars, 1);
	if(c == '\n') {
		IncrementSmallInt(counts->s_lines, 1);
		counts->s_linepos = MakeSmallInt(0);
	} else
		IncrementSmallInt(counts->s_linepos, 1);
	PutC(c, st);
	return c;
}

/*
 * Rather more efficient than calling putcStream() many times.
 */
static void
putsStream(s, st)
char *s;
register Stream *st;
{
	register char *t;
	register int c;
	register Stream *counts;
	register char *lp;

	counts = st->s_counts;
	for(lp = t = s; (c = *t) != '\0'; t++) {
		if(c == '\n') {
			IncrementSmallInt(counts->s_lines, 1);
			lp = t;
		}
		PutC(c, st);
	}
	IncrementSmallInt(counts->s_chars, (t - s) - 1);
	if(lp == s)
		IncrementSmallInt(counts->s_linepos, (t - s) - 1);
	else
		counts->s_linepos = MakeSmallInt((t - lp) - 1);
}

static int
eofStream(st)
register Stream *st;
{
	return feof(st->s_fp);
}

/*
 * Get a character from a string stream.
 * Assumes that the stream is already validated and that eof
 * will be handled by the caller.
 */
static int
getcString(st)
register Stream *st;
{
	register int c;

	if(st->s_c == st->s_end) {
		c = EOF;
		st->s_iseof = StarToAtom(&SymEOF);
	} else {
		c = *(st->s_c)++;
		IncrementSmallInt(st->s_chars, 1);
		if(c == '\n') {
			IncrementSmallInt(st->s_lines, 1);
			st->s_linepos = MakeSmallInt(0);
		} else
			IncrementSmallInt(st->s_linepos, 1);
	}
	return c;
}

/*
 * Push a character back on an input string stream.
 * WARNING!  No error checking.
 * BUG!  Pushing back the newline character stuffs the linepos count.
 */
static int
ungetcString(c, st)
register int c;
register Stream *st;
{
	if(st->s_c == st->s_base)
		panic("Too many characters pushed back on input string.");

	if(c == EOF) {
		--(st->s_c);
		st->s_end = st->s_c;
		st->s_iseof = StarToAtom(&SymFail);
	} else {
		*--(st->s_c) = c;
		IncrementSmallInt(st->s_chars, -1);
		if(c == '\n') {
			IncrementSmallInt(st->s_lines, -1);
			st->s_linepos = MakeSmallInt(0);	/* BUG! */
		} else
			IncrementSmallInt(st->s_linepos, -1);
	}
	return c;
}

static void
expandString(st)
register Stream *st;
{
	register int o;
	register char *s;

	s = st->s_base;
	o = (st->s_max - s) * 2;
	s = realloc(s, o);
	if(s == (char *) NULL)
		panic("Can't expand string for writable string stream");
	st->s_max = s + o;
	o = s - st->s_base;
	st->s_base = s;
	st->s_c += o;
}

/*
 * Put a character on a string stream.
 * Assumes that the stream is already validated.
 * Expands the string with realloc() if necessary.
 */
static int
putcString(c, st)
register int c;
register Stream *st;
{
	if(st->s_c == st->s_max)
		expandString(st);
	IncrementSmallInt(st->s_chars, 1);
	if(c == '\n') {
		IncrementSmallInt(st->s_lines, 1);
		st->s_linepos = MakeSmallInt(0);
	} else
		IncrementSmallInt(st->s_linepos, 1);
	*(st->s_c)++ = c;
	return c;
}

/*
 * Rather more efficient than calling putcString() many times.
 */
static void
putsString(s, st)
char *s;
Stream *st;
{
	register char *t, *p, *e;
	register int c;
	register Stream *counts;
	char *lp;

	counts = st->s_counts;
	for(lp = t = s; ; ) {
		p = st->s_c;
		e = st->s_max;
		for( ; (c = *t) != '\0'; t++) {
			if(c == '\n') {
				IncrementSmallInt(counts->s_lines, 1);
				lp = t;
			}
			if(p < e)
				*p++ = c;
			else
				break;
		}
		st->s_c = p;
		if(c == '\0')
			break;
		expandString(st);
	}
	if(lp == s)
		IncrementSmallInt(counts->s_linepos, (t - s) - 1);
	else
		counts->s_linepos = MakeSmallInt((t - lp) - 1);
	counts->s_chars = MakeSmallInt(p - st->s_base);
}

static int
eofString(st)
register Stream *st;
{
	return st->s_c == st->s_end;
}

static void
initStream(st)
register Stream *st;
{
	st->s_header = StarToStrHeader(7, &SymStream);
	st->s_name = NIL;
	st->s_mode = NIL;
	st->s_identifier = NIL;
	st->s_iseof = StarToAtom(&SymFail);
	st->s_chars = MakeSmallInt(0);
	st->s_lines = MakeSmallInt(0);
	st->s_linepos = MakeSmallInt(0);
	st->s_fp = (FILE *) NULL;
	st->s_get = getcStream;
	st->s_unget = ungetcStream;
	st->s_put = putcStream;
	st->s_puts = putsStream;
	st->s_eof = eofStream;
	st->s_instance = 0;
	st->s_npushed = 0;
	st->s_counts = st;
	st->s_pid = -1;
	st->s_base = (char *) NULL;
	st->s_c = (char *) NULL;
	st->s_end = (char *) NULL;
	st->s_max = (char *) NULL;
}

void
initIO()
{
	register int i;

#ifdef BSD4
	nstreams
		= nofiles
		= getdtablesize();
#else /* BSD4 */
	nstreams
		= nofiles
		= _NFILE;
#endif /* BSD4 */
	Flags[flgNSTREAMS] = MakeSmallInt(nstreams);

	streams = (Stream *) malloc(nstreams * sizeof(Stream));

	for(i = 0; i < nstreams; i++)
		initStream(streams + i);

	streams[0].s_fp = stdin;
	streams[0].s_name = StarToAtom(&SymUserInput);
	streams[0].s_mode = StarToAtom(&SymRead);
	streams[0].s_identifier = StarToAtom(&SymUserInput);
	streams[0].s_counts = streams + 2;
	inputStream = streams + 0;

	streams[1].s_fp = stdout;
	streams[1].s_name = StarToAtom(&SymUserOutput);
	streams[1].s_mode = StarToAtom(&SymWrite);	/* BUG.  Might be append. */
	streams[1].s_identifier = StarToAtom(&SymUserOutput);
	streams[1].s_counts = streams + 2;
	outputStream = streams + 1;

	streams[2].s_fp = stderr;
	streams[2].s_name = StarToAtom(&SymUserError);
	streams[2].s_mode = StarToAtom(&SymWrite);	/* BUG.  Might be append. */
	streams[2].s_identifier = StarToAtom(&SymUserError);
	streams[2].s_lines = MakeSmallInt(1);
}

/*
 * Attach various descriptive data to a newly opened stream.
 */
static Stream *
describeStream(fp, name, mode)
FILE *fp;
Object name, mode;
{
	register int fd;
	register Stream *st;

	fd = fileno(fp);
	st = streams + fd;
	st->s_fp = fp;
	st->s_get = getcStream;
	st->s_unget = ungetcStream;
	st->s_put = putcStream;
	st->s_puts = putsStream;
	st->s_eof = eofStream;
	st->s_name = name;
	st->s_mode = mode;
	st->s_identArray[0] = StrHeader2Stream;
	st->s_identArray[1] = MakeSmallInt(fd);
	st->s_identArray[2] = MakeSmallInt(st->s_instance);
	st->s_identifier = StarToStr(st->s_identArray);
	st->s_iseof = StarToAtom(&SymFail);
	st->s_chars = MakeSmallInt(0);
	st->s_lines = MakeSmallInt(1);
	st->s_linepos = MakeSmallInt(0);
	st->s_npushed = 0;

	return st;
}

/*
 * Attach various descriptive data to a newly opened string stream.
 */
static Stream *
describeString(buf, len, size, mode)
register char *buf;
int len, size;
Object mode;
{
	register int fd;
	register Stream *st;

	for(fd = nofiles; fd < nstreams; fd++)
		if(streams[fd].s_base == (char *) NULL)
			break;
	if(fd == nstreams) {
		nstreams += EXTRASTREAMS;
		Flags[flgNSTREAMS] = MakeSmallInt(nstreams);
		st = (Stream *) realloc((char *)streams, nstreams * sizeof(Stream));
		if(st == (Stream *) NULL)
			panic("Stream table lost during realloc() in describeString()");
		for(fd = nstreams - EXTRASTREAMS; fd < nstreams; fd++)
			initStream(st + fd);
		if(st != streams) {
			inputStream = st + (inputStream - streams);
			outputStream = st + (outputStream - streams);
			for(fd = 0; fd < nstreams - EXTRASTREAMS; fd++) {
				if(IsStr(st[fd].s_identifier))
					st[fd].s_identifier = StarToStr(st[fd].s_identArray);
				st[fd].s_counts = st + (st[fd].s_counts - streams);
			}
			streams = st;
		}
		fd = nstreams - EXTRASTREAMS;
	}
	st = streams + fd;
	st->s_base = buf;
	st->s_c = buf;
	st->s_end = buf + len;
	st->s_max = buf + size;
	st->s_get = getcString;
	st->s_unget = ungetcString;
	st->s_put = putcString;
	st->s_puts = putsString;
	st->s_eof = eofString;
	st->s_name = NIL;
	st->s_mode = mode;
	st->s_identArray[0] = StrHeader2Stream;
	st->s_identArray[1] = MakeSmallInt(fd);
	st->s_identArray[2] = MakeSmallInt(st->s_instance);
	st->s_identifier = StarToStr(st->s_identArray);
	st->s_iseof = StarToAtom(&SymFail);
	st->s_chars = MakeSmallInt(0);
	st->s_lines = MakeSmallInt(1);
	st->s_linepos = MakeSmallInt(0);

	return st;
}

/*
 * Open a stream.
 * All special case processing, name conversion, argument validation
 * and other uglies should be done somewhere else.
 * WARNING!  This means that it doesn't handle user, user_*.
 *
 * This procedure just opens a stream.
 */
Stream *
openStream(X1, X2)
register Object X1, X2;
{
	Atom *mode;

	DeRef(X1);
	DeRef(X2);
	if(!IsAtom(X2)) {
		warning("Atom expected in openStream().");
		return NULL;
	}
	mode = (Atom *)eRef(X2);

	switch(systemAtomIndex(mode)) {
	when indSymRead:
	case indSymWrite:
	case indSymAppend: {
		register FILE *fp;
		char type[2];

		type[0] = eCharStar(mode->a_pname)[0];
		type[1] = '\0';
		if(!IsAtom(X1)) {
			warning("Atom expected in openStream().");
			return NULL;
		}
		fp = fopen(eCharStar(((Atom *)eRef(X1))->a_pname), type);
		if(fp == (FILE *) NULL) {
			if(testFlagOn(flgFILE))
				warning("Can't open file in openStream");
			return NULL;
		}

		return describeStream(fp, X1, X2);
	}
	when indSymStringRead: {
		register int len;
		register char *buf;

		len = length(X1);
		if(len == -1) {
			warning("List of character codes expected in openStream()");
			return NULL;
		}
		buf = malloc(len + 1);
		if(buf == (char *) NULL) {
			warning("Unable to allocate buffer for stream in openStream()");
			return NULL;
		}
		if(listToString(X1, buf, buf + len + 1, 0) == (unsigned char *) NULL) {
			warning("List of character codes expected in openStream()");
			return NULL;
		}
		return describeString(buf, len, len + 1, X2);
	}
	when indSymStringWrite: {
		register char *buf;
		int len;

		if(!IsSmallInt(X1) || eSmallInt(X1) <= 0) {
			warning("Positive integer expected in openStream().");
			return NULL;
		}
		len = eSmallInt(X1);
		buf = malloc(len);
		if(buf == (char *) NULL) {
			warning("Unable to allocate buffer for stream in openStream()");
			return NULL;
		}
		return describeString(buf, 0, len, X2);
	}
	break;
	default:
		warning("Unknown mode in openStream()");
		return NULL;
	}
}

/*
 * Close a stream.  Note the special actions taken on the stdio streams.
 *
 * If chars is non-null and the stream is a write-mode string, a Prolog
 * list, (or string if no 0 bytes are found) of the collected output
 * is built and returned in *chars.
 *
 * The confusion of functions is silly but saves another bytecode.
 */
int
closeStream(stream, chars)
Object stream, *chars;
{
	register Stream *st;

	st = validateStream(stream, 'e');
	if(st == (Stream *) NULL) {
		if(testFlagOn(flgFILE))
			warning("Invalid stream to CLOSE");
		return 0;
	}
	switch(st - streams) {			/* N.B.  Probably invokes a divide */
	when 0:
		if(testFlagOn(flgFILE))
			warning("Closing user_input -- re-opening it on terminal");
		(void)freopen("/dev/tty", "r", st->s_fp);
		st->s_instance++;
		st->s_iseof = StarToAtom(&SymFail);
		return 1;
	when 1:
		if(testFlagOn(flgFILE))
			warning("Closing user_output -- re-opening it on terminal");
		(void)freopen("/dev/tty", "w", st->s_fp);
		st->s_instance++;
		return 1;
	when 2:
		if(testFlagOn(flgFILE))
			warning("Not permitted to close user_error");
		return 0;

	break;
	default: {
		register int i;

		switch(systemAtomIndex((Atom *)eRef(st->s_mode))) {
		when indSymRead:
		case indSymWrite:
		case indSymAppend: {
			int child, pid, status;

			child = st->s_pid;
			(void)fclose(st->s_fp);
			st->s_fp = (FILE *) NULL;
			if(chars != (Object *) NULL)
				*chars = NIL;
			if(child != -1) {
				/* Only wait if last stream to child closed. */
				for(i = 0; i < nstreams; i++)
					if(streams[i].s_pid == child)
						break;
				/* BUG!  This could be done better on BSD systems. */
				if(i == nstreams)
					do {
						pid = wait(&status);
					} while(pid != child && pid != -1);
			}
		}
		when indSymStringWrite:
			if(chars != (Object *) NULL) {
				register char *s, *e;
				int len;

				s = st->s_base;
				e = st->s_c;
				len = e - s;
				heapOverflowCheck(CMR->mr_h, NWORDS(len + 1));
				{
					register char *t;

					t = (char *) CMR->mr_h;
					*chars = StarToString(t);
					while(s < e) {
						i = *s++;
						if(i == '\0')
							break;
						*t++ = i;
					}
					*t = '\0';
				}
				if(i != '\0')
					CMR->mr_h += NWORDS(len + 1);
				else {
					register Object *H;

					H = CMR->mr_h;
					*chars = StarToList(H);
					heapOverflowCheck(H, len * 2);
					s = st->s_base;
					while(s < e) {
						H[0] = MakeSmallInt(*s++);
						H[1] = StarToList(H + 2);
						H += 2;
					}
					H[-1] = NIL;
					CMR->mr_h = H;
				}
			}
		case indSymStringRead:		/* Fall Through */
			free(st->s_base);
			st->s_base = (char *) NULL;
			st->s_c = (char *) NULL;
			st->s_end = (char *) NULL;
			st->s_max = (char *) NULL;

		break;
		default:
			panic("Impossible mode in closeStream()");
		}

		st->s_pid = -1;
		st->s_name = NIL;
		st->s_mode = NIL;
		st->s_instance++;
		st->s_identifier = NIL;

		return 1;
	}
	}
}

int
fork4(child, in, out, err)
Object *child, *in, *out, *err;
{
	FILE *fpIn, *fpOut, *fpErr;
	int pid;
	int pipeIn[2], pipeOut[2], pipeErr[2];
	extern FILE *fdopen();

	if(pipe(pipeIn) == -1) {
		warning("Unable to create pipe in fork4()");
		*child = MakeSmallInt(-1);
		return 1;
	}
	if(pipe(pipeOut) == -1) {
		close(pipeIn[0]); close(pipeIn[1]);
		warning("Unable to create pipe in fork4()");
		*child = MakeSmallInt(-1);
		return 1;
	}
	if(err != NULL && pipe(pipeErr) == -1) {
		close(pipeIn[0]); close(pipeIn[1]);
		close(pipeOut[0]); close(pipeOut[1]);
		warning("Unable to create pipe in fork4()");
		*child = MakeSmallInt(-1);
		return 1;
	}

	pid = fork();
	switch(pid) {
	when -1:
		close(pipeIn[0]); close(pipeIn[1]);
		close(pipeOut[0]); close(pipeOut[1]);
		if(err != NULL) {
			close(pipeErr[0]); close(pipeErr[1]);
		}
		warning("Can't fork in fork4()");
		*child = MakeSmallInt(-1);
		return 1;

	when 0:
		close(pipeIn[0]);
		dup2(pipeIn[1], 1);
		close(pipeIn[1]);
		streams[1].s_iseof = StarToAtom(&SymFail);
		streams[1].s_chars = MakeSmallInt(0);
		streams[1].s_lines = MakeSmallInt(1);
		streams[1].s_linepos = MakeSmallInt(0);
		streams[1].s_npushed = 0;
		streams[1].s_pid = -1;

		if(err != NULL) {
			close(pipeErr[0]);
			dup2(pipeErr[1], 2);
			close(pipeErr[1]);
			streams[2].s_iseof = StarToAtom(&SymFail);
			streams[2].s_chars = MakeSmallInt(0);
			streams[2].s_lines = MakeSmallInt(1);
			streams[2].s_linepos = MakeSmallInt(0);
			streams[2].s_npushed = 0;
			streams[2].s_pid = -1;
		}

		close(pipeOut[1]);
		dup2(pipeOut[0], 0);
		close(pipeOut[0]);
		streams[0].s_iseof = StarToAtom(&SymFail);
		streams[0].s_chars = MakeSmallInt(0);
		streams[0].s_lines = MakeSmallInt(1);
		streams[0].s_linepos = MakeSmallInt(0);
		streams[0].s_npushed = 0;
		streams[0].s_pid = -1;

		return 0;

	break;
	default:
		*child = MakeSmallInt(pid);

		close(pipeIn[1]);
		fpIn = fdopen(pipeIn[0], "r");
		*in = describeStream(fpIn,
					StarToAtom(&SymPipe), StarToAtom(&SymRead))
				->s_identifier;
		streams[fileno(fpIn)].s_pid = pid;

		if(err != NULL) {
			close(pipeErr[1]);
			fpErr = fdopen(pipeErr[0], "r");
			*err = describeStream(fpErr,
						StarToAtom(&SymPipe), StarToAtom(&SymRead))
					->s_identifier;
			streams[fileno(fpErr)].s_pid = pid;
		}

		close(pipeOut[0]);
		fpOut = fdopen(pipeOut[1], "w");
		*out = describeStream(
					fpOut, StarToAtom(&SymPipe), StarToAtom(&SymWrite))
				->s_identifier;
		streams[fileno(fpOut)].s_pid = pid;

		return 1;
	}
}

/*
 * Validate a stream for r)eading, w)riting, or e)xistence.
 * Returns Stream or NULL if invalid.
 *
 * Note that the existence of 'user' is an ambiguous question :-).
 */
Stream *
validateStream(st, mode)
register Object st;
register int mode;
{
	DeRef(st);
	if(IsStr(st)) {
		register Stream *s;
		register Object t;
		register Object *x;

		x = eRef(st);
		if((Structure) x[0] != StrHeader2Stream)
			return NULL;
		t = x[1]; DeRef(t);
		if(!IsSmallInt(t))
			return NULL;
		t = eSmallInt(t);
		if(t < 0 || t > nstreams)
			return NULL;
		s = streams + t;
		if(s->s_mode == NIL)
			return NULL;
		t = x[2]; DeRef(t);
		if(!IsSmallInt(t) || eSmallInt(t) != s->s_instance)
			return NULL;
		switch(systemAtomIndex((Atom *) eRef(s->s_mode))) {
		when indSymRead:
		case indSymStringRead:
			if(mode == 'w')
				return NULL;
		when indSymWrite:
		case indSymStringWrite:
		case indSymAppend:
			if(mode == 'r')
				return NULL;
		}
		return s;
	} else if(IsAtom(st)) {
		switch(systemAtomIndex((Atom *) eRef(st))) {
		when indSymUser:
			if(mode == 'r')
				return streams + 0;
			else if(mode == 'w')
				return streams + 1;
			/* else			 e)xistence is meaningless for user (sic) */
		when indSymUserInput:
			if(mode == 'r' || mode == 'e')
				return streams + 0;
		when indSymUserOutput:
			if(mode == 'w' || mode == 'e')
				return streams + 1;
		when indSymUserError:
			if(mode == 'w' || mode == 'e')
				return streams + 2;
		}
		return NULL;
	} else
		return NULL;
}

/*
 * Get one character from a stream.  Returns NIL if interrupted.
 *
 * One reason for putting this in a separate routine is to remove the
 * setjmp() from interpret().  Some compilers refuse to allocate registers
 * to local variables if a routine contains setjmp().
 */
Object
p_sget(st, printing)
register Stream *st;
register int printing;
{
	register int c;

	/* BUG?  Race conditions? */
	if(setjmp(interrupt_return_point)) {
		InterruptAction = intCOUNT;
		return NIL;
	} else {
		InterruptAction = intRETURN;
		if(st->s_iseof == StarToAtom(&SymEOF)) {
			warning("Attempt to read past end of file in SGET");
			if(st == streams + 0) {
				if(testFlagOn(flgFILE))
					warning("Re-opening user_input on terminal");
				(void)freopen("/dev/tty", "r", st->s_fp);
				st->s_instance++;
				st->s_iseof = StarToAtom(&SymFail);
			}
			longjmp(re_entry_point, 1);		/* Abort */
		}
		do {
			c = (*st->s_get)(st);
		} while(printing && 0 <= c && c <= 32);
		if(c == EOF)
			st->s_iseof = StarToAtom(&SymEOF);
	}
	InterruptAction = intCOUNT;
	return MakeSmallInt(c);
}

void
putlStream(x, st)
register Object x;
register Stream *st;
{
	for( ; ; ) {
		DeRef(x);
		if(IsNIL(x))
			break;
		else if(IsString(x)) {
			(*st->s_puts)((char *) eRef(x), st);
			break;
		} else if(IsList(x)) {
			register Object c;
			register Object *l;

			l = eRef(x);
			c = l[0]; DeRef(c);
			x = l[1];
			if(!IsSmallInt(c)) {
				warning("Invalid character in string to putlStream()");
				break;
			}
			c = eSmallInt(c);
			if(c <= 0 || c > MAXCHAR) {
				warning("Invalid character in string to putlStream()");
				break;
			}
			(*st->s_put)(c, st);
		} else {
			warning("Non-list given to putlStream()");
			break;
		}
	}
}

static int (*get)();		/* Character access functions for tokenizer */
static void (*unget)();
static int (*eof)();		/* EOF function for tokenizer */
static Stream *stream;		/* Stream for tokenizer */
static char *string;		/* String for tokenizer */
static int stringeof;		/* Null char read from string */

/*  Someone trying to convert a large amount of code from another dialect
    to NU Prolog found it harder than it needed to be because the tokeniser
    never bothered to say _where_ the things it found objectionable were.
    In order to provide something better than nothing at all, RAOK added
    er_line_no.  The idea is that
    - for strings, in which "lines" don't make much sense, there is no
      attempt to report any sort of line number
    - for input from files, the error message includes the line where
      the tokeniser _started_.  This line number is set twice: once when
      we start, and again after layout and comments have been skipped.
      (Fixing this would require that the code be tidied up.)
    In order to set this up, I have made sure that "stream" is ALWAYS
    defined when io_token() is called; it is NULL iff input is coming
    from a "string".
*/
static int er_line_no;                /* Line number for error report */

static void Panic(s)
char *s;
{
	char buffer[256];
	if(!stream)
		sprintf(buffer, "%s <in io_token()>", s);
	else
		sprintf(buffer, "%s near line %d <in io_token()>", s, er_line_no);
	panic(s);
}

static void Warning(s)
char *s;
{
	char buffer[256];
	if(!stream)
		sprintf(buffer, "%s <in io_token()>", s);
	else
		sprintf(buffer, "%s near line %d <in io_token()>", s, er_line_no);
	warning(s);
}

static int
io_fgetc()
{
	return (*stream->s_get)(stream);
}

static void
io_fungetc(c)
int c;
{
	(*stream->s_unget)(c, stream);
}

static int
io_feof()
{
	return feof(stream->s_fp);
}

static int
io_sgetc()
{
	register int c;

	c = *string++;
	if(c != 0)
		return c;
	else {
		stringeof = 1;
		return -1;
	}
}

/*
 * Doesn't actually change the string involved.
 * Every use of this function happens to be equivalent
 * to backspacing the input.
 */
static void
io_sungetc(c)
int c;
{
	--string;
}

static int
io_seof()
{
	return stringeof;
}

static int skipped;

/*
 * Get first non-space, non-comment character on fp.
 * Set skipped if any characters are skipped.
 */
static int
skipjunk()
{
	register int c;

	skipped = 0;
	c = (*get)();
	for(;;) {
		if(c == EOF)
			return EOF;
		else if(isspace(c)) {
			skipped = 1;
			do {
				c = (*get)();
			} while(c != EOF && isspace(c));
		} else if(c == '%') {
			skipped = 1;
			while(c != '\n' && c != EOF)
				c = (*get)();
		} else if(c == '/') {
			c = (*get)();
			if(c == '*') {
				skipped = 1;
				for(;;) {
					do {
						c = (*get)();
					} while(c != '*' && c != EOF);
					if(c == EOF) {
						Warning("Unterminated comment");
						return c;
					}
					c = (*get)();
					if(c == '/') {
						c = (*get)();
						break;
					} else
						(*unget)(c);
				}
			} else {
				(*unget)(c);
				return '/';
			}
		} else
			return c;
	}
}

char charTypes[] = {
		' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
		' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
		' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
		' ',	' ',	' ',	' ',	' ',	' ',	' ',	' ',
		' ',	'!',	'"',	'=',	'a',	' ',	'=',	'\'',
		'!',	'!',	'=',	'=',	'!',	'=',	'=',	'=',
		'0',	'0',	'0',	'0',	'0',	'0',	'0',	'0',
		'0',	'0',	'=',	'!',	'=',	'=',	'=',	'=',
		'=',	'A',	'A',	'A',	'A',	'A',	'A',	'A',
		'A',	'A',	'A',	'A',	'A',	'A',	'A',	'A',
		'A',	'A',	'A',	'A',	'A',	'A',	'A',	'A',
		'A',	'A',	'A',	'!',	'=',	'!',	'=',	'A',
		'=',	'a',	'a',	'a',	'a',	'a',	'a',	'a',
		'a',	'a',	'a',	'a',	'a',	'a',	'a',	'a',
		'a',	'a',	'a',	'a',	'a',	'a',	'a',	'a',
		'a',	'a',	'a',	'!',	'!',	'!',	'=',	' ',
};

/*
 * Table of printable characters.
 * Escapes are 1, whitespace's are 2, space and tab are 3, visibles are 4.
 */
char isPrint[] = {
		0,	0,	0,	0,	0,	0,	0,	0,
		0,	3,	2,	0,	2,	2,	0,	0,
		0,	0,	0,	0,	0,	0,	0,	0,
		0,	0,	0,	0,	0,	0,	0,	0,
		3,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	4,
		4,	4,	4,	4,	4,	4,	4,	1,
};

static int
charesc(c)
register int c;
{
	if(testFlagOff(flgCHAR))
		return c;

	/*
	 * Go round the loop until we get a character to return.
	 * May take several trys, as for "\c   \177"
	 */
	for( ; ; ) {
		c = (*get)();
		switch(c) {
		when 'b': return 8;
		when 't': return 9;
		when 'n': return 10;
		when 'v': return 11;
		when 'f': return 12;
		when 'r': return 13;
		when 'e': return 27;
		when 's': return 32;
		when 'd': return 127;

		when '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7': {
			register int i, n;

			i = c - '0';
			for(n = 1; n < 3; n++) {
				c = (*get)();
				if('0' <= c && c < '8')
					i = i * 8 + c - '0';
				else {
					(*unget)(c);
					return i;
				}
			}
			return i;
		}
		when '^':
			c = (*get)();
			if(c == EOF)
				return EOF;
			else
				return c & 0x1f;
		when 'c':
			do {
				c = (*get)();
			} while(c >= 0 && c <= 32 || c >= 127);
			if(c != '\\')
				return c;

		break;
		default:
			if(c >= 0 && c <= 32 || c >= 127) {
				c = (*get)();
				if(c != '\\')
					return c;
			} else
				return c;
		}
	}
}

/*
 * Get the next string forming a token on Stream st.
 */
int
getToken(st, token, type, sbase, stop)
Stream *st;
Object *token, *type;
char *sbase;
char *stop;
{
	int len;

	stream = st;
	get = io_fgetc;
	unget = io_fungetc;
	eof = io_feof;
	er_line_no = eSmallInt(st->s_lines);

	/* BUG?  Race conditions? */
	if(setjmp(interrupt_return_point))
		len = -2;
	else {
		InterruptAction = intRETURN;
		len = io_token(token, type, sbase, stop);
	}

	InterruptAction = intCOUNT;
	return len;
}

/*
 * Get the next string forming a token on st.
 */
int
tokenize(st, token, type, rest, sbase, stop)
char *st;
Object *token, *type;
char *sbase;
char *stop;
Object *rest;
{
	int bytes;

	stream = (Stream *)NULL;
	string = st;
	stringeof = 0;
	get = io_sgetc;
	unget = io_sungetc;
	eof = io_seof;

	bytes = io_token(token, type, sbase, stop);
	if(stringeof)
		*rest = NIL;
	else
		*rest = StarToString(string);
	return bytes;
}

/* Maximum length of the string of characters than makes a number. */
#define MAXNUMBERLENGTH 256

/*
 * Get the next string forming a token using (*get)().
 * Return it and its type.
 * Skip whitespace and comments.
 *
 * Return number of bytes allocated from sbase plus sizeof(Word)
 * to avoid having to consider whether (char *)H
 * points to the first free character on the heap or the sizeof(Word)'th.
 * (Am I being needlessly careful here?)
 * If eof is encountered in an awkward place, return -1.
 */
int
io_token(token, type, sbase, stop)
Object *token, *type;
char *sbase;
register char *stop;
{
	register int c;
	register char *s;
	int sign, real, r;
	register Word i;
					/* Leave a margin to avoid some overflow checks */
	char buf[MAXNUMBERLENGTH + 4];
	register char *b;				/* Pointer into buf */

	if((*eof)()) {
		Warning("Attempted to read past end of file");
		return -1;
	}

	c = skipjunk();
	if(stream != NULL)
		er_line_no = eSmallInt(stream->s_lines);
	s = sbase;
	if(c == EOF) {
		*type = StarToAtom(&SymEnd_of_file);
		*token = NIL;
		return 0;
	} else
		switch(charTypes[c]) {
		when 'a': {
			do {
				if(s > stop)
					Panic("String space overflow");
				*s++ = c;
				c = (*get)();
			} while(isalpha(c) || isdigit(c) || c == '_' || c == '$');
			(*unget)(c);
			*s = '\0';
			*token = StarToAtom(enterAtom(sbase, stab, (Atom *) NULL));
			*type = StarToAtom(&SymAtom);
			return 0;
		}
		when 'A': {
			do {
				if(s > stop)
					Panic("String space overflow");
				*s++ = c;
				c = (*get)();
			} while(isalpha(c) || isdigit(c) || c == '_' || c == '$');
			(*unget)(c);
			*s = '\0';
			*token = StarToString(sbase);
			*type = StarToAtom(&SymVar);
			return 1 + (s - sbase);
		}
		when '0':
			sign = '+';
number:											/* Yeuch! */
			b = buf;
			real = 0;
			r = 10;
			if(sign == '-')
				*b++ = '-';
			do {
				if(b > buf + MAXNUMBERLENGTH) {
					Warning("Number too long");
					return -1;
				}
				*b++ = c;
				c = (*get)();
			} while(isdigit(c));
			*b = '\0';						/* Don't increment b */
			i = (Word)atol(buf);
			if(c == '\'') {
				if(i < 0 || i > 36) {
					Warning("Radix out of range");
					return -1;
				} else if(i == 0) {
					i = (*get)();
					if(i == EOF) {
						Warning("Unterminated radix number");
						return -1;
					} else if(i == '\\') {
						i = charesc(i);
						if(i == EOF) {
							Warning("Unterminated radix number");
							return -1;
						}
					}
				} else {
					r = i;
					i = 0;
					c = (*get)();
					if(c == EOF) {
						Warning("Unterminated radix number");
						return -1;
					}
					for( ; ; ) {
						if(isdigit(c) && c - '0' < r)
							i = r * i + c - '0';
						else if(islower(c) && c - 'a' + 10 < r)
							i = r * i + c - 'a' + 10;
						else if(isupper(c) && c - 'A' + 10 < r)
							i = r * i + c - 'A' + 10;
						else {
							(*unget)(c);
							break;
						}
						c = (*get)();
					}
				}
			} else {
				if(c == '.') {
					c = (*get)();
					if(!isdigit(c)) {
						/*
						 * An integer followed by a '.' need not be a float.
						 */
						(*unget)(c);
						c = '.';
					} else {
						real = 1;
						*b++ = '.';
						for( ; isdigit(c); c = (*get)()) {
							if(b > buf + MAXNUMBERLENGTH) {
								Warning("Number too long");
								return -1;
							}
							*b++ = c;
						}
					}
				}

				if(c == 'e' || c == 'E') {
					real = 1;
					*b++ = c;
					c = (*get)();
					if(c == '+' || c == '-') {
						*b++ = c;
						c = (*get)();
					}
					if(!isdigit(c)) {
						/*
						 * An integer followed by an 'e' or an 'E'
						 * is forced to be a float.
						 */
						Warning("Exponent expected");
						return -1;
					}
					do {
						if(b > buf + MAXNUMBERLENGTH) {
							Warning("Number too long");
							return -1;
						}
						*b++ = c;
						c = (*get)();
					} while(isdigit(c));
				}

				(*unget)(c);
			}

			*type = StarToAtom(&SymNumber);
			if(real) {
				if(r != 10) {
					Warning("Float not in base 10");
					return -1;
				}
				*b = '\0';
				s = (char *) ALIGNFLOAT(s);
				*(Real *)s = atof(buf);
				*token = StarToFloat(s);
				s += sizeof(Real);
				return s - sbase;
			} else if(SmallInt(i)) {
				*token = MakeSmallInt(i);
				return 0;
			} else {
				*(Word *)s = i;
				*token = MakeInt32(s);
				return sizeof(Word);
			}
		when '"': {
			c = (*get)();
			if(c == '"') {
				c = (*get)();
				if(c != '"') {
					(*unget)(c);
					*token = NIL;
					*type = StarToAtom(&SymAtom);
					return 0;
				}
			}
			do {
				do {
					if(s > stop)
						Panic("String space overflow");
					if(c == '\\') {
						c = charesc(c);
						if(c == EOF) {
							Warning("Unterminated string");
							return -1;
						}
					}
					*s++ = c;
					c = (*get)();
				} while(c != '"' && c != EOF);
				if(c == EOF) {
					Warning("Unterminated string");
					return -1;
				}
				c = (*get)();
			} while(c == '"');
			(*unget)(c);
			*s = '\0';
			*token = StarToString(sbase);
			*type = StarToAtom(&SymString);
			return 1 + (s - sbase);
		}
		when '\'': {
			c = (*get)();
			if(c == '\'') {
				c = (*get)();
				if(c != '\'') {
					(*unget)(c);
					*token = StarToAtom(enterAtom("", stab, (Atom *) NULL));
					*type = StarToAtom(&SymQuoted);
					return 0;
				}
			}
			do {
				do {
					if(s > stop)
						Panic("String space overflow");
					if(c == '\\') {
						c = charesc(c);
						if(c == EOF) {
							Warning("Unterminated string");
							return -1;
						}
					}
					*s++ = c;
					c = (*get)();
				} while(c != '\'' && c != EOF);
				if(c == EOF) {
					Warning("Unterminated quoted atom");
					return -1;
				}
				c = (*get)();
			} while(c == '\'');
			(*unget)(c);
			*s = '\0';
			*token = StarToAtom(enterAtom(sbase, stab, (Atom *) NULL));
			*type = StarToAtom(&SymQuoted);
			return 0;
		}
		when '!': {
			if(c == '(' && skipped)
				*s++ = ' ';				/* Return ' (' for ( */
			*s++ = c;
			*s = '\0';
			*token = StarToAtom(enterAtom(sbase, stab, (Atom *) NULL));
			*type = StarToAtom(&SymAtom);
			return 0;
		}
		when '=': {
			if(c == '-' || c == '+') {
				sign = c;
				c = (*get)();
				if(isdigit(c))
					goto number;
				(*unget)(c);
				c = sign;
			}
			do {
				if(s > stop)
					Panic("String space overflow");
				*s++ = c;
				c = (get)();
				if(c == '/') {
					c = (*get)();
					(*unget)(c);
					if(c == '*') {
						/* Put the comment back to be read later. */
						(*unget)('/');
						c = ' ';
						break;
					} else {
						c = '/';
					}
				}
			} while(charTypes[c] == '=');
			if((isspace(c) || c == '%') && *sbase == '.' && s - sbase == 1) {
				*s++ = ' ';				/* Return '. ' for .<space> */
				*s = '\0';				/* Just in case! */
				if(c == '%')
					(*unget)(c);	/* Put the comment back to be read later. */
				*token = StarToAtom(&SymTerminator);
				*type = StarToAtom(&SymEnd);
				return 0;
			} else
				(*unget)(c);
			*s = '\0';
			*token = StarToAtom(enterAtom(sbase, stab, (Atom *) NULL));
			*type = StarToAtom(&SymAtom);
			return 0;
		}
		break;
		default:
			*token = MakeSmallInt(c);
			*type = StarToAtom(&SymJunk);
			return 0;
		}
}
