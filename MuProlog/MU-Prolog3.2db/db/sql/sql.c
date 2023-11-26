/*
sql.c

C routines to talk with UNIFY database.

flags:
	Trace: follow creation/destruction of pipes
	Watch: follow material in/out of pipes
	FCNTL: set buffers to work with system V
*/

#include "types.h"
#include "pred.h"
#include "sql.h"
#include <sys/ioctl.h>

#define SPACE		' '
#define MARKER		'|'
#define CR		'\n'
#define NIL		'\0'
#define TAB		'\t'

#define LEN		2000
#define SHORT		512
#define true		1
#define false		0

#define DB_FINISH	"?- db_end."
#define DB_ERROR	"?- db_error(\"db_end expected\")."

static DBQ dbq[MAXDBQ];

/*
	Defines and arrays for dealing with unify.
*/
#define ok_update(S)	(! strncmp(S, "recognized update", 17))
#define start_output(S)	(! strncmp(S, "--", 2))
#define no_output(S)	(! strncmp(S, "There were no records selected", 30))
#define end_output(S)	(! strncmp(S, "sql> ", 5))

#define DB_PATH		"DBPATH=                                              "
#define SPOOLER		"${SPOOLER=\"lpr\"}"
#ifndef UNIFY
#define UNIFY		"/stude/unify/lib"
#define SQL_PATH	"/stude/unify/bin"
#define SQL_EXEC	"/stude/unify/bin/SQL"
#endif

extern char **environ;
static char *env[100], init_env = false;

#define new(N)		((char *) malloc((N)+1))

/*
	Syntax error messages in UNIFY.
*/
struct {
	int length;
	char *mesg;
} errs[] =  {
	59, "A 'having' clause cannot contain nested aggregate functions",
	57, "A 'having' clause must be preceded by a 'group by' clause",
	48, "A 'having' clause requires an aggregate function",
	50, "A literal tuple contains the wrong number of items",
	57, "Aggregate functions are not allowed in the 'where' clause",
	51, "Aggregate function imbalance in the 'select' clause",
	65, "Aggregate functions may not be nested without a 'group by' clause",
	60, "Aggregate functions nested too deeply in the 'select' clause",
	64, "An aggregate function is required when using a 'group by' clause",
	58, "An expression cannot be compared with a literal tuple list",
	14, "Can't create '",
	11, "Fatal error",
	33, "Insufficient memory for sort line",
	25, "Invalid 'group by' clause",
	23, "Invalid 'having' clause",
	28, "Invalid 'select' list syntax",
	22, "Invalid 'where' clause",
	12, "Invalid date",
	29, "Invalid field specification: ",
	15, "Invalid field: ",
	19, "Invalid query block",
	58, "Invalid record type specification in the 'select' clause: ",
	21, "Invalid record type: ",
	12, "Invalid time",
	17, "Not enough memory",
	12, "Syntax error",
	12, "syntax error",
	11, "The field '",
	21, "Type conversion error",
	24, "Unrecognized sql command",
	52, "Warning: 'group by' and 'order by' in the same query",
	57, "Warning: 'unique may not be used with aggregate functions",
	16, "on or about line",
	0, (char *) NULL
}, extra_err[] = {	/* first word of message is a field name */
	24, "is an invalid field name",
	25, "is an invalid record type",
	0, (char *) NULL
};


/*
	Prolog function to initiate query from an external db.
*/
p_sql_query(t, l)
	Ptr t;
	levtype l;
{
	Ptr tq, td;
	levtype lq;
	int f;

	findbind((Ptr)targ(1, t), l, &td, &lq);
	findbind((Ptr)targ(2, t), l, &tq, &lq);
	findbind((Ptr)targ(3, t), l, &t, &l);
	if(ttype(t) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}
	if((f = mudd_req(atdict(td), tq, lq, DB_QYALL)) < 0)
		return(ERROR);
	nbind(t, l, ConsInt(f+MAXFILES));
	return(SUCCEED);
}


/*
	Prolog function to get next solution from database.
*/
p_sql_next(t, l)
	Ptr t;
	levtype l;
{
	char *read_next();

	Ptr ttr, rt, newt, tans;
	levtype ltr, lans, b;
	char *answer;

	findbind((Ptr)targ(1, t), l, &ttr, &ltr);
	if(ttype(ttr) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
	findbind((Ptr)targ(2, t), l, &tans, &lans);
	if(ttype(tans) != TVAR) {
		plerror(EEVAR);
		return(ERROR);
	}
	if(!(newt = stot(read_next(tnum(ttr)))))
		return(FAIL);
	g_rset(gtop) = (Int) (gbindings(gtop) + nvars);
	rsetend = grset(gtop);
	for(b = gbindings(gtop); b < (levtype) rsetend; b++) {
		b->btermp = NULL;
		b->blev = NULL;
	}
	*rsetend++ = RTERM;	/* reclaim term on backtracking */
	*rsetend++ = (Int) newt;
	addbind(tans, lans, newt, gbindings(gtop));
	return(SUCCEED);
}


/*
	Prolog function to wind up database query.
*/
p_sql_end(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
#ifdef Trace
	fprintf(stderr, "<- calling mudd_end, slot %d\n", tnum(t) - MAXFILES);
#endif Trace
	pfiles[tnum(t)] = NULL;
	mudd_end(tnum(t) - MAXFILES);
	return(SUCCEED);
}


/*
	Prolog function to abort database query.
*/
p_sql_abort(t, l)
	Ptr t;
	levtype l;
{
	findbind((Ptr)targ(1, t), l, &t, &l);
	if(ttype(t) != TNUM) {
		plerror(EDBQUERY);
		return(ERROR);
	}
	pfiles[tnum(t)] = NULL;
	mudd_abort(tnum(t) - MAXFILES);
	return(SUCCEED);
}


/*
	Prolog fn to modify external DB.
*/
p_sql_modify(t, l)
	Ptr t;
	levtype l;
{
	char *fgetstr(), *sdisplay();

	Ptr td;
	levtype l1;
	int slot;
	char str[SHORT], *tmp;
	FILE *stream;

	findbind((Ptr) targ(1, t), l, &td, &l1);
	findbind((Ptr) targ(2, t), l, &t, &l1);
	if(!IsAtom(td)) {
		plerror(EECONST);
		return(ERROR);
	}
#ifdef Trace
	tmp = sdisplay((Ptr) targ(1, t), l1, str);
	*(tmp) = NIL;
	fprintf(stderr, "<- calling modify, term %s\n", str);
#endif Trace
	if((slot = mudd_req(atdict(td), t, l1, DB_MOD)) < 0)
		return(ERROR);

	/* read term from answer pipe (ie. wait until modify has been done) */

	stream = dbq[slot].apipe;
	do {		/* skip dross */
		tmp = fgetstr(stream, str, SHORT);
#ifdef Watch
		fprintf(stderr, "(8) -> %s", str);
#endif Watch
	} while(tmp != (char *) NULL && !ok_update(tmp) && !syntax_error(tmp));
	if(!ok_update(tmp)) {
		fprintf(stderr, "Horrible error - modify failed\n");
		plerror(EDBQUERY);
		return(ERROR);
	}
	return(SUCCEED);
}


/*
	Request a pipe to talk with the given database with a query of the
	given type. Returns query number 0 to MAXDBQ-1, -1 if unable to
	make query.
*/
static mudd_req(db_int,t, l,type)
	int db_int;	/* database name (atdict reference) */
	Ptr t;		/* prolog term */
	levtype l;	/* level of term */
	char type;	/* the type of request DB_QY, DB_RT, etc */
{
	register q, qq;

	if(type == DB_QYALL) { /* partial match query - seek all answers */
			/* look for a passive process */
		for(q = 0; q < MAXDBQ; q++)
			if(dbq[q].db_int == db_int && dbq[q].flag == 'P') {
#ifdef Trace
				fprintf(stderr, "<- using passive [1])\n");
#endif Trace
				dbq_write(q, t, l, type);
				dbq[q].flag = 'S';
				return(q);
			}
			/* look for an empty slot */
		for(q = 0; q < MAXDBQ; q++)
			if(dbq[q].flag == '\0') {
				if(! dbq_new(db_int, q))
					return(-1);
#ifdef Trace
				fprintf(stderr, "<- using new [1])\n");
#endif Trace
				dbq_write(q, t, l, type);
				dbq[q].flag = 'S';
				dbq[q].db_int = db_int;
				return(q);
			}
			/* no slot available */
		plerror(EDBNOSLOT);
		return(-1);
	} else { /* modify */
			/* look for a passive process */
		for(q = 0 ; q < MAXDBQ ; q++)
			if(dbq[q].db_int == db_int && dbq[q].flag == 'P') {
#ifdef Trace
				fprintf(stderr, "<- using passive [2]\n");
#endif Trace
				dbq_write(q, t, l, type);
				return(q);
			}
			/* look for an empty slot */
		for(q = 0; q < MAXDBQ; q++) {
			if(dbq[q].flag == '\0') {
				if(! dbq_new(db_int, q))
					return(-1);
#ifdef Trace
				fprintf(stderr, "<- using new [2]\n");
#endif Trace
				dbq_write(q, t, l, type);
				dbq[q].flag = 'P';
				dbq[q].db_int = db_int;
				return(q);
			}
		}
			/* no slot available */
		plerror(EDBNOSLOT);
		return(-1);
	}
}


/*
	Set up environment for called process. Must be a complete copy of
	the cuurent environment, plus some pathnames etc.
*/
do_init_env()
{
	int i;
	char **tmp;

	init_env = true;
	env[0] = DB_PATH;
	env[1] = SPOOLER;
	env[2] = UNIFY;
	for(tmp = environ, i = 3 ; *tmp != (char *) NULL ; tmp++, i++)
		if(!strncmp(*tmp, "PATH", 4)) {
			env[i] = new(strlen(*tmp)+strlen(SQL_PATH)+2);
			sprintf(env[i], "%s:%s", *tmp, SQL_PATH);
		} else {
			env[i] = new(strlen(*tmp));
			strcpy(env[i], *tmp);
		}
	env[i] = (char *) NULL;
}


#ifndef BSD4
static char in_buf[LEN];
#endif


/*
	Fork a new UNIFY process.
*/
static dbq_new(db_int, slot)
	int db_int;
	int slot;
{
	int apd[2], qpd[2];
	char str[SHORT];

#ifdef Trace
	fprintf(stderr, "<- Opening pipe, slot %d, db %s\n",slot,dname(db_int));
#endif Trace
	if(!init_env)
		do_init_env();
	sprintf(&env[0][7], "%s", dname(db_int));
	if(pipe(qpd) < 0) {
		plerror(EDBNOPIPE);
		return(0);
	}
	if(pipe(apd) < 0) {
		close(qpd[0]);
		close(qpd[1]);
		plerror(EDBNOPIPE);
		return(0);
	}
	if(!fork()) {
		close(0);
		if(dup(qpd[0]) != 0)
			_exit(1);
		close(qpd[0]); close(qpd[1]);
		close(1);
		if(dup(apd[1]) != 1)
			_exit(1);
		close(apd[0]); close(apd[1]);
		close(2);	/* map stderr to stdout */
		if(dup(1) != 2)
			_exit(1);
		sprintf(str, "/bin/rm -f %s/LOCK*", dname(db_int));
		system(str);
		execle(SQL_EXEC, "SQL", 0, env);
		printf("?-db_error(dberr_exec).\n");
		fflush(stdout);
		_exit(1);
	}
	close(qpd[0]); close(apd[1]);
	dbq[slot].qpipe = fdopen(qpd[1], "w");
	dbq[slot].apipe = fdopen(apd[0], "r");
#ifndef BSD4
	setvbuf(dbq[slot].qpipe, in_buf, _IOLBF, LEN);
#endif /* needs doing properly */
	dbq[slot].db_int = 0;
	return(1);
}


/*
	Write query down pipe.
*/
static dbq_write(slot, t, l, type)
	int slot;
	Ptr t;
	levtype l;
	char type;
{
	char *sdisplay();

	register FILE *fp;
	char str[SHORT], *end;

	fp = dbq[slot].qpipe;
#ifdef Trace
	fprintf(stderr, "<- writing query to slot %d, flag (%c), type %c\n", slot, dbq[slot].flag, type);
#endif Trace
	if(dbq[slot].flag == '\0') {	/* new process */
		fprintf(fp, "lines 32767\n");
#ifdef Watch
		fprintf(stderr, "<- lines 32767\n");
#endif Watch
	}
	if(type == DB_QYALL)	/* breaking up "sequel(Vars, Exp)" */
		end = sdisplay((Ptr) targ(2, t), l, str);
	else		/* breaking up "modify(Exp)" */
		end = sdisplay((Ptr) targ(1, t), l, str);
	*(end-1) = NIL;
	fprintf(fp, "%s/\n", str+1);
#ifdef Watch
	fprintf(stderr, "<- %s/\n", str+1);
#endif Watch
	fflush(fp);
}


/*
	Get next record from pipe. Construct
		sequel([answers], _)
 	from result.
*/
char *read_next(slot)
int slot;
{
	char *fgetstr();

	char str[LEN], buf[LEN], *end, *begin, *tmp;
	int num;
	FILE *stream;

	slot -= MAXFILES;
	stream = dbq[slot].apipe;
	if(dbq[slot].flag == 'S') {	/* first call - skip dross */
		do {
			tmp = fgetstr(stream, str, LEN);
#ifdef Watch
			fprintf(stderr, "(1) -> %s", str);
#endif Watch
		} while(tmp != (char *) NULL && !start_output(tmp) &&
			!no_output(tmp) && !syntax_error(tmp));
		if(no_output(tmp)) { /* no records - read to next prompt */
			do {
				tmp = fgetstr(stream, str, LEN);
#ifdef Watch
				fprintf(stderr, "(2) -> %s", str);
#endif Watch
			} while(!end_output(tmp));
			/*
			*	fgetstr(stream, str, LEN);
			*/
#ifdef Watch
			fprintf(stderr, "(10) -> %s", str);
#endif Watch
			return(DB_FINISH);
		}
		if(!start_output(tmp))	/* must have error */
			if(tmp == (char *) NULL)
				return(DB_ERROR);
			else {
				sprintf(buf, "?- db_error(\"%s\").", tmp);
				return(buf);
			}
		dbq[slot].flag = 'A';
	}
	tmp = fgetstr(stream, str, LEN);
	if(tmp == (char *) NULL || syntax_error(tmp) || end_output(tmp)) {
#ifdef Watch
		fprintf(stderr, "(3) -> %s", str);
#endif Watch
		if(end_output(tmp))
			return(DB_FINISH);
		else if(syntax_error(tmp)) {
			sprintf(buf, "?- db_error(\"%s\").", tmp);
			return(buf);
		} else
			return(DB_ERROR);
	}
	if(*str == CR) {	/* skip header at top of new page */
		while(!start_output(str)) {
#ifdef Watch
			fprintf(stderr, "(5) -> %s", str);
#endif Watch
			fgetstr(stream, str, LEN);
		}
		fgetstr(stream, str, LEN);
	}
#ifdef Watch
	fprintf(stderr, "(6) -> %s", str);
#endif Watch
		/* transform line from UNIFY into correct format */
	sprintf(buf, "sequel([");
	for(begin = tmp = str ; *tmp != CR ; tmp++)
		if(*tmp == MARKER) {
			*tmp = NIL;
			if(digits(begin))
				sprintf(buf, "%s%d, ", buf, atoi(begin));
			else
				sprintf(buf, "%s\"'%s'\", ", buf, begin);
			begin = tmp+1;
		}
	*tmp = NIL;
	if(digits(begin))
		sprintf(buf, "%s%d], _).\n", buf, atoi(begin));
	else
		sprintf(buf, "%s\"'%s'\"], _).\n", buf, begin);
#ifdef Watch
	fprintf(stderr, "(7) -> %s", buf);
#endif Watch
	return(buf);
}


digits(str)
	char *str;
{
	for( ; *str != NIL ; str++)
		if(*str != SPACE && *str != '+' && *str != '-' && *str != TAB &&
				(*str < '0' || *str > '9'))
			return(0);
	return(1);
}


/*
	Abort pipe.
*/
static mudd_abort(slot)
	int slot;
{
#ifdef Trace
	fprintf(stderr, "<- calling mudd_abort, slot %d, flag (%c)\n", slot, dbq[slot].flag);
#endif Trace
	if(dbq[slot].flag == 'A' || dbq[slot].flag == 'S') {
		fclose(dbq[slot].qpipe);
		close(dbq[slot].apipe);
		/* wait(0); */			/* ?????? */
		dbq[slot].flag = '\0';
		dbq[slot].db_int = 0;
	}
}



/*
	Returns true if str is in the table of error messages.
*/
syntax_error(str)
char *str;
{
	char *strip_word();

	int i;
	char *tmp;

	for(i = 0 ; errs[i].length != 0 ; i++)
		if(*str == *(tmp = errs[i].mesg))	/* quick check */
			if(!strncmp(str, tmp, errs[i].length))
				return(true);
	for(i = 0 ; extra_err[i].length != 0 ; i++) {
		str = strip_word(str);
		if(*str == *(tmp = extra_err[i].mesg))	/* quick check */
			if(!strncmp(str, tmp, extra_err[i].length))
				return(true);
	}
	return(false);
}


/*
	Remove the first word from str, return the remaining string.
*/
char *strip_word(str)
char *str;
{
	while(*str != NIL && *str != SPACE && *str != TAB)
		str++;
	while(*str == SPACE || *str == TAB)
		str++;
	return(str);
}


/*
	Get string from given stream. If the first five characters are
	"sql> ", stop; otherwise continue up to a carraige return or LEN
	characters.
*/
char *fgetstr(stream, buf, len)
	FILE *stream;
	int len;
	char buf[];
{
	int i;
	char *tmp = buf;

	for(i = 0 ; i < LEN ; i++, tmp++) {
		/*	read(fd, tmp, 1); */
		*tmp = getc(stream);
		if(*tmp == ' ' && i == 4)
			if(!strncmp(buf, "sql>", 4)) {
				buf[i+1] = NIL;
				return(buf);
			}
		if(*tmp == CR) {
			*(tmp+1) = NIL;
			return(buf);
		}
	}
	buf[LEN-1] = NIL;
	return(buf);
}
