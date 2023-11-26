/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Justin Zobel
 */

/*
sql.c

C routines to talk with UNIFY database.

flags:
	Trace: follow creation/destruction of pipes
	Watch: follow material in/out of pipes
	FCNTL: set buffers to work with system V
*/

#include "mu.h"
#include "db.h"
#include <sys/ioctl.h>

/* TEMPORARY */
#if 0
#define Trace
#define Watch
#define	dbug(msg,obj)	{ fprintf(stderr,"%s:",msg);\
			  displayTerm(stderr,(obj)); fprintf(stderr,"\n"); }
#else
#define dbug(x,y)
#endif

#define SPACE		' '
#define MARKER		'|'
#define CR		'\n'
#define NUL		'\0'
#define TAB		'\t'

#define LEN		2000
#define SHORT		512
#define true		1
#define false		0

#define DB_FINISH	"?- db_end.\n"
#define DB_ERROR	"?- db_error(\"db_end expected\").\n"

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
#define	LOCKFILE	"lockfile"
#define	DDLOCKFILE	"ddlockfile"
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

static int mudd_req(), dbq_new();
static void dbq_write(), mudd_abort();

/* Call the various sql functions from Prolog. */
int
p_sql(f, X1, X2, X3)
int f;
Object *X1, *X2, *X3;
{
	switch(f) {
	when 0:
		return(p_sql_query(*X1, *X2, X3));
	when 1:
		return(p_sql_next(*X1, X2));
	when 2:
		return(p_sql_end(*X1));
	when 3:
		return(p_sql_modify(*X1, *X2));
	when 4:
		return(p_sql_abort(*X1));
	break;
	default:
		panic("Illegal SQL service requested.");
		return(0);
	}
}

/*
	Prolog function to initiate query from an external db.
*/
int
p_sql_query(db, qry, Qdesc)
register Object db, qry, *Qdesc;
{
	int f;
	char *query;

	DeRef(db);
	if(!IsAtom(db)) {
		panic("sql_query: first arg not an atom");
		return(FAIL);
	}

	DeRef(qry);
	if (!IsString(qry)) {
		panic("sql_query: second arg not a string");
		return(FAIL);
	}
	query = StringToChars(qry);

	DeRef(*Qdesc);
	/* Don't need to check IsVar on var args */

	if((f = mudd_req(db, query, DB_QYALL)) < 0) {
		panic("sql_query: can't commence query");
		return(FAIL);
	}
	*Qdesc = MakeSmallInt(f);
	return(SUCCEED);
}


/*
	Prolog function to get next solution from database.
*/
int
p_sql_next(trans, Next)
register Object trans, *Next;
{
	char *answer, *read_next();

	DeRef(trans);
	if (!IsSmallInt(trans)) {
		panic("sql_next: bad database descriptor");
		return(FAIL);
	}

	DeRef(*Next);
	/* Don't need to check IsVar on var args */

	if((answer = read_next(eSmallInt(trans))) == NULL)
		return(FAIL);
	*Next = MakeString(answer);
	return(SUCCEED);
}


/*
	Prolog function to wind up database query.
*/
int
p_sql_end(trans)
register Object trans;
{
	DeRef(trans);
	if (!IsSmallInt(trans)) {
		panic("sql_next: bad database descriptor");
		return(FAIL);
	}
#ifdef Trace
	fprintf(stderr, "<- calling mudd_end, slot %d\n", eSmallInt(trans));
#endif /* Trace */
	mudd_end(eSmallInt(trans));
	return(SUCCEED);
}


/*
	Prolog function to abort database query.
*/
int
p_sql_abort(trans)
register Object trans;
{
	DeRef(trans);
	if (!IsSmallInt(trans)) {
		panic("sql_next: bad database descriptor");
		return(FAIL);
	}
	mudd_abort(eSmallInt(trans));
	return(SUCCEED);
}


/*
	Prolog fn to modify external DB.
*/
int
p_sql_modify(db, cmd)
register Object db, cmd;
{
	char *fgetstr(), *sdisplay();

	int slot;
	char *cmdstr;
	char *tmp, str[SHORT];
	FILE *stream;

	DeRef(db);
	if(!IsAtom(db)) {
		panic("sql_modify: first arg not a string");
		return(FAIL);
	}

	DeRef(cmd);
	if (!IsString(cmd)) {
		panic("sql_modify: second arg not a string");
		return(FAIL);
	}
	cmdstr = StringToChars(cmd);

	sprintf(str, "%s/%s", AtomToString(db), LOCKFILE);
	if(access(str, 6)) {
		fprintf(stderr,"Warning: no write permission on database\n");
		return(FAIL);
	}
	sprintf(str, "%s/%s", AtomToString(db), DDLOCKFILE);
	if(access(str, 6)) {
		fprintf(stderr,"Warning: no write permission on database\n");
		return(FAIL);
	}

#ifdef Trace
	dbug("<- calling modify, term", cmd);
#endif /* Trace */
	if((slot = mudd_req(db, cmdstr, DB_MOD)) < 0) {
		panic("sql_modify: can't do modification");
		return(FAIL);
	}

	/* read term from answer pipe (ie. wait until modify has been done) */

	stream = dbq[slot].apipe;
	do {		/* skip dross */
		tmp = fgetstr(stream, str, SHORT);
#ifdef Watch
		fprintf(stderr, "(8) -> %s", str);
#endif /* Watch */
	} while(tmp != (char *) NULL && !ok_update(tmp) && !syntax_error(tmp));
	if(!ok_update(tmp)) {
		panic("sql_modify: Horrible error - modify failed");
		return(FAIL);
	}
}


/*
	Request a pipe to talk with the given database with a query of the
	given type. Returns query number 0 to MAXDBQ-1, -1 if unable to
	make query.
*/
static int
mudd_req(dbref, str, type)
int dbref;	/* database name */
char *str;	/* SQL command */
char type;	/* the type of request DB_QY, DB_RT, etc */
{
	register q, qq;

	if(type == DB_QYALL) { /* partial match query - seek all answers */
			/* look for a passive process */
		for(q = 0; q < MAXDBQ; q++)
			if(dbq[q].dbref == dbref && dbq[q].flag == 'P') {
#ifdef Trace
				fprintf(stderr, "<- using passive [1])\n");
#endif /* Trace */
				dbq_write(q, str, type);
				dbq[q].flag = 'S';
				return(q);
			}
			/* look for an empty slot */
		for(q = 0; q < MAXDBQ; q++)
			if(dbq[q].flag == '\0') {
				if(! dbq_new(dbref, q))
					return(-1);
#ifdef Trace
				fprintf(stderr, "<- using new [1])\n");
#endif /* Trace */
				dbq_write(q, str, type);
				dbq[q].flag = 'S';
				dbq[q].dbref = dbref;
				return(q);
			}
			/* no slot available */
		panic("No slots for SQL query");
		return(-1);
	} else { /* modify */
			/* look for a passive process */
		for(q = 0 ; q < MAXDBQ ; q++)
			if(dbq[q].dbref == dbref && dbq[q].flag == 'P') {
#ifdef Trace
				fprintf(stderr, "<- using passive [2]\n");
#endif /* Trace */
				dbq_write(q, str, type);
				return(q);
			}
			/* look for an empty slot */
		for(q = 0; q < MAXDBQ; q++) {
			if(dbq[q].flag == '\0') {
				if(! dbq_new(dbref, q))
					return(-1);
#ifdef Trace
				fprintf(stderr, "<- using new [2]\n");
#endif /* Trace */
				dbq_write(q, str, type);
				dbq[q].flag = 'P';
				dbq[q].dbref = dbref;
				return(q);
			}
		}
			/* no slot available */
		panic("No slots for SQL query");
		return(-1);
	}
}


/*
	Set up environment for called process. Must be a complete copy of
	the current environment, plus some pathnames etc.
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


#ifdef FCNTL
static char in_buf[LEN];
#endif


/*
	Fork a new UNIFY process.
*/
static int
dbq_new(dbref, slot)
int dbref;
int slot;
{
	int apd[2], qpd[2];
	char str[SHORT];
	extern FILE *fdopen();

#ifdef Trace
	fprintf(stderr, "<- Opening pipe, slot %d, db %s\n", slot, AtomToString(dbref));
#endif /* Trace */
	if(!init_env)
		do_init_env();
	sprintf(&env[0][7], "%s", AtomToString(dbref));
	if(pipe(qpd) < 0) {
		return(0);
	}
	if(pipe(apd) < 0) {
		close(qpd[0]);
		close(qpd[1]);
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
		sprintf(str, "/bin/rm -f %s/LOCK*", AtomToString(dbref));
		system(str);
		execle(SQL_EXEC, "SQL", 0, env);
		printf("?-db_error(dberr_exec).\n");
		fflush(stdout);
		_exit(1);
	}
	close(qpd[0]); close(apd[1]);
	dbq[slot].qpipe = fdopen(qpd[1], "w");
	dbq[slot].apipe = fdopen(apd[0], "r");
#ifdef FCNTL
	setvbuf(dbq[slot].qpipe, in_buf, _IOLBF, LEN);
#endif
	dbq[slot].dbref = 0;
	return(1);
}


/*
	Write query down pipe.
*/
static void
dbq_write(slot, qry, type)
int slot;
char *qry;
char type;
{
	register FILE *fp;

	fp = dbq[slot].qpipe;
#ifdef Trace
	fprintf(stderr, "<- writing query to slot %d, flag (%c), type %c\n", slot, dbq[slot].flag, type);
#endif /* Trace */
	if(dbq[slot].flag == '\0') {	/* new process */
		fprintf(fp, "lines 32767\n");
#ifdef Watch
		fprintf(stderr, "<- lines 32767\n");
#endif /* Watch */
	}
	fprintf(fp, "%s/\n", qry);
#ifdef Watch
	fprintf(stderr, "<- %s/\n", qry);
#endif /* Watch */
	fflush(fp);
}


static char str[LEN], buf[LEN];

/*
	Get next record from pipe. Construct
		sequel([answers], _)
 	from result.
*/
char *
read_next(slot)
int slot;
{
	char *fgetstr(), *quotify(), *strip_junk();

	char *end, *begin, *tmp;
	int num;
	FILE *stream;

	stream = dbq[slot].apipe;
	if(dbq[slot].flag == 'S') {	/* first call - skip dross */
		do {
			tmp = fgetstr(stream, str, LEN);
#ifdef Watch
			fprintf(stderr, "(1) -> %s", str);
#endif /* Watch */
		} while(tmp != (char *) NULL && !start_output(tmp) &&
			!no_output(tmp) && !syntax_error(tmp));
		if(no_output(tmp)) { /* no records - read to next prompt */
			do {
				tmp = fgetstr(stream, str, LEN);
#ifdef Watch
				fprintf(stderr, "(2) -> %s", str);
#endif /* Watch */
			} while(!end_output(tmp));
			/*
			*	fgetstr(stream, str, LEN);
			*/
#ifdef Watch
			fprintf(stderr, "(10) -> %s", str);
#endif /* Watch */
			return(DB_FINISH);
		}
		if(!start_output(tmp))	/* must have error */
			if(tmp == (char *) NULL)
				return(DB_ERROR);
			else {
				sprintf(buf, "?- db_error(\"%s\").\n", tmp);
				return(buf);
			}
		dbq[slot].flag = 'A';
	}
	tmp = fgetstr(stream, str, LEN);
	if(tmp == (char *) NULL || syntax_error(tmp) || end_output(tmp)) {
#ifdef Watch
		fprintf(stderr, "(3) -> %s", str);
#endif /* Watch */
		if(end_output(tmp))
			return(DB_FINISH);
		else if(syntax_error(tmp)) {
			sprintf(buf, "?- db_error(\"%s\").\n", tmp);
			return(buf);
		} else
			return(DB_ERROR);
	}
	if(*str == CR) {	/* skip header at top of new page */
		while(!start_output(str)) {
#ifdef Watch
			fprintf(stderr, "(5) -> %s", str);
#endif /* Watch */
			fgetstr(stream, str, LEN);
		}
		fgetstr(stream, str, LEN);
	}
#ifdef Watch
	fprintf(stderr, "(6) -> %s", str);
#endif /* Watch */
		/* transform line from UNIFY into correct format */
	sprintf(buf, "sequel([");
	for(begin = tmp = str ; *tmp != CR ; tmp++)
		if(*tmp == MARKER) {
			*tmp = NUL;
			if(digits(begin))
				sprintf(buf, "%s%d, ", buf, atoi(begin));
			else
				sprintf(buf,"%s\"%s\", ", buf,
					quotify(strip_junk(begin, tmp)));
			begin = tmp+1;
		}
	*tmp = NUL;
	if(digits(begin))
		sprintf(buf, "%s%d], _).\n", buf, atoi(begin));
	else
		sprintf(buf, "%s\"%s\"], _).\n", buf,
			quotify(strip_junk(begin, tmp)));
#ifdef Watch
	fprintf(stderr, "(7) -> %s", buf);
#endif /* Watch */
	return(buf);
}


char *
strip_junk(st, end)
	char *st, *end;
{
	for( ; *st == ' ' ; st++)
		;
	for(end-- ; *end == ' ' ; end--)
		;
	*(end+1) = NUL;
	return(st);
}

/*
	Convert `"' withing a string to `""'.
*/
char *
quotify(str)
	char *str;
{
	register char *tmp;
	static char buf[LEN];

	for(tmp = buf ; (*tmp = *str) != NUL ; str++, tmp++)
		if(*str == '"')
			*++tmp = '"';
	return(buf);
}


int
digits(str)
	char *str;
{
	for( ; *str != NUL ; str++)
		if(*str != SPACE && *str != '+' && *str != '-' && *str != TAB &&
				(*str < '0' || *str > '9'))
			return(0);
	return(1);
}


/*
	Abort pipe.
*/
static void
mudd_abort(slot)
	int slot;
{
#ifdef Trace
	fprintf(stderr, "<- calling mudd_abort, slot %d, flag (%c)\n", slot, dbq[slot].flag);
#endif /* Trace */
	if(dbq[slot].flag == 'A' || dbq[slot].flag == 'S') {
		fclose(dbq[slot].qpipe);
		close(dbq[slot].apipe);
		/* wait(0); */			/* ?????? */
		dbq[slot].flag = '\0';
		dbq[slot].dbref = 0;
	}
}



/*
	Returns true if str is in the table of error messages.
*/
int
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
char *
strip_word(str)
char *str;
{
	while(*str != NUL && *str != SPACE && *str != TAB)
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
char *
fgetstr(stream, buf, len)
FILE *stream;
char buf[];
int len;
{
	int i;
	char *tmp = buf;

	for(i = 0 ; i < LEN ; i++, tmp++) {
		*tmp = getc(stream);
		if(*tmp == ' ' && i == 4)
			if(!strncmp(buf, "sql>", 4)) {
				buf[i+1] = NUL;
				return(buf);
			}
		if(*tmp == CR) {
			*(tmp+1) = NUL;
			return(buf);
		}
	}
	buf[LEN-1] = NUL;
	return(buf);
}
