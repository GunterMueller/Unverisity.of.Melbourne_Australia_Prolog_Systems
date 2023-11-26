/*
	initdb.c

	Given an sql/unify database in the given directory, generate
	appropriate .con and .rules files

	Copyright (C) 1986, The University of Melbourne

*/

#define SPACE		' '
#define MARKER		'|'
#define CR		'\n'
#define NIL		'\0'
#define TAB		'\t'
#define	RWX		(04|02|01)

#define LEN		512
#define true		1
#define false		0

#include <stdio.h>
#include <fcntl.h>
#ifdef BSD4
#include <sys/file.h>
#endif

/*
	Defines and arrays for dealing with unify.
*/
#define end_output(S)	(! strncmp(S, "sql> ", 5))

#define DB_PATH		"DBPATH=                                              "
#define SPOOLER		"${SPOOLER=\"lpr\"}"
#ifndef UNIFY
#define UNIFY		"/stude/unify/lib"
#define SQL_PATH	"/stude/unify/bin"
#define SQL_EXEC	"/stude/unify/bin/SQL"
#endif
#ifndef NC
#define	NC		"/usr/melb/nc"
#endif

extern char **environ;
static char *env[100];

#define new(N)		((char *) malloc((N)+1))

FILE	*qpipe, *apipe, *con, *rules;

/*
	Fork unify process, ask it for relations/fields info.
*/
main(argc, argv)
	int argc;
	char *argv[];
{
	char *relations[LEN], **tmp;
	char cmd[LEN];

	if(argc != 2) {
		printf("Usage : initdb db\n");
		exit(1);
	}
	if (access(argv[1], RWX) < 0) {
		printf("No access permissions on %s\n", argv[1]);
		exit(1);
	}
	init_env();
	open_sql(argv[1]);
	get_relations(relations);
	for(tmp = relations ; *tmp != (char *) NULL ; tmp++)
		get_fields(*tmp);
	fclose(con); fclose(rules);
	sprintf(cmd, "cp %s/.rules %s/.rules.nl", argv[1], argv[1]);
	system(cmd);
	sprintf(cmd, "%s -c %s/.rules.nl", NC, argv[1]);
	system(cmd);
}


init_env()
{
	char **tmp;
	int i;

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
	Fork unify process with appropriate environment.
*/
open_sql(db_name)
	char *db_name;
{
	int fd, apd[2], qpd[2];
	char str[LEN];

	sprintf(&env[0][7], "%s", db_name);
	if(pipe(qpd) < 0) {
		printf("Pipe error\n");
		exit(1);
	}
	if(pipe(apd) < 0) {
		printf("Pipe error\n");
		exit(1);
	}
	if(!fork()) {
		close(0);
		if(dup(qpd[0]) != 0)
			exit(1);
		close(qpd[0]); close(qpd[1]);
		close(1);
		if(dup(apd[1]) != 1)
			exit(1);
		close(apd[0]); close(apd[1]);
		close(2);	/* map stderr to stdout */
		if(dup(1) != 2)
			exit(1);
		execle(SQL_EXEC, "SQL", 0, env);
		printf("Exec error\n");
		exit(1);
	}
	close(qpd[0]); close(apd[1]);
	qpipe = fdopen(qpd[1], "w");
	apipe = fdopen(apd[0], "r");
#ifdef FCNTL
	setvbuf(qpipe, in_buf, _IOLBF, LEN);
#endif
	sprintf(str, "%s/.con", db_name);
	if((fd = open(str, O_WRONLY | O_TRUNC | O_CREAT, 0644)) < 0) {
		printf("Open error on %s\n", str);
		exit(1);
	}
	con = fdopen(fd, "w");
	sprintf(str, "%s/.rules", db_name);
	if((fd = open(str, O_WRONLY | O_TRUNC | O_CREAT, 0644)) < 0) {
		printf("Open error on %s\n", str);
		exit(1);
	}
	rules = fdopen(fd, "w");
		/* Get clause indexing on predicates in .rules file */
	fprintf(rules, "?- field_names(X, Y) when X.\n");
	fprintf(rules, "?- field_defn(X, Y, Z) when X or Y.\n");
	return(1);
}


#define utol(C)		((C) - 'A' + 'a')
#define ucase(C)	((C) >= 'A' && (C) <= 'Z')

void utol_str(str)
	char *str;
{
	for( ; *str != NIL ; str++)
		if(ucase(*str))
			*str = utol(*str);
}


/*
	Copy the first word in str2 to str1, return the remained of str2
	with leading white space removed.
*/
char *remove_word(str1, str2)
	char *str1, *str2;
{
	while((*str1 = *str2) != SPACE && *str1 != CR && *str1 != TAB)
		str1++, str2++;
	*str1 = NIL;
	while(*str2 == TAB || *str2 == CR || *str2 == SPACE)
		str2++;
	return(str2);
}


/*
	Get string from given stream. If the first five characters are
	"sql> ", stop; otherwise continue up to a carraige return or LEN
	characters.
*/
char *fgetstr(stream, buf, len)
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


/*
	Get names of relations in sql database.
*/
get_relations(relations)
	char **relations;
{
	char *strip_sql();

	char str[LEN], buf[LEN], *tmp;
	int i = 0;

	fprintf(qpipe, "records\n");
	fflush(qpipe);
	do {		/* skip dross */
		tmp = fgetstr(apipe, str, LEN);
	} while(!end_output(tmp));
		/* each word is the name of a relation */
	tmp = fgetstr(apipe, str, LEN);
	while(!end_output(tmp)) {
		while(*tmp != NIL) {
			tmp = remove_word(buf, tmp);
			relations[i] = (char *) malloc(strlen(buf)+1);
			strcpy(relations[i++], buf);
		}
		tmp = fgetstr(apipe, str, LEN);
	}
	relations[i] = (char *) NULL;
}


/*
	Structure to store each field type.
*/
typedef struct {
	char *name;
	char *type;
	int length;
} field;

/*
	Get all field names for the given relation. The field name is the
	first word in each line.
*/
get_fields(relation)
	char *relation;
{
	char *strip_sql();

	char *tmp, str[LEN], buf[LEN], outbuf[LEN];
	field fields[100];
	int i = 0, j;

	fprintf(qpipe, "fields %s\n", relation);
	fflush(qpipe);
	do {
		tmp = fgetstr(apipe, str, LEN);
	} while(tmp != (char *) NULL && *tmp != CR);
	tmp = fgetstr(apipe, str, LEN);
		/* Put entry in .con */
	sprintf(outbuf, "relation(%s(", relation);
	while(!end_output(tmp)) {
			/* get field name */
		tmp = remove_word(buf, tmp);
		sprintf(outbuf, "%sF_%s", outbuf, buf);
		fields[i].name = (char *) malloc(strlen(buf)+1);
		strcpy(fields[i].name, buf);
			/* get field type */
		tmp = remove_word(buf, tmp);
		utol_str(buf);
		fields[i].type = (char *) malloc(strlen(buf)+1);
		strcpy(fields[i].type, buf);
			/* get field length */
		fields[i].length = atoi(tmp);
			/* read next line */
		tmp = fgetstr(apipe, str, LEN);
		if(!end_output(tmp))
			strcat(outbuf, ", ");
		i++;
	}
	if(outbuf[strlen(outbuf)-1] == '(') {
		fprintf(stderr, "Error: %s has no attributes.\n", relation);
		return;
	} else
		fprintf(con, "%s), sql).\n", outbuf);
		
		/* Put entries in .rules */
		/* field_names(Pred-name, List-of-argnames) */
	fprintf(rules, "field_names(%s, [", relation);
	for(j = 0 ; j < i ; j++) {
		fprintf(rules, "\"%s\"", fields[j].name);
		if(j < i - 1)
			fprintf(rules, ", ");
	}
	fprintf(rules, "]).\n");
	for(j = 0 ; j < i ; j++)
			/* field_defn(Field-name, Pred, Type-Length) */
		fprintf(rules, "field_defn(\"%s\", \"%s\", %s-%d).\n", fields[j].name, relation, fields[j].type, fields[j].length);
}
