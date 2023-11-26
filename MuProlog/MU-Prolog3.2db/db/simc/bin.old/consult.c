/*
 * consult.c  -  deductive database package
 *
 * This program inserts a bunch of records into a relation
 *
 * $Header: consult.c,v 1.2 85/06/08 16:54:17 jas Exp $
 * $Log:	consult.c,v $
 * Revision 1.2  85/06/08  16:54:17  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/06/08  16:53:32  jas
 * Initial revision
 * 
 * Revision 1.1  85/05/26  13:03:53  jas
 * Initial revision
 * 
 *
 */

#include "muddlib.h"
#ifdef BSD
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef DBUG
extern	Word	debug_flag;
#endif DBUG

int	nerrs = 0;
int	nasserts = 0;
#ifdef BSD
struct	rusage	start, current;
#else
struct	tms	start, current;
#endif

main(argc, argv)
Int argc;
String argv[];
{
	char	*db_name;
	char	*c, fact[200];
	int	how_many();
	int	how_often = 500;
	int	exit_flag = 0;

#ifdef DBUG
	int	debug_when = 0;
	int	on_when = 0;

	debug_flag = 0;
#endif DBUG

	signal(SIGQUIT,how_many);
#ifdef BSD
	getrusage(RUSAGE_SELF,&start);
#else
	times(&start);
#endif

	if (argc < 2) {
		fprintf(stderr, "Usage: [options] %s db\n", argv[0]);
		exit(1);
	}
	while (argc > 1) {
		argv++;
		if (argv[0][0] != '-')
			db_name = argv[0];
		else {
			switch (argv[0][1])
			{
#ifdef DBUG
			when 'a': debug_when |= ADD_DBUG;
			when 'd': debug_when |= DESC_DBUG;
#endif DBUG
			when 'x': exit_flag = 1;
			when 'f': sscanf(&(argv[0][2]), "%d", &how_often);
				  if (how_often <= 0) how_often = 500;
			}
#ifdef DBUG
			if (debug_when != 0 && argv[0][2] != '\0')
				sscanf(&(argv[0][2]), "%d", &on_when);
#endif DBUG
		}
		argc--;
	}
	while (gets(fact) != NULL) {
		if (fact[0] == '%')
			continue;
		or (fact[0] == ChNULL)
			continue;
		else {
			if ((c = rindex(fact,'.')) == NULL || *(c-1) != ')')
				fprintf(stderr, "Bad: %s\n", fact);
			else {
#ifdef DBUG
				if (nasserts >= on_when)
					debug_flag = debug_when;
#endif DBUG
				*c = ChNULL;
				if (assert(db_name,fact) < 0) {
					if (!exit_flag)
						nerrs++;
					else {
						fprintf(stderr,"consult failure\n");
						how_many();
						exit(0);
					}
				}
				else {
					nasserts++;
					if ((nasserts % how_often) == 0)
						how_many();
				}
			}
		}
	}
	how_many();
	exit(0);
}

how_many()
{
	float	elapsed;
	long	secs, usecs;

	if (nasserts == 0) return;

#ifdef BSD
	getrusage(RUSAGE_SELF,&current);
	secs =  (current.ru_utime.tv_sec - start.ru_utime.tv_sec);
	secs += (current.ru_stime.tv_sec - start.ru_stime.tv_sec);
	usecs =  (current.ru_utime.tv_usec - start.ru_utime.tv_usec);
	usecs += (current.ru_stime.tv_usec - start.ru_stime.tv_usec);
	elapsed = (float)secs + (float)usecs/1000000.0;
#else
	times(&current);
	secs =  (current.tms_utime - start.tms_utime);
	secs += (current.tms_stime - start.tms_stime);
	elapsed = (float)secs / 100.0;
#endif

	fprintf(stderr, "%0.1fsecs inserts:%d rate:%0.1fi/s size:%dk errs:%d\n",
		elapsed, nasserts,
		(float)nasserts/elapsed, (int)sbrk(0)/1024, nerrs);
}
