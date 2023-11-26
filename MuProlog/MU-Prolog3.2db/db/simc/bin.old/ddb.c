/*
 * ddb.c  -  deductive database package (interactive front end)
 *
 * Read commands from standard input and process each one as read
 *
 * $Header: ddb.c,v 1.2 85/06/08 16:53:41 jas Exp $
 * $Log:	ddb.c,v $
 * Revision 1.2  85/06/08  16:53:41  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  13:03:55  jas
 * Initial revision
 * 
 *
 */

#include "muddlib.h"
#include <signal.h>
#ifdef BSD
#include <sys/time.h>
#include <sys/resource.h>
#else
#define	random	rand
#endif

#ifdef DBUG
extern	Word	debug_flag;
#endif DBUG

int	show_match = 1;
int	nqueries = 0;
int	nsegmatches = 0;
int	nfacts = 0;

#ifdef BSD
struct	rusage	start, current;
#else
struct	tms	start, current;
#endif

Char	*my_name;
Char	*db_name;

Int	nargs;
Char	*op;
Char	*arg;
Char	*arg2;
Char	line[200];

Trans	*t[40];
Int	tt[40];
Int	qq[40];
Int	next = 0;

Trans	*tr;
Int	nmatches;


main(argc, argv)
Int argc;
String argv[];
{
	r_Char	*c;
	r_Int	i, qry;
	void	how_many();

	if (argc != 2) {
		fprintf(stdout, "Usage: %s db\n", argv[0]);
		exit(1);
	}

	c = rindex(argv[0],'/');
	my_name = c == 0 ? argv[0] : (char *)(c+1);
	db_name = argv[1];

	signal(SIGQUIT,how_many);
#ifdef BSD
	getrusage(RUSAGE_SELF,&start);
#else
	times(&start);
#endif

	while (printf("%s> ",my_name),fflush(stdout),gets(line) != NULL)
	{
		if (!isatty(0))
			fprintf(stdout,"%s\n",line);
		op = c = line;
		while (!isspace(*c) && *c != ChNULL)
			c++; /* skip op string */
		if (*c != ChNULL)
			*c++ = ChNULL;
		while (isspace(*c) && *c != ChNULL)
			c++; /* skip blanks */
		arg = c;
		while (!isspace(*c) && *c != ChNULL)
			c++; /* skip arg string */
		if (*c != ChNULL)
			*c++ = ChNULL;
		while (isspace(*c) && *c != ChNULL)
			c++; /* skip blanks */
		arg2 = c;
		if (*op == ChNULL)
			nargs = 0;
		or (*arg == ChNULL)
			nargs = 1;
		or (*arg2 == ChNULL)
			nargs = 2;
		else
			nargs = 3;

		if (streq(op, "assert"))
			do_assert();
		or (streq(op, "query"))
			start_query();
		or (streq(op, "retract")) {
			if (nargs != 2) {
				printf("usage: retract term\n");
				fflush(stdout);
			}
			else
				retractall(argv[1], arg);
		}
		or (streq(op, "output")) {
			if (nargs != 2) {
				printf("usage: output filename\n");
				fflush(stdout);
			}
			else {
				if (freopen(arg,"a",stdout) == FilePNULL) {
					fprintf(stdout,"Can't output on %s\n",arg);
					continue;
				}
				setbuf(stdout,NULL);
			}
		}
#ifndef DBUG
		or (streq(op, "on")) {
			if (streq(arg,"showmatch")) show_match = 1;
			else {
				printf("illegal flag\n");
				fflush(stdout);
			}
		}
		or (streq(op, "off")) {
			if (streq(arg,"showmatch")) show_match = 0;
			else {
				printf("illegal flag\n");
				fflush(stdout);
			}
		}
#else DBUG
		or (streq(op, "on")) {
			if (streq(arg,"all")) debug_flag = 0xffff;
			or (streq(arg,"add")) debug_flag |= ADD_DBUG;
			or (streq(arg,"desc")) debug_flag |= DESC_DBUG;
			or (streq(arg,"get")) debug_flag |= GET_DBUG;
			or (streq(arg,"match")) debug_flag |= MATCH_DBUG;
			or (streq(arg,"parse")) debug_flag |= PARSE_DBUG;
			or (streq(arg,"tree")) debug_flag |= TREE_DBUG;
			or (streq(arg,"vdesc")) debug_flag |= VDESC_DBUG;
			or (streq(arg,"malloc")) debug_flag |= MALLOC_DBUG;
			or (streq(arg,"showmatch")) show_match = 1;
			else {
				printf("illegal debug flag\n");
				fflush(stdout);
			}
		}
		or (streq(op, "off")) {
			if (streq(arg,"all")) debug_flag = 0;
			or (streq(arg,"add")) debug_flag &= ~ADD_DBUG;
			or (streq(arg,"desc")) debug_flag &= ~DESC_DBUG;
			or (streq(arg,"get")) debug_flag &= ~GET_DBUG;
			or (streq(arg,"match")) debug_flag &= ~MATCH_DBUG;
			or (streq(arg,"parse")) debug_flag &= ~PARSE_DBUG;
			or (streq(arg,"tree")) debug_flag &= ~TREE_DBUG;
			or (streq(arg,"vdesc")) debug_flag &= ~VDESC_DBUG;
			or (streq(arg,"malloc")) debug_flag &= ~MALLOC_DBUG;
			or (streq(arg,"showmatch")) show_match = 0;
			else {
				printf("illegal debug flag\n");
				fflush(stdout);
			}
		}
#endif DBUG
		or (streq(op, "run")) {
			while (next > 0) {
				qry = random() % next;
				if (trans_fetch(t[qry]) != StrNULL) {
					tt[qry]++;
/****
					fprintf(stdout,"Query#%-3d : %s\n",
						qq[qry], t[qry]->query); 
****/
					if (show_match)
					fprintf(stdout,"Answer#%-3d: %s\n",
						tt[qry], t[qry]->rec_buf);
					nfacts++;
				}
				else {
					if (show_match)
						fprintf(stdout,"End of#%-3d: %s\n",
							qq[qry], t[qry]->query);
					fprintf(stdout,"fetched: %d blocks  %d segs  %d recs\n",
						t[qry]->nblocks_fetched,
						t[qry]->nsegs_fetched,
						t[qry]->nrecs_fetched);
					trans_close(t[qry]);
					for (i = qry; i < next-1; i++) {
						t[i] = t[i+1];
						tt[i] = tt[i+1];
						qq[i] = qq[i+1];
					}
					next--;
				}
			}
		}
		or (streq(op, "help"))
			help();
		or (streq(op, "quit")) {
			how_many();
			exit(0);
		}
		or (nargs == 0)
			continue;
		else {
			printf("%s: illegal cmd\n", op);
			fflush(stdout);
		}
	}
	printf("quit\n");
	fflush(stdout); 
	how_many();
	exit(0);
}

do_assert()
{
	Trans	*tr;

	if (nargs != 2) {
		printf("usage: assert term\n");
		fflush(stdout);
		return;
	}

	if ((tr = trans_open(db_name,arg,(Op_ASSERT|Op_ONE))) == TransNULL)
		return;
	
	nmatches = rec_insert(tr);

	trans_close(tr); tr = TransNULL;
}

start_query()
{
	int sm;

	if (nargs == 2) {
		if ((t[next] = trans_open(db_name,arg,(Op_QUERY|Op_ALL))) == TransNULL) {
			printf("can't open transaction\n");
			fflush(stdout);
			return;
		}
	}
#ifdef SFB
	or (nargs == 3) {
fprintf(stderr,"trans_sfbopen(%s,%s,QUERY,%s)\n",db_name,arg,arg2);
		if ((t[next] = (Trans *)trans_sfbopen(db_name,arg,(Op_QUERY|Op_ALL),arg2)) == TransNULL) {
			printf("can't open transaction\n");
			fflush(stdout);
			return;
		}
	}
#endif SFB
	else {
		printf("usage: query term [sfbvec]\n");
		fflush(stdout);
		return;
	}
		
	switch (index_type(t[next]->relation))
	{
	when SIMC_INDEX:
		sm = b_count(t[next]->seg_matches, Ns(t[next]->relation));
	when DSIMC_INDEX:
		sm = t[next]->seg_ncombs;
	}
	printf("%d possible segment matches\n", sm);
	qq[next] = next;
	tt[next++] = 0;
	nqueries++;
	nsegmatches += sm;
}

help()
{
	printf("commands:\n");
	printf("\tassert term\n");
	printf("\tretract term\n");
	printf("\tquery term\n");
#ifndef DBUG
	printf("\ton showmatch\n");
	printf("\toff showmatch\n");
#else DBUG
	printf("\ton [add|desc|get|match|parse|tree|vdesc|showmatch]\n");
	printf("\toff [add|desc|get|match|parse|tree|vdesc|showmatch]\n");
#endif DBUG
	printf("\toutput filename\n");
	printf("\thelp\n");
	printf("\tquit\n");
	fflush(stdout);
}

void
how_many()
{
	float	elapsed;
	long	secs, usecs;

	if (nqueries == 0) return;

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

	fprintf(stderr, "%0.1fsecs queries:%d %0.1ff/q %0.1fseg/q rate:%0.1fq/s %0.1ff/s size:%dk\n",
		elapsed, nqueries,
		(float)nfacts/(float)nqueries,
		(float)nsegmatches/(float)nqueries,
		(float)nqueries/elapsed,
		(float)nfacts/elapsed,
		(int)sbrk(0)/1024);
}
