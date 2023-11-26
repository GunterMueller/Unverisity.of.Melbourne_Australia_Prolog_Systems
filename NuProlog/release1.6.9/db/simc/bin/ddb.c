/*
 * ddb.c  -  deductive database package (interactive front end)
 *
 * Read commands from standard input and process each one as read
 *
 * Copyright (C) 1985, 1986, 1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"
#ifdef BSD
#include <sys/time.h>
#include <sys/resource.h>
#else
#define	random	rand
#endif

#ifdef DBUG
extern	Word	debug_flag;
#endif /* DBUG */

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

Pool	*poole = PoolNULL;
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
	int	how_many();

	c = rindex(argv[0],'/');
	my_name = c == 0 ? argv[0] : (char *)(c+1);

	if (argc != 2) {
		fprintf(stderr, "Usage: %s db\n", my_name);
		exit(1);
	}
	db_name = argv[1];

	signal(SIGQUIT,how_many);
#ifdef BSD
	getrusage(RUSAGE_SELF,&start);
#else
	times(&start);
#endif

	while (printf("%s> ",my_name),fflush(stdout),gets(line) != NULL)
	{
		if (!isatty(0) || !isatty(1))
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
		else if (*arg == ChNULL)
			nargs = 1;
		else if (*arg2 == ChNULL)
			nargs = 2;
		else
			nargs = 3;

		if (streq(op, "assert"))
			do_assert();
		else if (streq(op, "pool"))
			open_pool();
		else if (streq(op, "query"))
			start_query();
		else if (streq(op, "retract")) {
			if (nargs != 2) {
				printf("usage: retract term\n");
				fflush(stdout);
			}
			else
				trans_retract(argv[1], arg);
		}
		else if (streq(op, "output")) {
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
		else if (streq(op, "on")) {
			if (streq(arg,"showmatch")) show_match = 1;
			else {
				printf("illegal flag\n");
				fflush(stdout);
			}
		}
		else if (streq(op, "off")) {
			if (streq(arg,"showmatch")) show_match = 0;
			else {
				printf("illegal flag\n");
				fflush(stdout);
			}
		}
#else /* DBUG */
		else if (streq(op, "on")) {
			if (streq(arg,"all")) debug_flag = 0xffff;
			else if (streq(arg,"add")) debug_flag |= ADD_DBUG;
			else if (streq(arg,"desc")) debug_flag |= DESC_DBUG;
			else if (streq(arg,"get")) debug_flag |= GET_DBUG;
			else if (streq(arg,"match")) debug_flag |= MATCH_DBUG;
			else if (streq(arg,"parse")) debug_flag |= PARSE_DBUG;
			else if (streq(arg,"tree")) debug_flag |= TREE_DBUG;
			else if (streq(arg,"vdesc")) debug_flag |= VDESC_DBUG;
			else if (streq(arg,"malloc")) debug_flag |= MALLOC_DBUG;
			else if (streq(arg,"showmatch")) show_match = 1;
			else {
				printf("illegal debug flag\n");
				fflush(stdout);
			}
		}
		else if (streq(op, "off")) {
			if (streq(arg,"all")) debug_flag = 0;
			else if (streq(arg,"add")) debug_flag &= ~ADD_DBUG;
			else if (streq(arg,"desc")) debug_flag &= ~DESC_DBUG;
			else if (streq(arg,"get")) debug_flag &= ~GET_DBUG;
			else if (streq(arg,"match")) debug_flag &= ~MATCH_DBUG;
			else if (streq(arg,"parse")) debug_flag &= ~PARSE_DBUG;
			else if (streq(arg,"tree")) debug_flag &= ~TREE_DBUG;
			else if (streq(arg,"vdesc")) debug_flag &= ~VDESC_DBUG;
			else if (streq(arg,"malloc")) debug_flag &= ~MALLOC_DBUG;
			else if (streq(arg,"showmatch")) show_match = 0;
			else {
				printf("illegal debug flag\n");
				fflush(stdout);
			}
		}
#endif /* DBUG */
		else if (streq(op, "run")) {
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
		else if (streq(op, "help"))
			help();
		else if (streq(op, "quit")) {
			how_many();
			exit(0);
		}
		else if (nargs == 0)
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
	if (nargs != 2) {
		printf("usage: assert term\n");
		fflush(stdout);
	}
	else if (trans_assert(db_name, arg) < 0) {
		printf("couldn't insert record\n");
		fflush(stdout);
	}
}

open_pool()
{
	if (nargs != 3) {
		printf("usage: pool rel arity\n");
		fflush(stdout);
	}
	else if ((poole = pool_open(db_name,arg,arg2)) == PoolNULL) {
		printf("can't open buffer pool\n");
		fflush(stdout);
	}
}

start_query()
{
	int sm, i;

	if (nargs == 2) {
		printf("simple dsimc query\n");
		fflush(stdout);
		if ((t[next] = trans_open(db_name,arg,opQUERY,0)) == TransNULL) {
			printf("can't open transaction\n");
			fflush(stdout);
			return;
		}
	}
	else if (nargs == 3) {
		if (poole == PoolNULL) {
			printf("no pool for transaction\n");
			fflush(stdout);
			return;
		}
fprintf(stderr,"trans_sfbquery(%s,%s,QUERY,%s)\n",db_name,arg,arg2);
		if ((t[next] = (Trans *)trans_sfbquery(poole,arg,arg2)) == TransNULL) {
			printf("can't open transaction\n");
			fflush(stdout);
			return;
		}
	}
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
		for (sm = i = 0; i < MAXLEVELS; i++) {
			if (t[next]->ov_matches[i] == WordNULL) continue;
			sm += b_count(t[next]->ov_matches[i], Nls(t[next]->relation,i));
		}
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
	printf("\tquery term [sfbvec]\n");
	printf("\tpool relation arity\n");
#ifndef DBUG
	printf("\ton showmatch\n");
	printf("\toff showmatch\n");
#else /* DBUG */
	printf("\ton [add|desc|get|match|parse|tree|vdesc|showmatch]\n");
	printf("\toff [add|desc|get|match|parse|tree|vdesc|showmatch]\n");
#endif /* DBUG */
	printf("\toutput filename\n");
	printf("\thelp\n");
	printf("\tquit\n");
	fflush(stdout);
}

int
how_many()
{
	float	elapsed;
	long	secs, usecs;

	if (nqueries == 0) return 0;

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
	return 0;
}

