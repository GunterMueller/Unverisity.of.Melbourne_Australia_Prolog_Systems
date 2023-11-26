/*
 * mudd.c  -  deductive database package (front-end)
 *
 * This program implements a process which talks to MU-Prolog
 * along a pipe ... it reads requests from one side of pipe,
 * performs operations, and writes any output down other side
 * of pipe as Prolog terms
 *
 * $Header: mudd.c,v 1.1 85/05/26 13:03:59 jas Exp $
 * $Log:	mudd.c,v $
 * Revision 1.1  85/05/26  13:03:59  jas
 * Initial revision
 * 
 */

#include "muddlib.h"
#include "mudd.h"

main(argc, argv)
Int argc;
String argv[];
{
	r_Int	op;
	r_Char	*fac;

	signal(SIGINT, SIG_IGN);
	if (argc != 3) {
		error("usage");
		/* Usage: mudd db rel */
		exit(1);
	}

	if ((fac = (char *)malloc((unsigned)1024)) == StrNULL) {
		error("no_mem_for_fact_buf");
		exit(1);
	}

	while ((op = getchar()) != EOF && fact_read(stdin,fac,argv[2]) > 0)
	{
		switch ((char) op)
		{
		case DB_QY:
			onequery(argv[1], fac);
			printf("?-db_end.\n");
			break;
		case DB_QYALL:
			queryall(argv[1], fac);
			printf("?-db_end.\n");
			break;
		case DB_RT:
			retract(argv[1], fac);
			printf("db_end.\n");
			break;
		case DB_RTALL:
			retractall(argv[1], fac);
			printf("db_end.\n");
			break;
		case DB_ASS:
			assert(argv[1], fac);
			break;
		case DB_ASA:
			assert(argv[1], fac);
			break;
		default:
			error("command");
			exit(1);
		}
		fflush(stdout);
	}
	exit(0);
}
