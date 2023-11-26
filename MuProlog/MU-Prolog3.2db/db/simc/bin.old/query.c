/*
 * rmatch.c  -  deductive database package (retrieval test)
 *
 * This prints all records matching specified query
 *
 * $Header: query.c,v 1.2 85/06/08 16:53:58 jas Exp $
 * $Log:	query.c,v $
 * Revision 1.2  85/06/08  16:53:58  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  13:04:02  jas
 * Initial revision
 * 
 *
 */

#include "muddlib.h"

main(argc, argv)
Int argc;
String argv[];
{
	if (argc != 3) {
		fprintf(stderr, "Usage: %s db query\n", argv[0]);
		exit(1);
	}

	queryall(argv[1],argv[2]);
	
	exit(0);
}
