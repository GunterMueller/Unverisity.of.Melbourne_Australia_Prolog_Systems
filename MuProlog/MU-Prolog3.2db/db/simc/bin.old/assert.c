/*
 * rassert.c  -  deductive database package (insert test)
 *
 * This program inserts a record into a relation
 *
 * $Header: assert.c,v 1.2 85/06/08 16:53:15 jas Exp $
 * $Log:	assert.c,v $
 * Revision 1.2  85/06/08  16:53:15  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  13:03:53  jas
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
		fprintf(stderr, "Usage: %s db fact\n", argv[0]);
		exit(1);
	}
	assert(argv[1],argv[2]);
	exit(0);
}
