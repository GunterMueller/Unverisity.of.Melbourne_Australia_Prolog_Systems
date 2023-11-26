/*
 * util.c  -  deductive database package (utility operations)
 *
 * $Header: util.c,v 1.5 85/12/06 15:10:30 jas Exp $
 * $Log:	util.c,v $
 * Revision 1.5  85/12/06  15:10:30  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:24:31  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:47  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:49:09  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:45  jas
 * Initial revision
 * 
 * 
 */

#include "muddlib.h"

#ifdef DBUG
Word	debug_flag = 0;
#endif DBUG

/*
 * error:
 *	Print error message
 */
void
error(msg)
String msg;
{
#if 0
	printf("?-db_error(%s).\n", msg);
#else
	puts(msg);
#endif
	fflush(stdout);
}


/*
 * fatal:
 *	Print error message, then die
 */
void
fatal(msg)
String msg;
{
	error(msg);
	exit(1);
}
