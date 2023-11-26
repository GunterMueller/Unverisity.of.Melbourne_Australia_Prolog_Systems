/*
 * util.h  -  deductive database package (utility definitions)
 *
 * $Header: util.h,v 1.5 85/12/06 15:09:45 jas Exp $
 * $Log:	util.h,v $
 * Revision 1.5  85/12/06  15:09:45  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:19  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:02  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:52:11  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:31  jas
 * Initial revision
 * 
 * 
 */

#ifndef	UTIL_H
#define	UTIL_H

/*
 * error:
 *	Send error message to client process
 */
extern	void	error(/* message */);

/*
 * fatal:
 *	Send error message to client process, then die
 */
extern	void	fatal(/* message */);

#ifdef DBUG
/*
 * Debugging
 */

#define	ADD_DBUG	(1 << 0)
#define	DESC_DBUG	(1 << 1)
#define	GET_DBUG	(1 << 2)
#define	MATCH_DBUG	(1 << 3)
#define	PARSE_DBUG	(1 << 4)
#define	TREE_DBUG	(1 << 5)
#define	VDESC_DBUG	(1 << 6)
#define	MALLOC_DBUG	(1 << 7)

/*
 * debug_flag:
 *	A word to tell which debugging styles are active
 */
#ifdef DBUG
extern	Word	debug_flag;
#endif DBUG

/*
 * debug:
 *	Determine whether debugging is required for following statements
 */
#define	debug(type)	if ((debug_flag & (type)) != 0)

#endif DBUG

#endif	UTIL_H
