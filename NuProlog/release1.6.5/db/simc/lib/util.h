/*
 * util.h  -  deductive database package (utility definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	UTIL_H
#define	UTIL_H

/*
 * str_copy:
 *	Make a permanent copy of a string in volatile storage
 */
extern	String	str_copy(/* string */);

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
 * Debugging styles
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
extern	Word	debug_flag;

/*
 * debug:
 *	Determine whether debugging is required for following statements
 */
#define	debug(type)	if ((debug_flag & (type)) != 0)

#endif /* DBUG */

#endif /*	UTIL_H */
