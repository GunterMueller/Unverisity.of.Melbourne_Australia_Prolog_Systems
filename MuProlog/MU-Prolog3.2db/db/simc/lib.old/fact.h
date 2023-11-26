/*
 * fact.h  -  deductive database package (fact definitions)
 *
 * $Header: fact.h,v 1.5 85/12/06 15:09:27 jas Exp $
 * $Log:	fact.h,v $
 * Revision 1.5  85/12/06  15:09:27  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:22:58  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:30:48  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:51:23  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:21  jas
 * Initial revision
 * 
 * 
 */

#ifndef	FACT_H
#define	FACT_H

#define	MAXFACT		1024	/* max chars in a fact */

/*
 * fact_read:
 *	read fact from the query input stream
 */
extern	Int	fact_read(/* dfile, fact, relation */);

/*
 * fact_write:
 *	Write fact to answer output stream
 */
extern	void	fact_write(/* dfile, fact */);

#endif	FACT_H
