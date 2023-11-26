/*
 * rec.h  -  deductive database package (record definitions)
 *
 * $Header: rec.h,v 1.5 85/12/06 15:09:36 jas Exp $
 * $Log:	rec.h,v $
 * Revision 1.5  85/12/06  15:09:36  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:22:54  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:30:45  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:51:43  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:26  jas
 * Initial revision
 * 
 * 
 */

#ifndef	REC_H
#define	REC_H

#define	UNUSED		ONES			/* val for unused rec slots */
#define	UNUSED_BIT	(Word)0x80000000	/* mark bit for unused slots */
						/* Machine Dependent! */

/*
 * setup_rec_matches:
 *	Find all records in a segment which might match query
 */
extern	void	setup_rec_matches(/* transaction */);

/*
 * rec_insert:
 *	Insert a record into the database
 */
extern	Int	rec_insert(/* transaction */);

/*
 * rec_delete:
 *	Delete a record from the database
 */
extern	Int	rec_delete(/* transaction */);

/*
 * rec_fetch:
 *	Fetch a record from the segment buffer
 */
extern	Int	rec_fetch(/* transaction, buffer */);

#endif	REC_H
