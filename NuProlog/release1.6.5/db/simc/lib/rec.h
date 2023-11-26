/*
 * rec.h  -  deductive database package (record definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
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

/*
 * rectext:
 *	Fetch text of a record from segment buffer
 */
extern	String	rectext();

#endif /*	REC_H */
