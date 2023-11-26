/*
 * trans.h  -  deductive database package (transaction definitions)
 *
 * $Header: trans.h,v 1.5 85/12/06 15:09:43 jas Exp $
 * $Log:	trans.h,v $
 * Revision 1.5  85/12/06  15:09:43  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:17  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:30:59  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:52:01  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:29  jas
 * Initial revision
 * 
 * 
 */

#ifndef	TRANS_H
#define	TRANS_H

/*
 * Opn:
 *	operations which can be performed on relations
 */

typedef Word	Opn;

#define Op_ONE		001	/* only apply operation to one match */
#define Op_ALL		002	/* apply operation to all that match */

#define Op_RETRACT	010	/* delete fact(s) from database/relation */
#define Op_ASSERT	020	/* insert fact into database/relation */
#define Op_QUERY	040	/* retrieve fact(s) from database/relation */

#define	issingle(opn)	((opn) & Op_ONE)
#define	isall(opn)	((opn) & Op_ALL)

#define	isretract(opn)	((opn) & Op_RETRACT)
#define	isassert(opn)	((opn) & Op_ASSERT)
#define	isquery(opn)	((opn) & Op_QUERY)

#define	iswriting(opn)	((opn) & (Op_RETRACT|Op_ASSERT))


/*
 * Trans:
 *	Info related to one transaction on a relation
 *	Includes information on the relation, the fact and state info
 */
typedef struct {
	Word	operation;	/* what is this transaction doing */
	String	input;		/* input string for transaction */
	Elem	*query;		/* parse tree for query on this relation */
	Reln	*relation;	/* the relation being transacted upon */

	Int	nsegbits;	/* count of bits set in seg descriptor */
	float	seg_ratio;	/* Us/Ks -> how many bits to use in search */
	Int	*seg_list;	/* list of bit slices to be examined/set */
	Word	*seg_matches;	/* bit-string giving "matching" segments */
	Word	*ov_matches[MAXOVLEVELS]; /* "matching" ovflow segs in DSIMC */
	Int	cur_seg;	/* which segment we are up to */
	Int	cur_level;	/* which overflow level in DSIMC indexing */
	Segt	*segment;	/* data segment descriptor */

	Word	cluster;	/* original hash for record */
	Word	clustars;	/* original missing bits vector */
	Int	seg_gaps;	/* bits to permute for DSIMC indexing */
	Int	seg_off;	/* which segment within block we are up to */
	Int	seg_count;	/* counter for DSIMC indexing */
	Int	seg_ncombs;	/* number of permutations for DSIMC indexing */

	Int	nrecbits;	/* count of bits set in rec descriptor */
	float	rec_ratio;	/* Ur/Kr -> how many bits to use in search */
	Int	*rec_list;	/* list of bit slices to be examined/set */
	Word	*rec_matches;	/* bit-string giving "matching" records */
	Int	cur_rec;	/* which record we are up to in cur_seg */
	String	rec_buf;	/* buffer to hold text of last record read */

	Int	nsegs_fetched;	/* # segments fetched */
	Int	nblocks_fetched;/* # blocks fetched */
	Int	nrecs_fetched;	/* # records fetched */
	Int	nsdescs_anded;	/* # seg descs anded */
} Trans;

#define	r_Trans		register Trans
#define	TransNULL	(Trans *)NULL


/*
 * trans_open:
 *	Commence a transaction; make a transaction descriptor
 */
extern	Trans	*trans_open(/* rel, qry, op */);

#ifdef SFB
/*
 * trans_open:
 *	Commence a transaction; make a transaction descriptor
 */
extern	Trans	*trans_open(/* rel, qry, op */);
#endif SFB

/*
 * trans_close:
 *	Complete a transaction; de-allocate transaction descriptor
 */
extern	void	trans_close(/* trans */);

/*
 * trans_assert:
 *	Insert a new record into the database
 */
extern	Int	trans_assert(/* rel, qry, op */);

/*
 * trans_retract:
 *	Delete record(s) from the database
 */
extern	Int	trans_retract(/* rel, qry, op */);

/*
 * trans_query:
 *	Retrieve record(s) from the database
 */
extern	Int	trans_query(/* rel, qry, op */);

/*
 * trans_fetch:
 *	Fetch the next record matching the query from the database
 */
extern	String	trans_fetch(/* trans */);

/*
 * User interface to transaction functions
 */
#define assert(rel,fac)		trans_assert(rel, fac, (Op_ASSERT|Op_ONE))
#define retract(rel,fac)	trans_retract(rel, fac, (Op_RETRACT|Op_ONE))
#define retractall(rel,fac)	trans_retract(rel, fac, (Op_RETRACT|Op_ALL))
#define onequery(rel,fac)	trans_query(rel, fac, (Op_QUERY|Op_ONE))
#define queryall(rel,fac)	trans_query(rel, fac, (Op_QUERY|Op_ALL))

extern	Trans	*glob_tr;

#endif	TRANS_H
