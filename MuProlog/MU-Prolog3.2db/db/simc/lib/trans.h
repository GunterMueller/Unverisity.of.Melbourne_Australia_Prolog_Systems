/*
 * trans.h  -  deductive database package (transaction definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 */

#ifndef	TRANS_H
#define	TRANS_H

/*
 * Opn:
 *	operations which can be performed on relations
 */

typedef Word	Opn;

#define opRETRACT	0001	/* delete fact(s) from database/relation */
#define opASSERT	0002	/* insert fact into database/relation */
#define opQUERY		0004	/* retrieve fact(s) from database/relation */

#define	opSPLIT		0010	/* this insertion is part of a split */

#define	isretract(opn)	((opn) & opRETRACT)
#define	isassert(opn)	((opn) & opASSERT)
#define	isquery(opn)	((opn) & opQUERY)

#define	iswriting(opn)	((opn) & (opRETRACT|opASSERT))

#define	cansplit(opn)	(((opn) & (opSPLIT)) == 0)
#define	splitting(opn)	((opn) & (opSPLIT))


/*
 * Pool:
 *	Info required for an SFB query on a relation
 */
typedef struct {
	String	database;
	Reln	*relation;
	Word	*buffers;
} Pool;

#define	r_Pool		register Pool
#define	PoolNULL	((Pool *)NULL)


/*
 * Trans:
 *	Info related to one transaction on a relation
 *	Includes information on the relation, the fact and state info
 */
typedef struct {
	String	database;	/* which database transaction is in */
	String	input;		/* input string for transaction */
	Word	operation;	/* what is this transaction doing */
	Elem	*query;		/* parse tree for query on this relation */
	Reln	*relation;	/* the relation being transacted upon */
	Pool	*buffers;	/* buffer pool info for DSMIC/SFB */

	Int	nsegbits;	/* count of bits set in seg descriptor */
	float	seg_ratio;	/* Us/Ks -> how many bits to use in search */
	Int	*seg_list;	/* list of bit slices to be examined/set */
	Word	*seg_matches;	/* bit-string giving "matching" segments */
	Word	*ov_matches[MAXLEVELS]; /* "matching" ovflow segs in DSIMC */
	Int	cur_seg;	/* which segment we are up to */
	Int	cur_level;	/* which overflow level in DSIMC indexing */
	Segt	*segment;	/* data segment descriptor */

	Word	cluster;	/* original hash for record */
	Word	clustars;	/* original missing bits vector */
	Int	seg_off;	/* which segment within block we are up to */

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
	Int	n_segopens;
	Int	n_segreads;
} Trans;

#define	r_Trans		register Trans
#define	TransNULL	(Trans *)NULL


/*
 * trans_open:
 *	Commence a transaction; make a transaction descriptor
 */
extern	Trans	*trans_open(/* database, fact, operation, start_level */);

/*
 * trans_sfbquery:
 *	Commence an sfb query on an open relation with a buffer pool
 */
extern	Trans	*trans_open(/* pool, query, sfb_vector */);

/*
 * pool_open:
 *	Allocate a pool of buffers and an open relation for an sfb query
 */
extern	Pool	*pool_open(/* database, relation, arity */);

/*
 * trans_fetch:
 *	Fetch the next record matching the query from the database
 */
extern	String	trans_fetch(/* trans */);

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

#endif	TRANS_H
