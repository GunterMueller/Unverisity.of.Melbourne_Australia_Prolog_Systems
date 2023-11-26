/*
 * reln.h  -  deductive database package (relation definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	RELN_H
#define	RELN_H

#define MAXPATH		256	/* maximum path length of file name */
#define	MAXRNAME	32	/* max length of relation name */
#define	MAXKEYS		64	/* max number of keys in relation */

/*
 * Various indexing schemes supported by the package
 */
#define	SIMC_INDEX	0	/* standard superimposed coding */
#define	DSIMC_INDEX	1	/* hashing + superimposed coding */

/*
 * Reln:
 *	Descriptor holding static info about a relation
 */
typedef	struct Reln {
	Char	*rd_name;	/* name of database/relation */
	Int	rd_length;	/* length of rd_name */
	struct	Reln *rd_next;	/* hash chain link */

	Int	rd_operation;	/* to check consistent opening of relation */
	File	rd_descf;	/* file pointer to info + descriptors */
	Int	rd_nopens;	/* number of transactions using relation */
/* 	
	Below here is REAL static info
	These fields must *always* appear in contiguous block
	The size of this block (in bytes) is given by "info_bytes()"
*/
	Word	rd_index;	/* specifies index scheme */
	Int	rd_masksz;	/* size of structure mask (bits) */
	Long	rd_cntroffset;	/* offset of record counters */
	Long	rd_descoffset;	/* offset of segment descriptors */
	Int	rd_rcwordsz;	/* size of record codeword (bits) */
	Int	rd_rdescsz;	/* size of record descriptor (bits) */
	Int	rd_rnbits;	/* # bits set in record code word */
	Int	rd_rubits;	/* # bits used in record code word */
	Int	rd_scwordsz;	/* size of segment codeword (bits) */
	Int	rd_sdescsz;	/* size of segment descriptor (bits) */
	Int	rd_snbits;	/* # bits set in segment code word */
	Int	rd_subits;	/* # bits used in segment code word */
	Int	rd_nrecs;	/* max records per segment */
	Int	rd_nsegs;	/* max segments in file */
	Int	rd_ndfiles;	/* number of data files */
	Int	rd_avreclen;	/* average record length */
	Int	rd_seg_info_size;/* # bytes in segment buffers */
	Int	rd_seg_size;	/* # bytes in segment (buf + data) */
	Int	rd_local_segs;	/* # segments in each data file */
	Int	rd_block_size;	/* # bytes per index block */
	Int	rd_sdesc_block;	/* # seg descriptors per index block */
/*
 * Arrays to handle multiple levels in DSIMC
 * Except for loadfac[], these are not static.
 */
	Int	rd_loadfac[MAXLEVELS]; /* load factors for each level */
	Int	rd_splitp[MAXLEVELS]; /* split pointers for each level */
	Int	rd_depth[MAXLEVELS]; /* # bits to consider in addresses */
	Int	rd_magic[MAXLEVELS][MAXBITS]; /* magic numbers for computing
						 split pointers */
	Int	rd_nlevrecs[MAXLEVELS]; /* # records at each level */
	Int	rd_nlevsegs[MAXLEVELS]; /* # segments at each level */
	Int	rd_loadcnt[MAXLEVELS]; /* # records since last split for level */
#ifdef TODO
	Faddr	rd_sdoffset[MAXLEVELS]; /* base of sdesc blocks for level */
	Faddr	rd_sdmaxset[MAXLEVELS]; /* highest sdesc block for level */
#endif
/*
	This is not really REAL static info
	But is used to bracket static info for read & write
*/
	Elem	*rd_skel;	/* parse tree of template */
} Reln;

#define	r_Reln		register Reln
#define	RelnNULL	(Reln *)NULL

#define	MAXRINFFILES	6	/* max rel info files db sys can have open */
#define	MAXRHASH	15	/* # entries in relation hash table */

extern	Int	n_rel_files;	/* number of open relation info files */

extern	Reln	*reln_table[MAXRHASH]; /* hash table for reln descs */


/*
 * Relation information access methods
 */
#define	Mr(rel)		((rel)->rd_masksz)
#define	Br(rel)		((rel)->rd_rcwordsz)
#define	Dr(rel)		((rel)->rd_rdescsz)
#define	Kr(rel)		((rel)->rd_rnbits)
#define	Ur(rel)		((rel)->rd_rubits)
#define	Nr(rel)		((rel)->rd_nrecs)
#define	Ms(rel)		((rel)->rd_masksz)
#define	Bs(rel)		((rel)->rd_scwordsz)
#define	Ds(rel)		((rel)->rd_sdescsz)
#define	Ks(rel)		((rel)->rd_snbits)
#define	Us(rel)		((rel)->rd_subits)
#define	Ns(rel)		((rel)->rd_nsegs)
#define	Nlr(rel,lev)	(((rel)->rd_nlevrecs)[lev])
#define	Nls(rel,lev)	(((rel)->rd_nlevsegs)[lev])
#define	loadfac(rel,lev)	(((rel)->rd_loadfac)[lev])
#define	loadcnt(rel,lev)	(((rel)->rd_loadcnt)[lev])
#define	splitp(rel,lev)	(((rel)->rd_splitp)[(lev)])
#define	depth(rel,lev)	((rel)->rd_depth[(lev)])
#define	magic(rel,lev,inx) \
			((rel)->rd_magic[(lev)][(inx)])
#define	Nd(rel)		((rel)->rd_ndfiles)
#define	Av(rel)		((rel)->rd_avreclen)
#define	Nsb(rel)	((rel)->rd_sdesc_block)
#define	offset(rel)	((rel)->rd_descoffset)
#define	coffset(rel)	((rel)->rd_cntroffset)
#define	dbrelname(rel)	((rel)->rd_name)
#define	numkeys(rel)	((rel)->rd_skel->e_nargs)
#define	skeleton(rel)	((rel)->rd_skel)
#define	descfile(rel)	((rel)->rd_descf)
#define	local_segs(rel)	((rel)->rd_local_segs)
#define	seg_info_size(rel) ((rel)->rd_seg_info_size)
#define	seg_size(rel)	((rel)->rd_seg_size)
#define	index_type(rel) ((rel)->rd_index)
#define	static_info(rel) ((char *)&((rel)->rd_index))
#define	info_bytes(rel)	((char *)&((rel)->rd_skel) - (char *)&((rel)->rd_index))

/*
 * Only for DSIMC indexing
 */
#define	block_size(rel)		((rel)->rd_block_size)
#define	segs_per_block(rel)	(1 << ((rel)->rd_sdesc_block))
#define	log2_segs_per_block(rel) ((rel)->rd_sdesc_block)

/*
 * For each relation, there is a file:
 *
 * +---------+-----------------+----- . . . -----+
 * |  Reln  |  Rec Counters   | Segt Descriptors |
 * +---------+-----------------+----- . . . -----+
 *
 * For each segment, there is a part of a data file:
 *
 * +----- . . . -----+----- . . . -----+----- . . . .
 * | Rec Descriptors | Record Pointers | Record data
 * +----- . . . -----+----- . . . -----+----- . . . .
 */


/*
 * rel_open:
 *	Open files associated with relation and set up relation descriptor
 */
extern	Reln	*rel_open(/* db_name, query, operation */);

/*
 * rel_close:
 *	Release access to a relation, close & unlock files
 */
extern	void	rel_close(/* rel */);

/*
 * rel_info_update:
 *	Update "static" information on the relation
 */
extern	Bool	rel_info_update(/* rel */);

/*
 * set_counter:
 */
extern	Int	set_counter(/* rel, segaddr, newcount */);

#ifdef DBUG
/*
 * rel_print:
 *	Dump contents of a relation descriptor
 */
extern void	rel_print(/* reln_descriptor */);
#endif /* DBUG */

#endif /*	RELN_H */
