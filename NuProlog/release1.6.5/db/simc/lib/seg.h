/*
 * seg.h  -  deductive database package (segment definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	SEG_H
#define	SEG_H


/*
 * Segt:
 *	Descriptors for "open" data segments
 */
typedef struct Segt {
	Char	*sd_name;	/* name of database/relation */
	Int	sd_segid;	/* segment number */
	struct	Segt *sd_next;	/* hash chain pointer */
	Word	sd_operation;	/* what segment opened for (read/write) */
	Int	sd_nopens;	/* # transactions using this segment */
#ifdef TIME_STAMP
	Int	sd_timestamp;	/* when segment was last read */
#endif
	DFile	*sd_file;	/* descriptor for data file */
	Faddr	sd_offset;	/* location of seg within data file */
	Word	*sd_seg_buf;	/* segment buffer */
	struct	Segt *sd_qnext;/* active segment queue link */
} Segt;

#define	r_Segt		register Segt
#define	SegtNULL	(Segt *)NULL

#define seg_opn(seg)	((seg)->sd_operation)
#define	seg_base(seg)	((seg)->sd_offset)
#define	seg_buffer(seg)	((seg)->sd_seg_buf)
#define	seg_dfile(seg)	((seg)->sd_file)
#define	datafile(seg)	((seg)->sd_file->fd_file)

/*
 * For DSIMC, segment addresses are encoded into
 * 
 * +------+-------------------------+
 * |8-bits| 24-bits                 |
 * +------+-------------------------+
 *  level   seg number within level
 */
#define	segAddr(S,L)	(((S) & 0xffffff) | (((L) & 0xff) << 24))
#define	segLevel(addr)	(((addr) >> 24) & 0xff)
#define	segNumber(addr)	((addr) & (0xffffff))

#if 1
#define	MAXSEGMENTS	16 	/* max segments db system can have open */
				/* More specifically, number of segment
				   buffers which can exist at any time.
				   Segment buffers can be *very* large,
				   and so we can't afford to have one of
				   them open for each open segment */
#define	MAXSHASH	512	/* # entries in Segt hash table */
#else
#define	MAXSEGMENTS	1024 	/* max segments db system can have open */
				/* More specifically, number of segment
				   buffers which can exist at any time.
				   Segment buffers can be *very* large,
				   and so we can't afford to have one of
				   them open for each open segment */
#define	MAXSHASH	2048	/* # entries in Segt hash table */
#endif

extern	Int	n_segments;	/* number of open segments */

extern	Segt*	seg_table[MAXSHASH]; /* hash table for data file descs */

/*
 * Each segment buffer contains the following information:
 *
 * +------------------------------------+
 * |0				      Nr|0
 * |  Bit slices for rec codeword bits	|
 * |					|Dr
 * +------------------------------------+
 * |					|
 * |      Bit slices for mask bits	|Dr
 * +------------------------------------+
 * |   Bit slice to indictate presence	|Dr+1
 * +------------------------------------+
 * |					|0
 * |   <record_size,block_size,offset>	|
 * |					|Nr
 * +------------------------------------+
 * | <free_space,local_free,oflow_free>	|
 * +------------------------------------+
 * |		  Data ...		|
 * |					|
 */

/*
 * Recptr:
 *	Structures containig info about size/offset of records
 */
typedef struct Recptr {
	short	rp_recsize;	/* size of record (doubles as next ptr) */
	short	rp_blksize;	/* size of empty block (hole) */
	Word	rp_offset;	/* offset in file of record */
	Word	rp_hash;	/* hash/cluster word for record */
} Recptr;

#define	rp_nextfree	rp_recsize	/* nasty alias for next_free index */


/*
 * Segptr:
 *	A couple of pointers for managing space in segments
 */
typedef struct Segptr {
	Word	sp_freespace;	/* file offset of first free space */
	short	sp_localfree;	/* index of first "free" Recptr */
	short	sp_extrnfree;	/* index if first "free" Recptr for overflow */
} Segptr;


/*
 * setup_seg_matches:
 *	Find all segments which contain potential matches
 */
extern	void	setup_simc_seg_matches(/* transaction */);

/*
 * seg_fetch:
 *	Read in a segment from the data file (store in buffer)
 */
extern	void	seg_fetch(/* transaction */);	

/*
 * open_segment:
 *	Open data file, possibly deallocating another
 */
extern	Bool	open_segment(/* data_segment_descriptor */);

#ifdef OBSOLETE
/*
 * release_sdesc:
 *	Deallocate an open data segment buffer
 */
extern	void	release_sdesc();
#endif /* OBSOLETE */

/*
 * seg_open:
 *	Open a file pointer to a data segment
 *	Maybe we use an already available descriptor
 */
extern	Segt	 *seg_open(/* relation, segment_number, level, operation */);

/*
 * seg_close:
 *	One transaction has finished with data file
 *	The descriptor may persist if other transactions still need it
 */
extern	void	seg_close(/* data_segment_descriptor */);

/*
 * mknewsegbuf:
 *	Allocate a new segment buffer for a relation and initialise it
 */
extern	Word	*mknewsegbuf(/* relation, segment_base */);

/*
 * add_seg:
 *	Add a new segment to a relation on a specified level
 */
extern	Bool	add_segs(/* relation, level, new_top_segment */);

/*
 * clearSegBuf:
 *	Allocate a new segment buffer for a relation and initialise it
 */
extern	void	clearSegBuf(/* relation, buffer, segment_base */);

#ifdef DBUG
/*
 * seg_print:
 *	Dump contents of data segment descriptor
 */
extern	void	seg_print(/* data_segment_descriptor */);

#endif /* DBUG */

#endif /*	SEG_H */
