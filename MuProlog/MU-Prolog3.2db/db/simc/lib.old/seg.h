/*
 * seg.h  -  deductive database package (segment definitions)
 *
 * $Header: seg.h,v 1.5 85/12/06 15:09:41 jas Exp $
 * $Log:	seg.h,v $
 * Revision 1.5  85/12/06  15:09:41  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:22:52  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:30:43  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:51:48  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:28  jas
 * Initial revision
 * 
 * 
 */

#ifndef	SEG_H
#define	SEG_H


/*
 * Segt:
 *	Descriptors for "open" data segments
 */
typedef struct Segt {
	Char	*sd_name;	/* name of database/relation */
	Int	sd_segnum;	/* segment number */
	struct	Segt *sd_next;	/* hash chain pointer */

	Int	sd_operation;	/* what segment opened for (read/write) */
	Int	sd_nopens;	/* # transactions using this segment */
	Int	sd_timestamp;	/* when segment was last read */
	DFile	*sd_file;	/* descriptor for data file */
	Faddr	sd_offset;	/* location of seg within data file */
	Word	*sd_seg_buf;	/* segment buffer */
	struct	Segt *sd_qnext;/* active segment queue link */
} Segt;

#define	r_Segt		register Segt
#define	SegtNULL	(Segt *)NULL

#define	seg_base(seg)	((seg)->sd_offset)
#define	seg_buffer(seg)	((seg)->sd_seg_buf)
#define	datafile(seg)	((seg)->sd_file->fd_file)

#ifdef SFB
#define	MAXSEGMENTS	32 	/* max segments db system can have open */
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
#endif SFB

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
extern	void	setup_seg_matches(/* transaction */);

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
#endif OBSOLETE

/*
 * seg_open:
 *	Open a file pointer to a data segment
 *	Maybe we use an already available descriptor
 */
extern	Segt	 *seg_open(/* db_rel, segment_number, operation */);

/*
 * seg_close:
 *	One transaction has finished with data file
 *	The descriptor may persist if other transactions still need it
 */
extern	void	seg_close(/* data_segment_descriptor */);

/*
 * set_segment:
 *	Set up appropriate segment, stars-mask, combinations
 *	for a DSIMC hash code, and a specified level
 */
extern	void	set_segment(/* transaction, level */);

/*
 * ovflow_segment:
 *	Work out address of overflow seg at given level
 *	Duplicates some functionality of set_segment()
 */
extern	Int	ovflow_segment(/* transaction, level */);

#ifdef DBUG
/*
 * seg_print:
 *	Dump contents of data segment descriptor
 */
extern	void	seg_print(/* data_segment_descriptor */);

#endif DBUG

#endif	SEG_H
