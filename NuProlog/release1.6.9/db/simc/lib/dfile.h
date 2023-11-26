/*
 * dfile.h  -  deductive database package (data file management definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	DFILE_H
#define	DFILE_H

#define MAXPATH		256	/* maximum path length of file name */
#define	MAXRNAME	32	/* max length of relation name */

/*
 * MAXLEVELS specifies the maximum number of levels in DSIMC files,
 * which use recursive linear hashing to place records.
 * Normally distributed databases should not come anywhere near this;
 * only in degenerate cases (e.g. inserting the same record lots and
 * lots of times) should we approach this depth of hashing.
 * If we do, though, we have big problems since the system doesn't
 * handle this case at present ... it will simply refuse to insert
 * the offending records.
 * This parameter can be increased without affecting anything much
 * (simply means that whenever we access the relation information for
 * a DSIMC database, we have to read a few more bytes).
 */
#define	MAXLEVELS	16	/* maximum levels in DSIMC files */

/*
 * DFile:
 *	Descriptors for open data files
 */
typedef struct DFile {
	Char	*fd_name;	/* name of database/relation */
	Int	fd_datafile;	/* number of data file */
	struct	DFile *fd_next;	/* hash chain pointer */

	Int	fd_nopens;	/* number of transactions using this file */
	Word	fd_opn;		/* current operation on this data file */
	File	fd_file;	/* Unix file pointer to data */
#ifdef TIME_STAMP
	Int	fd_timestamp;	/* when file was last locked/unlocked */
#endif
} DFile;

#define	r_DFile		register DFile
#define	DFileNULL	(DFile *)NULL

#define	MAXDATAFILES	10 	/* max data files db system can have open */
#define	MAXFHASH	64	/* # entries in DFile hash table */

extern	Int	n_data_files;	/* number of open data files */

extern	DFile	*dfile_table[MAXFHASH]; /* hash table for data file descs */

#ifdef TIME_STAMP
extern	Int	time_stamp;	/* timestamp for sharing segment buffers */
#endif

/*
 * For DSIMC, file identifiers are encoded into
 * 
 * +------+-------------------------+
 * |8-bits| 24-bits                 |
 * +------+-------------------------+
 *  level   file number within level
 */
#define	fileIdent(F,L)	(((F) & 0xffffff) | (((L) & 0xff) << 24))
#define	fileLevel(addr)	(((addr) >> 24) & 0xff)
#define	fileNumber(addr)	((addr) & (0xffffff))

/*
 * open_data_file:
 *	Open data file, possibly deallocating another
 */
extern	Bool	open_data_file(/* data_file_descriptor */);

/*
 * release_dfile:
 *	Deallocate an open data file pointer
 */
extern	void	release_dfile();

/*
 * dfile_open:
 *	Open a file pointer to a data segment
 *	Maybe we use an already available descriptor
 */
extern	DFile	 *dfile_open(/* db_name, data_file, operation */);

#ifdef DBUG
/*
 * dfile_print:
 *	Dump contents of data file descriptor
 */
extern	void	dfile_print(/* data_file_descriptor */);

#endif /* DBUG */

#endif /*	DFILE_H */
