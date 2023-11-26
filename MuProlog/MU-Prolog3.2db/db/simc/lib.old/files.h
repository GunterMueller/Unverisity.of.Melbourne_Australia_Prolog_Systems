/*
 * files.h  -  deductive database package (file management definitions)
 *
 * $Header: files.h,v 1.5 85/12/06 15:09:31 jas Exp $
 * $Log:	files.h,v $
 * Revision 1.5  85/12/06  15:09:31  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:11  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:30:55  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:48:23  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:22  jas
 * Initial revision
 * 
 * 
 */

#ifndef	FILES_H
#define	FILES_H

#define MAXPATH		256	/* maximum path length of file name */
#define	MAXRNAME	32	/* max length of relation name */

#define	MAXOVLEVELS	10	/* maximum depth of ovflow chains */

#define	Ki		8	/* ratio of level i segs to level i+1 */
#define	log2Ki		3	/* log base 2 of Ki (to save time) */

/*
/*
 * Hash:
 *	Hash table chaining nodes
 *	DFile's, Segt's and Reln's must all begin with
 *	a sequence of fields identical to this structure
 */
typedef struct Hash {
	Char	*h_name;	/* ptr to "db/rel" string */
	Int	h_discrim;	/* used as length | seg# | data_file# */
	struct	Hash *h_next;	/* hash chain link */
} Hash;

#define	r_Hash		register Hash
#define	HashNULL	(Hash *)NULL

/*
 * Modes for doing hash table look-ups
 */

#define	STRING_MODE	0
#define	POINTER_MODE	1


/*
 * DFile:
 *	Descriptors for open data files
 */
typedef struct DFile {
	Char	*fd_name;	/* name of database/relation */
	Int	fd_datafile;	/* number of data file */
	struct	DFile *fd_next;	/* hash chain pointer */

	Int	fd_nopens;	/* number of transactions using this file */
	Int	fd_timestamp;	/* when file was last locked/unlocked */
	File	fd_file;	/* Unix file pointer to data */
} DFile;

#define	r_DFile		register DFile
#define	DFileNULL	(DFile *)NULL

#define	MAXDATAFILES	10 	/* max data files db system can have open */
#define	MAXFHASH	64	/* # entries in DFile hash table */

extern	Int	n_data_files;	/* number of open data files */

extern	DFile	*dfile_table[MAXFHASH]; /* hash table for data file descs */

extern	Int	time_stamp;	/* timestamp for sharing segment buffers */


/*
 * search:
 *	Look up database/relation in hash table
 */
extern	Bool	search(/* db_name, rfname, table, tab_size, ptr, loc */);

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

#ifdef OBSOLETE
/*
 * dfile_close:
 *	One transaction has finished with data file
 *	The descriptor may persist if other transactions still need it
 */
extern	void	dfile_close(/* data_file_descriptor */);
#endif OBSOLETE

/*
 * db_cleanup:
 *	Clean up database and unlock files when unusual condition occurs
 */
extern	void	db_cleanup();

/*
 * addressofindex:
 *	Convert a segment number into the relative location in the
 *	file where the segment is stored (may need to be scaled)
 *	This implements the recursive linear hashed file structure
 */
extern	Int addressofindex(/* segno, level */);

#ifdef DBUG
/*
 * dfile_print:
 *	Dump contents of data file descriptor
 */
extern	void	dfile_print(/* data_file_descriptor */);

#endif DBUG

#endif	FILES_H
