/*
 * dfile.c  -  deductive database package (data file operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

/*
 * Global data/segment file variables
 */
#ifdef TIME_STAMP
Int	time_stamp = 1;		 /* Timestamp for sharing segment buffers */
#endif

/*
 * Global variables for data file descriptor cache
 */
Int	n_data_files = 0;
DFile	*dfile_table[MAXFHASH] = { DFileNULL, };

/*
 * dfile_open:
 *	Open a file pointer to a data segment
 *	Hopefully we use an already available descriptor
 */
DFile *
dfile_open(db_rel,data_file,level,operation)
String db_rel;
Int data_file;
Int level;
Opn operation;
{
	DFile	**ptr;
	DFile	*desc;
	Word	file_id;

	file_id = fileIdent(data_file, level);

	/*
	 * If search ok, use existing descriptor
	 */
	if (search(db_rel, file_id, POINTER_MODE, (Hash**)dfile_table,
			MAXFHASH, (Hash***)(&ptr), (Hash**)(&desc)))
	{
		if (!open_data_file(desc,operation))
			fatal("reopen_data_file");
		goto DFileSucceed;
	}

	/*
	 * Create new open data file descriptor
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"new_dfile:");
#endif DBUG
	if ((desc = mknew(DFile)) == DFileNULL) {
		error("no_mem_for_dfile");
		return(DFileNULL);
	}
	desc->fd_file = FileNULL;
	
	desc->fd_name = db_rel;
#ifdef TIME_STAMP
	desc->fd_timestamp = 0;
#endif
	desc->fd_datafile = file_id;
	desc->fd_next = DFileNULL;
	desc->fd_nopens = 0;

	if (!open_data_file(desc,operation)) {
		error("open_data_file");
		goto DFileFail;
	}

	/*
	 * Link new desc into hash table
	 */
	*ptr = desc;

DFileSucceed:
#ifdef DBUG
	debug(DESC_DBUG) dfile_print("Open DFile:",desc);
#endif DBUG
	return(desc);

DFileFail:
	if (desc != DFileNULL)
		cfree(desc, DFile);
	return(DFileNULL);
}

/*
 * open_data_file:
 *	Open a data file, possibly deallocating another
 */
Bool
open_data_file(fd,op)
DFile *fd;
Opn op;
{
	Int	file_id;
	Bool	need_to_lock;
	Char	buf[MAXPATH+MAXRNAME+5];

	file_id = fd->fd_datafile;
	need_to_lock = (fd->fd_file == FileNULL || fd->fd_nopens == 0);

	if (fd->fd_file != FileNULL &&
		!iswriting(fd->fd_opn) && iswriting(op)) {
		close(fd->fd_file);
		n_data_files--;
		fd->fd_file = FileNULL;
	}
	if (fd->fd_file == FileNULL) {
		sprintf(buf, "%s/%02d.%04d",
			fd->fd_name, fileLevel(file_id), fileNumber(file_id));
		if (n_data_files >= MAXDATAFILES)
			release_dfile();
		if (iswriting(op)) {
			if ((fd->fd_file = open(buf, (O_RDWR|O_CREAT), 0644)) < 0)
				return(FALSE);
		}
		else {
			if ((fd->fd_file = open(buf, (O_RDONLY), 0644)) < 0)
				return(FALSE);
		}
		n_data_files++;
	}
	fd->fd_opn = op;
	if (iswriting(op))
#ifdef LOCK_SH
		flock(fd->fd_file, LOCK_EX);
#else
		/* do nothing */;
#endif
	or (isquery(op) && need_to_lock) {
#ifdef LOCK_SH
		flock(fd->fd_file, LOCK_SH);
#endif
#ifdef TIME_STAMP
		fd->fd_timestamp = time_stamp++;
#else
		/* nothing */;
#endif
	}
	return(TRUE);
}

/*
 * release_dfile:
 *	Close down an old data file descriptor because
 *	we need a new one and Unix won't give us any more
 */
void
release_dfile()
{
	Int	i;
	DFile	*d, *bestd;
	Int	best_nopens = 999999; 

	for (i = 0; i < MAXFHASH; i++) {
		if (dfile_table[i] == DFileNULL)
			continue;
		for (d = dfile_table[i]; d != NULL; d=d->fd_next) {
			if ( d->fd_file != FileNULL &&
				d->fd_nopens < best_nopens) {
				bestd = d;
				best_nopens = d->fd_nopens;
			}
		}
	}
	if (best_nopens == 999999)
		fatal("dfile_tables_corrupted");
#ifdef DBUG
	debug(DESC_DBUG) dfile_print("Release DFile:",bestd);
#endif DBUG
	close(bestd->fd_file); /* also UNLOCKS */
	bestd->fd_file = FileNULL;
	n_data_files--;
}

#ifdef DBUG
/*
 * dfile_print:
 *	Dump contents of data file descriptor
 */
void
dfile_print(label,fd)
String label;
DFile *fd;
{
	Int	i;
	Word	fid;

	fprintf(stderr,"===%s\n",label);
	if (fd == DFileNULL) {
		fprintf(stderr,"DFileNULL\n");
		return;
	}
	fid = fd->fd_datafile;
	fprintf(stderr,"%s/%02d.%04d nx:%x no:%d fp:%x op:%x",
		fd->fd_name,fileLevel(fid),fileNumber(fid),
		fd->fd_next,fd->fd_nopens,fd->fd_file);
}
#endif DBUG
