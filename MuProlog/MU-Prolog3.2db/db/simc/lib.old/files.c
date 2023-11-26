/*
 * files.c  -  deductive database package (file management operations)
 *
 * $Header: files.c,v 1.5 85/12/06 15:09:58 jas Exp $
 * $Log:	files.c,v $
 * Revision 1.5  85/12/06  15:09:58  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:24:21  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:37  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:48:59  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:35  jas
 * Initial revision
 * 
 * 
 */

#include "muddlib.h"


/*
 * Global data/segment file variables
 */

Int	time_stamp = 1;		 /* Timestamp for sharing segment buffers */


/*
 * search:
 *	Look up descriptor for relation/segment/data_file in hash table
 */
Bool
search(name, discrim, mode, table, table_size, pointer, location)
String name;
Int discrim;
Int mode;
Hash *table[];
Int table_size;
Hash ***pointer;
Hash **location;
{
	r_Char	*c;
	r_Int	hash;
	r_Hash	**ptr;
	r_Hash	*next;

	if (mode == STRING_MODE) {
		hash = 0;
		for (c = name; *c != ChNULL; c++)
			hash += *c;
		hash += discrim;
	}
	else
		hash = (Int)name + discrim;

	hash = (hash & 0x7fff) % table_size;

	ptr = &table[hash];
	next = table[hash];
	/*
	 * For STRING mode, we need to do full string comparison
	 * otherwise, we only need to compare pointers
	 */
	if (mode == STRING_MODE)
		while (next != NULL) {
			if (discrim == next->h_discrim &&
				streq(name,next->h_name)) {
				*location = next;
				*pointer = ptr;
				return(TRUE);
			}
			else {
				ptr = &(next->h_next);
				next = next->h_next;
			}
		}
	else
		while (next != NULL) {
			if (discrim == next->h_discrim &&
				name == next->h_name) {
				*location = next;
				*pointer = ptr;
				return(TRUE);
			}
			else {
				ptr = &(next->h_next);
				next = next->h_next;
			}
		}

	*location = NULL;
	*pointer = ptr;
	return(FALSE);
}


/*
 * Global variables for data file descriptors
 */

Int	n_data_files = 0;

DFile	*dfile_table[MAXFHASH] = { DFileNULL, };

/*
 * open_data_file:
 *	Open a data file, possibly deallocating another
 */
Bool
open_data_file(fd,op)
DFile *fd;
Opn op;
{
	Bool	need_to_lock;
	Char	buf[MAXPATH+MAXRNAME+5];

	need_to_lock = (fd->fd_file == FileNULL || fd->fd_nopens == 0);

	if (fd->fd_file == FileNULL) {
		sprintf(buf, "%s/%04d", fd->fd_name, fd->fd_datafile);
		if (n_data_files >= MAXDATAFILES)
			release_dfile();
		if ((fd->fd_file = open(buf, O_RDWR, 0644)) < 0)
			return(FALSE);
		n_data_files++;
	}
	if (iswriting(op))
#ifdef LOCK_SH
#ifndef elxsi
		flock(fd->fd_file, LOCK_EX);
#else
		/* do nothing */;
#endif
#else
		/* do nothing */;
#endif
	or (isquery(op) && need_to_lock) {
#ifdef LOCK_SH
#ifndef elxsi
		flock(fd->fd_file, LOCK_SH);
#endif
#endif
		fd->fd_timestamp = time_stamp++;
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

/*
 * dfile_open:
 *	Open a file pointer to a data segment
 *	Maybe we use an already available descriptor
 */
DFile *
dfile_open(db_rel,data_file,operation)
String db_rel;
Int data_file;
Opn operation;
{
	DFile	**ptr;
	DFile	*desc;

	/*
	 * If search ok, use existing descriptor
	 */
	if (search(db_rel, data_file, POINTER_MODE, (Hash**)dfile_table,
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
	desc->fd_timestamp = 0;
	desc->fd_datafile = data_file;
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
 * db_cleanup:
 *	Clean up database and unlock files when unusual condition occurs
 */
void
db_cleanup()
{
	/*
	 * Traverse rdesc_table looking for open relation descriptor files
	 */

	/*
	 * Traverse dfile_table looking for open data segments
	 */
}

/*
 * addressofindex:
 *	Convert a segment number into the relative location in the
 *	file where the segment is stored (may need to be scaled)
 *	This implements the recursive linear hashed file structure
 */
Int
addressofindex(segno,level)
Int segno;
Int level;
{
	r_Int	c;
	r_Int	prod;
	r_Int	lower;
	Int	magic();

	if (level == 0)
		c = segno;
	else {
#ifdef GENERALISED
		for (i = 0, prod = 1; i < level; i++)
			prod *= Ki;
#else
		prod = 1 << (log2Ki * level);
#endif
		c = (segno+1) * prod - 1;
	}
	return(magic(c) + level);
}

/*
 * ovflowaddress:
 *	Compute location of "num"th overflow segment at level "lev"
 */
Int
ovflowaddress(num,lev)
Int num;
Int lev;
{
	r_Int	c;
	Int	magic();

#ifdef GENERALISED
	for (i = 0, c = num; i < lev; i++)
		c *= Ki;
#else
	c = num << (log2Ki * lev);
#endif
	return(magic(c));
}

/*
 * magic:
 *	Magic function to convert ceiling'ed index to location
 */
private Int
magic(c)
int c;
{
	int	prod;
	int	sum, sum1;

	sum = c;
	prod = Ki;

	while ((sum1 = c/prod) >= 1) {
		sum += sum1;
		prod *= Ki;
	}
	return(sum);
}

/*
 * ceil:         _   _
 *	Compute | x/y |
 */
Int
ceil(x,y)
{
	r_Int	divide, lower;

	divide = x / y;
	lower = divide * y;
	if (lower < x)
		return(divide + 1);
	else
		return(divide);
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

	fprintf(stderr,"===%s\n",label);
	if (fd == DFileNULL) {
		fprintf(stderr,"DFileNULL\n");
		return;
	}
	fprintf(stderr,"%s/%04d nx:%x no:%d fp:%x op:%x",
		fd->fd_name,fd->fd_datafile,
		fd->fd_next,fd->fd_nopens,fd->fd_file);
}

#endif DBUG
