/*
 * reln.c  -  deductive database package (relation operations)
 *
 * $Header: reln.c,v 1.5 85/12/06 15:10:17 jas Exp $
 * $Log:	reln.c,v $
 * Revision 1.5  85/12/06  15:10:17  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:24:14  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:33  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:48:54  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:41  jas
 * Initial revision
 * 
 * 
 */

#include "muddlib.h"

/*
 * rel_create:
 *	Create a new relation in the specified database
 */
Bool
rel_create(db_name, rel_name, typ, dr, ds, kr, ks, ur, us, nr, ns, nd, av, skel)
String db_name;	/* name of directory containing database */
String rel_name;	/* relation name */
Word typ;	/* which indexing scheme to use */
Int dr, ds;	/* total # bits in record and segment descriptors */
Int kr, ks;	/* # bits to be set in rec and seg descriptors */
Int ur, us;	/* # bits to be used in rec and seg descriptors */
Int nr, ns;	/* records per segment, max segments in data file */
Int nd, av;	/* number of data files, average record size */
String skel;	/* string describing structure of keys */
{
	r_Int	i,j, maxs;
	r_Int	s1, b1;
	r_Char	*c;
	r_Reln	*info;
	r_Int	num_bytes;
	r_Word	*b, *buf;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Long	off;
	Int	total_segs;
	Int	total_blks;
	File	segf;
	File	descf;
	Bool	status = FALSE;
	Char	filename[MAXPATH];
	Char	reldir[MAXPATH];
	Char	cmd[MAXPATH+10];
	struct	stat fstatus;
	extern	Int errno;
	Int	ceil();

	/*
	 * Allocate a static info descriptor for relation
	 */
	if ((info = mknew(Reln)) == RelnNULL) {
		error("out_of_memory");
		return(FALSE);
	}

	/*
	 * Create/open files & directories for relation
	 */
	sprintf(reldir, "%s/%s", db_name, rel_name);
	if (stat(reldir,&fstatus) < 0) {
		if (errno == ENOENT) {
			sprintf(cmd, "mkdir %s", reldir);
			system(cmd);
			if (stat(reldir,&fstatus) < 0) {
				fprintf(stderr, "Can't create %s\n", reldir);
				exit(1);
			}
		}		
		else {
			fprintf(stderr, "Can't find %s\n", reldir);
			exit(1);
		}
	}
	or ((fstatus.st_mode & S_IFDIR) == 0) {
		fprintf(stderr, "%s not a directory\n", reldir);
		exit(1);
	}

	sprintf(filename, "%s/%s/desc", db_name, rel_name);
	if ((info->rd_name = mkstr(strlen(filename))) == StrNULL) {
		error("no_mem_for_name");
		free((char *)info);
		return(FALSE);
	}
	strcpy(info->rd_name, filename);
	if ((descf = open(filename, (O_WRONLY|O_CREAT|O_TRUNC), 0644)) < 0) {
		error("open_desc_file");
		free((char *)info->rd_name);
		free((char *)info);
		return(FALSE);
	}	

	/*
	 * Set up indexing scheme type for relation
	 */
	switch (info->rd_index = typ)
	{
	when SIMC_INDEX:
		info->rd_sdesc_block = info->rd_block_size = 0;
		total_segs = maxs = ns;
	when DSIMC_INDEX:
#ifdef TODO
		Make 4096 into a parameter
#endif TODO
		info->rd_block_size = 4096;
		i = 1; j = 0;
		while (ds*nbytes(i) <= 4096) {
			i <<= 1; j++;
		}
		if (i < 2) {
			error("block_too_small_for_seg_desc");
			free((char *)info->rd_name);
			free((char *)info);
			return(FALSE);
		}
		info->rd_sdesc_block = j-1;

		/*
		 * Determine total number of segments needed, including
		 * user required segments + overflow segments
		 */
		for (i = 0; i < MAXOVLEVELS; i++)
			info->rd_nlevsegs[i] = 0;
		info->rd_nlevsegs[0] = s1 = ns;
		for (i = 1; i < MAXOVLEVELS; i++)
		{
			s1 = ceil(s1,Ki);
			info->rd_nlevsegs[i] = s1;
			if (s1 == 1) break;
		}
		total_segs = addressofindex(0,i)+1;
		total_blks = ceil(ns,segs_per_block(info));
#if 0
printf("Ns:%d total_segs:%d total_blks:%d\n",ns,total_segs,total_blks);
#endif

	otherwise:
		error("bad_index_type");
		free((char *)info->rd_name);
		free((char *)info);
		return(FALSE);
	}

	/*
	 * Set up constant parameters for relation
	 * (describing descriptor sizes, #segs, #recs, etc.)
	 */
	info->rd_rdescsz = dr;
	info->rd_rnbits = kr;
	info->rd_rubits = ur;
	info->rd_nrecs = nr;
	info->rd_sdescsz = ds;
	info->rd_snbits = ks;
	info->rd_subits = us;
	info->rd_nsegs = ns;
	info->rd_ndfiles = nd>ns ? ns : nd;
	info->rd_avreclen = av;
	info->rd_seg_buf_size = nr * sizeof(Recptr) + nbytes(nr) * (dr+1) + sizeof(Segptr);

	/*
	 * Round segment size up to nearest page boundary
	 */
	info->rd_seg_size = info->rd_seg_buf_size + nr * av;
#if 0
printf("seg_size:%d\n", info->rd_seg_size);
#endif
	info->rd_seg_size = alignup(info->rd_seg_size, PAGESIZE);
	info->rd_local_segs = ceil(total_segs,nd);
	info->rd_total_segs = total_segs;

	/*
	 * Align record counters to start on a Word boundary
	 * Align segment descriptors to start on page boundary
	 */
	off = info_bytes(info) + strlen(skel) + 1;
	info->rd_cntroffset = off = alignup(off, sizeof(Word));
	off += (total_segs+2) * sizeof(Word);
	info->rd_descoffset = alignup(off, PAGESIZE);

	/*
	 * Set up skeleton, describing structures of keys
	 * Also, sets up mask size field in info structure
	 * and stores cluster masks
	 */
	if (!skel_parse(info,skel)) {
		error("template_parse");
		goto RelCreateFail;
	}
#ifdef DBUG
	debug(TREE_DBUG)
		skel_print("Unnormalised skel tree:\n",info->rd_skel);
#endif DBUG

	/*
	 * Set up size of codewords (excluding mask bits)
	 */
	info->rd_rcwordsz = dr - info->rd_masksz;
	info->rd_scwordsz = ds - info->rd_masksz;

#ifdef DBUG
#ifdef TODO
	Store normalised form of skeleton string in desc file
#endif TODO
	debug (TREE_DBUG) {
		if (!skel_normalise(info)) {
			error("template_normalise");
			goto RelCreateFail;
		}
		skel_print("Normalised skel tree:\n",info->rd_skel);
	}
#endif DBUG

	/*
	 * Write static relation info to the descriptor file,
	 */
	num_bytes = info_bytes(info);
	if (write(descf, static_info(info), num_bytes) != num_bytes) {
		error("reln_make");
		goto RelCreateFail;
	}
	num_bytes = strlen(skel) + 1;
	if (write(descf, skel, num_bytes) != num_bytes) {
		error("reln_make");
		goto RelCreateFail;
	}

	/*
	 * Initialise record counters to zeroes
	 */
	num_bytes = (total_segs+2) * sizeof(Word);
	if ((buf = (Word *)malloc(num_bytes)) == WordNULL) {
		error("no_mem_for_zero_buf");
		goto RelCreateFail;
	}
	for (i = 0, b = buf; i < total_segs; i++, b++)
		*b = 0;
	*b++ = total_segs-1;	/* first overflow segment */
	*b = 0;		/* count of records in db */
	lseek(descf, (Faddr)info->rd_cntroffset, L_SET);
	if (write(descf, (char *)buf, num_bytes) != num_bytes) {
		error("writing_zero_buf");
		goto RelCreateFail;
	}
	free((char *)buf);

	/*
	 * Initialise segment descriptors to zeroes
	 * This is a bit sneaky and relies on the fact that if
	 * we seek way beyond the end of the file and write a
	 * little piece of text, everything in between becomes zero
	 */
	switch (index_type(info))
	{
	when SIMC_INDEX:
		lseek(descf, (Faddr)(offset(info) + ds*nbytes(ns)), L_SET);
	when DSIMC_INDEX:
		i = total_blks + 1;
		lseek(descf, (Faddr)(offset(info) + i*block_size(info)), L_SET);
	}
	write(descf, "End desc file", 13);

	/*
	 * Allocate a segment buffer and initialise it
	 */
	num_bytes = info->rd_seg_buf_size;
	if ((buf = (Word *)malloc(num_bytes)) == WordNULL) {
		error("no_mem_for_seg_buf");
		goto RelCreateFail;
	}
	for (i = 0, b = buf; i < nwords(nr)*(dr+1); i++, b++)
		*b = 0;
	rec_ptr = (Recptr *)b;
	for (i = 0; i < nr; i++) {
		rec_ptr[i].rp_nextfree = i+1;
		rec_ptr[i].rp_blksize = 0;
		rec_ptr[i].rp_offset = UNUSED;
	}
	rec_ptr[nr-1].rp_nextfree = -1;
	seg_ptr = (Segptr *)&rec_ptr[nr];
	seg_ptr->sp_localfree = 0;
	seg_ptr->sp_extrnfree = -1;

	/*
	 * Create segment data files
	 */
	for (i = 0; i < nd; i++)
	{
		sprintf(filename, "%s/%s/%04d", db_name, rel_name, i);
		if ((segf = open(filename,(O_WRONLY|O_CREAT|O_TRUNC),0664)) < 0) {
			error("seg_create");
			goto RelCreateFail;
		}
		seg_ptr->sp_freespace = num_bytes;
		for (j = 0; j < info->rd_local_segs; j++) {
			lseek(segf, (Faddr)j*info->rd_seg_size, L_SET);
			if (write(segf, (char*)buf, num_bytes) != num_bytes) {
				error("writing_seg_file");
				goto RelCreateFail;
			}
			seg_ptr->sp_freespace += info->rd_seg_size;
		}
		lseek(segf, (Faddr)j*info->rd_seg_size, L_SET);
		write(segf, "Overflow space",  15);
		close(segf);
	}

	status = TRUE;

RelCreateFail:
	skel_free(info->rd_skel);
	free((char *)info->rd_name);
	free((char *)buf);
	free((char *)info);
	if (isopen(segf)) close(segf);
	if (isopen(descf)) close(descf);
	return(status);
}

/*
 * Global variables for relation descriptors
 */

Int	n_rel_files = 0;

Reln	*reln_table[MAXRHASH] = { RelnNULL, };


/*
 * open_rel_file:
 *	Open a relation info file, possibly de-allocating another
 */
Bool
open_rel_file(rel)
Reln *rel;
{
	char	buf[MAXPATH+MAXRNAME+10];
	void	release_rel_file();

	if (rel->rd_descf == FileNULL) {
		sprintf(buf, "%s/desc", rel->rd_name);
		if (n_rel_files >= MAXRINFFILES)
			release_rel_file();
		if ((rel->rd_descf = open(buf, O_RDWR, 0644)) < 0)
			return(FALSE);
		n_rel_files++;
	}
	return(TRUE);
}

/*
 * release_rel_file:
 *	Close down an old relation file descriptor because
 *	we need a new one and Unix won't give us any more
 */
void
release_rel_file()
{
	Int	i;
	Reln	*d, *bestd;
	Int	best_nopens = 999999; 

	for (i = 0; i < MAXRHASH; i++) {
		if (reln_table[i] == RelnNULL)
			continue;
		for (d = reln_table[i]; d != RelnNULL; d = d->rd_next) {
			if (d->rd_descf != FileNULL &&
				d->rd_nopens < best_nopens) {
				bestd = d;
				best_nopens = d->rd_nopens;
			}
		}
	}
	if (best_nopens == 999999)
		fatal("reln_tables_corrupted");
#ifdef DBUG
	debug(DESC_DBUG) rel_print("Release Reln:",bestd);
#endif DBUG
	close(bestd->rd_descf); /* also UNLOCKS */
	bestd->rd_descf = FileNULL;
	n_rel_files--;
}

Char	skelbuf[512];	/* buffer for template string */

/*
 * rel_open:
 *	Set up descriptor for static information about relation
 *	Look in hash table to see if we already have a descriptor
 *	If we do, use that one, otherwise make a new one
 */
Reln *
rel_open(db_name, rel_name, operation)
String db_name;
String rel_name;
Opn operation;
{
	Reln	**ptr;
	Reln	*info;
	Int	lock_type;
	Int	num_bytes;
	Char	db_rel[MAXPATH+MAXRNAME+2];

	sprintf(db_rel, "%s/%s", db_name, rel_name);
	num_bytes = strlen(db_rel);

	/*
	 * If search ok, use existing descriptor
	 */
	if (search(db_rel, -num_bytes, STRING_MODE, (Hash**)reln_table,
			MAXRHASH, (Hash***)(&ptr), (Hash**)(&info))) {
		if (!open_rel_file(info))
			fatal("reopen_rel_file");
		info->rd_operation = operation;
		info->rd_nopens++;
		goto RelOpenSucceed;
	}

	/*
	 * Create new static info descriptor
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr, "new_rinfo:");
#endif DBUG
	if ((info = mknew(Reln)) == RelnNULL) {
		error("out_of_memory");
		return(RelnNULL);
	}
	info->rd_descf = FileNULL;
	info->rd_next = RelnNULL;
	
	/*
	 * Fill in other "dynamic" information
	 */
	info->rd_nopens = 1;
	info->rd_operation = operation;
	info->rd_length = -num_bytes;
	if ((info->rd_name = mkstr(num_bytes)) == StrNULL) {
		error("no_mem_for_name_buf");
		goto RelOpenFail;
	}
	strcpy(info->rd_name, db_rel);

	/*
	 * Fill in most of static info from descriptor file
	 */
	if (!open_rel_file(info)) {
		error("open_rel_file");
		goto RelOpenFail;
	}
	num_bytes = info_bytes(info);
	if (read(info->rd_descf, static_info(info), num_bytes) != num_bytes) {
		error("rel_header");
		goto RelOpenFail;
	}

	/*
	 * Make template tree
	 */
#ifdef TODO
	Would be nice if the TREE, rather than template string, was stored
	Or, at the very least, a Normalised template string
#endif TODO
	read(info->rd_descf, skelbuf, 256/*sizeof(buf)*/);
	if (!skel_parse(info, skelbuf)) {
		error("template_parse");
		goto RelOpenFail;
	}
	if (!skel_normalise(info)) {
		error("template_normalise");
		goto RelOpenFail;
	}

	/*
	 * Link into rinf hash table
	 */
	*ptr = info;

RelOpenSucceed:
#ifdef LOCK_SH
	lock_type = iswriting(operation) ? LOCK_EX : LOCK_SH;
#ifndef elxsi
	if (flock(info->rd_descf, lock_type) < 0)
		fatal("seg_desc_lock_failed");
#endif
#endif
#ifdef DBUG
	debug(DESC_DBUG) rel_print("Open Reln:",info);
#endif DBUG
	return(info);

RelOpenFail:
	if (info->rd_descf != FileNULL) {
		close(info->rd_descf);
		info->rd_descf = FileNULL;
	}
XX	cfree(info->rd_name, Char);
XX	cfree(info, Reln);
	return(RelnNULL);
}


/*
 * rel_close:
 *	One transaction has finished with static relation info
 */
void
rel_close(rel)
Reln *rel;
{
	if (rel == RelnNULL)
		return;

#ifdef LOCK_SH
#ifndef elxsi
	if (--rel->rd_nopens == 0 && rel->rd_descf != FileNULL)
		flock(rel->rd_descf, LOCK_UN);
#endif
#else
	rel->rd_nopens--;
#endif
#ifdef DBUG
	debug(DESC_DBUG) rel_print("Close Reln:", rel);
#endif DBUG
}

#ifdef DBUG

/*
 * rel_print:
 *	Dump contents of relation info file descriptor
 */
void
rel_print(label, rel)
String label;
Reln *rel;
{
	Int	i;

	fprintf(stderr,"%s\n",label);
	if (rel == RelnNULL) {
		fprintf(stderr,"RelnNULL\n");
		return;
	}
	fprintf(stderr,"Reln@%x %s/%d nx:%x no:%d fp:%x op:%x",
		rel, rel->rd_name, rel->rd_length, rel->rd_next,
		rel->rd_nopens, rel->rd_descf, rel->rd_operation);
	fprintf(stderr, "coff:%lx doff:%lx Nr:%d Ns:%d M:%d Br:%d Bs:%d Kr:%d Ks:%d\n",
		rel->rd_cntroffset, rel->rd_descoffset,
		rel->rd_nrecs, rel->rd_nsegs,
		rel->rd_masksz, rel->rd_rcwordsz, rel->rd_scwordsz,
		rel->rd_rnbits, rel->rd_snbits);
	if (rel->rd_skel == ElemNULL)
		fprintf(stderr,"No skel\n");
	else
		skel_print("skel:",rel->rd_skel);
}

#endif DBUG
