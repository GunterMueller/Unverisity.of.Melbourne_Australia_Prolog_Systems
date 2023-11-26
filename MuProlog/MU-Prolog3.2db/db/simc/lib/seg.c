/*
 * seg.c  -  deductive database package (segment operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 */

#include "simc.h"


/*
 * "Block" manipulation functions
 * A Block is an efficiency/extensibility hack for
 * the DSIMC index scheme ... it is basically just
 * a set of segment descs in a suitable block size
 * for the underlying OS/disk system
 * Using blocks, we can easily add more segments to
 * the file without requiring wholesale reorganisation
 * of the segment descriptor slices all the time
 * (i.e. the slices don't grow, they get stored in
 * fixed-size groups, and we add more groups if needed)
 */

/*
 * next_seg_in_block:
 *	Determine next seg in this block which contains possible matches
 */
Int
next_seg_in_block(tr)
Trans *tr;
{
	Word	seg_mask;
	Int	block_base, seg_off, new_seg;
	r_Reln	*rel = tr->relation;
	r_Int	lev = tr->cur_level;

	seg_mask = segs_per_block(rel) - 1;
	block_base = tr->cur_seg & ~seg_mask;
#if 0
fprintf(stderr,"seg_off: %d ",tr->seg_off);
b_display("seg matches:",tr->seg_matches,segs_per_block(rel));
#endif
	tr->seg_off = new_seg =
		b_nextset(tr->seg_off+1, tr->seg_matches, segs_per_block(rel));

#if 0
fprintf(stderr,"newseg:%d\n",new_seg);
#endif
	if (new_seg < 0)
		return(-1);
	else {
		new_seg = block_base + new_seg;
		while (tr->cur_seg != new_seg && tr->cur_seg >= 0) {
			tr->cur_seg = b_nextset(tr->cur_seg+1, tr->ov_matches[lev], Nls(rel,lev));
#if 0
fprintf(stderr,"nsib suggests %d\n",tr->cur_seg);
#endif
		}
		return(tr->cur_seg);
	}
}

/*
 * next_block:
 *	Determine next block which contains likely seg matches
 */
Int
next_block(tr)
Trans *tr;
{
	Word	seg_mask;
	Int	block_base, new_block;
	r_Int	lev = tr->cur_level;
	r_Reln	*rel = tr->relation;

	seg_mask = segs_per_block(rel) - 1;
	block_base = tr->cur_seg & ~seg_mask;

#if 0
fprintf(stderr,"nb starts @ %d\n",tr->cur_seg);
#endif
	if (tr->cur_seg < 0)
		return(-1);
	do {
		tr->cur_seg = b_nextset(tr->cur_seg+1, tr->ov_matches[lev], Nls(rel,lev));
#if 0
fprintf(stderr,"nb suggests %d\n",tr->cur_seg);
#endif
		if (tr->cur_seg < 0) {
#if 0
fprintf(stderr,"no more segs on level %d\n",tr->cur_level);
#endif
			return(-1);
		}
		new_block = tr->cur_seg & ~seg_mask;
	} while (new_block == block_base);
	tr->seg_off = -1;
#if 0
fprintf(stderr,"nb:suggest %d\n",tr->cur_seg);
#endif
	return(tr->cur_seg);
}

/*
 * do_next_seg:
 *	Determine the number of the next segment to look at
 */
Int
do_next_seg(tr)
Trans *tr;
{
	r_Int	lev;
	r_Reln	*rel = tr->relation;

	switch (index_type(rel))
	{
	when SIMC_INDEX:
		tr->cur_seg = b_nextset(tr->cur_seg+1, tr->seg_matches, Ns(rel));
	when DSIMC_INDEX:
		if (tr->cur_level == 0) 
		{
			while (next_seg_in_block(tr) < 0)
			{
				if (next_block(tr) < 0) {
					tr->cur_level++;
					tr->cur_seg = -1;
					goto OverflowLevels;
				}
				setup_block_seg_matches(tr);
			}
		}
		else {
OverflowLevels:
			lev = tr->cur_level;
			if (lev < MAXLEVELS && Nls(rel, lev) > 0) {
				tr->cur_seg = b_nextset(tr->cur_seg+1,
							tr->ov_matches[lev],
							Nls(rel,lev));
				if (tr->cur_seg < 0) {
					tr->cur_level++;
					tr->cur_seg = -1;
					goto OverflowLevels;
				}
			}
		}
	}
	return(tr->cur_seg);
}


/*
 * Segment matching global vars
 */

private	Word	*par_matches;	/* holds segment matches for each key */
private	Word	*slice;		/* buffer to read bit-sliced seg descriptors */
private	Int	slice_size;	/* how many bits in seg match bit-strings */
private	Word	*index_block;	/* block of seg descs for DSIMC index scheme */

/*
 * setup_seg_matches:
 *	Setup the seg_matches bit string in a transaction
 */
void
setup_simc_seg_matches(tr)
Trans *tr;
{
	r_Reln	*rel = tr->relation;
	void	do_seg_matches();

	/*
	 * The bits set in "matches" tell us which segments to fetch
	 * Initially we set all bits to 1 ... assume everything matches
	 *  they will be successively AND'ed out as we scan seg descs
	 */
	b_ones(tr->seg_matches, Ns(rel));

	slice_size = Ns(rel);
	slice = b_make(slice_size);
	par_matches = b_make(slice_size);

	/*
	 * For each key, we form a bit-sliced string of
	 * potential matches and AND all these together
	 */
	do_seg_matches(tr, tr->query);

	/*
	 * Free up bit-slice buffers
	 */
	b_free(slice); slice = WordNULL;
	b_free(par_matches); par_matches = WordNULL;
#ifdef DBUG
	debug(MATCH_DBUG) b_display("seg matches:",tr->seg_matches,slice_size);
#endif DBUG
#if 0
b_display("seg matches:",tr->seg_matches,slice_size);
#endif
}

/*
 * setup_dsimc_seg_matches:
 *	Set up some dsimc transaction structures (current seg, etc.)
 *	Generate all permutations for hash value for a DSIMC query
 *	Produces a bit-list of "interesting" segments on ALL levels
 */
void
setup_dsimc_seg_matches(tr)
Trans *tr;
{
	Word	hash, gaps;
	r_Int	lev, perm, ncombs;
	r_Reln	*rel = tr->relation;

	tr->cur_seg = segmentOfHash(rel, tr->cluster, 0);

	/*
	 * Take the "stars" vector and remove all stars which
	 * will not contribute to the segment number (i.e. for
	 * a level of depth d, consider only the first d stars)
	 * Otherwise b_next_perm generates 2^(31-d) unnecessary
	 * permutations below
	 */
	lev = depth(rel, 0);
	hash = tr->cluster;
	gaps = tr->clustars & ((01 << (lev+1)) - 1) << (MAXBITS-lev-1);
	ncombs = 01 << b_count(&gaps, WORDSIZE);
#if 0
fprintf(stderr,"hash:%08x gaps:%08x ncombs:%d\n",hash,gaps,ncombs);
#endif

	/*
	 * Generate all permutations, and set bits in matching segs vector
	 * for all levels of the database
	 */
	for (perm = 0; perm < ncombs; perm++) {
		b_next_perm(&hash, gaps, perm);
#if 0
fprintf(stderr,"suggest [%08x] (%d,0)",hash,segmentOfHash(rel,hash,0));
#endif
		for (lev = 0; lev < MAXLEVELS; lev++) {
			if (tr->ov_matches[lev] == WordNULL) break;
#if 0
if (lev > 0)
fprintf(stderr," -> (%d,%d)",segmentOfHash(rel,hash,lev), lev);
#endif
			b_set(segmentOfHash(rel,hash,lev), tr->ov_matches[lev], Nls(rel,lev));
		}
#if 0
putc('\n',stderr);
#endif
	}
#if 0
for (lev = 0; lev < MAXLEVELS; lev++) {
	if (tr->ov_matches[lev] == WordNULL) break;
	fprintf(stderr,"level %d: ",lev);
	b_fprint(stderr, tr->ov_matches[lev], Nls(rel,lev));
	putc('\n',stderr);
}
#endif
}


/*
 * setup_block_seg_matches:
 *	Read the block into a buffer, compute potential matching segments,
 *	then throw away the buffer
 */
setup_block_seg_matches(tr)
Trans *tr;
{
	r_Int	segno, block;
	r_Word	seg_mask;
	r_Reln	*rel = tr->relation;
	r_Int	lev = tr->cur_level;
	Int	which_block;
	Int	block_offset;

	slice_size = segs_per_block(rel);
	seg_mask = segs_per_block(rel) - 1;
	which_block = tr->cur_seg >> log2_segs_per_block(rel);
	block_offset = (Faddr)(offset(rel) + which_block*block_size(rel));

	/*
	 * Set up the initial seg_matches bit string using
	 * the segments suggested by the "*"s (from seg_gaps)
	 */
	b_zero(tr->seg_matches, slice_size);
	segno = tr->cur_seg;
	do {
#if 0
fprintf(stderr,"sno:%d sno&mask:%d\n", segno, segno&seg_mask);
#endif
		b_set(segno & seg_mask, tr->seg_matches, slice_size);
		segno = b_nextset(segno+1, tr->ov_matches[lev], Nls(rel, lev));
		block = segno >> log2_segs_per_block(rel);
	} while (block == which_block);
#if 0
b_display("suggested block seg matches:",tr->seg_matches,slice_size);
#endif

	/*
	 * If there are not many combinations,
	 * don't bother checking seg desc matches
	 * (saves having to read the seg desc block
	 *  at the expense of reading 1 or 2 unnecessary segs)
	 */
#ifdef DBUG
	debug(GET_DBUG) {
		fprintf(stderr, "block:%d  ", which_block);
		b_display("seg slice: ",tr->seg_matches,slice_size);
	}
#endif DBUG
	if (b_count(tr->seg_matches, slice_size) <= 2)
		goto CleanUpSegMatches;

	/*
	 * Grab a copy of the index block
	 */
	if ((index_block = (Word *)malloc(block_size(rel))) == WordNULL) {
		error("no_mem_for_index_block");
		return;
	}
	lseek(descfile(rel), block_offset, L_SET);
	read(descfile(rel), (char *)index_block, block_size(rel));
	tr->nblocks_fetched++;

	/*
	 * Now that inital seg_matches string is set up,
	 * we perform the AND/OR trick to find final seg_matches
	 */
	par_matches = b_make(slice_size);
	do_seg_matches(tr, tr->query);

CleanUpSegMatches:
	/*
	 * Clean up afterwards
	 */
	cfree(index_block, Word);
	b_free(par_matches);
#ifdef DBUG
	debug(GET_DBUG) b_display("block seg matches:",tr->seg_matches,slice_size);
#endif DBUG
#if 0
b_display("block seg matches:",tr->seg_matches,slice_size);
#endif
}

/*
 * do_seg_matches:
 *	Perform the operation of generating segment match bit-string
 */
void
do_seg_matches(tr,node)
Trans *tr;
Elem *node;
{
	r_Int	i, *bit;
	r_Int	nbits_to_use = 0;
	r_Reln *rel = tr->relation;
	Faddr	slice_offset;

	if (node->e_isvar) {
		b_ones(par_matches, slice_size);
	}
	else {
		/*
		 * Recursively repeat this process for all sub-elements
		 */
		for (i = 0; i < node->e_maxargs; i++) {
			do_seg_matches(tr, node->e_args[i]);
			b_and(tr->seg_matches, par_matches, par_matches, slice_size);
		}

		/*
		 * AND:{b1,b2, ... ,bn}
		 */
		if (node->e_snbits == 0)
			nbits_to_use = 0;
		else {
			nbits_to_use = (Int)(tr->seg_ratio * (float)node->e_snbits + 1.0);
			if (nbits_to_use > node->e_snbits)
				nbits_to_use = node->e_snbits;
		}
#ifdef DBUG
		debug(MATCH_DBUG)
			fprintf(stderr,"%s:sbits_to_use:%d\n",node->e_name,nbits_to_use);
#endif DBUG
		bit = node->e_segbits;
		b_copy(tr->seg_matches, par_matches, slice_size);
		for (i = 0; i < nbits_to_use; i++, bit++)
		{
			switch (index_type(rel))
			{
			when SIMC_INDEX:
				slice_offset = (Faddr)(offset(rel) +
						nBytes(slice_size) * (*bit));
				lseek(descfile(rel), slice_offset, L_SET);
				read(descfile(rel), (char *)slice, nBytes(slice_size));
			when DSIMC_INDEX:
				slice = &(index_block[*bit * nWords(slice_size)]);
			}
			b_and(slice, par_matches, par_matches, slice_size);
#ifdef DBUG
			debug(MATCH_DBUG) {
				fprintf(stderr,"%15s:bit:%d:\n",node->e_name,*bit);
				b_display("\t\ts_ANDslice:",slice,slice_size);
				b_display("\t\ts_AND_res :",par_matches,slice_size);
			}
#endif DBUG
			tr->nsdescs_anded++;
		}

		/*
		 * OR mask bit with result of AND'ing codeword bits
		 * But we don't bother for fields which are always
		 * going to be constant (such as the relation name)
		 */
		if (node->e_rmaskpos > 0) {
			switch (index_type(rel))
			{
			when SIMC_INDEX:
				slice_offset = (Faddr)(offset(rel) +
						nBytes(slice_size) * node->e_smaskpos);
				lseek(descfile(rel), slice_offset, L_SET);
				read(descfile(rel), (char *)slice, nBytes(slice_size));
			when DSIMC_INDEX:
				slice = &(index_block[node->e_smaskpos * nWords(slice_size)]);
			}
			b_or(slice, par_matches, par_matches, slice_size);
#ifdef DBUG
			debug(MATCH_DBUG) {
				fprintf(stderr,"%15s:bit:%d:\n",node->e_name,node->e_smaskpos);
				fprintf(stderr,"%15s:",node->e_name);
				b_display("s_OR_slice:",slice,slice_size);
			}
#endif DBUG
		}
		b_copy(par_matches, tr->seg_matches, slice_size);
#ifdef DBUG
		debug(MATCH_DBUG) {
			fprintf(stderr,"%15s:",node->e_name);
			b_display("s_OR_res  :",par_matches,slice_size);
		}
#endif DBUG
	}
}

/*
 * seg_fetch:
 *	Grab the next segment during a transaction
 *	Must also set up the record matches bit string
 */
void
seg_fetch(tr)
Trans *tr;
{
	r_Reln	*rel = tr->relation;

#ifdef DBUG
	debug(GET_DBUG) fprintf(stderr,"fetch seg(%d,%d)\n",tr->cur_seg,tr->cur_level);
#endif DBUG

	/*
	 * Open data file for this segment
	 */
	seg_close(tr->segment);
	tr->segment = seg_open(rel, tr->cur_seg ,tr->cur_level, tr->operation);
	if (tr->segment == SegtNULL)
		fatal("fetch_segment");
#ifdef DBUG
	debug(DESC_DBUG)
	fprintf(stderr,"open desc %x :%s/%d: FILE *%x no:%d ndf:%d\n",
		tr->segment, tr->segment->sd_name, tr->segment->sd_segid,
		datafile(tr->segment), tr->segment->sd_nopens, n_data_files);
#endif DBUG

	/*
	 * Initialise record info in transaction descriptor
	 */
	tr->cur_rec = -1;
	if (!isassert(tr->operation))
		setup_rec_matches(tr);
	tr->nsegs_fetched++;
}


/*
 * Global variables for data segment descriptors
 */

Int	n_segments = 0;

Segt	*seg_head = SegtNULL;
Segt	*seg_tail = SegtNULL;

Segt	*seg_table[MAXSHASH] = { SegtNULL, };

/*
 * open_segment:
 *	Open a data segment, possibly deallocating another's seg buffer
 *	to prevent data space growing unbounded (seg buffers are large)
 */
Bool
open_segment(rel, level, sd)
Reln *rel;
Int level;
Segt *sd;
{
	Word	*bb;
	Recptr	*rr;
	Segptr	*ss;
	Int	nchars;
	Bool	need_to_read;
	r_DFile	*fd = sd->sd_file;

#ifdef DBSTATS
	tr->n_segopens++;
#endif
#ifdef TIME_STAMP
	need_to_read = (sd->sd_timestamp != fd->fd_timestamp ||
#else
	need_to_read = (
#endif
			sd->sd_seg_buf == WordNULL ||
			fd->fd_file == FileNULL);

	if (sd->sd_seg_buf == WordNULL) {
		if (n_segments >= MAXSEGMENTS) {
			/*
			 * release old segment buffer
			 */
#if 0
fprintf(stderr, "releasing (%d,%d)\n",
		segNumber(seg_head->sd_segid), segLevel(seg_head->sd_segid));
#endif
			cfree(seg_head->sd_seg_buf, Word);
			n_segments--;
			seg_head = seg_head->sd_qnext;
		}
#ifdef DBUG
		debug(MALLOC_DBUG) fprintf(stderr,"seg_buf:");
#endif DBUG
		if ((sd->sd_seg_buf = (Word *)malloc(seg_size(rel))) == WordNULL) {
			error("Out of memory for segment buffers");
			return(FALSE);
		}
		if (seg_tail == SegtNULL)
			seg_tail = seg_head = sd;
		else {
			seg_tail->sd_qnext = sd;
			seg_tail = sd;
		}
		n_segments++;
	}
	if (need_to_read) {
#ifdef DBSTATS
		tr->n_segreads++;
#endif
#if 0
fprintf(stderr, "reading seg(%d,%d) @ %x\n",
	segNumber(sd->sd_segid), segLevel(sd->sd_segid), sd->sd_offset);
#endif
		if (!open_data_file(fd,sd->sd_operation))
			return(FALSE);
		bb = sd->sd_seg_buf;
		rr = (Recptr *)&bb[nWords(Nr(rel)) * (Dr(rel)+1)];
		ss = (Segptr *)&rr[Nr(rel)];
		lseek(fd->fd_file, sd->sd_offset, L_SET);
		nchars = read(fd->fd_file, (Char *)sd->sd_seg_buf, seg_size(rel));
#if 0
fprintf(stderr, "reading seg(%d,%d) @ %x got nc:%d fs:%d lf:%d\n",
	segNumber(sd->sd_segid), segLevel(sd->sd_segid), sd->sd_offset,
	nchars, ss->sp_freespace, ss->sp_localfree);
#endif
		if (nchars < seg_size(rel)) {
			error("Couldn't read segment");
			return(FALSE);
		}
#ifdef TIME_STAMP
		sd->sd_timestamp = fd->fd_timestamp;
#endif
	}

	return(TRUE);
}

#ifdef OBSOLETE
/*
 * release_sdesc:
 *	Close down an old data segment descriptor because
 *	we don't want to have too many HUGE segment buffers
 */
void
release_sdesc()
{
	Int	i;
	Segt	*d, *bestd;
	Int	best_nopens = 999999; 

	for (i = 0; i < MAXSHASH; i++) {
		if (seg_table[i] == SegtNULL)
			continue;
		for (d = seg_table[i]; d != NULL; d = d->sd_next) {
			if (d->sd_seg_buf != WordNULL) {
				bestd = d;
				best_nopens = d->sd_nopens;
				goto FoundOne;
			}
/*
			if (d->sd_seg_buf != WordNULL &&
				d->sd_nopens < best_nopens) {
				bestd = d;
				best_nopens = d->sd_nopens;
			}
*/
		}
	}
FoundOne:
	if (best_nopens == 999999)
		fatal("seg_tables_corrupted");
#ifdef DBUG
	debug(DESC_DBUG) seg_print("Release Segt:",bestd);
#endif DBUG
	cfree(bestd->sd_seg_buf, Word);
	n_segments--;
}
#endif OBSOLETE

/*
 * seg_open:
 *	Open a data segment descriptor (buffer + data file descriptor)
 *	Maybe we use an already available descriptor
 */
Segt *
seg_open(rel,segno,level,operation)
Reln *rel;
Int segno;
Int level;
Opn operation;
{
	Segt	**ptr;
	Segt	*desc;
	String	db_rel;

	db_rel = dbrelname(rel);

	/*
	 * If search ok, use existing descriptor
	 */
	if (search(db_rel, segAddr(segno,level), POINTER_MODE, (Hash**)seg_table,
			MAXSHASH, (Hash***)(&ptr), (Hash**)(&desc)))
	{
		if (!open_segment(rel, level, desc))
			fatal("reopen_segment");
		desc->sd_operation = operation;
		goto SegSucceed;
	}

	/*
	 * Create new open data segment
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"new_sdesc:");
#endif DBUG
	if ((desc = mknew(Segt)) == SegtNULL) {
		error("no_mem_for_sdesc");
		return(SegtNULL);
	}
	desc->sd_file = DFileNULL;
	desc->sd_seg_buf = WordNULL;
	
	desc->sd_name = db_rel;
	desc->sd_segid = segAddr(segno,level);
	desc->sd_nopens = 0;
#ifdef TIME_STAMP
	desc->sd_timestamp = -2;
#endif
	desc->sd_next = SegtNULL;
	desc->sd_operation = operation;
	desc->sd_offset = (Faddr)((segno % local_segs(rel)) * seg_size(rel));
#if 0
fprintf(stderr,"new seg info for (%d,%d) @ %x in %02d.%04d (%d,%d)\n",
	segno,level,desc->sd_offset,level,
	segno/local_segs(rel),local_segs(rel),seg_size(rel));
#endif
	desc->sd_file = dfile_open(db_rel,segno/local_segs(rel),level,operation);

	if (desc->sd_file == DFileNULL) {
		error("Can't open data file");
		goto SegFail;
	}

	if (!open_segment(rel, level, desc)) {
		error("open_segment");
		goto SegFail;
	}

	/*
	 * Link new desc into hash table
	 */
	*ptr = desc;

SegSucceed:
	desc->sd_nopens++;
	desc->sd_file->fd_nopens++;
#ifdef DBUG
	debug(DESC_DBUG) seg_print("Open Segt:",desc);
#endif DBUG
	return(desc);

SegFail:
	if (desc != SegtNULL) {
		cfree(desc->sd_seg_buf, Word);
		cfree(desc, Segt);
	}
	return(SegtNULL);
}

/*
 * seg_close:
 *	One transaction has finished with data segment
 */
void
seg_close(sd)
Segt *sd;
{
	r_DFile	*fd;

	if (sd == SegtNULL)
		return;

	fd = sd->sd_file;

	sd->sd_nopens--;
	fd->fd_nopens--;
	if (sd->sd_nopens == 0) {
		if (fd->fd_nopens == 0 &&
			fd->fd_file != FileNULL) {
#ifdef LOCK_SH
			flock(fd->fd_file, LOCK_UN);
#endif
#ifdef TIME_STAMP
			fd->fd_timestamp = time_stamp++;
#else
			/* nothing */;
#endif
		}
	}
	or (iswriting(sd->sd_operation)) {
		sd->sd_operation = opQUERY;
#ifdef LOCK_SH
		if (fd->fd_file != FileNULL)
			flock(fd->fd_file, LOCK_SH);
#endif
	}
#ifdef DBUG
	debug(DESC_DBUG) seg_print("Close Segt:",sd);
#endif DBUG
}

/*
 * seg_write:
 *	Dump the contents of a segment buffer into its data file
 */
Bool
seg_write(sd, rel)
Segt *sd;
Reln *rel;
{
	r_Int	nchars;

#ifdef BUG
	if (!open_data_file(seg_dfile(sd), seg_opn(sd)))
#else
	if (!open_data_file(seg_dfile(sd), opASSERT))
#endif
		return(FALSE);
	lseek(datafile(sd), seg_base(sd), L_SET);
	nchars = write(datafile(sd), (Char *)seg_buffer(sd), seg_size(rel));
	if (nchars < seg_size(rel)) {
		error("Couldn't write segment");
fprintf(stderr, "seg_op: %x, file: %08x\n", seg_opn(sd), seg_dfile(sd)->fd_datafile);
		return;
	}
}

/*
 * add_segs:
 *	Add new segments to a given level in a specified relation
 *	until we have added enough to reach the specified new maximum
 */
Bool
add_segs(rel, level, newmax)
Reln *rel;
Int level;
Int newmax;
{
	r_Char	*bbuf;
	Word	*buf;
	DFile	*segf;
	Faddr	segbase;
	Int	nbytes, segaddr, segfile;

#if 0
fprintf(stderr,"add_segs to %s on lev %d\n",dbrelname(rel),level);
#endif
	/*
	 * Allocate a segment buffer and initialise it
	 */
	if ((buf = mknewsegbuf(rel, 0)) == WordNULL) {
		error("Can't make buffer for new segment");
		return(FALSE);
	}

	while (Nls(rel,level) <= newmax)
	{
		/*
		 * Determine logical & physical address of new segment
		 */
		segaddr = Nls(rel,level);
		segbase = (segaddr % local_segs(rel)) * seg_size(rel);
		segfile = segaddr/local_segs(rel);
		nbytes = seg_size(rel);
#if 0
fprintf(stderr,"add_seg: number:%d base:%d file:%d\n",segaddr,segbase,segfile);
#endif

		/*
		 * Open file to contain new segment (may create new file)
		 */
		segf = dfile_open(dbrelname(rel), segfile, level, opASSERT);
		if (segf == DFileNULL) {
			error("Can't open data file for new segment\n");
			return(FALSE);
		}

		/*
		 * Write new initialised segment into the file
		 */
		clearSegBuf(rel, buf, segbase);
		lseek(segf->fd_file, segbase, L_SET);
		if (write(segf->fd_file, (char*)buf, nbytes) != nbytes) {
			error("Can't write new segment");
			return(FALSE);
		}
		Nls(rel,level)++;
	}

	/*
	 * Update relation info & clean up
	 */
	rel_info_update(rel);
	free((char *)buf);
	return(TRUE);
}

/*
 * mknewsegbuf:
 *	Allocate a new segment buffer for a relation and initialise it
 */
Word *
mknewsegbuf(rel, base)
Reln *rel;
Word base;
{
	r_Word	*buf;
	r_Int	nbytes;
	void	clearSegBuf();

#if 0
fprintf(stderr,"mknewsgbuf for %s @ %x\n",dbrelname(rel),base);
#endif
	/*
	 * Allocate a segment buffer
	 */
	nbytes = seg_size(rel);
	if ((buf = (Word *)malloc(nbytes)) == WordNULL) {
		error("No memory for segment buffer");
		return(WordNULL);
	}
	clearSegBuf(rel,buf,base);
	return(buf);
}

/*
 * clearSegBuf:
 *	Initialise contents of a segment buffer for specified relation
 *	It now contains absolutely no records
 */
void
clearSegBuf(rel,buf,base)
Reln *rel;
Word *buf;
Word base;
{
	r_Int	i;
	r_Word	*b;
	register Recptr *r;
	Word	*top;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Int	nbytes, nrecs;

	/*
	 * Initialise record descriptor (slices) area
	 */
	nrecs = Nr(rel);
	top = &buf[nWords(nrecs) * (Dr(rel)+1)];
	for (b = buf; b < top; b++)
		*b = 0;

	/*
	 * Initialise record info structures
	 */
	rec_ptr = (Recptr *)b;
	for (i = 0, r = rec_ptr; i < nrecs; i++, r++) {
		r->rp_nextfree = i+1;
		r->rp_blksize = 0;
		r->rp_hash = UNUSED;
		r->rp_offset = UNUSED;
	}
	rec_ptr[nrecs-1].rp_nextfree = -1;

	/*
	 * Set up segment list pointers
	 */
	seg_ptr = (Segptr *)&rec_ptr[nrecs];
	seg_ptr->sp_localfree = 0;
	seg_ptr->sp_extrnfree = -1;
	seg_ptr->sp_freespace = base + seg_info_size(rel);

	/*
	 * Clear data area
	 */
	nbytes = seg_size(rel);
	b = (Word *)&seg_ptr[1];
	top = &buf[nbytes/sizeof(Word)];
	while (b < top)
		*b++ = 0;
}

#ifdef DBUG
/*
 * seg_print:
 *	Dump contents of data file descriptor
 */
void
seg_print(label,seg)
String label;
Segt *seg;
{
	Int	i;

	fprintf(stderr,"===%s\n",label);
	if (seg == SegtNULL) {
		fprintf(stderr,"SegtNULL\n");
		return;
	}
	fprintf(stderr,"%s/%d nx:%x no:%d fp:%x op:%x",
		seg->sd_name,seg->sd_segid,seg->sd_next,
		seg->sd_nopens,seg->sd_file,seg->sd_operation);
	dfile_print("dfile_for_seg:",seg->sd_file);
	if (seg->sd_seg_buf == WordNULL)
		fprintf(stderr," No seg buf\n");
	else {
		putc('\n',stderr);
		for (i = 0; i < 8; i++)
			fprintf(stderr,"%08x ",seg->sd_seg_buf[i]);
		putc('\n',stderr);
	}
}

#endif DBUG
