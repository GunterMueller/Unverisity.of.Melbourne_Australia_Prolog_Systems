/*
 * seg.c  -  deductive database package (segment operations)
 *
 * $Header: seg.c,v 1.5 85/12/06 15:10:21 jas Exp $
 * $Log:	seg.c,v $
 * Revision 1.5  85/12/06  15:10:21  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:46  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:09  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:48:32  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:42  jas
 * Initial revision
 * 
 * 
 */

#include "muddlib.h"

/*
 * "Block" manipulation functions
 * A Block is an efficiency/extensibility hack for
 * the DSIMC index scheme ... it is basically just
 * a set of segment descs in a suitable block size
 * for the underlying OS/disk system
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

	seg_mask = segs_per_block(rel) - 1;
	block_base = tr->cur_seg & ~seg_mask;
	tr->seg_off = new_seg =
		b_nextset(tr->seg_off+1, tr->seg_matches, segs_per_block(rel));

	if (new_seg < 0)
		return(-1);
	else {
		new_seg = block_base + new_seg;
		while (tr->cur_seg != new_seg) {
			b_next_perm(&tr->cur_seg, tr->seg_gaps, ++tr->seg_count);
#if 0
fprintf(stderr,"next_seg: seg:%x count:%d\n",tr->cur_seg,tr->seg_count);
#endif
			if (tr->seg_count >= tr->seg_ncombs ||
				tr->cur_seg > Ns(rel)-1)
				return(-1);
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
	r_Reln	*rel = tr->relation;

	seg_mask = segs_per_block(rel) - 1;
	block_base = tr->cur_seg & ~seg_mask;

	do {
		b_next_perm(&tr->cur_seg, tr->seg_gaps, ++tr->seg_count);
#if 0
fprintf(stderr,"next_block: seg:%x count:%d\n",tr->cur_seg,tr->seg_count);
#endif
		new_block = tr->cur_seg & ~seg_mask;
		if (tr->seg_count >= tr->seg_ncombs ||
			tr->cur_seg > Ns(rel)-1)
			return(-1);
	} while (new_block == block_base);
	tr->seg_off = -1;
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
					++tr->cur_level;
					tr->cur_seg = -1;
					goto OverflowLevels;
				}
				setup_block_seg_matches(tr);
			}
		}
		else {
OverflowLevels:
			lev = tr->cur_level;
			tr->cur_seg = b_nextset(tr->cur_seg+1, tr->ov_matches[lev], Nls(rel,lev));
			if (tr->cur_seg < 0) {
				++tr->cur_level;
				if (Nls(rel, tr->cur_level) > 0)
					goto OverflowLevels;
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
setup_seg_matches(tr)
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
}

/*
 * setup_block_seg_matches:
 *	Read the block into a buffer, compute potential matching segments,
 *	then throw away the buffer
 */
setup_block_seg_matches(tr)
Trans *tr;
{
	r_Int	i, ii, j, nbits, ncombs;
	r_Word	seg_mask, gaps;
	r_Int	which_block;
	r_Int	block_base;
	r_Int	block_offset;
	r_Reln	*rel = tr->relation;
	Word	seg_off;

	slice_size = segs_per_block(rel);
	seg_mask = segs_per_block(rel) - 1;
	which_block = tr->cur_seg >> log2_segs_per_block(rel);
	block_base = which_block << log2_segs_per_block(rel);
	block_offset = (Faddr)(offset(rel) + which_block*block_size(rel));

	/*
	 * Set up the initial seg_matches bit string
	 * This one bit per combination as defined by gaps
	 */
	b_zero(tr->seg_matches, slice_size);
	nbits = 0;
	gaps = tr->seg_gaps & seg_mask;
	while (gaps > 0) {
		if (gaps & 1)
			nbits++;
		gaps >>= 1;
	}
	gaps = tr->seg_gaps & seg_mask;
	ncombs = (1 << nbits);
	seg_off = tr->cur_seg & seg_mask;
	for (i = 0; i < ncombs; i++) {
		b_next_perm(&seg_off, gaps, i);
		b_set(seg_off, tr->seg_matches, slice_size);
	}

	/*
	 * If there are not many combinations,
	 * don't bother checking seg desc matches
	 */
#ifdef DBUG
	debug(GET_DBUG) {
		fprintf(stderr, "block:%d  ", which_block);
		b_display("seg slice: ",tr->seg_matches,slice_size);
	}
#endif DBUG
	if (b_count(tr->seg_matches, slice_size) <= 2)
		goto ComputeOverflows;

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

	/*
	 * Now that we know the most likely matching segs,
	 * we also mark their overflow segs for examination
	 */
ComputeOverflows:
	for_every_bit(i, tr->seg_matches, slice_size)
	{
		ii = i + block_base;
		for (j = 1; j < MAXOVLEVELS; j++) {
			if (tr->ov_matches[j] == WordNULL) break;
			b_set(ovflow_segment(tr,ii,j), tr->ov_matches[j], Nls(rel,j));
		}
	}
	
	/*
	 * Clean up afterwards
	 */
	cfree(index_block, Word);
	b_free(par_matches);
#ifdef DBUG
	debug(MATCH_DBUG) b_display("block seg matches:",tr->seg_matches,slice_size);
#endif DBUG
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
						nbytes(slice_size) * (*bit));
				lseek(descfile(rel), slice_offset, L_SET);
				read(descfile(rel), (char *)slice, nbytes(slice_size));
			when DSIMC_INDEX:
				slice = &(index_block[*bit * nwords(slice_size)]);
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
						nbytes(slice_size) * node->e_smaskpos);
				lseek(descfile(rel), slice_offset, L_SET);
				read(descfile(rel), (char *)slice, nbytes(slice_size));
			when DSIMC_INDEX:
				slice = &(index_block[node->e_smaskpos * nwords(slice_size)]);
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
	Int	segaddr;
	Char	segfile[MAXPATH];
	r_Reln	*rel = tr->relation;

#ifdef DBUG
	debug(GET_DBUG) fprintf(stderr,"fetch seg#%d\n",tr->cur_seg);
#endif DBUG

	/*
	 * Open data file for this segment
	 */
	seg_close(tr->segment);
	switch (index_type(rel))
	{
	when SIMC_INDEX:
		segaddr = tr->cur_seg;
	when DSIMC_INDEX:
		segaddr = addressofindex(tr->cur_seg,tr->cur_level);
	}
	tr->segment = seg_open(tr,dbrelname(rel),segaddr,tr->operation);
	if (tr->segment == SegtNULL)
		fatal("fetch_segment");
#ifdef DBUG
	debug(DESC_DBUG)
	fprintf(stderr,"open desc %x :%s/%d: FILE *%x no:%d ndf:%d\n",
		tr->segment, tr->segment->sd_name, tr->segment->sd_segnum,
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
open_segment(sd, seg_buf_sz)
Segt *sd;
Int seg_buf_sz;
{
	r_DFile	*fd = sd->sd_file;
	Bool	need_to_read;

	need_to_read = (sd->sd_timestamp != fd->fd_timestamp ||
			sd->sd_seg_buf == WordNULL ||
			fd->fd_file == FileNULL);

	if (sd->sd_seg_buf == WordNULL) {
		if (n_segments >= MAXSEGMENTS) {
			/*
			 * release old segment buffer
			 */
			cfree(seg_head->sd_seg_buf, Word);
			n_segments--;
			seg_head = seg_head->sd_qnext;
		}
#ifdef DBUG
		debug(MALLOC_DBUG) fprintf(stderr,"seg_buf:");
#endif DBUG
		if ((sd->sd_seg_buf = (Word *)malloc(seg_buf_sz)) == WordNULL) {
			error("no_mem_for_seg_buf");
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
		if (!open_data_file(fd,sd->sd_operation))
			return(FALSE);
		lseek(fd->fd_file, sd->sd_offset, L_SET);
		read(fd->fd_file, (Char *)sd->sd_seg_buf, seg_buf_sz);
		sd->sd_timestamp = fd->fd_timestamp;
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
seg_open(tr,db_rel,seg_num,operation)
Trans *tr;
String db_rel;
Int seg_num;
Opn operation;
{
	r_Reln	*rel = tr->relation;
	Segt	**ptr;
	Segt	*desc;
	Int	segno;
	Char	file_name[MAXPATH];

	/*
	 * If search ok, use existing descriptor
	 */
	if (search(db_rel, seg_num, POINTER_MODE, (Hash**)seg_table,
			MAXSHASH, (Hash***)(&ptr), (Hash**)(&desc)))
	{
		if (!open_segment(desc,seg_size(rel)))
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
	desc->sd_segnum = seg_num;
	desc->sd_nopens = 0;
	desc->sd_timestamp = -2;
	desc->sd_next = SegtNULL;
	desc->sd_operation = operation;
	desc->sd_offset = (Faddr)((seg_num % local_segs(rel)) * seg_size(rel));

	if ((desc->sd_file = dfile_open(db_rel,seg_num/local_segs(rel),operation)) == DFileNULL) {
		error("open_data_file_desc");
		goto SegFail;
	}

	if (!open_segment(desc,seg_size(rel))) {
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
#ifndef elxsi
			flock(fd->fd_file, LOCK_UN);
#endif
#endif
			fd->fd_timestamp = time_stamp++;
		}
	}
	or (iswriting(sd->sd_operation)) {
		sd->sd_operation = Op_QUERY;
#ifdef LOCK_SH
#ifndef elxsi
		if (fd->fd_file != FileNULL)
			flock(fd->fd_file, LOCK_SH);
#endif
#endif
	}
#ifdef DBUG
	debug(DESC_DBUG) seg_print("Close Segt:",sd);
#endif DBUG
}

/*
 * set_segment:
 *	Set up appropriate segment, stars-mask, combinations
 *	for a DSIMC hash code, and a specified level
 */
void
set_segment(tr,level)
Trans *tr;
Int level;
{
	r_Int	cluster;
	r_Int	nsegs, nsegs1, ncombs;
	r_Word	seg_mask, seg_shift, stars;
	r_Reln	*rel = tr->relation;

	nsegs = Nls(rel, level);
	
	if (nsegs == 0) {
		tr->cur_seg = -1;
		return;
	}

	/*
	 * Work out a mask to generate segment numbers
	 * from the raw hash code (tr->cluster).
	 */
	nsegs1 = nsegs - 1;
	seg_mask = 0;
	seg_shift = 31;	/* PORTABLE! */
	while (seg_mask < nsegs1) {
		seg_shift--;
		seg_mask = (seg_mask << 1) | 1;
	}

	/*
	 * Now we work out the segment index for
	 * the hash at this level by ANDing with the mask
	 */
	cluster = tr->cluster >> seg_shift;
	if (cluster >= nsegs)
		cluster >>= 1;
	tr->cur_seg = cluster;

	/*
	 * Set up gaps vector and compute number of combinations 
	 */
	tr->seg_gaps = tr->clustars >> seg_shift;
	tr->seg_count = 0;
	for (ncombs = 1, stars = tr->seg_gaps; stars != 0; stars >>= 1)
		if ((stars & 1) != 0)
			ncombs <<= 1;
	tr->seg_ncombs = ncombs;
#if 0
printf("stars:%08x cluster:%08x\n",tr->clustars,tr->cluster);
printf("seg_mask:%08x gaps:%08x cluster:%08x(%d) nsegs:%d\n",
		seg_mask,tr->seg_gaps,cluster,cluster,nsegs);
#endif
}

/*
 * ovflow_segment:
 *	Work out address of overflow seg at given level
 *	Duplicates some functionality of set_segment()
 */
Int
ovflow_segment(tr,seg,level)
Trans *tr;
Int seg;
Int level;
{
	r_Int	cluster;
	r_Int	nsegs, nsegs1;
	r_Word	seg_mask, seg_shift;
	r_Reln	*rel = tr->relation;

	nsegs = Nls(rel, level);
	
	if (nsegs == 0)
		return(-1);

	/*
	 * Work out a shift to generate segment numbers
	 * from the raw hash code (tr->cluster).
	 */
	nsegs1 = nsegs - 1;
	seg_mask = 0;
	seg_shift = 31;	/* PORTABLE! */
	while (seg_mask < nsegs1) {
		seg_shift--;
		seg_mask = (seg_mask << 1) | 1;
	}

	/*
	 * Now we work out the segment index for
	 * the hash at this level by ANDing with the mask
	 */
	cluster = seg >> seg_shift;
	if (cluster >= nsegs)
		cluster >>= 1;
	return(cluster);
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
		seg->sd_name,seg->sd_segnum,seg->sd_next,
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
