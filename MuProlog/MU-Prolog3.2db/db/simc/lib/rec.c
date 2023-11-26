/*
 * rec.c  -  deductive database package (record operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 */

#include "simc.h"

/*
 * Record matching global vars
 */

Word	*par_matches;		/* holds record matches for each key */
Word	*slice;			/* buffer to read bit-sliced rec descriptors */
Int	slice_size;		/* how many bits in rec match bit-strings */

/*
 * setup_rec_matches:
 *	Setup the rec_matches bit string in a transaction
 */
void
setup_rec_matches(tr)
Trans *tr;
{
	r_Reln	*rel = tr->relation;
	r_Segt	*seg = tr->segment;
	void	do_rec_matches();

#if 0
seg_print("setup_rec_matches:",rel->r_segment);
#endif
	/*
	 * The bits set in "matches" tell us which records to fetch
	 * Initially we set to 1 all bits corresponding to records
	 *  stored in the segment (which we obtain from the Presence
	 *  slice at end of descriptor slices), they will be
	 *  successively AND'ed out as we scan record descriptors
	 */
	b_copy(&(seg_buffer(seg)[nWords(Nr(rel))*Dr(rel)]), tr->rec_matches, Nr(rel));
	slice_size = Nr(rel);
	par_matches = b_make(slice_size);

	/*
	 * For each key, we form a bit-sliced string of
	 * potential matches and AND all these together
	 */
	do_rec_matches(tr, tr->query);
#ifdef DBUG
	debug(MATCH_DBUG) b_display("rec matches:",tr->rec_matches,Nr(rel));
#endif DBUG

	/*
	 * Free up bit-slice buffers
	 */
	b_free(par_matches); par_matches = WordNULL;
#ifdef DBUG
	debug(MATCH_DBUG) {
		fprintf(stderr,"seg#%d ",tr->cur_seg);
		b_display("rec matches:",tr->rec_matches,Nr(rel));
	}
#endif DBUG
}

/*
 * do_rec_matches:
 *	Perform the operation of generating record match bit-string
 */
void
do_rec_matches(tr,node)
Trans *tr;
Elem *node;
{
	r_Segt	*seg = tr->segment;
	r_Int	i, *bit;
	r_Int	nbits_to_use;

	if (node->e_isvar) {
		b_ones(par_matches, slice_size);
	}
	else {
		/*
		 * Recursively repeat this process for all sub-elements
		 */
		for (i = 0; i < node->e_maxargs; i++) {
			do_rec_matches(tr, node->e_args[i]);
			b_and(tr->rec_matches, par_matches, tr->rec_matches, slice_size);
		}

		/*
		 * AND:{b1,b2, ... ,bn}
		 */
		if (node->e_snbits == 0)
			nbits_to_use = 0;
		else {
			nbits_to_use = (Int)(tr->rec_ratio * (float)node->e_rnbits + 1.0);
			if (nbits_to_use > node->e_rnbits)
				nbits_to_use = node->e_rnbits;
		}
#ifdef DBUG
		debug(MATCH_DBUG)
			fprintf(stderr,"%s:rbits_to_use:%d\n",
					node->e_name,nbits_to_use);
#endif DBUG
		bit = node->e_recbits;
		b_copy(tr->rec_matches, par_matches, slice_size);
		for (i = 0; i < nbits_to_use; i++, bit++)
		{
			slice = &(seg_buffer(seg)[(*bit)*nWords(slice_size)]);
			b_and(slice, par_matches, par_matches, slice_size);
#ifdef DBUG
			debug(MATCH_DBUG) {
				fprintf(stderr,"%15s:bit:%d:\n",node->e_name,*bit);
				b_display("\t\tr_ANDslice:",slice,slice_size);
				b_display("\t\tr_AND_res :",par_matches,slice_size);
			}
#endif DBUG
		}

		/*
		 * OR mask bit with result of AND'ing codeword bits
		 * But we don't do this for fields which are always
		 * going to be constant (e.g. the relation name)
		 */
		if (node->e_rmaskpos > 0) {
			slice = &(seg_buffer(seg)[node->e_rmaskpos*nWords(slice_size)]);
			b_or(slice, par_matches, par_matches, slice_size);
#ifdef DBUG
			debug(MATCH_DBUG) {
				fprintf(stderr,"%15s:bit:%d:\n",node->e_name,node->e_rmaskpos);
				b_display("\t\tr_OR_slice:",slice,slice_size);
				b_display("\t\tr_OR_res  :",par_matches,slice_size);
			}
#endif DBUG
		}
		b_copy(par_matches, tr->rec_matches, slice_size);
#ifdef DBUG
		debug(MATCH_DBUG) {
			fprintf(stderr,"%s:",node->e_name);
			b_display("r_matches:",par_matches,slice_size);
		}
#endif DBUG
	}
}

/*
 * rec_insert:
 *	Add a new fact into the database
 */
Int
rec_insert(tr)
Trans *tr;
{
	Int	simc_insert(), dsimc_insert();

	switch (index_type(tr->relation))
	{
	when SIMC_INDEX:
		return(simc_insert(tr));
	when DSIMC_INDEX:
		return(dsimc_insert(tr));
	}
}

/*
 * simc_insert:
 *	Add a new fact into a SIMC database
 */
Int
simc_insert(tr)
Trans *tr;
{
	r_Reln	*rel = tr->relation;
	r_Segt	*seg;
	r_Int	i;
	r_Int	*bit;
	r_Word	*seg_buf;
	r_Char	*buf;
	Int	segaddr;
	Int	rec_no;
	Char	*start_keys;
	Char	*end_keys;
	Int	rec_size;
	Faddr	rec_offset;
	Word	*slice;
	Int	slice_size;
	Faddr	slice_offset;
	Int	simc_best_segment();
	Word	simc_best_rec_slot();

	/*
	 * For a key which looks like "rel(k1,k2,k3,...,kn)"
	 *  we write out only the chars in "k1,k2,k3,...,kn"
	 */
	start_keys = index(tr->input,'(')+1;
	end_keys = rindex(tr->input,')');
	rec_size = end_keys - start_keys;

	/*
	 * Figure out where to put record
	 * Hash fact to give segment number
	 */
	segaddr = simc_best_segment(tr);
#if 0
fprintf(stderr,"%s -> (%d,%d)\n",tr->input,tr->cur_seg,tr->cur_level);
#endif
	if (segaddr < 0) {
		error("Can't find segment to insert record");
		return(-1);
	}
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"choose seg: %d\n",segaddr);
#endif DBUG

	/*
	 * Update segment descriptors
	 */
	simc_sdesc_update(tr);

	seg_fetch(tr);
	seg = tr->segment;
	seg_buf = seg_buffer(seg);
#if 0
fprintf(stderr,"inserting %s\n",tr->input);
#endif

	rec_offset = simc_best_rec_slot(rel,seg,rec_size,tr->cluster,&rec_no);
	tr->cur_rec = rec_no;
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"choose rec: %d\n",rec_no);
#endif DBUG

	/*
	 * Update record descriptors
	 */
	slice_size = Nr(rel);
#if 0
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"set bit#%d in rec slices",rec_no);
#endif DBUG
#endif
	for (i = 0, bit = tr->rec_list; i < tr->nrecbits; i++, bit++)
	{
#if 0
#ifdef DBUG
		debug(ADD_DBUG) fprintf(stderr," %d",*bit);
#endif DBUG
#endif
		slice = &(seg_buf[nWords(slice_size)*(*bit)]);
		b_set(rec_no, slice, slice_size);
	}
#if 0
#ifdef DBUG
	debug(ADD_DBUG) putc('\n',stderr);
#endif DBUG
#endif

	/*
	 * Set Presence bit
	 */
	slice = &(seg_buf[nWords(slice_size)*Dr(rel)]);
	b_set(rec_no, slice, slice_size);

	/*
	 * At last! we write the record into the data file
	 * If it's within the segment, we simply dump the
	 * segment buffer, else we dump pointers etc. and
	 * seek to end of file to write record.
	 */
	if (rec_offset < seg_base(seg)+seg_size(rel)) {
		buf = (Char *)seg_buf;
		buf = &(buf[rec_offset-seg_base(seg)]);
		strncpy(buf, start_keys, rec_size);
		lseek(datafile(seg), seg_base(seg), L_SET);
		write(datafile(seg), (Char *)seg_buf, seg_size(rel));
	}
	else {
		lseek(datafile(seg), seg_base(seg), L_SET);
		write(datafile(seg), (Char *)seg_buf, seg_info_size(rel));
		lseek(datafile(seg), (Faddr)rec_offset, L_SET);
		write(datafile(seg), start_keys, rec_size);
	}

	/*
	 * Update various counters for relation
	 * Rel_close unlocks relation,
	 * but leaves relation info lying around
	 */
	inc_counter(rel, segaddr);
	inc_counter(rel, Ns(rel)+1);
	Nlr(rel,0)++;
	rel_info_update(rel);
	rel_close(rel);

	/*
	 * Indicate that the record was succesfully inserted
	 */
	return(1);
}

/*
 * dsimc_insert:
 *	Add a new fact into a DSIMC database
 */
Int
dsimc_insert(tr)
Trans *tr;
{
	r_Reln	*rel = tr->relation;
	r_Segt	*seg;
	r_Int	i;
	r_Int	*bit;
	r_Word	*seg_buf;
	r_Char	*buf;
	Int	lev, load;
	Int	segaddr;
	Int	rec_no;
	Char	*start_keys;
	Char	*end_keys;
	Int	rec_size;
	Faddr	rec_offset;
	Word	*slice;
	Int	slice_size;
	Faddr	slice_offset;
	Segt	*dsimc_best_segment();
	Word	dsimc_best_rec_slot();

	/*
	 * For a key which looks like "rel(k1,k2,k3,...,kn)"
	 *  we write out only the chars in "k1,k2,k3,...,kn"
	 */
	start_keys = index(tr->input,'(')+1;
	end_keys = rindex(tr->input,')');
	rec_size = end_keys - start_keys;

	/*
	 * Find a likely segment in which to insert new record
	 * We really do this to find a likely LEVEL, which then
	 *  tells us whether we need to split ...
	 * After splitting, we need to check again, just in case
	 *  we managed to fill up the segment we had decided on
	 */
	if (dsimc_best_segment(tr, rec_size) == SegtNULL) {
		error("Can't even find segment to insert record");
		return(-1);
	}
	
	/*
	 * If we have reached the load factor, split a segment
	 */
	lev = tr->cur_level;
	load = loadfac(rel,lev);
	for (i = 0; i < MAXLEVELS; i++) {
		if (cansplit(tr->operation) && loadcnt(rel,i) >= loadfac(rel,i)) {
#if 0
fprintf(stderr,"while inserting %s ... ",tr->input);
#endif
			split(tr->database, rel, i);
		}
	}

	/*
	 * Now we really (at last!) ...
	 * Determine which segment the record belongs/fits in
	 * Uses hash value of record to give segment number
	 */
	tr->segment = seg = dsimc_best_segment(tr, rec_size);
	if (seg == SegtNULL) {
		error("Can't even find segment to insert record");
		return(-1);
	}
	seg_buf = seg_buffer(seg);

#ifdef DBUG
	debug(ADD_DBUG)
		fprintf(stderr,"choose seg: (%d,%d)\n",tr->cur_seg,tr->cur_level);
#endif DBUG

#if 0
fprintf(stderr,"%s -> (%d,%d) [%08x]\n",
		tr->input,tr->cur_seg,tr->cur_level,tr->cluster);
#endif
	/*
	 * Determine whereabouts in selected segment the record belongs
	 */
	rec_offset = dsimc_best_rec_slot(rel,seg,rec_size,tr->cluster,&rec_no);
	if (rec_offset == UNUSED) {
		error("Can't find space to place record in selected segment!");
		return(-1);
	}
	tr->cur_rec = rec_no;
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"choose rec: %d\n",rec_no);
#endif DBUG

	/*
	 * Update segment descriptors
	 */
	dsimc_sdesc_update(tr);

	/*
	 * Update record descriptors
	 */
	slice_size = Nr(rel);
#if 0
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"set bit#%d in rec slices",rec_no);
#endif DBUG
#endif
	for (i = 0, bit = tr->rec_list; i < tr->nrecbits; i++, bit++)
	{
#if 0
#ifdef DBUG
		debug(ADD_DBUG) fprintf(stderr," %d",*bit);
#endif DBUG
#endif
		slice = &(seg_buf[nWords(slice_size)*(*bit)]);
		b_set(rec_no, slice, slice_size);
	}
#if 0
#ifdef DBUG
	debug(ADD_DBUG) putc('\n',stderr);
#endif DBUG
#endif

	/*
	 * Set Presence bit
	 */
	slice = &(seg_buf[nWords(slice_size)*Dr(rel)]);
	b_set(rec_no, slice, slice_size);

	/*
	 * At last! we write the record into the data file
	 */
	buf = (Char *)seg_buf;
	buf = &(buf[rec_offset-seg_base(seg)]);
	strncpy(buf, start_keys, rec_size);
	lseek(datafile(seg), seg_base(seg), L_SET);
	write(datafile(seg), (Char *)seg_buf, seg_size(rel));

	/*
	 * Update various counters for relation
	 * Rel_close unlocks relation,
	 * but leaves relation info lying around
	 */
	Nlr(rel,tr->cur_level)++;
	for (i = 0; i <= tr->cur_level; i++)
		loadcnt(rel, i)++;
	rel_info_update(rel);
	rel_close(rel);

	/*
	 * Indicate that the record was succesfully inserted
	 */
	return(1);
}

/*
 * rec_delete:
 *	Remove a fact from the database
 */
Int
rec_delete(tr)
Trans *tr;
{
	r_Reln	*rel = tr->relation;
	r_Segt	*seg = tr->segment;
	r_Int	i, slice_size;
	r_Word	end_of_seg;
	r_Int	r = tr->cur_rec;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Int	*counters, ncounters;

	/*
	 * Work out useful locations in data segment
	 */
	rec_ptr = (Recptr *)&(seg_buffer(seg)[nWords(Nr(rel))*(Dr(rel)+1)]);
	seg_ptr = (Segptr *)&(rec_ptr[Nr(rel)]);
	end_of_seg = seg_base(seg) + seg_size(rel);

	/*
	 * This should never happen
	 */
	if (rec_ptr[r].rp_offset == UNUSED) {
		error("data_file_corrupted");
		return(-1);
	}

	/*
	 * Update segment free lists
	 */
	if (rec_ptr[r].rp_offset < end_of_seg) {
		/*
		 * Link into free list for segment local data space
		 */
		rec_ptr[r].rp_nextfree = seg_ptr->sp_localfree;
		rec_ptr[r].rp_hash = UNUSED;
		seg_ptr->sp_localfree = r;
	}
	else {
		/*
		 * Link into free list for segment overflow space
		 */
		rec_ptr[r].rp_nextfree = seg_ptr->sp_extrnfree;
		rec_ptr[r].rp_hash = UNUSED;
		seg_ptr->sp_extrnfree = r;
	}

	/*
	 * Update record descriptor slices (including Presence bit)
	 */
	slice_size = Nr(rel);
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"reset bit#%d in rec slices\n",r);
#endif DBUG
	for (i = 0; i <= Dr(rel); i++)
	{
		slice = &(seg_buffer(seg)[nWords(slice_size)*i]);
		b_reset(r, slice, slice_size);
	}

	/*
	 * Write updates to data file
	 */
	lseek(datafile(seg), seg_base(seg), L_SET);
	write(datafile(seg), (char *)seg_buffer(seg), seg_info_size(rel));

	/*
	 * Update various counters for relation
	 */
	switch (index_type(rel))
	{
	when SIMC_INDEX:
		ncounters = (Ns(rel)+2) * sizeof(Int);
		if ((counters = (Int *)malloc(ncounters)) == IntNULL)
			return(-1);
		lseek(descfile(rel), coffset(rel), L_SET);
		read(descfile(rel), (char *)counters, ncounters);
		if (counters[tr->cur_seg] > Nr(rel))
			counters[tr->cur_seg] = Nr(rel)-1;
		else
			counters[tr->cur_seg]--;
		lseek(descfile(rel), coffset(rel), L_SET);
		write(descfile(rel), (char *)counters, ncounters);
		free((char *)counters);
	when DSIMC_INDEX:
		Nlr(rel,tr->cur_level)--;
		for (i = 0; i <= tr->cur_level; i++)
			loadcnt(rel, i)--;
		rel_info_update(rel);
	}

	/*
	 * Indicate that the record was succesfully deleted
	 */
	return(1);
}

/*
 * rec_fetch:
 *	Grab the next record during a transaction
 */
Int
rec_fetch(tr)
Trans *tr;
{
	r_Reln	*rel = tr->relation;
	r_Segt	*seg = tr->segment;
	Recptr	*rec_ptr, *rec;
	Word	rec_hash;

#ifdef DBUG
	debug(GET_DBUG)
		fprintf(stderr,"fetch rec(%d) seg(%d,%d)\n",
				tr->cur_rec, tr->cur_seg, tr->cur_level);
#endif DBUG
	if (seg_buffer(seg) == WordNULL &&
		!open_segment(rel,tr->cur_level,seg,seg_size(rel)))
		fatal("Can't reopen segment");
#ifdef DBUG
	debug(VDESC_DBUG) seg_print("rec_fetch:",seg);
#endif DBUG
	rec_ptr = (Recptr *)&(seg_buffer(seg)[nWords(Nr(rel))*(Dr(rel)+1)]);
	rec = &rec_ptr[tr->cur_rec];
#ifdef DBUG
	debug(GET_DBUG)
		fprintf(stderr, "fetch from offset: %x\n", rec->rp_offset);
#endif DBUG
	if (rec->rp_hash == UNUSED || rec->rp_offset == UNUSED) {
		error("Trying to fetch non-existent record!");
		return(-1);
	}
	rec_hash = rec_ptr[tr->cur_rec].rp_hash;
	if (((~tr->clustars & (tr->cluster^rec_hash))<<1) != 0)
		return(-1);

	cfree(tr->rec_buf, Char);
	if ((tr->rec_buf = rectext(rec, seg, rel)) == StrNULL)
		return(-1);
	tr->nrecs_fetched++;
	return(0);
}

/*
 * simc_best_rec_slot:
 *	Determine best record slot in data segment to insert fact
 */
Word
simc_best_rec_slot(rel,seg,rec_size,clust,rec_no)
Reln *rel;
Segt *seg;
Int rec_size;
Word clust;
Int *rec_no;
{
	r_Word	end_of_seg;
	r_Word	rec_offset;
	r_Int	r, prevr;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Int	best_prev, best_rec, smallest;

	/*
	 * Compute interesting locations within the segment
	 */
	rec_ptr = (Recptr *)&(seg_buffer(seg)[nWords(Nr(rel))*(Dr(rel)+1)]);
	seg_ptr = (Segptr *)&(rec_ptr[Nr(rel)]);
	end_of_seg = seg->sd_offset + seg_size(rel);

	/*
	 * First try to find spot in local data segment space
	 */
#if 0
fprintf(stderr,"trying local seg space\n");
#endif
	prevr = UNUSED;
	r = seg_ptr->sp_localfree;
	while (r != UNUSED) {
#if 0
fprintf(stderr,"off:%d sz:%d nx:%d fspace:%x eos:%x\n",
	rec_ptr[r].rp_offset,rec_ptr[r].rp_blksize,rec_ptr[r].rp_recsize,
	seg_ptr->sp_freespace, end_of_seg);
#endif
		if (rec_ptr[r].rp_offset == UNUSED &&
			seg_ptr->sp_freespace+rec_size < end_of_seg) {
			/*
			 * Free spot at end of local data space
			 */
			rec_ptr[r].rp_blksize = rec_size;
			rec_offset = seg_ptr->sp_freespace;
			rec_ptr[r].rp_offset = rec_offset;
			rec_ptr[r].rp_hash = clust;
			seg_ptr->sp_freespace += rec_size;
			break;
		}
		or (rec_ptr[r].rp_offset != UNUSED &&
			rec_size <= rec_ptr[r].rp_blksize) {
			/*
			 * Use a "hole" in middle of local data space
			 */
			rec_offset = rec_ptr[r].rp_offset;
			break;
		}
		prevr = r;
		r = rec_ptr[r].rp_nextfree;
	}
	if (r != UNUSED) {
#if 0
fprintf(stderr, "found some\n");
#endif
		if (prevr == UNUSED)
			seg_ptr->sp_localfree = rec_ptr[r].rp_nextfree;
		else
			rec_ptr[prevr].rp_nextfree = rec_ptr[r].rp_nextfree;
		/*
		 * Note: rp_nextfree == rp_recsize
		 */
		rec_ptr[r].rp_recsize = rec_size;
		*rec_no = r;
		return(rec_offset);
	}

	/*
	 * Now try to find spot in data overflow space
	 */
#if 0
fprintf(stderr, "trying data ovflow space\n");
#endif
	prevr = UNUSED;
	r = seg_ptr->sp_extrnfree;
	while (r != UNUSED) {
		if (rec_size <= rec_ptr[r].rp_blksize) {
			/*
			 * Use a "hole" in middle of overflow data space
			 */
			rec_offset = rec_ptr[r].rp_offset;
			break;
		}
		prevr = r;
		r = rec_ptr[r].rp_nextfree;
	}
	if (r != UNUSED) {
#if 0
fprintf(stderr, "found some\n");
#endif
		if (prevr == UNUSED)
			seg_ptr->sp_extrnfree = rec_ptr[r].rp_nextfree;
		else
			rec_ptr[prevr].rp_nextfree = rec_ptr[r].rp_nextfree;
		rec_ptr[r].rp_recsize = rec_size;
		*rec_no = r;
		return(rec_offset);
	}

	/*
	 * Now try to find a slot to throw away from overflow space
	 */
#if 0
fprintf(stderr, "find ovflow slot to throw away\n");
#endif
	prevr = UNUSED;
	r = seg_ptr->sp_extrnfree;
	best_prev = UNUSED;
	best_rec = UNUSED;
	smallest = 9999;
	while (r != UNUSED) {
		if (rec_ptr[r].rp_blksize < smallest) {
			smallest = rec_ptr[r].rp_blksize;
			best_rec = r;
			best_prev = prevr;
		}
		prevr = r;
		r = rec_ptr[r].rp_nextfree;
	}
	if (best_rec != UNUSED) {
#if 0
fprintf(stderr, "done it\n");
#endif
		if (best_prev == UNUSED)
			seg_ptr->sp_extrnfree = rec_ptr[best_rec].rp_nextfree;
		else
			rec_ptr[best_prev].rp_nextfree = rec_ptr[best_rec].rp_nextfree;
		rec_ptr[best_rec].rp_recsize = rec_size;
		rec_ptr[best_rec].rp_blksize = rec_size;
		rec_offset = lseek(datafile(seg), (Faddr)0, L_XTND);
		rec_ptr[best_rec].rp_offset = rec_offset;
		rec_ptr[best_rec].rp_hash = clust;
		*rec_no = best_rec;
		return(rec_offset);
	}

	/*
	 * Now try to find a slot to throw away from local segment space
	 */
#if 0
fprintf(stderr, "find loc seg slot to throw away\n");
#endif
	prevr = UNUSED;
	r = seg_ptr->sp_localfree;
	best_prev = UNUSED;
	best_rec = UNUSED;
	smallest = 9999;
	while (r != UNUSED) {
		if (rec_ptr[r].rp_blksize < smallest) {
			smallest = rec_ptr[r].rp_blksize;
			best_rec = r;
			best_prev = prevr;
		}
		prevr = r;
		r = rec_ptr[r].rp_nextfree;
	}
	if (best_rec != UNUSED) {
#if 0
fprintf(stderr, "done it\n");
#endif
		if (best_prev == UNUSED)
			seg_ptr->sp_localfree = rec_ptr[best_rec].rp_nextfree;
		else
			rec_ptr[best_prev].rp_nextfree = rec_ptr[best_rec].rp_nextfree;
		rec_ptr[best_rec].rp_hash = clust;
		rec_ptr[best_rec].rp_recsize = rec_size;
		rec_ptr[best_rec].rp_blksize = rec_size;
		rec_offset = lseek(datafile(seg), (Faddr)0, L_XTND);
		rec_ptr[best_rec].rp_offset = rec_offset;
		*rec_no = best_rec;
		return(rec_offset);
	}

	/*
	 * Hmmmm ... should never get here
	 */
	return((Word)UNUSED);
}

/*
 * dsimc_seg_has_room:
 *	Determine whether there is room in the segment
 */
Bool
dsimc_seg_has_room(rel,seg,rec_size)
Reln *rel;
Segt *seg;
Int rec_size;
{
	r_Word	end_of_seg;
	r_Word	rec_offset;
	r_Int	r;
	Recptr	*recptr;
	Segptr	*segptr;

	/*
	 * Compute interesting locations within the segment
	 */
	recptr = (Recptr *)&(seg_buffer(seg)[nWords(Nr(rel))*(Dr(rel)+1)]);
	segptr = (Segptr *)&(recptr[Nr(rel)]);
	end_of_seg = seg->sd_offset + seg_size(rel);

	/*
	 * Look down freespace list for a slot where the record fits
	 */
#if 0
fprintf(stderr,"checking for room in segment (%d,%d,%d,%d)\n",
	segptr->sp_localfree,segptr->sp_extrnfree,segptr->sp_freespace,end_of_seg);
#endif
	r = segptr->sp_localfree;
	while (r != UNUSED) {
#if 0
fprintf(stderr,"r:%d off:%d sz:%d nx:%d fspace:%x eos:%x\n",
	r, recptr[r].rp_offset,recptr[r].rp_blksize,recptr[r].rp_recsize);
#endif
		if (recptr[r].rp_offset == UNUSED &&
			segptr->sp_freespace+rec_size < end_of_seg)
			return(TRUE);
		if (recptr[r].rp_offset != UNUSED &&
			rec_size <= recptr[r].rp_blksize)
			return(TRUE);
		r = recptr[r].rp_nextfree;
	}
#if 0
fprintf(stderr,"no room\n");
#endif
	return(FALSE);
}

/*
 * dsimc_best_rec_slot:
 *	Determine best record slot in data segment to insert fact
 */
Word
dsimc_best_rec_slot(rel,seg,rec_size,clust,rec_no)
Reln *rel;
Segt *seg;
Int rec_size;
Word clust;
Int *rec_no;
{
	r_Word	end_of_seg;
	r_Word	rec_offset;
	r_Int	r, prevr;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Int	best_prev, best_rec, smallest;

	/*
	 * Compute interesting locations within the segment
	 */
	rec_ptr = (Recptr *)&(seg_buffer(seg)[nWords(Nr(rel))*(Dr(rel)+1)]);
	seg_ptr = (Segptr *)&(rec_ptr[Nr(rel)]);
	end_of_seg = seg->sd_offset + seg_size(rel);

	/*
	 * First try to find spot in local data segment space
	 */
#if 0
fprintf(stderr,"trying local seg space\n");
#endif
	prevr = UNUSED;
	r = seg_ptr->sp_localfree;
	while (r != UNUSED) {
#if 0
fprintf(stderr,"off:%d sz:%d nx:%d fspace:%x eos:%x\n",
	rec_ptr[r].rp_offset,rec_ptr[r].rp_blksize,rec_ptr[r].rp_recsize,
	seg_ptr->sp_freespace, end_of_seg);
#endif
		if (rec_ptr[r].rp_offset == UNUSED &&
			seg_ptr->sp_freespace+rec_size < end_of_seg) {
			/*
			 * Free spot at end of local data space
			 */
			rec_ptr[r].rp_blksize = rec_size;
			rec_offset = seg_ptr->sp_freespace;
			rec_ptr[r].rp_offset = rec_offset;
			seg_ptr->sp_freespace += rec_size;
			break;
		}
		or (rec_ptr[r].rp_offset != UNUSED &&
			rec_size <= rec_ptr[r].rp_blksize) {
			/*
			 * Use a "hole" in middle of local data space
			 */
			rec_offset = rec_ptr[r].rp_offset;
			break;
		}
		prevr = r;
		r = rec_ptr[r].rp_nextfree;
	}
	if (r != UNUSED) {
#if 0
fprintf(stderr, "found some\n");
#endif
		if (prevr == UNUSED)
			seg_ptr->sp_localfree = rec_ptr[r].rp_nextfree;
		else
			rec_ptr[prevr].rp_nextfree = rec_ptr[r].rp_nextfree;
		/*
		 * Note: rp_nextfree == rp_recsize
		 */
		rec_ptr[r].rp_recsize = rec_size;
		rec_ptr[r].rp_hash = clust;
		*rec_no = r;
		return(rec_offset);
	}

	/*
	 * Now try to find a slot to throw away from local segment space
	 */
#if 0
fprintf(stderr, "find loc seg slot to throw away\n");
#endif
	prevr = UNUSED;
	r = seg_ptr->sp_localfree;
	best_prev = UNUSED;
	best_rec = UNUSED;
	smallest = 9999;
	while (r != UNUSED) {
		if (rec_ptr[r].rp_blksize < smallest) {
			smallest = rec_ptr[r].rp_blksize;
			best_rec = r;
			best_prev = prevr;
		}
		prevr = r;
		r = rec_ptr[r].rp_nextfree;
	}
	if (best_rec != UNUSED) {
#if 0
fprintf(stderr, "done it\n");
#endif
		if (best_prev == UNUSED)
			seg_ptr->sp_localfree = rec_ptr[best_rec].rp_nextfree;
		else
			rec_ptr[best_prev].rp_nextfree = rec_ptr[best_rec].rp_nextfree;
		rec_ptr[best_rec].rp_hash = clust;
		rec_ptr[best_rec].rp_recsize = rec_size;
		rec_ptr[best_rec].rp_blksize = rec_size;
		rec_offset = lseek(datafile(seg), (Faddr)0, L_XTND);
		rec_ptr[best_rec].rp_offset = rec_offset;
		*rec_no = best_rec;
		return(rec_offset);
	}

	/*
	 * Hmmmm ... should never get here
	 */
	return((Word)UNUSED);
}

/*
 * simc_best_segment:
 *	Find best segment for SIMC indexing
 *	Split segments into two types: primary and overflow
 *	Try first to fit into a primary segment, and if we
 *	can't, wallow around in the overflow segments until
 *	we find one with enough room
 */
Int
simc_best_segment(tr)
Trans *tr;
{
	r_Int	i, j, s;
	r_Int	segno;
	r_Reln	*rel = tr->relation;
	Int	Nps; /* # primary segs */
	Int	Nos; /* # overflow segs */
	Int	*counters, ncounters;
	Int	best_seg, best_cnt;

	/*
	 * Read in record counts so we know which segs are full
	 */
	ncounters = (Ns(rel)+2) * sizeof(Int);
	if ((counters = (Int *)malloc(ncounters)) == IntNULL)
		return(-1);
	lseek(descfile(rel), coffset(rel), L_SET);
	read(descfile(rel), (char *)counters, ncounters);

	/*
	 * Arbitrarily choose:
	 * 70% primary segments and 30% overflow segment
	 */
	Nps = (Int)(0.7 * Ns(rel));
	Nos = Ns(rel) - Nps;

	/*
	 * Determine the primary segment where
	 * we would prefer to locate the record
	 */
	segno = cluster % Nps;

	for (i = 0; i < Ns(rel); i++) {
		if (counters[segno] < Nr(rel)) {
			free((char *)counters);
			return(tr->cur_seg = segno);
		}
		or (counters[segno] == Nr(rel)) {
			best_seg = -1; best_cnt = Nr(rel);
			s = counters[Ns(rel)];
			if (s >= Nps) {
				for (j = 0; j < Nos; j++, s = ((s+1)%Nos)+Nps) {
					if (counters[s] < best_cnt) {
						best_cnt = counters[s];
						best_seg = s;
						/* (best_cnt < Nr(rel)/2) */
						if (best_cnt < (Nr(rel)>>1)) {
							if (best_seg == Ns(rel)-1)
								counters[Ns(rel)] = Nps;
							else
								
								counters[Ns(rel)] = best_seg+1;
							break;
						}
					}
				}
			}
			if (best_seg == -1) {
				/*
				 * No room in overflow segs, try primary segs
				 */
				for (j = 0; j < Nps; j++, s = (s+1)%Nps) {
					if (counters[s] < best_cnt) {
						best_cnt = counters[s];
						best_seg = s;
						/* (best_cnt < Nr(rel)/2) */
						if (best_cnt < (Nr(rel)>>1)) {
							if (best_seg == Nps-1)
								counters[Ns(rel)] = 0;
							else
								counters[Ns(rel)] = best_seg+1;
							break;
						}
					}
				}
			}
			/*
			 * Give up
			 */
			if (best_seg == -1) {
				free((char *)counters);
				return(-1);
			}
			counters[segno] += best_seg+1;
		}
		segno = counters[segno]-Nr(rel)-1;
	}
	lseek(descfile(rel), coffset(rel), L_SET);
	write(descfile(rel), (char *)counters, ncounters);
	free((char *)counters);
	return(-1);
}

/*
 * dsimc_best_segment:
 *	Find best segment for DSIMC indexing
 *	Use recursive linear hashing to work out where to put the record
 */
Segt *
dsimc_best_segment(tr, rec_size)
Trans *tr;
Int rec_size;
{
	r_Reln	*rel = tr->relation;
	r_Int	level, segno;
	r_Segt	*seg;
	Bool	ok;

	/*
	 * Try to place new record on the lowest level
	 * If it fails on this level go to next level, until we run out
	 */
	for (level = 0; level < MAXLEVELS; level++)
	{
		segno = segmentOfHash(rel, tr->cluster, level);
		if (segno == -1) {
			/* Start new overflow level */
			tr->cur_seg = 0;
			tr->cur_level = level;
			add_segs(rel, level, 0);
			rel_info_update(rel);
			seg = seg_open(rel, 0, level, tr->operation);
			if (seg == SegtNULL) {
				error("Can't open new segment");
				return(SegtNULL);
			}
			return(seg);
		}
#ifdef DBUG
		debug(ADD_DBUG)
			printf("trying seg:%d lev:%d\n",segno,level);
#endif
		/*
		 * Check whether the suggested segment has room
		 * i.e. find a record slot and data space for the record
		 * If we can't get one of these, go to next ovflow level
		 */
		seg = seg_open(rel, segno, level, tr->operation);
		if (seg == SegtNULL) {
			error("Can't open segment to check for room");
			return(SegtNULL);
		}
		ok = dsimc_seg_has_room(rel, seg, rec_size);
		if (ok) {
			tr->cur_seg = segno;
			tr->cur_level = level;
			return(seg);
		}
	}

	/*
	 * Crude (i.e. nonexistent) overflow handling
	 */
	error("Overflow too deep");
	return(SegtNULL);
}

/*
 * simc_sdesc_update:
 *	Update segment descriptors for superimposed codeword addition
 */
simc_sdesc_update(tr)
Trans *tr;
{
	r_Int	i, *bit;
	r_Int	slice_size;
	r_Int	slice_offset;
	r_Word	*slice;
	r_Int	seg_no = tr->cur_seg;
	r_Reln	*rel = tr->relation;

	slice_size = Ns(rel);
	slice = b_make(slice_size);
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"set bit#%d in seg slices",seg_no);
#endif
	for (i = 0, bit = tr->seg_list; i < tr->nsegbits; i++, bit++)
	{
#ifdef DBUG
		debug(ADD_DBUG) fprintf(stderr," %d",*bit);
#endif DBUG
		slice_offset = (Faddr)(offset(rel) + (*bit)*nBytes(slice_size));
		lseek(descfile(rel), slice_offset, L_SET);
		read(descfile(rel), (char *)slice, nBytes(slice_size));
		if (!b_test(seg_no, slice, Ns(rel))) {
			b_set(seg_no, slice, Ns(rel));
			lseek(descfile(rel), slice_offset, L_SET);
			write(descfile(rel), (char *)slice, nBytes(slice_size));
		}
	}
#ifdef DBUG
	debug(ADD_DBUG) putc('\n',stderr);
#endif DBUG
	b_free(slice);
}

/*
 * dsimc_sdesc_update:
 *	Update segment descriptors for hashed superimposed codeword addition
 */
dsimc_sdesc_update(tr)
Trans *tr;
{
	r_Int	i, *bit;
	r_Word	*slice;
	r_Int	slice_size;
	r_Word	*index_block;
	r_Int	which_block;
	r_Int	block_offset;
	r_Int	seg_within_block;
	r_Int	seg_no;
	r_Reln	*rel = tr->relation;

	slice_size = segs_per_block(rel);
	if ((index_block = (Word *)malloc(block_size(rel))) == WordNULL) {
		error("no_mem_for_index_block");
		return;
	}

	/*
	 * Work out which level 0 segment we are associated with
	 */
	seg_no = segmentOfHash(rel, tr->cluster, 0);

	/*
	 * Determine which block for this segment and read in the block
	 */
	which_block = seg_no >> log2_segs_per_block(rel);
	block_offset = (Faddr)(offset(rel) + which_block*block_size(rel));
	lseek(descfile(rel), block_offset, L_SET);
	read(descfile(rel), (char *)index_block, block_size(rel));

	seg_within_block = seg_no & (segs_per_block(rel)-1);
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"set bit#%d in seg slices",seg_within_block);
#endif
	for (i = 0, bit = tr->seg_list; i < tr->nsegbits; i++, bit++)
	{
#ifdef DBUG
		debug(ADD_DBUG) fprintf(stderr," %d",*bit);
#endif DBUG
		slice = &(index_block[*bit * nWords(slice_size)]);
		b_set(seg_within_block, slice, slice_size);
	}
#ifdef DBUG
	debug(ADD_DBUG) putc('\n',stderr);
#endif DBUG
	lseek(descfile(rel), block_offset, L_SET);
	write(descfile(rel), (char *)index_block, block_size(rel));
	free((char *)index_block);
}

/*
 * rectext:
 *	Fetch the text of a record from a segment in a relation
 *	Format it so that it looks the same as when it was inserted
 */
String
rectext(rec, seg, rel)
Recptr *rec;
Segt *seg;
Reln *rel;
{
	r_Char	*buf, *c, *sbuf;
	r_Int	nbytes, rec_size;
	Int	rel_size, rec_index;

	/*
	 * Make a buffer to store the formatted record
	 */
	if ((c = rindex(dbrelname(rel), '/')) == (Char *)0) {
		error("Invalid database/relation string");
		return(StrNULL);
	}
	c++;
	rel_size = strlen(c);
	rec_size = rec->rp_recsize;
	nbytes = rel_size + rec_size + 4;
	if ((buf = mkstr(nbytes)) == StrNULL) {
		error("Can't make record buffer");
		return(StrNULL);
	}

	/*
	 * Format record by pre-pending relation name to key values
	 */
	strcpy(buf, c);
	c = buf + rel_size;
	*c++ = '(';

	/*
	 * If the record is in the overflow area, we must read() it
	 * Otherwise, we can grab it quickly from the segment buffer
	 */
	sbuf = (Char *)seg_buffer(seg);
	rec_index = (Faddr)rec->rp_offset - seg_base(seg);
#if 0
fprintf(stderr,"rec:index:%d offset:%x size:%d buf:%x\n",
		rec_index,rec->rp_offset,rec_size,sbuf);
#endif
	if (rec_index < seg_size(rel))
		strncpy(c, sbuf+rec_index, rec_size);
	else {
		lseek(datafile(seg), (Faddr)rec->rp_offset, L_SET);
		if (read(datafile(seg), c, rec_size) != rec_size) {
			error("Can't read record");
			return(StrNULL);
		}
	}
	c += rec_size;
	*c++ = ')';
#if 0
	*c++ = '.'; /* for MU-Prolog */
#endif
	*c = ChNULL;

	return(buf);
}

