/*
 * rec.c  -  deductive database package (record operations)
 *
 * $Header: rec.c,v 1.5 85/12/06 15:10:11 jas Exp $
 * $Log:	rec.c,v $
 * Revision 1.5  85/12/06  15:10:11  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:50  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:12  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:48:35  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:39  jas
 * Initial revision
 * 
 * 
 */

#include "muddlib.h"

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
	b_copy(&(seg_buffer(seg)[nwords(Nr(rel))*Dr(rel)]), tr->rec_matches, Nr(rel));
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
			slice = &(seg_buffer(seg)[(*bit)*nwords(slice_size)]);
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
			slice = &(seg_buffer(seg)[node->e_rmaskpos*nwords(slice_size)]);
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
	r_Reln	*rel = tr->relation;
	r_Segt	*seg;
	r_Int	i;
	r_Int	*bit;
	r_Word	*seg_buf;
	r_Char	*buf;
	Int	seg_no;
	Int	rec_no;
	Char	*start_keys;
	Char	*end_keys;
	Int	rec_size;
	Faddr	rec_offset;
	Word	*slice;
	Int	slice_size;
	Faddr	slice_offset;
	Int	find_best_segment();
	Word	find_best_rec_slot();

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
	seg_no = find_best_segment(tr);
	if (seg_no < 0) {
		error("no_seg_for_rec");
		return(-1);
	}
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"choose seg: %d\n",seg_no);
#endif DBUG

	/*
	 * Update segment descriptors
	 */
	switch (index_type(rel))
	{
	when SIMC_INDEX:
		simc_sdesc_update(tr);
	when DSIMC_INDEX:
		dsimc_sdesc_update(tr);
	}

	/*
	 * By now, we have finished with the relation descriptor
	 * file so we can use "rel_close" to unlock it ... the
	 * static info about the relation, however, lingers on
	 */
	rel_close(rel);

	seg_fetch(tr);
	seg = tr->segment;
	seg_buf = seg_buffer(seg);

	rec_offset = find_best_rec_slot(rel,seg,rec_size,&rec_no);
	tr->cur_rec = rec_no;
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"choose rec: %d\n",rec_no);
#endif DBUG

	/*
	 * Update record descriptors
	 */
	slice_size = Nr(rel);
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"set bit#%d in rec slices",rec_no);
#endif DBUG
	for (i = 0, bit = tr->rec_list; i < tr->nrecbits; i++, bit++)
	{
#ifdef DBUG
		debug(ADD_DBUG) fprintf(stderr," %d",*bit);
#endif DBUG
		slice = &(seg_buf[nwords(slice_size)*(*bit)]);
		b_set(rec_no, slice, slice_size);
	}
#ifdef DBUG
	debug(ADD_DBUG) putc('\n',stderr);
#endif DBUG

	/*
	 * Set Presence bit
	 */
	slice = &(seg_buf[nwords(slice_size)*Dr(rel)]);
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
		write(datafile(seg), (Char *)seg_buf, seg_buf_size(rel));
		lseek(datafile(seg), (Faddr)rec_offset, L_SET);
		write(datafile(seg), start_keys, rec_size);
	}

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
	r_Word	rec_offset;
	r_Int	r = tr->cur_rec;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Int	*counters, ncounters;

	/*
	 * Work out useful locations in data segment
	 */
	rec_ptr = (Recptr *)&(seg_buffer(seg)[nwords(Nr(rel))*(Dr(rel)+1)]);
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
		seg_ptr->sp_localfree = r;
	}
	else {
		/*
		 * Link into free list for segment overflow space
		 */
		rec_ptr[r].rp_nextfree = seg_ptr->sp_extrnfree;
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
		slice = &(seg_buffer(seg)[nwords(slice_size)*i]);
		b_reset(r, slice, slice_size);
	}

	/*
	 * Write updates to data file
	 */
	lseek(datafile(seg), seg_base(seg), L_SET);
	write(datafile(seg), (char *)seg_buffer(seg), seg_buf_size(rel));

	/*
	 * Update record counter in relation descriptor file
	 */
	ncounters = (Ts(rel)+2) * sizeof(Int);
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
#ifdef TODO
	static	Char	fbuf[BUFSIZ];
	Char	*quotify();
#else
	r_Char	*fbuf;
#endif
	Faddr	rec_offset;
	r_Int	rec_size;
	r_Char	*fac = tr->rec_buf;
	r_Reln	*rel = tr->relation;
	r_Segt	*seg = tr->segment;
	Recptr	*rec_ptr;
	Char	*sbuf;
	Int	rec_index;

#ifdef DBUG
	debug(GET_DBUG)
		fprintf(stderr,"fetch seg#%d rec#%d\n",tr->cur_seg,tr->cur_rec);
#endif DBUG
	if (seg_buffer(seg) == WordNULL &&
		!open_segment(seg,seg_size(rel)))
		fatal("data_file_reopen");
#ifdef DBUG
	debug(VDESC_DBUG) seg_print("rec_fetch:",seg);
#endif DBUG
	/*
	 * Starting location of record in segment buffer
	 */
	rec_ptr = (Recptr *)&(seg_buffer(seg)[nwords(Nr(rel))*(Dr(rel)+1)]);
	rec_offset = (Faddr)rec_ptr[tr->cur_rec].rp_offset;
	rec_size = rec_ptr[tr->cur_rec].rp_recsize;
#ifdef DBUG
	debug(GET_DBUG)
		fprintf(stderr, "fetch from offset: %x\n", rec_offset);
#endif DBUG
	if (rec_offset == UNUSED)
		goto FetchFail;

#ifdef TODO
	/*
	 * If the record is in the overflow area, we must read() it
	 * Otherwise, we can grab it quickly from the segment buffer
	 */
	sbuf = (Char *)seg_buffer(seg);
	rec_index = rec_offset - seg_base(seg);
	if (rec_index < seg_size(rel))
		strncpy(tmpbuf, sbuf+rec_index, rec_size);
	else {
		lseek(datafile(seg), rec_offset, L_SET);
		if (read(datafile(seg), tmpbuf, rec_size) != rec_size)
			goto FetchFail;
	}

	/*
	 * Form record by appending key values to quoted relation name
	 */
	sprintf(fac, "'%s'(%s).", tr->query->e_name, quotify(tmpbuf));
#else UNQUOTED_ATOMS
	/*
	 * Form record by appending relation name to key values
	 */
	strcpy(fac,tr->query->e_name);
	fbuf = &(fac[strlen(fac)]);
	*fbuf++ = '(';

	/*
	 * If the record is in the overflow area, we must read() it
	 * Otherwise, we can grab it quickly from the segment buffer
	 */
	sbuf = (Char *)seg_buffer(seg);
	rec_index = rec_offset - seg_base(seg);
	if (rec_index < seg_size(rel))
		strncpy(fbuf, sbuf+rec_index, rec_size);
	else {
		lseek(datafile(seg), rec_offset, L_SET);
		if (read(datafile(seg), fbuf, rec_size) != rec_size)
			goto FetchFail;
	}
	fbuf += rec_size;
	*fbuf++ = ')';
	*fbuf++ = '.'; /* to make MU-Prolog happy */
	*fbuf = ChNULL;
#endif UNQUOTED_ATOMS

	tr->nrecs_fetched++;
	return(0);

FetchFail:
	*fac = ChNULL;
	return(-1);
}

/*
 * find_best_rec_slot:
 *	Determine best record slot in data segment to insert fact
 */
Word
find_best_rec_slot(rel,seg,rec_size,rec_no)
Reln *rel;
Segt *seg;
Int rec_size;
Int *rec_no;
{
	r_Word	end_of_seg;
	r_Word	rec_offset;
	r_Int	r, prevr;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Int	best_prev, best_rec, smallest;

	/*
	 * Find space in segment data area
	 */
	rec_ptr = (Recptr *)&(seg_buffer(seg)[nwords(Nr(rel))*(Dr(rel)+1)]);
	seg_ptr = (Segptr *)&(rec_ptr[Nr(rel)]);
	end_of_seg = seg->sd_offset + seg_size(rel);

	/*
	 * First try to find spot in local data segment space
	 */
	prevr = UNUSED;
	r = seg_ptr->sp_localfree;
	while (r != UNUSED) {
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
		if (best_prev == UNUSED)
			seg_ptr->sp_extrnfree = rec_ptr[best_rec].rp_nextfree;
		else
			rec_ptr[best_prev].rp_nextfree = rec_ptr[best_rec].rp_nextfree;
		rec_ptr[best_rec].rp_recsize = rec_size;
		rec_ptr[best_rec].rp_blksize = rec_size;
		rec_offset = lseek(datafile(seg), (Faddr)0, L_XTND);
		rec_ptr[best_rec].rp_offset =  rec_offset;
		*rec_no = best_rec;
		return(rec_offset);
	}

	/*
	 * Now try to find a slot to throw away from local segment space
	 */
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
		if (best_prev == UNUSED)
			seg_ptr->sp_localfree = rec_ptr[best_rec].rp_nextfree;
		else
			rec_ptr[best_prev].rp_nextfree = rec_ptr[best_rec].rp_nextfree;
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
 * find_best_segment:
 *	Use information in key to determine best segment to put it in
 *	This implements clustering of similar records
 */
Int
find_best_segment(tr)
Trans *tr;
{
	r_Int	segaddr;
	r_Int	ncounters;
	r_Int	*counters;
	r_Reln	*rel = tr->relation;

	/*
	 * Read in record counts so we know which segs are full
	 */
	ncounters = (Ts(rel)+2) * sizeof(Int);
	if ((counters = (Int *)malloc(ncounters)) == IntNULL)
		return(-1);
	lseek(descfile(rel), coffset(rel), L_SET);
	read(descfile(rel), (char *)counters, ncounters);

	switch (index_type(rel))
	{
	when SIMC_INDEX:
		segaddr = simc_best_seg(tr,counters);
	when DSIMC_INDEX:
		segaddr = dsimc_best_seg(tr,counters);
	}

	/*
	 * We only update counters if there are no errors
	 */
	if (segaddr != -1) {
		counters[segaddr]++;
		/* counters[Ns(rel)+1]++; */
		lseek(descfile(rel), coffset(rel), L_SET);
		write(descfile(rel), (char *)counters, ncounters);
	}
	free((char *)counters);
	return(segaddr);
}

/*
 * simc_best_seg:
 *	Find best segment for SIMC indexing
 *	Split segments into two types: primary and overflow
 *	Try first to fit into a primary segment, and if we
 *	can't, wallow around in the overflow segments until
 *	we find one with enough room
 */
Int
simc_best_seg(tr,counters)
Trans *tr;
Int *counters;
{
	r_Int	i, j, s;
	r_Int	segno;
	r_Int	size;
	r_Int	Nps; /* # primary segs */
	r_Int	Nos; /* # overflow segs */
	r_Reln	*rel = tr->relation;
	Int	best_seg, best_cnt;

	Nps = (Int)(0.7 * Ns(rel));
	Nos = Ns(rel) - Nps;

	segno = cluster % Nps;

	for (i = 0; i < Ns(rel); i++) {
		if (counters[segno] < Nr(rel))
			return(tr->cur_seg = segno);
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
			if (best_seg == -1)
				return(-1);
			counters[segno] += best_seg+1;
		}
		segno = counters[segno]-Nr(rel)-1;
	}
	return(-1);
}

/*
 * dsimc_best_seg:
 *	Find best segment for DSIMC indexing
 *	Use recursive linear hashing to work out where to put the record
 */
Int
dsimc_best_seg(tr,counters)
Trans *tr;
Int *counters;
{
	r_Int	level;
	r_Int	segaddr;
	r_Reln	*rel = tr->relation;

	for (level = 0; level < MAXOVLEVELS; level++)
	{
		set_segment(tr, level);
		if (tr->cur_seg == -1) {
			error("overflow_too_deep");
			return(-1);
		}
		segaddr = addressofindex(tr->cur_seg, level);
#ifdef DBUG
		debug(ADD_DBUG) printf("seg:%d lev:%d addr:%d cntrs:%d ",
				tr->cur_seg,level,segaddr,counters[segaddr]);
#endif
		if (counters[segaddr] < Nr(rel)) {
			tr->cur_level = level;
			return(segaddr);
		}
	}

	/*
	 * Crude overflow handling ... flag an error
	 */
	return(-1);
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
		slice_offset = (Faddr)(offset(rel) + (*bit)*nbytes(slice_size));
		lseek(descfile(rel), slice_offset, L_SET);
		read(descfile(rel), (char *)slice, nbytes(slice_size));
		if (!b_test(seg_no, slice, Ns(rel))) {
			b_set(seg_no, slice, Ns(rel));
			lseek(descfile(rel), slice_offset, L_SET);
			write(descfile(rel), (char *)slice, nbytes(slice_size));
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
	r_Int	i, *bit, nsegs0;
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
	seg_no = ovflow_segment(tr,tr->cluster,0);

	/*
	 * Determine which block for this segment and read in the block
	 */
	which_block = seg_no >> log2_segs_per_block(rel);
	block_offset = (Faddr)(offset(rel) + which_block*block_size(rel));
	lseek(descfile(rel), block_offset, L_SET);
	read(descfile(rel), (char *)index_block, block_size(rel));

	seg_within_block = seg_no & (segs_per_block(rel)-1);
#if 0
fprintf(stderr,"mask:%x\n",segs_per_block(rel)-1);
fprintf(stderr,"seg_desc_no:%d block:%d offset:%d\n", seg_no, which_block, seg_within_block);
#endif
#ifdef DBUG
	debug(ADD_DBUG) fprintf(stderr,"set bit#%d in seg slices",seg_within_block);
#endif
	for (i = 0, bit = tr->seg_list; i < tr->nsegbits; i++, bit++)
	{
#ifdef DBUG
		debug(ADD_DBUG) fprintf(stderr," %d",*bit);
#endif DBUG
		slice = &(index_block[*bit * nwords(slice_size)]);
		b_set(seg_within_block, slice, slice_size);
	}
#ifdef DBUG
	debug(ADD_DBUG) putc('\n',stderr);
#endif DBUG
	lseek(descfile(rel), block_offset, L_SET);
	write(descfile(rel), (char *)index_block, block_size(rel));
	free(index_block);
}
