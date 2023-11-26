/*
 * split.c  -  deductive database package (dsimc split operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by James Thom, Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

/*
 * split:
 *	Split the segment under the split-pointer on specified level
 */
split(db, rel, level)
String db;
Reln *rel;
Int level;
{
	r_Int	lev;
	Int	nrecs[MAXLEVELS];
	Segt	seg[MAXLEVELS];
	Word	*segbuf[MAXLEVELS];
	Int	segno, ovseg, maxlevel, load;

	/*
	 * Determine split segment and increment split pointer
	 */
	segno = incr_sp(rel, level);
#if 0
fprintf(stderr,"Split (%d,%d)\t",segno,level);
fprintf(stderr,"Lc:");
for (lev=0; lev<MAXLEVELS; lev++)
	fprintf(stderr," %d",loadcnt(rel,lev));
fprintf(stderr,"\n");
#endif
	/*
	 * Collect all the records which are to be relocated in this split
	 * This means all records in the split segment, plus all records
	 * in the overflow segments of the split segment
	 */
	for (lev = level; lev < MAXLEVELS && Nls(rel,lev) > 0; lev++)
	{
		r_Word	*b1, *b2;
		Segt	*oldseg;
		Word	*end_seg, *oldsbuf, *empbuf, *presence;

		ovseg = ovflowSegment(rel, segno, level, lev);
		oldseg = seg_open(rel, ovseg, lev, opRETRACT);
		if (oldseg == SegtNULL)
			fatal("Can't open segment");
		oldsbuf = (Word *)seg_buffer(oldseg);

		/*
		 * Work out how many records are in the segment
		 */
		presence = (Word *)&oldsbuf[nWords(Nr(rel))*Dr(rel)];
		nrecs[lev] = b_count(presence, Nr(rel));
#if 0
fprintf(stderr,"Relocating %d recs from (%d,%d) into (%d,%d) & (%d,%d)\n",
			nrecs[lev],ovseg,lev,2*segno,level,2*segno+1,level);
#endif
		/*
		 * If there ain't no records in the segment,
		 * we can save a lot of useless mucking around
		 */
		if (nrecs[lev] == 0) {
			seg_close(oldseg);
			seg_write(oldseg, rel);
			continue;
		}

		/*
		 * Make the original segment appear completely empty:
		 * (1) clean out the segment buffer
		 * (2) clear the segment descriptor
		 * (3) set the record counter to zero
		 *
		 * Notes:
		 * (1) must be done by making a new clean segment (empbuf)
		 * and copying it into the segment buffer given to us via
		 * seg_open(), and dumping the cleared buffer via seg_close()
		 * to keep the seg-buf cache, etc consistent; we also retain
		 * a copy of the original seg (in segbuf[lev] via a fake seg
		 * struct)
		 */
		if ((segbuf[lev] = mknewsegbuf(rel, seg_base(oldseg))) == WordNULL)
			fatal("Can't make new segment buffer\n");
		if ((empbuf = mknewsegbuf(rel, seg_base(oldseg))) == WordNULL)
			fatal("Can't make new segment buffer\n");
		end_seg = &oldsbuf[seg_size(rel)/sizeof(Word)];

		/*
		 * Save a copy of old segment buffer, then
		 * clear out segment buffer obtained from seg_open()
		 */
		for (b1 = segbuf[lev], b2 = oldsbuf; b2 < end_seg; b1++, b2++)
			*b1 = *b2;
		for (b1 = oldsbuf, b2 = empbuf; b1 < end_seg; b1++, b2++)
			*b1 = *b2;
		free((char *)empbuf);

		/*
		 * Set up "fake" segment structure for old seg buf copy
		 */
		seg[lev] = *oldseg; /* struct assignment */
		seg[lev].sd_seg_buf = segbuf[lev];

		/*
		 * Dump empty segment, clear seg descs,
		 * fix number recs counter, add new segs
		 */
		seg_close(oldseg);
		seg_write(oldseg, rel);
		sdesc_clear(rel, ovseg, lev);
	}
	maxlevel = lev;

	/*
	 * Update various counters for split segment family
	 */
	for (lev = level, load = 0; lev < maxlevel; lev++)
		load += nrecs[lev];
	for (lev = 0; lev < level; lev++)
		loadcnt(rel, lev) -= load;
	loadcnt(rel, level) -= loadfac(rel, level);
	for (lev = level; lev < maxlevel; lev++)
	{
		Nlr(rel, lev) -= nrecs[lev];
		loadcnt(rel, lev) -= load;
		load -= nrecs[lev];
	}
	add_segs(rel, level, 2*segno+1);
	rel_info_update(rel);

	/*
	 * Re-insert all records from split segment family;
	 * "trans_assert" will decide where they should go
	 */
	for (lev = level; lev < maxlevel; lev++)
	{
		r_Int	i=0;
		Recptr	*r;
		String	rec;
		Recptr	*rec_ptrs, *top_ptrs;

		if (nrecs[lev] == 0)
			continue;
		rec_ptrs = (Recptr *)&(segbuf[lev][nWords(Nr(rel))*(Dr(rel)+1)]);
		top_ptrs = (Recptr *)&(rec_ptrs[Nr(rel)]);
		for (r = rec_ptrs; r < top_ptrs; r++) {
			if (r->rp_hash == UNUSED || r->rp_offset == UNUSED)
				continue;
			if ((rec = rectext(r, &seg[lev], rel)) != StrNULL) {
				Trans *tr; Int n;
				tr = trans_open(db, rec, (opASSERT|opSPLIT), level);
				if (tr == TransNULL)
					continue;
				n = rec_insert(tr);
				trans_close(tr);
				free(rec);
			}
		}
		free((char *)segbuf[lev]);
	}
#if 0
fprintf(stderr,"End split (%d,%d)\t", segno, level);
fprintf(stderr,"Lc:");
for (lev=0; lev<MAXLEVELS; lev++)
	fprintf(stderr," %d",loadcnt(rel,lev));
fprintf(stderr,"\n");
#endif
}

/*
 * unsplit:
 *	Coalesce the split-pointer's buddies when enough records removed
 */
unsplit(db, rel, level)
String db;
Reln *rel;
Int level;
{
}

/*
 * incr_sp:
 *	Increment the split pointer for a relation on a given level
 *	NOTE:	this function updates relation info;
 *		make sure reln_info_update() is done afterwards
 */
Int
incr_sp(rel,level)
Reln *rel;
Int level;
{
	r_Int	i;
	r_Int	d = depth(rel,level);
	r_Int	*m = &(magic(rel,level,0));

#if 0
fprintf(stderr, "incr_sp\n");
#endif
	if (splitp(rel,level) == ((01 << d) - 1)) {
		/* splitting bucket 0 and creating new depth */
		for (i = 0; i < d; m[i++] = 0);
		depth(rel,level)++;
		splitp(rel,level) = 0;
		return(0);
	}

	for (i = 0; (m[i+1] * 2 == m[i]) && (m[i] != 0); i++);

	if (m[i+1] * 2 + 2 == m[i])
		i++;
	else
		i = 0;
	splitp(rel,level)++;
	return((01 << (d-i-1)) + m[i]++);
}

/*
 * decr_sp:
 *	Decrement the split pointer for a relation on a given level
 *	NOTE:	this function updates relation info;
 *		make sure reln_info_update() is done afterwards
 */
Int
decr_sp(rel,level)
Reln *rel;
Int level;
{
	r_Int	i;
	r_Int	d = depth(rel,level);
	r_Int	*m = &(magic(rel,level,0));
	
	if (splitp(rel,level) == 0) {
		/* compacting into bucket 0 and decreasing depth */
		if (d == 0)
			return(-1);
		depth(rel,level)--;
		splitp(rel,level) = (01 << d) - 1;
		for (i = 0; i < d; i++)
			m[i] = 01 << (d-i-1);
		return(0);
	}

	for (i = 0; m[i+1] * 2 == m[i]; i++);

	splitp(rel,level)--;
	return((01 << (d-i-1)) + --m[i]);
}
