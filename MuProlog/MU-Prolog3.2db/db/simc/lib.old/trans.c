/*
 * trans.c  -  deductive database package (transaction operations)
 *
 * $Header: trans.c,v 1.5 85/12/06 15:10:27 jas Exp $
 * $Log:	trans.c,v $
 * Revision 1.5  85/12/06  15:10:27  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:24:27  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:43  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:49:05  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:43  jas
 * Initial revision
 * 
 * 
 */

#include "muddlib.h"

/*
 * next_rec:
 *	Set the cur_rec field to the next matching record
 */
#define	next_rec(TR) \
	(TR->cur_rec = \
		b_nextset(TR->cur_rec+1, TR->rec_matches, Nr(TR->relation)))

/*
 * next_seg:
 *	Set the cur_seg field to the next matching record
 */
#define	next_seg(TR) \
	do_next_seg(TR)


/*
 * trans_open:
 *	Commence a transaction on an open relation
 */
Trans *

#ifdef SFB

trans_open(db_name, query, operation)
String db_name;
String query;
Opn operation;
{
	Trans *trans_sfbopen();

	return(trans_sfbopen(db_name, query, operation, NULL));
}

Trans *
trans_sfbopen(db_name, query, operation, sfbvec)

#else

trans_open(db_name, query, operation)

#endif

String db_name;
String query;
Opn operation;
#ifdef SFB
String sfbvec;
#endif SFB
{
	r_Trans	*tr;
	r_Reln	*rel;
	r_Char	*buf;
	Word	stars, max_mask;
	r_Int	i, ncombs, num_bytes;
	Char	reln_name[MAXRNAME];

	/*
	 * Make ourselves a transaction descriptor
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"new_trans:");
#endif DBUG
	if ((tr = mknew(Trans)) == TransNULL)
	{
		error("no_mem_for_transaction");
		return(TransNULL);
	}
	/*
	 * Initialise fields in transaction descriptor
	 */
	tr->operation = operation;
	tr->input = StrNULL;
	tr->query = ElemNULL;
	tr->relation = RelnNULL;

	tr->nsegbits = 0;
	tr->seg_ratio = 0.0;
	tr->seg_list = IntNULL;
	tr->seg_matches = WordNULL;
	tr->cur_seg = -1;

	tr->cur_level = 0;
	tr->cluster = 0;
	tr->clustars = 0;
	tr->seg_gaps = 0;
	tr->seg_off = tr->seg_count = tr->seg_ncombs = 0;
	tr->segment = SegtNULL;

	for (i = 0; i < MAXOVLEVELS; i++)
		tr->ov_matches[i] = WordNULL;

	tr->nrecbits = 0;
	tr->rec_ratio = 0.0;
	tr->rec_list = IntNULL;
	tr->rec_matches = WordNULL;
	tr->cur_rec = -1;
	tr->rec_buf = StrNULL;

	tr->nsegs_fetched = tr->nrecs_fetched = 0;
	tr->nblocks_fetched = tr->nsdescs_anded = 0;

	/*
	 * Determine which relation this is and open it
	 */
	if ((buf = (Char *)index(query,'(')) == StrNULL) {
		error("bad_query");
		goto TransFail;
	}
	else {
		r_Char	*b = reln_name;
		r_Char	*c = query;

		while (c < buf) *b++ = *c++;
		*b = '\0';
	}

	/*
	 * Allocate space for input string and make a copy of it
	 */
	if ((tr->input = mkstr(strlen(query))) == StrNULL) {
		error("no_space_for_input");
		goto TransFail;
	}
	strcpy(tr->input, query);

	/*
	 * Open a relation descriptor for this transaction
	 */
	tr->relation = rel = rel_open(db_name, reln_name, operation);
	if (rel == RelnNULL) {
		error("rel_open");
		goto TransFail;
	}

	/*
	 * Allocate space for lists of set record & segment bits
	 */
#ifdef TODO
	Change these lists to arrays of shorts
#endif TODO
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"seg_list:");
#endif DBUG
	num_bytes = (numkeys(rel)*Ks(rel)+Mr(rel)) * sizeof(Int);
	if ((tr->seg_list = (Int *)malloc((unsigned)num_bytes)) == IntNULL) {
		error("no_mem_for_seg_list");
		goto TransFail;
	}

#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"rec_list:");
#endif DBUG
	num_bytes = (numkeys(rel)*Kr(rel)+Mr(rel)) * sizeof(Int);
	if ((tr->rec_list = (Int *)malloc((unsigned)num_bytes)) == IntNULL) {
		error("no_mem_for_rec_list");
		goto TransFail;
	}

	/*
	 * Allocate space for fetched-record buffer
	 * Allow for records five times as large as the average
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"fac_str:");
#endif DBUG
	if ((tr->rec_buf = mkstr(5*Av(rel))) == StrNULL) {
		error("no_mem_for_rec_buf");
		goto TransFail;
	}

	/*
	 * Allocate space for lists of matching records & segments
	 */
	if ((tr->rec_matches = b_make(Nr(rel))) == WordNULL) {
		error("no_mem_for_rec_matches");
		goto TransFail;
	}
	switch (index_type(rel))
	{
	when SIMC_INDEX:
		if ((tr->seg_matches = b_make(Ns(rel))) == WordNULL) {
			error("no_mem_for_seg_matches");
			goto TransFail;
		}
	when DSIMC_INDEX:
		if ((tr->seg_matches = b_make(segs_per_block(rel))) == WordNULL) {
			error("no_mem_for_seg_matches");
			goto TransFail;
		}
		for (i = 1; i < MAXOVLEVELS; i++) {
			if (rel->rd_nlevsegs[i] == 0) break;
			tr->ov_matches[i] = b_make(rel->rd_nlevsegs[i]);
			if (tr->ov_matches[i] == WordNULL) {
				error("no_mem_for_ov_seg_matches");
				goto TransFail;
			}
		}
	}

	/*
	 * Parse the query
	 */
	if (!key_parse(tr, query)) {
		error("query_parse");
		goto TransFail;
	}
	tr->cluster = cluster;
	tr->clustars = clustars;

#ifdef SFB
	/*
	 * Modify cluster/clustars according to sfbvec
	 */
	if (sfbvec != NULL) {
		Int i;
		Char *c;
		Word desc, stars;

		desc = stars = 0;
		for (c = sfbvec; *c != '\0'; c++)
		{
			desc <<= 1;
			stars <<= 1;
			switch (*c)
			{
			case '1':
				desc |= 1;
				break;
			case '*':
				stars |= 1;
				break;
			case '0':
			default:
				/* ignore 0's */
				/* treat any other junk as 0's */
				break;
			}
		}
fprintf(stderr,"clus:%08x clar:%08x\n", tr->cluster, tr->clustars);
fprintf(stderr,"desc:%08x star:%08x\n", desc, stars);
		tr->cluster |= desc;
		tr->clustars &= stars;
fprintf(stderr,"clus:%08x clar:%08x\n", tr->cluster, tr->clustars);
	}
#endif SFB

#ifdef DBUG
	debug(VDESC_DBUG) trans_print("new transaction:",tr);
#endif DBUG
	/*
	 * For query, set up matching segments bit string,
	 * and fetch first segment
	 */
	if (!isassert(operation)) {
		switch (index_type(rel))
		{
		when SIMC_INDEX:
			setup_seg_matches(tr);
		when DSIMC_INDEX:
			set_segment(tr,0);
			setup_block_seg_matches(tr);
		}
		if (next_seg(tr) >= 0)
			seg_fetch(tr);
		rel_close(rel);
	}
	return(tr);

	/*
	 * When we can't create the transaction objects
	 */
TransFail:
	error("trans_open_fail");
	trans_close(tr);
	return(TransNULL);
}

/*
 * trans_close:
 *	clean up after completing a transaction
 */
void
trans_close(tr)
Trans *tr;
{
	r_Int	i;

	if (tr == TransNULL)
		return;
XX	cfree(tr->input, Char);
	skel_free(tr->query);
XX	tr->query = ElemNULL;
	if (!isassert(tr->operation))
		rel_close(tr->relation);
XX	tr->relation = RelnNULL;

XX	cfree(tr->seg_list, Int);
XX	cfree(tr->seg_matches, Word);
XX	for (i = 0; i < MAXOVLEVELS; i++)
XX		cfree(tr->ov_matches[i],Word);
	seg_close(tr->segment);
XX	tr->segment = SegtNULL;

XX	cfree(tr->rec_list, Int);
XX	cfree(tr->rec_matches, Word);
XX	cfree(tr->rec_buf, Char);

XX	cfree(tr, Trans);
}

/*
 * trans_assert:
 *	Add a new record to the database
 */
Int
trans_assert(db_name, fac, operation)
String db_name;
String fac;
Opn operation;
{
	r_Trans	*tr;
	r_Int	nmatches;

	if ((tr = trans_open(db_name,fac,operation)) == TransNULL)
		return(-1);
	
	nmatches = rec_insert(tr);

	trans_close(tr); tr = TransNULL;

	return(nmatches == 1 ? 1 : -1);
}

/*
 * trans_retract:
 *	Delete record(s) matching input query from the database
 */
Int
trans_retract(db_name, query, operation)
String db_name;
String query;
Opn operation;
{
	r_Trans	*tr;
	r_Int	nmatches = 0;

	if ((tr = trans_open(db_name,query,operation)) == TransNULL)
		return(-1);
	
#ifdef TODO
	Fetching for retracts MUST do unification ????!

	Or else, trans_retract doesn't actually do the
	deleting, it simply returns stuff to Prolog, which
	explicitly requests the deletion to occur.
#endif TODO
	if (issingle(operation)) {
		if (trans_fetch(tr)) {
			rec_delete(tr);
			nmatches++;
		}
	}
	else {
		while (trans_fetch(tr)) {
			rec_delete(tr);
			nmatches++;
		}
	}

	trans_close(tr); tr = TransNULL;
	return(nmatches);
}

/*
 * trans_query:
 *	Print the values of all "matching" records in the database
 */
Int
trans_query(db_name, qry, operation)
String db_name;
String qry;
Opn operation;
{
	r_Trans	*tr;
	r_Int	nmatches = 0;

	if ((tr = trans_open(db_name,qry,operation)) == TransNULL)
		return(-1);
	
	if (issingle(operation)) {
		if (trans_fetch(tr)) {
			fact_write(stdout, tr->rec_buf);
			nmatches++;
		}
	}
	else {
		while (trans_fetch(tr)) {
			fact_write(stdout, tr->rec_buf);
			nmatches++;
		}
	}

	trans_close(tr); tr = TransNULL;
	return(nmatches);
}

/*
 * trans_fetch:
 *	fetch the next matching record from the database
 */
String
trans_fetch(tr)
Trans *tr;
{
#ifdef DBUG
	debug(GET_DBUG)
		fprintf(stderr,"tr_fetch:cur_seg:%d:cur_rec:%d\n",
					tr->cur_seg,tr->cur_rec);
#endif DBUG
GetNextRec:
	if (next_rec(tr) < 0)
	{
		/*
		 * If no more matches in this segment, try next
		 */
	GetNextSeg:
#ifdef DBUG
		debug(GET_DBUG) fprintf(stderr,"end_seg:%d\n",tr->cur_seg);
#endif DBUG
		if (next_seg(tr) < 0)
			return(StrNULL); /* end of query */
		seg_fetch(tr);
		if (next_rec(tr) < 0) {
			/*
			 * False match ... rarely happens
			 * Can be minimised by large segment descriptors
			 */
			goto GetNextSeg;
		}
	}
	if (rec_fetch(tr) < 0)
		goto GetNextRec;
	if (match(tr->query, tr->rec_buf))
		return(tr->rec_buf);
	else
		goto GetNextRec;
}


#ifdef DBUG

/*
 * trans_print:
 *	Dump contents of a trans record all over stderr
 */
trans_print(msg,tr)
String msg;
Trans *tr;
{
	r_Int	i, *list;

	fprintf(stderr,"%s\n",msg);
	if (tr == TransNULL) {
		fprintf(stderr,"TransNULL\n");
		return;
	}
	fprintf(stderr,"Trans@%x opn:%x seg:%d rec:%d\n",
		tr, tr->operation, tr->cur_seg, tr->cur_rec);
	rel_print("rel_desc:",tr->relation);

	fprintf(stderr,"Segment:  nbits:%d ratio:%0.3f\n",
		tr->nsegbits, tr->seg_ratio);
	fprintf(stderr,"seg_list: ");
	if (tr->seg_list == IntNULL)
		fprintf(stderr, "Empty");
	else
		for (i = 0, list = tr->seg_list; i < tr->nsegbits; i++, list++)
			fprintf(stderr, "%d ", *list);
	putc('\n',stderr);
	fprintf(stderr,"seg_matches: ");
	if (tr->relation == RelnNULL)
		fprintf(stderr,"None\n");
	else
		b_display("", tr->seg_matches, Ns(tr->relation));
	seg_print("seg_desc:", tr->segment);

	fprintf(stderr,"Record:  nbits:%d ratio:%0.3f\n",
		tr->nrecbits, tr->rec_ratio);
	fprintf(stderr,"rec_list: ");
	if (tr->rec_list == IntNULL)
		fprintf(stderr, "Empty");
	else
		for (i = 0, list = tr->rec_list; i < tr->nrecbits; i++, list++)
			fprintf(stderr, "%d ", *list);
	putc('\n',stderr);
	fprintf(stderr,"rec_matches: ");
	if (tr->relation == RelnNULL)
		fprintf(stderr,"None\n");
	else
		b_display("", tr->rec_matches, Ns(tr->relation));
	fprintf(stderr,"rec_buf: %s", tr->rec_buf==NULL?"NULL":tr->rec_buf);
}

#endif DBUG
