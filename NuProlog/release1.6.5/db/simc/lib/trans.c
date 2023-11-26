/*
 * trans.c  -  deductive database package (transaction operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

#ifdef DBSTATS
Int	n_segopens = 0;
Int	n_segreads = 0;
#endif

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
 * hash_value:
 *	Return hash cluster value for Prolog system
 */
Int
hash_value(tr)
Trans *tr;
{
	switch (index_type(tr->relation))
	{
	when SIMC_INDEX:
		return(cluster % (Int)(Ns(tr->relation)));
	when DSIMC_INDEX:
		return(segmentOfHash(tr->relation, tr->cluster, 0));
	}
	return(-1);
}

/*
 * trans_assert:
 *	Add a new record to the database
 */
Int
trans_assert(db_name, fact)
String db_name;
String fact;
{
	r_Trans	*tr;
	r_Int	nmatches;

	if ((tr = trans_open(db_name, fact, opASSERT, 0)) == TransNULL)
		return(-1);
	nmatches = rec_insert(tr);
	trans_close(tr);
	return(nmatches == 1 ? 1 : -1);
}

/*
 * trans_retract:
 *	Delete record(s) matching input query from the database
 *	This has a serious BUG in that it does not check for false
 *	matches before deleting "matching" records ... too hard in C
 */
Int
trans_retract(db_name, query)
String db_name;
String query;
{
	r_Trans	*tr;
	r_Int	nmatches = 0;

	if ((tr = trans_open(db_name, query, opRETRACT, 0)) == TransNULL)
		return(-1);
	while (trans_fetch(tr)) {
		/* BUG: should do unification before deleting */
		rec_delete(tr);
		nmatches++;
	}
	trans_close(tr);
	return(nmatches);
}

/*
 * trans_query:
 *	Print the values of all "matching" records in the database
 *	Doesn't check for false matches, so may print spurious answers
 */
Int
trans_query(db_name, query)
String db_name;
String query;
{
	r_Trans	*tr;
	r_Int	nmatches = 0;

	if ((tr = trans_open(db_name, query, opQUERY, 0)) == TransNULL)
		return(-1);
	while (trans_fetch(tr) != StrNULL) {
		printf("%s\n", tr->rec_buf);
		nmatches++;
	}
	trans_close(tr); tr = TransNULL;
	return(nmatches);
}

/*
 * trans_open:
 *	Commence a transaction on a relation in a named database
 */
Trans *
trans_open(db_name, query, operation, start_level)
String db_name;
String query;
Opn operation;
Int start_level;
{
	r_Trans	*tr;
	r_Reln	*rel;
	r_Char	*buf;
	Word	stars, max_mask;
	r_Int	i, ncombs, nbytes;
	Char	reln_name[MAXRNAME];

	/*
	 * Make ourselves a transaction descriptor
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"new_trans:");
#endif /* DBUG */
	if ((tr = mknew(Trans)) == TransNULL)
	{
		error("no_mem_for_transaction");
		return(TransNULL);
	}
	/*
	 * Initialise fields in transaction descriptor
	 */
	tr->operation = operation;
	tr->database = StrNULL;
	tr->input = StrNULL;
	tr->query = ElemNULL;
	tr->relation = RelnNULL;

	tr->nsegbits = 0;
	tr->seg_ratio = 0.0;
	tr->seg_list = IntNULL;
	tr->seg_matches = WordNULL;
	tr->cur_seg = -1;

	tr->cur_level = start_level;
	tr->cluster = tr->clustars = 0;
	tr->seg_off = -1;
	tr->segment = SegtNULL;

	for (i = 0; i < MAXLEVELS; i++)
		tr->ov_matches[i] = WordNULL;

	tr->nrecbits = 0;
	tr->rec_ratio = 0.0;
	tr->rec_list = IntNULL;
	tr->rec_matches = WordNULL;
	tr->cur_rec = -1;
	tr->rec_buf = StrNULL;

	tr->nsegs_fetched = tr->nrecs_fetched = 0;
	tr->nblocks_fetched = tr->nsdescs_anded = 0;
	tr->n_segreads = tr->n_segopens = 0;

	/*
	 * Determine which relation this is and open it
	 */
	if ((buf = (Char *)index(query,'(')) == StrNULL) {
		error("Strange looking query\n");
		goto TransFail;
	}
	else {
		r_Char	*b = reln_name;
		r_Char	*c = query;

		while (c < buf) *b++ = *c++;
		*b = '\0';
	}

	/*
	 * Allocate space for database name and make a copy of it
	 */
	if ((tr->database = str_copy(db_name)) == StrNULL) {
		error("Not enough memory for transaction\n");
		goto TransFail;
	}

	/*
	 * Allocate space for input string and make a copy of it
	 */
	if ((tr->input = str_copy(query)) == StrNULL) {
		error("Not enough memory for transaction\n");
		free((char *)tr->database);
		goto TransFail;
	}

	/*
	 * Open a relation descriptor for this transaction
	 */
	tr->relation = rel = rel_open(db_name, reln_name, operation);
	if (rel == RelnNULL) {
		error("Can't open relation");
		goto TransFail;
	}

	/*
	 * Allocate space for lists of set record & segment bits
	 */
#ifdef TODO
	Change these lists to arrays of shorts
#endif /* TODO */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"seg_list:");
#endif /* DBUG */
	nbytes = (numkeys(rel)*Ks(rel)+Mr(rel)) * sizeof(Int);
	if ((tr->seg_list = (Int *)malloc((unsigned)nbytes)) == IntNULL) {
		error("no_mem_for_seg_list");
		goto TransFail;
	}

#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"rec_list:");
#endif /* DBUG */
	nbytes = (numkeys(rel)*Kr(rel)+Mr(rel)) * sizeof(Int);
	if ((tr->rec_list = (Int *)malloc((unsigned)nbytes)) == IntNULL) {
		error("no_mem_for_rec_list");
		goto TransFail;
	}

	/*
	 * Allocate space for fetched-record buffer
	 * Allow for records five times as large as the average
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"fac_str:");
#endif /* DBUG */
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
		for (i = 0; i < MAXLEVELS; i++) {
			if (Nls(rel,i) == 0) break;
			tr->ov_matches[i] = b_make(Nls(rel,i));
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
#if 0
fprintf(stderr,"hash:%08x star:%08x\n",cluster,clustars);
#endif


#ifdef DBUG
	debug(VDESC_DBUG) trans_print("new transaction:",tr);
#endif /* DBUG */
	/*
	 * For operations which require searching (query & delete),
	 * set up matching segments bit string, and fetch first segment
	 */
	if (!isassert(operation)) {
		switch (index_type(rel))
		{
		when SIMC_INDEX:
			setup_simc_seg_matches(tr);
		when DSIMC_INDEX:
			setup_dsimc_seg_matches(tr);
			tr->cur_seg = b_nextset(0, tr->ov_matches[0], Nls(rel,0));
			if (tr->cur_seg < 0)
				{b_zero(tr->seg_matches, segs_per_block(rel));}
			else
				setup_block_seg_matches(tr);
		}
		if (next_seg(tr) >= 0)
			seg_fetch(tr);
#ifdef FUNNY
		rel_close(rel);
#endif /* FUNNY */
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
 * trans_sfbquery:
 *	Commence an SFB query transaction on an open relation
 */
Trans *
trans_sfbquery(poole, query, opn, sfbvec)
Pool *poole;
String query;
Int opn;
String sfbvec;
{
	r_Trans	*tr;
	r_Reln	*rel;
	Word	stars, max_mask;
	r_Int	i, nbytes;

	/*
	 * Make ourselves a transaction descriptor
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"new_trans:");
#endif /* DBUG */
	if ((tr = mknew(Trans)) == TransNULL)
	{
		error("no_mem_for_transaction");
		return(TransNULL);
	}
	/*
	 * Initialise fields in transaction descriptor
	 */
	tr->operation = opn;
	tr->database = StrNULL;
	tr->input = StrNULL;
	tr->query = ElemNULL;
	tr->relation = RelnNULL;

	tr->nsegbits = 0;
	tr->seg_ratio = 0.0;
	tr->seg_list = IntNULL;
	tr->seg_matches = WordNULL;
	tr->cur_seg = -1;

	tr->cur_level = 0;
	tr->cluster = tr->clustars = 0;
	tr->seg_off = -1;
	tr->segment = SegtNULL;

	for (i = 0; i < MAXLEVELS; i++)
		tr->ov_matches[i] = WordNULL;

	tr->nrecbits = 0;
	tr->rec_ratio = 0.0;
	tr->rec_list = IntNULL;
	tr->rec_matches = WordNULL;
	tr->cur_rec = -1;
	tr->rec_buf = StrNULL;

	tr->nsegs_fetched = tr->nrecs_fetched = 0;
	tr->nblocks_fetched = tr->nsdescs_anded = 0;
	tr->n_segreads = tr->n_segopens = 0;

	/*
	 * Allocate space for database name and make a copy of it
	 */
	if ((tr->database = str_copy(poole->database)) == StrNULL) {
		error("Not enough memory for transaction\n");
		goto TransFail;
	}

	/*
	 * Make a copy of the input query string
	 */
	if ((tr->input = str_copy(query)) == StrNULL) {
		error("no_mem_to_store_query");
		goto TransFail;
	}

	/*
	 * Use the information in the buffer pool
	 */
	tr->buffers = poole;
	tr->relation = rel = poole->relation;

	/*
	 * Allocate space for lists of set record & segment bits
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"seg_list:");
#endif /* DBUG */
	nbytes = (numkeys(rel)*Ks(rel)+Mr(rel)) * sizeof(Int);
	if ((tr->seg_list = (Int *)malloc((unsigned)nbytes)) == IntNULL) {
		error("no_mem_for_seg_list");
		goto TransFail;
	}

#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"rec_list:");
#endif /* DBUG */
	nbytes = (numkeys(rel)*Kr(rel)+Mr(rel)) * sizeof(Int);
	if ((tr->rec_list = (Int *)malloc((unsigned)nbytes)) == IntNULL) {
		error("no_mem_for_rec_list");
		goto TransFail;
	}

	/*
	 * Allocate space for fetched-record buffer
	 * Allow for records five times as large as the average
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"fac_str:");
#endif /* DBUG */
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
	if ((tr->seg_matches = b_make(segs_per_block(rel))) == WordNULL) {
		error("no_mem_for_seg_matches");
		goto TransFail;
	}
	for (i = 0; i < MAXLEVELS; i++) {
		if (rel->rd_nlevsegs[i] == 0) break;
		tr->ov_matches[i] = b_make(rel->rd_nlevsegs[i]);
		if (tr->ov_matches[i] == WordNULL) {
			error("no_mem_for_ov_seg_matches");
			goto TransFail;
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

	/*
	 * Modify cluster/clustars according to sfbvec
	 */
	if (sfbvec != NULL) {
		Int i;
		Char *c;
		Word desc, stars;

		/*
		 * scan 31 *usable* bits to determine segments
		 */
		desc = stars = 0;
		for (c = sfbvec, i = 0; i < 31; c++, i++)
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
			case '\0':
				stars |= 1;
				c--;
				break;
			case '0':
			default:
				/* ignore 0's */
				/* treat any other junk as 0's */
				break;
			}
		}
#if 0
fprintf(stderr,"clus:%08x clar:%08x\n", tr->cluster, tr->clustars);
fprintf(stderr,"desc:%08x star:%08x\n", desc, stars);
#endif
		tr->cluster = (tr->cluster & stars) | desc;
		tr->clustars &= stars;
#if 0
fprintf(stderr,"clus:%08x clar:%08x\n", tr->cluster, tr->clustars);
#endif
	}

#ifdef DBUG
	debug(VDESC_DBUG) trans_print("new transaction:",tr);
#endif /* DBUG */
	setup_dsimc_seg_matches(tr);
	tr->cur_seg = b_nextset(0, tr->ov_matches[0], Nls(rel,0));
	if (tr->cur_seg < 0)
		{b_zero(tr->seg_matches, segs_per_block(rel));}
	else
		setup_block_seg_matches(tr);
	if (next_seg(tr) >= 0)
		seg_fetch(tr);

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
	if (tr->input == NULL)
		return;
#ifdef DBSTATS
	n_segopens += tr->n_segopens; n_segreads += tr->n_segreads;
	fprintf(stdout,"query:%s nsegopens:%d nsegreads:%d\n",
			tr->input, tr->n_segopens, tr->n_segreads);
	fprintf(stdout,"totsegopens:%d totsegreads:%d\n", n_segopens, n_segreads);
#endif
	cfree(tr->database, Char);
	cfree(tr->input, Char);
	skel_free(tr->query);
XX	tr->query = ElemNULL;
	if (!isassert(tr->operation))
		rel_close(tr->relation);
XX	tr->relation = RelnNULL;

XX	cfree(tr->seg_list, Int);
XX	cfree(tr->seg_matches, Word);
XX	for (i = 0; i < MAXLEVELS; i++)
XX		cfree(tr->ov_matches[i],Word);
	seg_close(tr->segment);
XX	tr->segment = SegtNULL;

XX	cfree(tr->rec_list, Int);
XX	cfree(tr->rec_matches, Word);
XX	cfree(tr->rec_buf, Char);

XX	cfree(tr, Trans);
}

/*
 * trans_fetch:
 *	fetch the next matching record from the database
 */
String
trans_fetch(tr)
Trans *tr;
{
#ifdef DBUG_OFF
	debug(GET_DBUG)
		fprintf(stderr,"tr_fetch:cur_seg:%d:cur_rec:%d\n",
					tr->cur_seg,tr->cur_rec);
#endif /* DBUG */
GetNextRec:
	if (tr->cur_seg < 0)
		return(StrNULL); /* end of query */
	if (next_rec(tr) < 0)
	{
		/*
		 * If no more matches in this segment, try next
		 */
	GetNextSeg:
#ifdef DBUG
		debug(GET_DBUG) fprintf(stderr,"end_seg:%d\n",tr->cur_seg);
#endif /* DBUG */
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
#ifdef DBUG
		debug(GET_DBUG) fprintf(stderr,"end_seg:%d\n",tr->cur_seg);
#endif /* DBUG */
	}
	if (rec_fetch(tr) < 0)
		goto GetNextRec;
	if (match(tr->query, tr->rec_buf))
		return(tr->rec_buf);
	else
		goto GetNextRec;
}


/*
 * pool_open:
 *	Allocate a pool of buffers and open relation for an SFB query
 */
Pool *
pool_open(db, rel, arity)
char *db, *rel;
int arity;
{
	r_Pool	*buf;

	if ((buf = mknew(Pool)) == PoolNULL) {
		error("No memory for buffer pool");
		return(PoolNULL);
	}

	buf->database = str_copy(db);
	if (buf->database == StrNULL) {
		error("Can't store db name for buffer pool");
		free((char *)buf);
		return(PoolNULL);
	}

	buf->relation = rel_open(db, rel, /*arity,*/ opQUERY);
	if (buf->relation == RelnNULL) {
		error("Can't open relation for buffer pool");
		free((char *)buf->database);
		free((char *)buf);
		return(PoolNULL);
	}

	return(buf);
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

#endif /* DBUG */
