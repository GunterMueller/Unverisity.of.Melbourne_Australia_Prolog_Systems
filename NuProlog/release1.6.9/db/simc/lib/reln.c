/*
 * reln.c  -  deductive database package (relation operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

/*
 * rel_template:
 *	Retrieve a copy of the query template with SFB vectors
 *
 *	e.g. "p(a,f(X))"  =>  "p(a,f([0,1,_,_,0])")
 */
rel_template(poole, query, template)
Pool *poole;
String query;
Char *template;
{
	r_Reln *rel;
	r_Char *t, *tt;
	Char *buf_ptr;
	Char temp_buf[BUFSIZ];

	rel = poole->relation;
	buf_ptr = temp_buf;

	queryToVec(rel, query, &buf_ptr);
	*buf_ptr = ChNULL;

	for (t = template, tt = temp_buf; *tt != ChNULL; )
		*t++ = *tt++;
	*t = ChNULL;
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
open_rel_file(rel,operation)
Reln *rel;
Opn operation;
{
	char	buf[MAXPATH+MAXRNAME+10];
	void	release_rel_file();

	/*
	 * If the last operation on this relation wasn't a write,
	 * and this one is, then close and re-open to get write permission
	 */
#if 0
fprintf(stderr,"descf:%08x relop:%d newop:%d\n",rel->rd_descf,rel->rd_operation,operation);
#endif
	if (rel->rd_descf != FileNULL &&
		!iswriting(rel->rd_operation) && iswriting(operation)) {
		close(rel->rd_descf);
		n_rel_files--;
		rel->rd_descf = FileNULL;
	}
	if (rel->rd_descf == FileNULL) {
		sprintf(buf, "%s/desc", rel->rd_name);
		if (n_rel_files >= MAXRINFFILES)
			release_rel_file();
		if (iswriting(operation)) {
#if 0
fprintf(stderr,"open %s for read/write\n",buf);
#endif
			if ((rel->rd_descf = open(buf, O_RDWR, 0644)) < 0)
				return(FALSE);
		}
		else {
#if 0
fprintf(stderr,"open %s for read only\n",buf);
#endif
			if ((rel->rd_descf = open(buf, O_RDONLY, 0644)) < 0)
				return(FALSE);
		}
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
#endif /* DBUG */
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
	Int	nbytes;
	Char	db_rel[MAXPATH+MAXRNAME+2];

	sprintf(db_rel, "%s/%s", db_name, rel_name);
	nbytes = strlen(db_rel);

	/*
	 * If search ok, use existing descriptor
	 */
	if (search(db_rel, -nbytes, STRING_MODE, (Hash**)reln_table,
			MAXRHASH, (Hash***)(&ptr), (Hash**)(&info))) {
		if (!open_rel_file(info, operation))
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
#endif /* DBUG */
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
	info->rd_length = -nbytes;
	if ((info->rd_name = mkstr(nbytes)) == StrNULL) {
		error("no_mem_for_name_buf");
		goto RelOpenFail;
	}
	strcpy(info->rd_name, db_rel);

	/*
	 * Fill in most of static info from descriptor file
	 */
	if (!open_rel_file(info, operation)) {
		error("open_rel_file");
		goto RelOpenFail;
	}
	nbytes = info_bytes(info);
	if (read(info->rd_descf, static_info(info), nbytes) != nbytes) {
		error("rel_header");
		goto RelOpenFail;
	}

	/*
	 * Make template tree
	 */
#ifdef TODO
	Would be nice if the TREE, rather than template string, was stored
	Or, at the very least, a Normalised template string
#endif /* TODO */
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
	if (flock(info->rd_descf, lock_type) < 0)
		fatal("seg_desc_lock_failed");
#endif
#ifdef DBUG
	debug(DESC_DBUG) rel_print("Open Reln:",info);
#endif /* DBUG */
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
	if (--rel->rd_nopens == 0)
		flock(rel->rd_descf, LOCK_UN);
#else
	rel->rd_nopens--;
#endif
#ifdef DBUG
	debug(DESC_DBUG) rel_print("Close Reln:", rel);
#endif /* DBUG */
}

/*
 * rel_info_update:
 *	Update the "static" information for an open relation
 */
Bool
rel_info_update(rel)
Reln *rel;
{
	r_Int	nbytes;

	/*
	 * Make sure relation descriptor file is still open
	 * Ensures that descriptor file is open for writing
	 */
	if (!open_rel_file(rel, opASSERT)) {
		error("Can't open relation file");
		return(-1);
	}

	/*
	 * Write static relation info to the descriptor file,
	 */
	nbytes = info_bytes(rel);
	lseek(descfile(rel), (Faddr)0, L_SET);
	if (write(descfile(rel), static_info(rel), nbytes) != nbytes) {
		error("Relation info update failed");
		return(FALSE);
	}
	return(TRUE);
}

/*
 * inc_counter:
 *	Increment value of counter for number of records in segment
 */
inc_counter(rel, segaddr)
Reln *rel;
Int segaddr;
{
	Faddr	off;
	Int	nrecs;

	/*
	 * Compute offset in relation descriptor file
	 * for the counter for this segment
	 */
	off = coffset(rel) + segaddr*sizeof(Int);

	/*
	 * Poke the count into the correct spot
	 */
	lseek(descfile(rel), off, L_SET);
	read(descfile(rel), (char *)&nrecs, sizeof(Int));
	nrecs++;
	lseek(descfile(rel), off, L_SET);
	write(descfile(rel), (char *)&nrecs, sizeof(Int));
}

/*
 * set_counter:
 *	Set value of counter for number of records in segment
 *	Returns value of counter before it was modified
 */
Int
set_counter(rel, segaddr, nrecs)
Reln *rel;
Int segaddr;
Int nrecs;
{
	Faddr	off;
	Int	orecs;

	/*
	 * Compute offset in relation descriptor file
	 * for the counter for this segment
	 */
	off = coffset(rel) + segaddr*sizeof(Int);

	/*
	 * Poke the count into the correct spot
	 */
	lseek(descfile(rel), off, L_SET);
	read(descfile(rel), (char *)&orecs, sizeof(Int));
	lseek(descfile(rel), off, L_SET);
	write(descfile(rel), (char *)&nrecs, sizeof(Int));

	return(orecs);
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

#endif /* DBUG */
