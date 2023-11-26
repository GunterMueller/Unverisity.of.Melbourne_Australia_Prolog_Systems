#include "rel.h"

rassert(rel, fact, op)
REL *rel;
FACT *fact;
OP *op;
{
	register level, page, i;
	BUCKET *bucket;
	static BUCKET newbkt;
	DESC *dptr;
	INDEX index;

	if (wrong(rel, fact)) {
		rerror("wrong_relation");
		return;
	}
	for (i = 0; i < rel->hd.nkeys; i++)
		if (!ground(fact, i)) {
			rerror("not_ground");
			return;
		}
	index = hash(rel, fact);
#ifdef LOCK_EX
	flock(fileno(rel->descf), LOCK_EX);
	flock(fileno(rel->mainf), LOCK_EX);
#endif
	for (level = 0; level < rel->hd.nlevels; level++) {
		if (rel->lev[level]->nrec >=
			rel->hd.lc * rel->lev[level]->nbucket) {
			bsplit(rel, level);
		}
		page = page_index(rel, level, index);
		dptr = rel->lev[level]->dtable[page];
		bucket = bget(rel, level, page);
		if (bmatch(rel, bucket, fact, op)) {
			dadd(rel, fact, dptr);
			rel->lev[level]->nrec++;
			bput(rel, level, page, bucket);
			dput(rel, dptr);
#ifdef LOCK_EX
			flock(fileno(rel->mainf), LOCK_SH);
			flock(fileno(rel->descf), LOCK_SH);
#endif
			return;
		}
	}

	/* no empty buckets - start a new level - page = 0*/

	level = newlevel(rel);
	dptr = rel->lev[level]->dtable[0];
	dadd(rel, fact, dptr);
	newbkt.nfacts = 0;
	newbkt.nbytes = 0;
	if (bmatch(rel, &newbkt, fact, op) == 0) {
		rerror("bmatch_add");
	} else {
		rel->lev[level]->nrec = 1;
	}
	bput(rel, level, 0, &newbkt);
	dput(rel, dptr);
#ifdef LOCK_EX
	flock(fileno(rel->mainf), LOCK_SH);
	flock(fileno(rel->descf), LOCK_SH);
#endif
}

extern char *malloc();

newlevel(rel)
REL *rel;
{
	int level;
	DESC *desc;

	level = rel->hd.nlevels++;
	if (level >= MAXLEV) {
		rerror("maxlev");
		rclose(rel);
		exit(1);
	}
	if ((rel->lev[level] = (LEVEL *) malloc (sizeof(LEVEL))) == NULL) {
		rerror("memory");
		rclose(rel);
		exit(1);
	}
	rel->lev[level]->depth = 0;
	rel->lev[level]->sp = 0;
	rel->lev[level]->nbucket = 1;
	rel->lev[level]->nrec = 0;
	desc = dnew(rel, (DESC *) NULL);
	rel->lev[level]->dtable[0] = desc;
	desc->level = level;
	desc->page = 0;
	desc->bkt = rel->hd.nbuckets++;
	rhdput(rel);
	return(level);
}
