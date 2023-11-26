#include "rel.h"

BUCKET *bget(rel, level, page)
REL *rel;
int level;
int page;
{
	static BUCKET bucket;
	register char *b;
	register i, j, index;
	DESC *dptr;

#ifdef STATS
	rel->n_bget++;
#endif
	dptr = rel->lev[level]->dtable[page];
	index = dptr->bkt;
	fseek(rel->mainf, (long)index * rel->hd.bsize, 0);
	fread(bucket.bbuf, rel->hd.bsize, 1, rel->mainf);
	b = bucket.bbuf;
	for (i = 0; i < dptr->nrec; i++) {
		bucket.fact[i].fname = rel->hd.name;
		bucket.fact[i].nkeys = rel->hd.nkeys;
		for (j = 0; j < rel->hd.nkeys; j++) {
			bucket.fact[i].field[j] = b;
			b += bucket.fact[i].fsz[j] = strlen(b) + 1;
		}
	}
	bucket.nfacts = i;
	bucket.nbytes = b - bucket.bbuf;
	return(&bucket);
}
