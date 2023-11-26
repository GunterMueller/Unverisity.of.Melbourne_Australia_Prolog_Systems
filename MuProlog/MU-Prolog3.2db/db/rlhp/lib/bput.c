#include "rel.h"

bput(rel, level, page, bucket)
REL *rel;
int level;
int page;
BUCKET *bucket;
{
	register index;

#ifdef STATS
	rel->n_bput++;
#endif
	/* This caught a bug in fseek */
/*	if (bucket->nfacts &&
/*		(page != page_index(rel, level, hash(rel, &bucket->fact[0])))) {
/*fprintf(stderr, "nfacts=%d, page=%d, page_index=%d\n", bucket->nfacts,
/*		page, page_index(rel, level, hash(rel, &bucket->fact[0])));
/*		rerror("glitch");
/*		rclose(rel);
/*		exit(1);
/*	}
*/
	index = rel->lev[level]->dtable[page]->bkt;
	fseek(rel->mainf, (long)index * rel->hd.bsize, 0);
	fwrite(bucket->bbuf, rel->hd.bsize, 1, rel->mainf);
	fflush(rel->mainf);
}
