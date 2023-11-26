#include "rel.h"

bsplit(rel, level)
REL *rel;
int level;
{
	register LEVEL *lptr;
	register npage, spage, i;
	INDEX index;
	BUCKET *bucket;
	static BUCKET spbkt, newbkt;

	lptr = rel->lev[level];
	spage = lptr->sp;
	if (spage == 0)
		lptr->depth++;
	npage = newpage(rel, level);
	bucket = bget(rel, level, spage);
	dnew(rel, lptr->dtable[spage]);
	spbkt.nfacts = newbkt.nfacts = 0;
	spbkt.nbytes = newbkt.nbytes = 0;
	for (i = 0; i < bucket->nfacts; i++) {
		index = hash(rel, &bucket->fact[i]);
		if (index & (01 << (lptr->depth - 1))) {
			/* shift fact to npage */

			newbkt.nbytes += factcopy(&bucket->fact[i],
				&newbkt.fact[newbkt.nfacts++],
				newbkt.bbuf + newbkt.nbytes);	
			dadd(rel, &bucket->fact[i], lptr->dtable[npage]);
		} else {
			/* leave fact in spage */

			spbkt.nbytes += factcopy(&bucket->fact[i],
				&spbkt.fact[spbkt.nfacts++],
				spbkt.bbuf + spbkt.nbytes);	
			dadd(rel, &bucket->fact[i], lptr->dtable[spage]);
		}
	}
	dput(rel, lptr->dtable[spage]);
	dput(rel, lptr->dtable[npage]);
	bput( rel, level, spage, &spbkt);
	bput( rel, level, npage, &newbkt);
	lptr->sp = (spage + 1) % (01 << (lptr->depth - 1));
}

newpage(rel, level)
REL *rel;
int level;
{
	register LEVEL *lptr;
	register DESC *dptr;

	lptr = rel->lev[level];
	dptr = dnew(rel, (DESC *) NULL);
	dptr->level = level;
	if ((dptr->page = lptr->nbucket++) >= MAXPAGE) {
		rerror("maxpage");
		rclose(rel);
		exit(1);
	}
	dptr->bkt = rel->hd.nbuckets++;
	lptr->dtable[dptr->page] = dptr;
	rhdput(rel);
	return(dptr->page);
}

