#include "rel.h"

DESC *dnew(rel, desc)
REL *rel;
DESC *desc;
{
	register i, dwords;

	if (desc == NULL) {
		if ((desc = (DESC *) malloc (rel->hd.dsize)) == NULL) {
			rerror("memory");
			rclose(rel);
			exit(1);
		}
		desc->level = -1;
		desc->page = -1;
		desc->bkt = -1;
	}
	dwords = (rel->hd.dsize - DHDSZ) / sizeof (long);
	for (i = 0; i < dwords; i++) {
		desc->bits[i] = 0;
	}
	desc->nrec = 0;
	return(desc);
}


dput(rel, desc)
REL *rel;
DESC *desc;
{
#ifdef STATS
	rel->n_dput++;
#endif
	fseek(rel->descf,
		(long) (desc->bkt * rel->hd.dsize + sizeof (struct header)), 0);
	fwrite(desc, rel->hd.dsize, 1, rel->descf);
	fflush(rel->descf);
}

dadd(rel, fact, desc)
REL *rel;
FACT *fact;
DESC *desc;
{
	register i;
	int bpos;

	/* contruct descriptor for fact and "or" into desc */
	for (i = 0; i < rel->hd.nkeys; i++) {
		if (ground(fact, i) && rel->hd.dh[i] > 0) {
/*
			bpos = (value(fact, i) % rel->hd.dh[i]) + rel->hd.d[i];
*/
			bpos = ((value(fact, i) * 4999L & 0x7fffffff) % rel->hd.dh[i]) + rel->hd.d[i];
					/* yes, pcc, we really DO want a long */
			desc->bits[bpos/DWBITS] |= (long)01L << (bpos % DWBITS);
		}
	}
	desc->nrec++;
}

dmatch(rel, desc1, desc2)
REL *rel;
DESC *desc1, *desc2;
{
	register i;
	int dwords;

	/* calculate if bits in desc1 are a subset of bits in desc2 */

	dwords = (rel->hd.dsize - DHDSZ) / sizeof (long);
	for (i = 0; i < dwords; i++) {
		if ((desc1->bits[i] & desc2->bits[i]) != desc1->bits[i])
			return(0);
	}
	return(1);
}
