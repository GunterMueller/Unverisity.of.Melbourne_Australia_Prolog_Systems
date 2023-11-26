
#include "rel.h"

bmatch(rel, bucket, fact, op)
REL *rel;
BUCKET *bucket;
FACT *fact;
OP op;
{
	register i,j;
	int nf, nb;
	int fbytes = 0;
	int r = 0;

	switch (op & F_OP) {
	case F_ADD:
		for (j = 0; j < fact->nkeys; j++)
			fbytes += fact->fsz[j];
		if ((bucket->nbytes + fbytes) > rel->hd.bsize)
			return(0);
		i = bucket->nfacts++;
		factcopy(fact, &bucket->fact[i], bucket->bbuf + bucket->nbytes);
		bucket->nbytes += fbytes;
		if (op & F_SYNC)
			factout(stdout, fact);
		return(1);
	case F_DELETE:
		nf = bucket->nfacts;
		nb = bucket->nbytes;
		for (i = 0; i < nf; i++) {
			if (factmatch(fact, &bucket->fact[i]) &&
					((op & F_ALL) || (r == 0))) {
				/* delete fact */

				if (op & F_ONE)
					factout(stdout, &bucket->fact[i]);
				for (j = 0; j < fact->nkeys; j++)
					bucket->nbytes -= bucket->fact[i].fsz[j];
				bucket->nfacts--;
				r++;
			} else {
				/* keep fact */

				if (r > 0) {
					/* need to compact remaining facts */
					factcopy(&bucket->fact[i],
						&bucket->fact[i-r],
						bucket->fact[i].field[0] - (nb - bucket->nbytes));
				}
			}
		}
		break;
	case F_PRINT:
		for (i = 0; i < bucket->nfacts; i++) {
			if (factmatch(fact, &bucket->fact[i])) {
				factout(stdout, &bucket->fact[i]);
				r++;
				if (op & F_ONE)
					break;
			}
		}
		break;
	default:
		rerror("horrible");
		rclose(rel);
		exit(1);
	}
	return(r);
}
