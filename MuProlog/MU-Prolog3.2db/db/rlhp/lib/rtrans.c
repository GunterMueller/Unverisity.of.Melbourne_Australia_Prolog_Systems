#include "rel.h"

rtrans(rel, fact, op)
REL *rel;
FACT *fact;
OP op;
{
	register i;
	int level;
	DESC desc;
	INDEX qy_index;
	INDEX index_mask = 0;
	int unknown;
	INDEX next, index;
	INDEX topbit, bits[INDEXBITS];

	if (wrong(rel, fact)) {
		rerror("wrong_relation");
		return;
	}
	dnew(rel, &desc);
	dadd(rel, fact, &desc);
	qy_index = hash(rel, fact);
	for (i = 0; i < rel->hd.nkeys; i++)
		if (!ground(fact, i))
			index_mask |= rel->hd.cv[i];
	if (op & F_DELETE) {
#ifdef LOCK_EX
		flock(fileno(rel->descf), LOCK_EX);
		flock(fileno(rel->mainf), LOCK_EX);
#endif
	}
	for (level = 0; level < rel->hd.nlevels; level++) {
		if (rel->lev[level]->depth == 0) {
			/* only one bucket at this level */
			if (transaction(rel, level, 0, fact, &desc, op) && (op & F_ONE)) {
#ifdef LOCK_EX
				flock(fileno(rel->mainf), LOCK_SH);
				flock(fileno(rel->descf), LOCK_SH);
#endif
				return;
			}
		} else {
			topbit = 01 << (rel->lev[level]->depth - 1);
			index = index_mask & ((01 << (rel->lev[level]->depth - 1)) - 1);
			unknown = 0;
			for (i = 0; i < INDEXBITS; i++)
				if (((index >> i) & 01) == 01) {
					bits[unknown++] = 01 << i;
				}
			for (next = 0; next < (01 << unknown); next++) {
				index = qy_index & ((01 << (rel->lev[level]->depth - 1)) - 1);
				for (i = 0; i < unknown; i++)
					if ((next >> i) & 01)
						index |= bits[i];
				if ((index < rel->lev[level]->sp) || (rel->lev[level]->sp == 0)) {
					/* bucket has been split */

					if (index_mask & topbit) {
						/* highest bit unknown search both buckets */
						if (transaction(rel, level, index, fact, &desc, op) && (op & F_ONE)) {
#ifdef LOCK_EX
							flock(fileno(rel->mainf), LOCK_SH);
							flock(fileno(rel->descf), LOCK_SH);
#endif
							return;
						}
						index |= topbit;
						if (transaction(rel, level, index, fact, &desc, op) && (op & F_ONE)) {
#ifdef LOCK_EX
							flock(fileno(rel->mainf), LOCK_SH);
							flock(fileno(rel->descf), LOCK_SH);
#endif
							return;
						}
					} else {
						/* highest bit known search one bucket only */

						index |= qy_index & topbit;
						if (transaction(rel, level, index, fact, &desc, op) && (op & F_ONE)) {
#ifdef LOCK_EX
							flock(fileno(rel->mainf), LOCK_SH);
							flock(fileno(rel->descf), LOCK_SH);
#endif
							return;
						}
					}
				} else {
					/* bucket not yet split */

					if (transaction(rel, level, index, fact, &desc, op) && (op & F_ONE)) {
#ifdef LOCK_EX
							flock(fileno(rel->mainf), LOCK_SH);
							flock(fileno(rel->descf), LOCK_SH);
#endif
						return;
					}
				}
			}
		}
	}
#ifdef LOCK_EX
	flock(fileno(rel->mainf), LOCK_SH);
	flock(fileno(rel->descf), LOCK_SH);
#endif
	
}


transaction(rel, level, index, fact, desc, op)
REL *rel;
int level;
INDEX index;
FACT *fact;
DESC *desc;
OP op;
{
	register i;
	DESC *dptr;
	BUCKET *bucket;

	i = 0;
	dptr = rel->lev[level]->dtable[(int) index];
	if (dmatch(rel, desc, dptr)) {
		bucket = bget(rel, level, (int) index);
		if (((i = bmatch(rel, bucket, fact, op)) > 0) && (op & F_DELETE)) {
			rel->lev[level]->nrec -= i;
			bdesc(rel, bucket, dptr);	/* reconstruct descriptor for that bucket */
			dput(rel, dptr);
			bput(rel, level, (int) index, bucket);
		}
	}
	return(i);
}
