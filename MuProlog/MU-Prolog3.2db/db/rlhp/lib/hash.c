#include "rel.h"
#include "stdio.h"

INDEX hash(rel, fact)
REL *rel;
FACT *fact;
{
	int i;
	INDEX index,x,y,z;

	index = 0;
	for (i = 0; i < rel->hd.nkeys; i++) {
		if (ground(fact, i)) {
			/* its not a variable */

			z = value(fact, i);
			for (x = rel->hd.cv[i], y = 0; x != 0; x >>= 1, y++)
				if (x % 2) {
					index |= (z % 2) << y;
					z >>= 1;
				}
		}
	}
	return(index);
}


page_index(rel, level, index)
REL *rel;
int level;
INDEX index;
{
	index &= ((01 << rel->lev[level]->depth) - 1);
	if (index >= rel->lev[level]->nbucket)
		index &= ((01 << (rel->lev[level]->depth - 1)) - 1);
	return((int) index);
}

long value(fact, k)
FACT *fact;
int k;		/* field number */
{
	register rot;
	register char *i;
	register val, j;

	val = 0;
	rot = 0;
		/* this spreads the bytes of the key over 16 bits */
	for (i = fact->field[k]; *i != '\0'; i++) {
		/* spread bits around for each byte too */
		j = *i*0x8e53;
		val ^= (j << (rot)) | (j >> (16 - rot));
		rot = (rot + 9) % 16;
	}
	return val & 0x7fffffff;	/* mask out sign bit */

	/* bottom 16 bits are best */
}
