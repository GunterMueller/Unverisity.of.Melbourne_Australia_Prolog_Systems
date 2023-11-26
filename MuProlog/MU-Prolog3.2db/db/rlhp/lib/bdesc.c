
#include "rel.h"

bdesc(rel, bucket, desc)
REL *rel;
BUCKET *bucket;
DESC *desc;
{
	register i;

	dnew(rel, desc);
	for (i = 0; i < bucket->nfacts; i++)
		dadd(rel, &bucket->fact[i], desc);
}
