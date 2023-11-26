#include <stdio.h>
#include "lib/rel.h"

main(argc, argv)
int argc;
char *argv[];
{
	REL *rel;
	register i;

	if (argc != 3) {
		fprintf(stderr, "Usage: prdd db rel\n");
		exit(1);
	}
	if ((rel = ropen(argv[1], argv[2])) == NULL) {
		fprintf(stderr, "Cant open %s/%s\n", argv[1], argv[2]);
		exit(1);
	}

 	printf("MUDD Version: %d.%d%c\n",
 		rel->hd.version/1000,
 		(rel->hd.version%1000)/100,
 		rel->hd.version%100 ? 'a' - 1 + rel->hd.version%100 : ' ');
	printf("relation: %s\n",
		rel->hd.name);
	printf("number of keys: %d\n",
		rel->hd.nkeys);
	printf("bucket size: %d (bytes)\n",
		rel->hd.bsize);
	printf("descriptor size: %d (bits)\n",
		(rel->hd.dsize - DHDSZ) * BBITS);
	printf("number of levels: %d\n",
		rel->hd.nlevels);
	printf("number of buckets: %d\n",
		rel->hd.nbuckets);
	printf("load control (L): %d\n",
		rel->hd.lc);
	printf("KEY\tDBITS\tOFFSETS\tCV\n");
	for (i = 0; i < rel->hd.nkeys; i++) {
		printf("%d\t%d\t%d\t%06o\n",
			i,
			rel->hd.dh[i],
			rel->hd.d[i],
			rel->hd.cv[i]);
	}
	printf("LEVEL\tDEPTH\tSP\tNBKTS\tNREC\n");
	for (i = 0; i < rel->hd.nlevels; i++) {
		printf("%d\t%d\t%d\t%d\t%d\n",
			i,
			rel->lev[i]->depth,
			rel->lev[i]->sp,
			rel->lev[i]->nbucket,
			rel->lev[i]->nrec);
	}
	rclose(rel);
}
