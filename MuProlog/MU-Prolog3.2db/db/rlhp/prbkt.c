#include <stdio.h>
#include "lib/rel.h"

main(argc, argv)
int argc;
char *argv[];
{
	REL *rel;
	static FACT ungrnd;
	register i;
	register key;
	int level, page;
	int rflag;
	DESC *desc;
	BUCKET *bucket;

	if ((argc == 6) && (strcmp(argv[1], "-r") == 0)) {
		argc--;
		argv++;
		rflag++;
	}
	if (argc != 5) {
		fprintf(stderr, "Usage: prbkt [-r] db rel level page\n");
		exit(1);
	}
	if ((rel = ropen(argv[1], argv[2])) == NULL) {
		fprintf(stderr, "Cant open %s/%s\n", argv[1], argv[2]);
		exit(1);
	}
	level = atoi(argv[3]);
	page = atoi(argv[4]);
	if (page < 0 || level < 0) {
		fprintf(stderr, "illegal (negative) page or level number\n");
		exit(1);
	}
	if (level >= rel->hd.nlevels) {
		fprintf(stderr, "no level %d\n", level);
		exit(1);
	}
	if (page >= rel->lev[level]->nbucket) {
		fprintf(stderr, "no page %d on level %d\n", page, level);
		exit(1);
	}
	

	desc = rel->lev[level]->dtable[page];
	printf("descriptor contents\n");
	printf("\tlevel: %d\n",
		desc->level);
	printf("\tpage: %d\n",
		desc->page);
	printf("\tactual bucket no.: %d\n",
		desc->bkt);
	printf("\tno. of records: %d\n",
		desc->nrec);
	printf("\tdescriptor:");
	for (key = 0; key < rel->hd.nkeys; key++) {
		printf("\n\tkey[%d]", key);
		for (i = rel->hd.d[key]; i < rel->hd.d[key] + rel->hd.dh[key];
			i++) {

			if ((i - rel->hd.d[key]) % 64 == 0)
				printf("\n\t");
			if ((i - rel->hd.d[key]) % 8 == 0)
				printf(" ");
			printf("%ld",
				(((desc->bits[i/DWBITS]) >> (i%DWBITS)) & 01L));
		}
	}

	bucket = bget(rel, level, page);

	printf("\nbucket contents\n");
	printf("\tno. of facts: %d\n", bucket->nfacts);
	printf("\tno. of bytes: %d\n\n", bucket->nbytes);
	
	if (rflag) {
		ungrnd.fname = rel->hd.name;
		ungrnd.nkeys = rel->hd.nkeys;
		for (i = 0; i < ungrnd.nkeys; i++) {
			ungrnd.field[i] = "_";
			ungrnd.fsz[i] = 2;
		}

		bmatch(rel, bucket, &ungrnd, F_PRINT | F_ALL);
	}
	rclose(rel);
}
