#include "lib/rel.h"

main(argc, argv)
int argc;
char *argv[];
{
	REL *rel;
	int dsz[MAXKEYS];
	int cvs[MAXKEYS];
	int nkeys, lc, bsz, nbkts, i;

	if (argc < 6) {
		fprintf(stderr,
		"Usage: newrel db rel nkeys lc bsz dsz ... cvs ... [nbkts]\n");
		exit(1);
	}
	nkeys = atoi(argv[3]);
	lc = atoi(argv[4]);
	bsz = atoi(argv[5]);
	if ((argc != (6 + 2 * nkeys)) && (argc != (7 + 2 * nkeys))) {
		fprintf(stderr,
		"Usage: newrel db rel nkeys lc bsz dsz ... cvs ... [nbkts]\n");
		exit(1);
	}
	for (i = 0; i < nkeys; i++) {
		dsz[i] = atoi(argv[i + 6]);
		cvs[i] = atoi(argv[i + nkeys + 6]);
	}
	if (argc == (7 + 2 * nkeys)) {
		nbkts = atoi(argv[6 + 2 * nkeys]);
	} else {
		nbkts = 1;
	}

	/* should really check dsz and cvs but cant be bothered */

	if ((rel = rcreat(argv[1], argv[2], nkeys, lc, bsz, dsz, cvs, nbkts)) == NULL){
		fprintf(stderr, "Cant open %s/%s\n", argv[1], argv[2]);
		exit(1);
	}
	rclose(rel);
}
