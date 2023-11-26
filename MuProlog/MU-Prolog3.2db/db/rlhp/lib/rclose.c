#include "rel.h"

rclose(rel)
REL *rel;
{
	register i, j;

#ifdef STATS
	fprintf(rel->statf, "%8d %8d %8d %8d\n",
		rel->n_bget,rel->n_bput,rel->n_dget,rel->n_dput);
	fclose(rel->statf);
#endif
#ifdef LOCK_EX
	flock(fileno(rel->mainf), LOCK_UN);
	flock(fileno(rel->descf), LOCK_UN);
#endif
	fclose(rel->mainf);
	fclose(rel->descf);
	for (i = 0; i < rel->hd.nlevels; i++) {
		for (j = 0; j < rel->lev[i]->nbucket; j++)
			free(rel->lev[i]->dtable[j]);
		free(rel->lev[i]);
	}
	free(rel);
}
