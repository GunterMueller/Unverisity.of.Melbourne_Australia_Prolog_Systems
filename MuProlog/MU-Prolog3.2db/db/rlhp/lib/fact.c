#include "fact.h"

factmatch(facta, factb)
FACT *facta;	/* may be ground */
FACT *factb;	/* must be ground */
{
	register i,j;

	for (i = 0; i < facta->nkeys; i++) {
		if (ground(facta, i)) {
			/* non-variable in facta */
			if (strcmp(facta->field[i], factb->field[i]) != 0) {
				/* fail - field in facta and factb not same */
				return(0);
			}
		} else {
			/* variable in facta */
			if ((facta->field[i][0] == '_') && (facta->fsz[i] == 2))
				/* succeed - underscore */
				continue;
			for (j = i+1; j < facta->nkeys; j++) {
				if ((strcmp(facta->field[i], facta->field[j]) == 0) &&
				    (strcmp(factb->field[i], factb->field[j]) != 0)) {
					/* fail - same variable in facta but
						fields in factb are different */
					return(0);
				}
			}
		}
	}
	/* succeed */
	return(1);
}


factcopy(facta, factb, factbbuf)
FACT *facta;
FACT *factb;
char factbbuf[];
{
	register i;
	register char *b;

	factb->fname = facta->fname;
	factb->nkeys = facta->nkeys;
	b = factbbuf;
	for (i = 0; i < facta->nkeys; i++) {
		factb->field[i] = b;
		strcpy(factb->field[i], facta->field[i]);
		b += factb->fsz[i] = facta->fsz[i];
	}
	return(b - factbbuf);	/* number of bytes copied */
}
