#include "rel.h"

FILE *fquery(database, fact)
char *database;
FACT *fact;
{
	int apd[2];
	REL *rel;
	

	if (pipe(apd) < 0) {
		return(NULL);
	}
	if (!fork()) {
		close(0);
		close(1);
		dup2(apd[1], 1);
		close(apd[0]);
		close(apd[1]);
		rel = ropen(database, fact->fname);
		rquery(rel, fact);
		rclose(rel);
		exit(0);
	}
	close(apd[1]);
	return(fdopen(apd[0], "r"));
}
