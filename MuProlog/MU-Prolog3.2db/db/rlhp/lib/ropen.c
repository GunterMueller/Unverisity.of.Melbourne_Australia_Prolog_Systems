#include "rel.h"

extern char *malloc();

REL *ropen(database, name)
char *database;
char *name;
{
	register REL *rel;
	char mainfile[MAXPATH], descfile[MAXPATH], statfile[MAXPATH];
	DESC *dptr;
	LEVEL *lptr;
	int i;
	unsigned d;

	if ((rel = (REL *) malloc(sizeof (REL))) == NULL) {
		rerror("memory");
		return(NULL);
	}
	if (strlen(database) + strlen(name)-3 > MAXPATH) {
		rerror("pathlength");
		return(NULL);
	}
	strcpy(mainfile, database);
	strcat(mainfile, "/");
	strcat(mainfile, name);
	if ((rel->mainf = fopen(mainfile, "r+")) == NULL) {
		rerror("open");
		return(NULL);
	}	
	strcpy(descfile, database);
	strcat(descfile, "/.");
	strcat(descfile, name);
	if ((rel->descf = fopen(descfile, "r+")) == NULL) {
		rerror("open");
		fclose(rel->mainf);
		return(NULL);
	}	
#ifdef STATS
	strcpy(statfile, database);
	strcat(statfile, "/_");
	strcat(statfile, name);
	if ((rel->statf = fopen(statfile, "a")) == NULL) {
		fclose(rel->mainf);
		fclose(rel->descf);
		rerror("open");
		return(NULL);
	}	
#endif
#ifdef LOCK_EX
	flock(fileno(rel->descf), LOCK_SH);
	flock(fileno(rel->mainf), LOCK_SH);
#endif
	if (fread(&rel->hd, sizeof(struct header), 1, rel->descf) != 1) {
		rerror("header");
		fclose(rel->mainf);
		fclose(rel->descf);
		return(NULL);
	}
	for (i = 0; i < rel->hd.nlevels; i++) {
		if ((rel->lev[i] = (LEVEL *) malloc (sizeof(LEVEL))) == NULL) {
			rerror("memory");
			fclose(rel->mainf);
			fclose(rel->descf);
			return(NULL);
		}
		rel->lev[i]->nrec = 0;
		rel->lev[i]->nbucket = 0;
	}
	if ((dptr = (DESC *) malloc (rel->hd.dsize)) == NULL) {
		rerror("memory");
		exit(1);
	}
#ifdef STATS
		rel->n_bget = 0;
		rel->n_bput = 0;
		rel->n_dget = 0;
		rel->n_dput = 0;
#endif
	while (fread(dptr, rel->hd.dsize, 1, rel->descf) == 1) {
#ifdef STATS
		rel->n_dget++;
#endif
		lptr = rel->lev[dptr->level];
		lptr->dtable[dptr->page] = dptr;
		lptr->nbucket++;
		lptr->nrec += dptr->nrec;
		if ((dptr = (DESC *) malloc (rel->hd.dsize)) == NULL) {
			rerror("memory");
			exit(1);
		}
	}
	free(dptr);	/* last one allocated not used */
	for (i = 0; i < rel->hd.nlevels; i++) {
		lptr = rel->lev[i];
		if (lptr->nbucket < 1) {
			rerror("horrible");
			exit(1);
		}
		d = lptr->nbucket - 1;
		lptr->depth = 0;
		while (d != 0) {	
			d >>= 1;
			lptr->depth++;
		}
		if (lptr->depth == 0)
			lptr->sp = 0;
		else
			lptr->sp = lptr->nbucket % (01 << (lptr->depth - 1));
	}
	return(rel);
}
