#include "rel.h"

extern char *malloc();

REL *rcreat(database, name, keys, l, bsz, dsz, cvs, nbkts)

char *database;	/* name of directory containing database */
char *name;	/* relation name */
int keys;	/* number of keys in relation */
int l;		/* load control (L) */
int bsz;	/* bucket size (in bytes) */
int dsz[];	/* bits in descriptor for each key */
		/* sum dsz[] = mutiple of bits in "long" */
int cvs[];	/* choice vectors for each key */
int nbkts;	/* initial number of buckets */

{
	register REL *rel;
	register i;
	char mainfile[MAXPATH], descfile[MAXPATH], statfile[MAXPATH];
	int levbkts;

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
	if ((rel->mainf = fopen(mainfile, "w+")) == NULL) {
		rerror("open");
		return(NULL);
	}	
	strcpy(descfile, database);
	strcat(descfile, "/.");
	strcat(descfile, name);
	if ((rel->descf = fopen(descfile, "w+")) == NULL) {
		rerror("open");
		fclose(rel->mainf);
		return(NULL);
	}	
#ifdef STATS
	strcpy(statfile, database);
	strcat(statfile, "/_");
	strcat(statfile, name);
	if ((rel->statf = fopen(statfile, "a")) == NULL) {
		rerror("open");
		fclose(rel->mainf);
		fclose(rel->descf);
		return(NULL);
	}	
#endif
#ifdef LOCK_EX
	flock(fileno(rel->descf), LOCK_EX);
	flock(fileno(rel->mainf), LOCK_EX);
#endif
	rel->hd.version = V_MAJOR + V_MINOR + V_REVISION;
	strcpy(rel->hd.name, name);
	if (keys > MAXKEYS || keys < 1) {
		rerror("nkeys");
		rclose(rel);
		return(NULL);
	}
	rel->hd.nkeys = keys;
	rel->hd.nlevels = 0;
	rel->hd.nbuckets = 0;
	if (bsz > MAXBSIZE) {
		rerror("bsize");
		rclose(rel);
		return(NULL);
	}
	rel->hd.bsize = bsz;
	rel->hd.lc = l;
	rel->hd.dh[0] = dsz[0];
	rel->hd.d[0] = 0;
	rel->hd.cv[0] = cvs[0];
	for (i = 1; i < keys; i++) {
		rel->hd.dh[i] = dsz[i];
		rel->hd.d[i] = rel->hd.d[i-1] + dsz[i-1];
		rel->hd.cv[i] = cvs[i];
	}
		/* size in bytes (! - LN) of header + descriptor */
	rel->hd.dsize = DHDSZ +
		(rel->hd.d[keys-1] + dsz[keys-1] + DWBITS-1)
			/ DWBITS * DWBITS / BBITS;
	if (rel->hd.dsize > MAXDSIZE) {
		rerror("dsize");
		rclose(rel);
		return(NULL);
	}
#ifdef STATS
	rel->n_bget = 0;
	rel->n_bput = 0;
	rel->n_dget = 0;
	rel->n_dput = 0;
#endif
	rhdput(rel);
			/* create empty file with nbkts total */
			/* and successive levels 1/16 previous size */
	while (nbkts > 1) {
		static BUCKET newbkt;
		int level;

		levbkts = nbkts / 16 * 15 + nbkts % 16;
		level = newlevel(rel);
		rel->lev[level]->nrec = 0;
		newbkt.nfacts = 0;
		newbkt.nbytes = 0;
		bput(rel, level, 0, &newbkt);
		dput(rel, rel->lev[level]->dtable[0]);
		for (i = 1; i < levbkts; i++)
			bsplit(rel, level);
		nbkts -= levbkts;
	}
#ifdef LOCK_EX
	flock(fileno(rel->mainf), LOCK_SH);
	flock(fileno(rel->descf), LOCK_SH);
#endif
	return(rel);
}
