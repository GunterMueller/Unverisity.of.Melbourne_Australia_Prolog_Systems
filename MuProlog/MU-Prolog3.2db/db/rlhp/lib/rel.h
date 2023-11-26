#include "desc.h"
#include "fact.h"
#include "bucket.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

#define V_MAJOR	3000	/* major version number */
#define V_MINOR	100	/* minor version number */
#define V_REVISION 3	/* revision number */
#define MAXLEV	128	/* maximum number of levels */
#define MAXPATH	128	/* maximum path length of file name */
#ifndef MAXPAGE
#define MAXPAGE	256	/* maximum number of pages on one level */
#endif

struct header {
	int version;		/* major version number */
	char name[16];		/* relation name */
	int nkeys;		/* number of keys */
	int bsize;		/* bucket size in bytes */
	int nlevels;		/* number of levels in file */
	int nbuckets;		/* number of buckets in file */
	int dsize;		/* descriptor size in bytes */
	int lc;			/* load control (L) */
	int d[MAXKEYS];		/* offset in descriptor for each key */
	int dh[MAXKEYS];	/* number of bits in descriptor for each key */
	INDEX cv[MAXKEYS];	/* choice vector to construct bucket indices */
};

typedef struct {
	int depth;		/* depth of level, (nbuckets <= 01 << depth) */
	int sp;			/* split pointer */
	int nrec;		/* number of records in this level */
	int nbucket;		/* number of data buckets in this level */
	DESC *dtable[MAXPAGE];	/* pointers to descriptors for each bucket */
} LEVEL;

typedef struct {
	struct header hd;	/* relation header */
	FILE *mainf;		/* main file containing actual data buckets */
	FILE *descf;		/* descriptor and header file */
	LEVEL *lev[MAXLEV];	/* pointers to information about each level */
#ifdef STATS
	FILE *statf;		/* statistics file */
	int n_bget;		/* number of buckets read */
	int n_bput;		/* number of buckets written */
	int n_dget;		/* number of descriptors read */
	int n_dput;		/* number of descriptors written */
#endif
} REL;

extern REL *rcreat();
extern REL *ropen();

#define rassert_sync(rel,fact)	rassert(rel,fact,F_ADD|F_SYNC)
#define rassert_async(rel,fact)	rassert(rel,fact,F_ADD|F_ASYNC)
#define retract(rel,fact)	rtrans(rel,fact,F_DELETE|F_ONE)
#define retractall(rel,fact)	rtrans(rel,fact,F_DELETE|F_ALL)
#define rquery(rel,fact)	rtrans(rel,fact,F_PRINT|F_ONE)
#define rqueryall(rel,fact)	rtrans(rel,fact,F_PRINT|F_ALL)

#define wrong(rel,fact) \
((rel->hd.nkeys != fact->nkeys) || (strcmp(rel->hd.name, fact->fname) != 0))
