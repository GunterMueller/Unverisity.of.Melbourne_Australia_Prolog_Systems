typedef unsigned long DWORD;

#define BBITS	8	 		/* number of bits in one (char) byte */
#define DWBITS	(BBITS * sizeof(DWORD)) /* number of bits in one (long) word of descriptor */

#ifndef MAXDWORDS
#define MAXDWORDS	8	 	/* maximum words in a descrpitor */
#endif

#define DHDSZ		(4 * sizeof (short))	/* size (in bytes) of descriptor header */
#define MAXDSIZE	(DHDSZ + MAXDWORDS * sizeof (long)) /* maximum size of descriptor */

typedef struct {
	short level;		/* level number */
	short page;		/* page number within level */
	short bkt;		/* bucket number within main file */
	short nrec;		/* number of records in bucket */
	DWORD bits[MAXDWORDS];	/* the actual descriptor */
} DESC;

extern DESC *dnew();
extern long value();
