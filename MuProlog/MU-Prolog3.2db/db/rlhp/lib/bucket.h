#ifndef MAXBSIZE
#define	MAXBSIZE 1024		/* bucket size */
#endif

typedef  struct {
	int nfacts;		/* number of facts in bucket */
	int nbytes;		/* number of bytes in bucket */
	FACT fact[MAXBSIZE/4];	/* facts in bucket */
	char bbuf[MAXBSIZE];	/* the bucket as stored on disc */
} BUCKET;

typedef int OP;

/* values for OP */

#define F_ONE		  01	/* only aplly operation to one match */
#define F_ALL		  02	/* apply operation to all that match */

#define F_DELETE	 010	/* retract or retractall */
#define F_ADD		 020	/* assert */
#define F_PRINT		 040	/* query */

#define F_SYNC		0100	/* synchronous */
#define F_ASYNC		0200	/* asynchronous */

/* masks for OP */

#define F_NUM		 03	/* one or all */
#define F_OP		070	/* actual operation */


typedef unsigned int INDEX;

#define INDEXBITS	(BBITS * sizeof (INDEX))

extern INDEX hash();

extern BUCKET *bget();
