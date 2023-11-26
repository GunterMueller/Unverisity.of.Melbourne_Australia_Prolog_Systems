/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * A  "smarter" malloc v1.0			William L. Sebok
 *					Sept. 24, 1984 rev. June 30,1986
 *
 *	malloc allocates and returns a pointer to a piece of memory nbytes
 *	in size.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * The code in this file is used with the kind permission of its author.
 * It is not the property of the University of Melbourne, but has been 
 * modified and added to there.
 */

#include "mu.h"

static int counter = 0;

static void mllcerr();

#ifdef debug
# define MALLOC_ASSERT(p,q) if (!(p)) mllcerr(q)
#else /* debug */
# define MALLOC_ASSERT(p,q)
#endif /* debug */

/*
 * The vax has wondrous instructions for inserting and removing items into
 * doubly linked queues.  On the vax the assembler output of the C compiler is
 * massaged by an sed script to turn these function calls into invocations of
 * the insque and remque machine instructions.
 */
/*
 * Well, it could be, but I've thrown it away in the name of simpler
 * makefiles.
 */

static void
insque(item, queu)
register struct qelem *item, *queu;
/* insert "item" after "queu" */
{
	register struct qelem *pueu;
	pueu = queu->q_forw;
	item->q_forw = pueu;
	item->q_back = queu;
	queu->q_forw = item;
	pueu->q_back = item;
}

static void
remque(item)
register struct qelem *item;
/* remove "item" */
{
	register struct qelem *queu, *pueu;
	pueu = item->q_forw;
	queu = item->q_back;
	queu->q_forw = pueu;
	pueu->q_back = queu;
}

static void
mlfree_end()
{
	register struct overhead *p;
	static int brkWarned = 0;

	p = FROMADJ(adjhead.q_back);
	if(	/* area is free and at end of memory */
	        p->ov_magic == MAGIC_FREE
	    &&	(char*)p + p->ov_length == (char *)CURBRK
	) {
		p->ov_magic = NULL;	/* decommission (just in case) */

		/* remove from end of adjacency chain */
		remque(TOADJ(p));
		/* remove from bucket chain */
		remque(TOBUK(p));

		/* release memory to system */
		if(BRK((char *)p)) {
			if(!brkWarned)
				warning("Can't reduce brk.");
			brkWarned = 1;
			/* Put the memory back in the arena. */
			p->ov_magic = MAGIC_FREE;
			insque(TOADJ(p), adjhead.q_back);
			insque(TOBUK(p), &buckets[mlindx(p->ov_length)]);
		}
	}
	return;
}

int
malloc_brk(x)
register caddr_t x;
{
	register int y;

	y = brk(x);
	if(y == 0)
		highWater = x;
	return(y);
}

/*
 * return to the system memory freed adjacent to the break 
 * default is Off
 */
int endfree = 0;

/* sizes of buckets currently proportional to log 2() */
Size mlsizes[] = {0, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768,
	65536, 131072, 262144, 524288, 1048576, 2097152, 4194304};

/* head of adjacency chain */
struct qelem adjhead;

/* head of bucket chains */
struct qelem buckets[NBUCKETS];

void
initMalloc()
{
	register int i;

	for(i = 0; i < NBUCKETS; i++)
		buckets[i].q_forw = buckets[i].q_back = buckets + i;
	adjhead.q_forw = adjhead.q_back = &adjhead;

	initCURBRK;
}

char *
malloc(nbytes)
Size nbytes;
{
	register struct overhead *p, *q;
	register struct qelem *bucket;
	register Size surplus;
	Size mlindx();

	nbytes = ((nbytes + (NALIGN-1)) & ~(NALIGN-1))
		+ sizeof(struct overhead);

	for (
	    bucket = &buckets[mlindx(nbytes)];
	    bucket < &buckets[NBUCKETS];
	    bucket++
	) { 
		register struct qelem *b;
		for(b = bucket->q_forw; b != bucket; b = b->q_forw) {
			p = FROMBUK(b);
if(p->ov_magic != MAGIC_FREE) {
	printf("\nmalloc: Entry %x not marked FREE but on Free List!\n", p);
}
			MALLOC_ASSERT(p->ov_magic == MAGIC_FREE,
"\nmalloc: Entry not marked FREE but on Free List!\n");
			if (p->ov_length >= nbytes) {
				remque(b);
				surplus = p->ov_length - nbytes;
				goto foundit;
			}
		}
	}

	/* obtain additional memory from system */
	{
		register Size i;
		p = (struct overhead *)CURBRK;

		i = ((Size)p)&(NALIGN-1);
		if (i != 0)
			p = (struct overhead *)((char *)p + NALIGN - i);

		if(BRK((char *)p + nbytes)) {
			return(NULL);
		}

		p->ov_length = nbytes;
		surplus = 0;

		/* add to end of adjacency chain */
		MALLOC_ASSERT((FROMADJ(adjhead.q_back)) < p,
"\nmalloc: Entry in adjacency chain found with address lower than Chain head!\n"
			);
		insque(TOADJ(p),adjhead.q_back);
	}

foundit:
	/* mark surplus memory free */
	if (surplus > (int)sizeof(struct overhead)) {
		/* if big enough, split it up */
		q = (struct overhead *)((char *)p + nbytes);

		q->ov_length = surplus;
		p->ov_length = nbytes;
		q->ov_magic = MAGIC_FREE;

		/* add surplus into adjacency chain */
		insque(TOADJ(q),TOADJ(p));

		/* add surplus into bucket chain */
		insque(TOBUK(q),&buckets[mlindx(surplus)]);
	}

	p->ov_magic = MAGIC_BUSY;
#ifdef debug
printf("Mallocing %d bytes at %x (#%d)\n", nbytes, (char*)p+sizeof(struct overhead), counter++);
#endif /* debug */
	return((char*)p + sizeof(struct overhead));
}

/*
 * Malloc some memory after the current brk.
 *
 * This is needed to ensure that the NU-Prolog stack areas are given
 * the best chance to expand without having to move.
 */
char *
balloc(nbytes)
register Size nbytes;
{
	register struct overhead *p;

	nbytes = ((nbytes + (NALIGN-1)) & ~(NALIGN-1)) + sizeof(struct overhead);
	p = (struct overhead *) (((Word)CURBRK + NALIGN - 1) & ~(NALIGN - 1));
	if(BRK((char *)p + nbytes)) {
		return(NULL);
	}
	p->ov_length = nbytes;
	p->ov_magic = MAGIC_BUSY;
	/* add to end of adjacency chain */
	MALLOC_ASSERT((FROMADJ(adjhead.q_back)) < p,
"\nmalloc: Entry in adjacency chain found with address lower than Chain head!\n"
		);
	insque(TOADJ(p),adjhead.q_back);
#ifdef debug
printf("Ballocing %d bytes at %x\n", nbytes, (char*)p+sizeof(struct overhead));
#endif /* debug */
	return((char*)p + sizeof(struct overhead));
}

char *
calloc(nelem, elsize)
Size nelem, elsize;
{
	register char *m;
	register Size i;

	i = nelem * elsize;
	m = malloc(i);
	if(m == (char *) NULL)
		return(m);
	bzero(m, i);

	return(m);
}

/* #ifdef BSD4 */
char *
memalign(alignment, n)
register unsigned alignment;
Size n;
{
	register char *m;

	m = malloc(n + alignment);
	if(m == (char *) NULL)
		return(m);

	return((char *)ALIGNANY(m, alignment));
}

#ifdef MACHINE_SUN4_SOLARIS
int
getpagesize()
{
	return (int)sysconf(_SC_PAGESIZE);
}
#endif /* MACHINE_SUN4_SOLARIS */


char *
valloc(n)
register Size n;
{
	/* BUG!  Can't free() this! */
	return(memalign(getpagesize(), n));
}
/* #endif */ /* BSD4 */

/*
 * select the proper size bucket
 */
Size
mlindx(n)
register Size n;
{
	register Size *p, *q, *r;
	p = &mlsizes[0];
	r = &mlsizes[NBUCKETS];
	/* binary search */
	while ((q = (p + (r-p)/2)) > p) {
		if (n < *q)
			r = q;
		else
			p = q;
	}
	return(q - &mlsizes[0]);
}

static void
mllcerr(p)
char *p;
{
	register char *q;
	q = p;
	while (*q++);	/* find end of string */
	(void)write(2,p,q-p-1);
#ifdef debug
	abort();
#endif /* debug */
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  realloc				William L. Sebok
 * A  "smarter" malloc v1.0		Sept. 24, 1984 rev. Oct 17,1986
 *
 *	realloc takes previously malloc-allocated area at mem, and tries
 *	 to change its size to nbytes bytes, moving it and copying its
 *	 contents if necessary.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

char *
realloc(mem, nbytes)
register char *mem; Size nbytes;
{
	register char *newmem;
	register struct overhead *p;
	Size surplus, length;

	if(mem == NULL)
		return(malloc(nbytes));

	/* if beyond current arena it has to be bad */
	if(mem > (char *) FROMADJ(adjhead.q_back) + sizeof(struct overhead)) {
		return(NULL);
	}

	p = (struct overhead *)(mem - sizeof(struct overhead));

	if(p->ov_magic != MAGIC_BUSY && p->ov_magic != MAGIC_FREE) {
		return(NULL);	/* already gone */
	}

	newmem = (char *) NULL;
	length = p->ov_length;
	nbytes = ((nbytes + (NALIGN-1)) & (~(NALIGN-1))) + sizeof(struct overhead);

	if(p->ov_magic == MAGIC_BUSY) {
		/* free may claim adjacent free memory, compacting storage */
		register int oendfree = endfree;

		endfree = 0;
		free(mem);	/* free it but don't let it contract break */
		endfree = oendfree;
		if(p->ov_magic != MAGIC_FREE) {	/* check if moved */
			p = FROMADJ(p->ov_adj.q_back);
			length = p->ov_length;
			newmem = (char *)p + sizeof(struct overhead);
		}
	}

	/* at this point p->ov_magic should be MAGIC_FREE */
	MALLOC_ASSERT(p->ov_magic == MAGIC_FREE, "\nrealloc: bad magic number.\n");

	surplus = length - nbytes;
	if(surplus >= 0) {
		/* present location large enough */
		remque(TOBUK(p));
		p->ov_magic = MAGIC_BUSY;
	} else if(((char *)p + p->ov_length) == CURBRK) {
		/* if at break, grow in place */
		if(BRK((char *)p + nbytes)) {
			printf("Can't brk %x (+%d)\n", (char *)p + nbytes, nbytes);
			return(NULL);
		}
		p->ov_length = nbytes;
		remque(TOBUK(p));
		p->ov_magic = MAGIC_BUSY;
	} else {
		newmem = malloc(nbytes - sizeof(struct overhead));
		if(newmem == NULL) {
			return(NULL);
		}
		surplus = 0;
	}

	/* if returned address is different, move data */
	if(newmem != NULL) {
		/* note: it is assumed that bcopy does the right thing on
		 * overlapping extents (true on the vax)
		 */
		(void)bcopy(mem, newmem, ((length < nbytes) ? length : nbytes)
		    - sizeof(struct overhead));
		 mem = newmem;
	}

	/* if more memory than we need then return excess to buckets */
	if(surplus > (int)sizeof(struct overhead)) {
		register struct overhead *q;

		q = (struct overhead *)((char *)p + nbytes);
		q->ov_length = surplus;
		q->ov_magic = MAGIC_FREE;
		insque(TOADJ(q), TOADJ(p));
		insque(TOBUK(q), &buckets[mlindx(surplus)]);
		p->ov_length -= surplus;
	}

	if(endfree)
		mlfree_end();

#ifdef debug
printf("Reallocing %d bytes at %x (#%d)\n", nbytes, mem, counter++);
#endif /* debug */
	return(mem);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  free					William L. Sebok
 * A "smarter" malloc v1.0		Sept. 24, 1984 rev. June 30,1986
 *
 * 	free takes a previously malloc-allocated area at mem and frees it.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
free(mem)
register char *mem;
{
	register struct overhead *p, *q;

#ifdef debug
printf("Freeing %x (#%d)\n", mem, counter++);
#endif /* debug */
	if(mem == NULL)
		return;

	p = (struct overhead *)(mem - sizeof(struct overhead));

	/* not advised but allowed */
	if(p->ov_magic == MAGIC_FREE)
		return;

	if(p->ov_magic != MAGIC_BUSY)
		mllcerr("attempt to free memory not allocated with malloc!\n");

	/* try to merge with previous free area */
	q = FROMADJ((TOADJ(p))->q_back);

	if(q != FROMADJ(&adjhead)) {
		MALLOC_ASSERT(q < p,
"\nfree: While trying to merge a free area with a lower adjacent free area,\n\
 addresses were found out of order!\n");
		/* If lower segment can be merged */
		if(q->ov_magic == MAGIC_FREE && (char *)q + q->ov_length == (char *)p) {
			/* remove lower address area from bucket chain */
			remque(TOBUK(q));

			/* remove upper address area from adjacency chain */
			remque(TOADJ(p));

			q->ov_length += p->ov_length;
			p->ov_magic = NULL;	/* decommission */
			p = q;
		}
	}

	/* try to merge with next higher free area */
	q = FROMADJ((TOADJ(p))->q_forw);

	if(q != FROMADJ(&adjhead)) {
		/* upper segment can be merged */
		MALLOC_ASSERT(q > p,
"\nfree: While trying to merge a free area with a higher adjacent free area,\n\
 addresses were found out of order!\n");
		if(q->ov_magic == MAGIC_FREE && (char *)p + p->ov_length == (char *)q) {
			/* remove upper from bucket chain */
			remque(TOBUK(q));

			/* remove upper from adjacency chain */
			remque(TOADJ(q));

			p->ov_length += q->ov_length;
			q->ov_magic = NULL;	/* decommission */
		}
	}

	p->ov_magic = MAGIC_FREE;

	/* place in bucket chain */
	insque(TOBUK(p),&buckets[mlindx(p->ov_length)]);

	if(endfree)
		mlfree_end();

	return;
}

void
cfree(mem)
char *mem;
{
	free(mem);
}

#if 0			/* We don't use this. */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  forget				William L. Sebok
 * A "smarter" malloc v1.0		Sept. 24, 1984 rev. June 30,1986
 *
 *	forget returns to the malloc arena all memory allocated by sbrk()
 *	 above "bpnt".
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

void
forget(bpnt)
char *bpnt;
{
	register struct overhead *p, *q, *r, *b;
	register Size l;
	struct overhead *crbrk;
	int pinvalid, oendfree;

	/*
	 * b = forget point
	 * p = beginning of entry
	 * q = end of entry, beginning of gap
	 * r = end of gap, beginning of next entry (or the break)
	 * pinvalid = true when groveling at forget point
	 */

	pinvalid = 0;
	oendfree = endfree;	endfree = 0;
	b = (struct overhead *)bpnt;
	p = FROMADJ(adjhead.q_back);
	r = crbrk = (struct overhead *)CURBRK;

	for( ; pinvalid == 0 && b < r; p = FROMADJ(TOADJ(r = p)->q_back)) {
		if(p == FROMADJ(&adjhead)
		 || (q = (struct overhead *)((char *)p + p->ov_length)) < b
		) {
			pinvalid = 1;
			q = b;
		}

		if(q == r)
			continue;

		MALLOC_ASSERT(q < r,
"\nforget: addresses in adjacency chain are out of order!\n");

		/* end of gap is at break */
		if(oendfree && r == crbrk) {
			if(BRK((char *)q))			/* free it yourself */
				panic("Can't set brk in forget()");
			crbrk = r = q;
			continue;
		}

		if(pinvalid)
			q = (struct overhead *) /* align q pointer */
				(((long)q + (NALIGN-1)) & (~(NALIGN-1)));

		l = (char *)r - (char *)q;
		/*
		 * note: unless something is screwy: (l%NALIGN) == 0
		 * as r should be aligned by this point
		 */

		if(l >= (int)sizeof(struct overhead)) {
			/* construct busy entry and free it */
			q->ov_magic = MAGIC_BUSY;
			q->ov_length = l;
			insque(TOADJ(q),TOADJ(p));
			free((char *)q + sizeof(struct overhead));
		} else if(pinvalid == 0) {
			/* append it to previous entry */
			p->ov_length += l;
			if(p->ov_magic == MAGIC_FREE) {
				remque(TOBUK(p));
				insque(TOBUK(p),&buckets[mlindx(p->ov_length)]);
			}
		}
	}
	endfree = oendfree;
	if(endfree)
		mlfree_end();
	return;
}

#endif /* 0 */

/*
 * This routine is based on bits of William Sebok's tstmalloc routine.
 */
void
showMallocArena()
{
	register struct overhead *p;
	register struct qelem *qp;
	register struct overhead *prev;

	prev = NULL;
	for(qp = adjhead.q_forw; qp != &adjhead; qp = qp->q_forw) {
		p = FROMADJ(qp);
		switch(p->ov_magic) {
		when MAGIC_FREE:
			printf("FREE at 0x%x (0x%x to 0x%x), length = %d\n",
				(char *)p,
				(char *)p + sizeof(struct overhead),
				(char *)p + p->ov_length,
				p->ov_length);
		when MAGIC_BUSY:
			printf("BUSY at 0x%x (0x%x to 0x%x), length = %d\n",
				(char *)p,
				(char *)p + sizeof(struct overhead),
				(char *)p + p->ov_length,
				p->ov_length);
		break;
		default:
			panic("Bad block found in malloc chain.");
		}
		if(prev != NULL && (char *)prev + prev->ov_length != (char *)p)
			printf("Error: Blocks not adjacent.\n");
		prev = p;
	}
}

Word
mallocedMemory()
{
	register struct overhead *p;
	register struct qelem *qp;
	register Word used;

	used = 0;
	for(qp = adjhead.q_forw; qp != &adjhead; qp = qp->q_forw) {
		p = FROMADJ(qp);
		switch(p->ov_magic) {
		when MAGIC_FREE:
			;
		when MAGIC_BUSY:
			used += p->ov_length;
		break;
		default:
			panic("Bad block found in malloc chain.");
		}
	}
	return(used);
}

#ifdef DEBUG
static int
checkRegion(msg, p, len, base, top)
char *msg;
char *p;
Word len;
char *base, *top;
{
	if(p == base)
		return;
	if(p < top && (p + len) > base) {
		fprintf(stderr, "%s\n", msg);
		fprintf(stderr, "0x%x to 0x%x overlaps 0x%x to 0x%x\n",
			p, p + len, base, top);
		return 0;
	}
	return 1;
}

int
checkMalloc(msg)
char *msg;
{
	register struct qelem *qp;
	register struct overhead *p;
	struct overhead *memBase;
	
	memBase = (struct overhead *)((char *)memory - sizeof(struct overhead));

	for(qp = adjhead.q_forw; qp != &adjhead; qp = qp->q_forw) {
		p = FROMADJ(qp);
		if(p->ov_magic == MAGIC_FREE || p->ov_magic == MAGIC_BUSY) {
			if(checkRegion("Memory", p, p->ov_length, memBase, memoryMax))
				continue;
		}
		fprintf(stderr, "%s\n", msg);
		fprintf(stderr, "p = 0x%x\n", p);
		panic("Bad block found in malloc chain.");
	}
	for(qp = &buckets[0]; qp < &buckets[NBUCKETS]; qp++) {
		register struct qelem *b;

		for(b = qp->q_forw; b != qp; b = b->q_forw) {
			p = FROMBUK(b);
			if(!checkRegion("Memory", p, p->ov_length, memBase, memoryMax)
			|| p->ov_magic != MAGIC_FREE) {
				fprintf(stderr, "%s\n", msg);
				fprintf(stderr, "p = 0x%x\n", p);
				panic("Free List corrupted.");
			}
			/*
			bzero((char *)(p + 1), p->ov_length - sizeof(struct overhead));
			*/
		}
	}
	return(1);
}
#endif /* DEBUG */
