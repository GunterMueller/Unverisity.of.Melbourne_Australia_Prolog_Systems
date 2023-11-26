/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * A  "smarter" malloc				William L. Sebok
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *	Algorithm:
 *	 Assign to each area an index "n". This is currently proportional to
 *	the log 2 of size of the area rounded down to the nearest integer.
 *	Then all free areas of storage whose length have the same index n are
 *	organized into a chain with other free areas of index n (the "bucket"
 *	chain). A request for allocation of storage first searches the list of
 *	free memory.  The search starts at the bucket chain of index equal to
 *	that of the storage request, continuing to higher index bucket chains
 *	if the first attempt fails.
 *	If the search fails then new memory is allocated.  Only the amount of
 *	new memory needed is allocated.  Any old free memory left after an
 *	allocation is returned to the free list.
 *
 *	  All memory areas (free or busy) handled by malloc are also chained
 *	sequentially by increasing address (the adjacency chain).  When memory
 *	is freed it is merged with adjacent free areas, if any.  If a free area
 *	of memory ends at the end of memory (i.e. at the break), and if the
 *	variable "endfree" is non-zero, then the break is contracted, freeing
 *	the memory back to the system.
 *
 *	Notes:
 *		ov_length field includes sizeof(struct overhead)
 *		adjacency chain includes all memory, allocated plus free.
 */

/*
 * The code in this file is used with the kind permission of its author.
 * It is not the property of the University of Melbourne, but has been 
 * modified and added to there.
 */

/* the following items may need to be configured for a particular machine */

/* alignment requirement for machine (in bytes) */
#define NALIGN (ALIGNMENT > sizeof(Word) ? ALIGNMENT : sizeof(Word))

/* size of an integer large enough to hold a character pointer */
typedef	Word Size;

/*
 * CURBRK returns the value of the current system break, i.e., the system's
 * idea of the highest legal address in the data area.  It is defined as
 * a macro for the benefit of systems that have provided an easier way to
 * obtain this number (such as in an external variable)
 */

#define CURBRK highWater
#define initCURBRK {lowWater = highWater = sbrk(0);}

/*
 * note that it is assumed that CURBRK remembers the last requested break to
 * the nearest byte (or at least the nearest word) rather than the nearest page
 * boundary.  If this is not true then the following BRK macro should be
 * replaced with one that remembers the break to within word-size accuracy.
 */

#define BRK(x) malloc_brk(x)

/* END of machine dependent portion */

#define	MAGIC_FREE	0x548a934c
#define	MAGIC_BUSY	0xc139569a

#define NBUCKETS	18

struct qelem {
	struct qelem *q_forw;
	struct qelem *q_back;
};

struct overhead {
	struct qelem	ov_adj;		/* adjacency chain pointers */ 
	struct qelem	ov_buk;		/* bucket chain pointers */
	long		ov_magic;
	Size		ov_length;
};

/*
 * The following macros depend on the order of the elements in struct overhead
 */
#define TOADJ(p)	((struct qelem *)(p))
#define FROMADJ(p)	((struct overhead *)(p))
#define FROMBUK(p)	((struct overhead *)( (char *)p - sizeof(struct qelem)))
#define TOBUK(p)	((struct qelem *)( (char *)p + sizeof(struct qelem)))
