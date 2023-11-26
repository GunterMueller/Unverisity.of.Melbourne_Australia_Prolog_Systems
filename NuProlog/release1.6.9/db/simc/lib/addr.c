/*
 * addr.c  -  deductive database package (address translation operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by James Thom, Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

/*
 * bits:
 *	Extract bits a-b from a word,
 *	where numbering is from left to right
 *	and bit#0 is the second leftmost bit in the word
 */
#define	bits(h,a,b) ((h >> (unsigned)(MAXBITS-(b)-1)) & ((01 << ((b)-(a) + 1)) - 1))


/*
 * segmentOfHash:
 *	Given the relation and DSIMC hash code,
 *	determine the segment number at specified level
 */
Int
segmentOfHash(rel,h,level)
Reln *rel;
Word h;
Int level;
{
	r_Int	i;
	r_Int	d = depth(rel,level);

#if 0
fprintf(stderr,"segOfHash(rel,%08x,%d) is ",h,level);
#endif
	if (Nls(rel,level) == 0)
		return(-1);
	if (bits(h,0,d-1) == 0)
		return(0);

	for (i = 0; bits(h,i,i) == 0; i++);

	if (bits(h,0,d-1) < (01 << (d-i-1)) + magic(rel,level,i)) {
#if 0
fprintf(stderr, "%08x\n", bits(h,0,d));
#endif
		return(bits(h,0,d));
	}
	else {
#if 0
fprintf(stderr, "%08x\n", bits(h,1,d));
#endif
		return(bits(h,0,d-1));
		/* return(bits(h,1,d)); */
	}
}

#if 0
/*
 * ovflowSegment:
 *	Given a segment number on level i, determine the number
 *	of the corresponding overflow segment on level j (j>i).
 *	Assumes Nls(rel,level) > 0.
 */
Int
ovflowSegment(rel,segno,level,ovlevel)
Reln *rel;
Word segno;
Int level, ovlevel;
{
	r_Int	b, shash;

#if 0
fprintf(stderr,"ovSeg(rel,%d(%08x),%d,%d)\n",segno,segno,level,ovlevel);
#endif
	if ((segno << 1) < Nls(rel,level))
		b = depth(rel,level);
	else
		b = depth(rel,level) - 1;

	shash = segno << (MAXBITS-b);

#if 0
fprintf(stderr,"segno<<1:%d b:%d shash:%08x\n",segno<<1,b,shash);
#endif
	return(segmentOfHash(rel, shash, ovlevel));
}
#else
/*
 * ovflowSegment:
 *	Given a segment number on level i, determine the number
 *	of the corresponding overflow segment on level j (j>i).
 *	Assumes Nls(rel,level) > 0.
 */
Int
ovflowSegment(rel,segno,level,ovlevel)
Reln *rel;
Word segno;
Int level, ovlevel;
{
	r_Int	d1, d2;

	d1 = depth(rel, level);
	d2 = depth(rel, ovlevel);

	segno >>= (d1 - d2);

	if (segno < Nls(rel,level))
		return(segno);
	else
#ifdef BUGGY
		return(segno >> 1);
#else
		return(segno & (~(01 << (d2))));
#endif
}
#endif
