/*
 * bits.c  -  deductive database package (bit string operations)
 *
 * $Header: bits.c,v 1.5 85/12/06 15:09:52 jas Exp $
 * $Log:	bits.c,v $
 * Revision 1.5  85/12/06  15:09:52  jas
 * Last_before_sfb
 * 
 * Revision 1.4  85/07/30  10:23:33  jas
 * Stable version ... releasable
 * 
 * Revision 1.3  85/06/13  10:31:06  jas
 * added clustering
 * 
 * Revision 1.2  85/06/08  16:48:29  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  12:54:32  jas
 * Initial revision
 * 
 * 
 */

#include <stdio.h>
#include "bits.h"
#include "util.h"

/*
 * b_make:
 *	Create a new bit string
 */
Word *
b_make(nbits)
Int nbits;
{
	r_Word	*bits;
	r_Int	len;
	r_Word	*data;

	/*
	 * Allocate some space for the bits
	 */
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"b_make:");
#endif DBUG
	if ((bits = (Word *)malloc((unsigned)nbytes(nbits))) == WordNULL)
		return(WordNULL);

	/*
	 * And zero out the data
	 */
	data = bits;
	for (len = 0; len < nbits; len += WORDSIZE)
		*data++ = 0;
	return(bits);
}


/*
 * b_fprint:
 *	print a bit string as hex digits (*not* terminated by '\n')
 */
void
b_fprint(fp,bits,nbits)
FileP fp;
Word *bits;
Int nbits;
{
	r_Int	len;

	if (bits == WordNULL)
		fprintf(fp,"WORDNULL");

	/*
	 * Go to most significant word (highest memory location)
	 */
	bits = &(bits[nwords(nbits)-1]);

	/*
	 * From most significant to least, print word at a time
	 */
	for (len = 0; len < nbits; len += WORDSIZE)
		fprintf(fp, "%08x", *bits--);
}


/*
 * b_count:
 *	Count how many bits are set
 */
Int
b_count(bits,nbits)
Word *bits;
Int nbits;
{
	r_Int	len, count, i;
	r_Word	w;

	/*
	 * Count bits, a word-at-a-time, starting from least sig word 
	 */
	count = 0;
	for (len = 0; len < nbits; len += WORDSIZE)
	{
		w = *bits++;
		for (i = 0; i < WORDSIZE; i++, w >>= 1) {
			if (w == 0)
				break;
			count += (w & 1);
		}
	}
	return(count);
}


/*
 * b_nextset:
 *	Return position of next bit set in bit string in the
 *	  direction from least significant to most significant
 *	Returns -1 if no more bits are set
 */
Int
b_nextset(from,bits,nbits)
Int from;
Word *bits;
Int nbits;
{
	r_Int	len, i, start;
	r_Word	w;

	/*
	 * Set up as if we have just scanned to the "from" bit position
	 */
	bits = &(bits[b_word(from)]);
	w = (Word)*bits >> b_pos(from);
	start = b_pos(from);

	/*
	 * Scan rest of words until we find a ONE bit
	 */
	for (len = floor(from); len < nbits; len += WORDSIZE)
	{
		for (i = start; i < WORDSIZE; i++, w >>= 1) {
			if (w == 0) {
				from += (WORDSIZE-i);
				break;
			}
			if (w & 1)
				return(from);
			else
				from++;
		}
		w = *++bits;
		start = 0;
	}
	return(-1);
}


#ifdef MAYBE
#ifndef lint

/*
 * b_lshift:
 *	Shift left logical by N bits
 */
void
b_lshift(N, bits, nbits)
Int N;
Word *bits;
Int nbits;
{
}


/*
 * b_rshift:
 *	Shift right logical by N bits
 */
void
b_rshift(N, bits, nbits)
Int N;
Word *bits;
Int nbits;
{
}

#endif !lint
#endif MAYBE

/*
 * b_and:
 *	bitwise AND on two bit strings
 */
void
b_and(bits1, bits2, bres, nbits)
Word *bits1, *bits2, *bres;
Int nbits;
{
	r_Int	len;

	/*
	 * AND together, a word-at-a-time, starting from least sig word 
	 */
	for (len = 0; len < nbits; len += WORDSIZE)
		*bres++ = *bits1++ & *bits2++;

	/*
	 * Mask out junk in the most significant word
	 */
	if (mod(nbits) != 0) {
		bres--; *bres &= (ONES >> (WORDSIZE - mod(nbits)));
	}
}


/*
 * b_or:
 *	bitwise OR on two bit strings
 */
void
b_or(bits1, bits2, bres, nbits)
Word *bits1, *bits2, *bres;
Int nbits;
{
	r_Int	len;

	/*
	 * OR together, a word-at-a-time, starting from least sig word 
	 */
	for (len = 0; len < nbits; len += WORDSIZE)
		*bres++ = *bits1++ | *bits2++;

	/*
	 * Mask out junk in the most significant word
	 */
	if (mod(nbits) != 0) {
		bres--; *bres &= (ONES >> (WORDSIZE - mod(nbits)));
	}
}


/*
 * b_xor:
 *	bitwise XOR on two bit strings
 */
void
b_xor(bits1, bits2, bres, nbits)
Word *bits1, *bits2, *bres;
Int nbits;
{
	r_Int	len;

	/*
	 * XOR together, a word-at-a-time, starting from least sig word 
	 */
	for (len = 0; len < nbits; len += WORDSIZE)
		*bres++ = *bits1++ ^ *bits2++;

	/*
	 * Mask out junk in the most significant word
	 */
	if (mod(nbits) != 0) {
		bres--; *bres &= (ONES >> (WORDSIZE - mod(nbits)));
	}
}


/*
 * b_not:
 *	Invert all bits in a bit string
 */
void
b_not(bits, nbits)
Word *bits;
Int nbits;
{
	r_Int	len;

	/*
	 * Invert, a word-at-a-time, starting from least sig word 
	 */
	for (len = 0; len < nbits; len += WORDSIZE, bits++)
		*bits = ~(*bits);

	/*
	 * Mask out junk in the most significant word
	 */
	if (mod(nbits) != 0) {
		bits--; *bits &= (ONES >> (WORDSIZE - mod(nbits)));
	}
}

/*
 * b_next_perm:
 *	Determine next permutation of a single Word bit-string
 */
void
b_next_perm(bits, gaps, counter)
Word *bits,gaps;
Word counter;
{
	int	i;

	for (i = 0; gaps != 0; gaps >>= 1, i++) {
		if ((gaps & 1) != 0) {
			if ((counter & 1) == 0)
				*bits &= ~(1 << i);
			else
				*bits |= (1 << i);
			counter >>= 1;
		}
	}
}
