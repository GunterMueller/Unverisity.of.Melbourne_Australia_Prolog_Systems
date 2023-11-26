/*
 * bits.h  -  deductive database package (bit string definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	BITS_H
#define	BITS_H

#include "defs.h"
#include "params.h"


/*
 * A bit string is simply a sequence of bits, interpreted as follows:
 *
 * Most significant                Least significant
 * +-------------------- ..... --------------------+
 * |                                               |
 * +-------------------- ..... --------------------+
 *  N                                             0
 *
 * Bit strings are implemented as contiguous sequences of Words, where
 *  a Word should be the "natural" unit of operation within the machine,
 *  e.g. a 32-bit word on a Vax, a 16-bit word on a PDP-11, etc.
 *
 * The Words in a bit string are interpreted as follows:
 *
 *  Low memory addresses        High memory addresses
 *  +--------+ +--------+ ..... +--------+ +--------+
 *  |        | |        |       |        | |XX      |
 *  +--------+ +--------+ ..... +--------+ +--------+
 *   ^      ^   ^      ^                      ^    ^
 *   n-1    0  2n-1    n                      N  N%n
 *
 * Note that, because of quantization, not all of the bits in the
 *  "most significant" Word of a bit-string may be used; those not
 *  used (marked with "X" above) will always be set to zero.
 */


/*
 * Machine dependent constants associated with bit-strings
 * (these values will be overidden by the values in "params.h")
 */

#ifndef WORDSIZE
#define WORDSIZE	32	/* how many bits in a Word */
#define	BYTESIZE	8	/* how many bits in a byte (char) */
#define	WORDPOWER	5	/* set to N if WORDSIZE == 2^N */
#endif

#define	ONES		((Word)~0) /* a Word full of ONEs */


/*
 * Functions on bit strings (most are implemented as macros)
 */

/*
 * nWords:
 *	Determine how many Words/bytes we need to hold N bits
 *	The first method is fast but relies on WORDSIZE being a power of two
 *	The second method is general (but relatively slow)
 */
#ifdef	WORDPOWER
#define	div(N)		((N) >> WORDPOWER)
#define	mod(N)		((N) & (WORDSIZE-1))
#define	floor(N)	((N) & ~(WORDSIZE-1))
#else /*	GENERAL_CASE */
#define	div(N)		((N) / WORDSIZE)
#define	mod(N)		((N) % WORDSIZE)
#define	floor(N)	(((N) / WORDSIZE) * WORDSIZE)
#endif /*	WORDPOWER */

#define	nWords(N)	((mod((N)) == 0) ? div((N)) : div((N))+1)
#define	nBytes(N)	(nWords((N)) * sizeof(Word))

#define	WordsInLong	(sizeof(Long)/sizeof(Word))

/*
 * powerOfTwo:
 *	Determine whether an Int value is a power of two
 *	Uses b_count to tally bits set (should be one)
 */
#define	powerOfTwo(I)	(b_count(&(I), WORDSIZE) == 1)

/*
 * b_make:
 *	Create a new bit string (with all bits ZERO)
 */
extern	Word	*b_make(/* nbits */);

/*
 * b_free:
 *	Release space occupied by bit string
 */
#define	b_free(bits)	cfree((bits), Word)

/*
 * b_fprint:
 *	Print a bit string as hex digits
 */
extern	void	b_fprint(/* file, bits, nbits */);

/* 
 * b_print:
 *	Print bit string as hex digits on standard output
 */
#define	b_print(bits,nbits)	b_fprint(stdout,bits,nbits)

/*
 * b_word:
 *	Return index of Word for given bit
 */
#define	b_word(which)	div(which)

/*
 * b_pos:
 *	Return bit position within Word of given bit
 */
#define	b_pos(which)	mod(which)

/*
 * b_bit:
 *	Return a Word with a ONE in the position of specified bit
 */
#define	b_bit(which)	(1 << b_pos(which))

/*
 * b_read:
 *	read in a bit string
 */
#define	b_read(fd, bits, nbits) \
			read(fd, bits, nBytes(nbits))

/*
 * b_write:
 *	Write out a bit string
 */
#define	b_write(fd, bits, nbits) \
			write(fd, bits, nBytes(nbits))

/*
 * b_copy:
 *	Copy contents of one bit string to another
 */
#define	b_copy(bsrc, bdest, nbits) \
	{ Word *_src = (bsrc); Word *_dest = (bdest); Int _len;\
		for (_len = 0; _len < (nbits); _len += WORDSIZE) \
			*_dest++ = *_src++; }

/*
 * b_count:
 *	Count how many bits are set to ONE
 */
extern	Int	b_count(/* bits, nbits */);

/*
 * for_every_bit:
 *	Generate a sequence of positions of set bits in the bit-string
 */
#define	for_every_bit(var, bits, nbits) \
	for (var = b_nextset(0,bits,nbits); \
		var >= 0; \
		var = b_nextset(var+1,bits,nbits))

/*
 * for_each_bit:
 *	Generate a sequence of positions of set bits in part of a bit-string
 */
#define	for_each_bit(var, bits, nbits, lo, hi) \
	for (var = b_nextset(lo,bits,nbits); \
		var >= 0 && var <= hi; \
		var = b_nextset(var+1,bits,nbits))

/*
 * b_nextset:
 *	Return position of next ONE bit
 */
extern	Int	b_nextset(/* start_pos, bits, nbits */);

/*
 * b_test:
 *	Test value of specified bit
 */
#define	b_test(which, bits, nbits) \
	(((which) < 0 || (which) > (nbits)) ? FALSE : \
		(bits[b_word(which)] & b_bit(which)) == 0 ? FALSE : TRUE)

/*
 * b_set:
 *	Set the value of specified bit to ONE
 */
#define	b_set(which, bits, nbits) \
	if ((which) < 0 || (which) >= nbits) \
		; \
	else \
		bits[b_word(which)] |= b_bit(which);
#if 0
	(((which) < 0 || (which) >= (nbits)) ? (void)0 : \
		(void)(bits[b_word(which)] |= b_bit(which)))
#endif

/*
 * b_reset:
 *	Reset value of specified bit to ZERO
 */
#define	b_reset(which, bits, nbits) \
	if ((which) < 0 || (which) >= (nbits)) \
		; \
	else \
		bits[b_word(which)] &= ~b_bit(which);
#if 0
	(((which) < 0 || (which) >= (nbits)) ? (void)0 : \
		(void)(bits[b_word(which)] &= ~(b_bit(which))))
#endif

/*
 * b_zero:
 *	Reset all bits to ZERO
 */
#define	b_zero(bits, nbits) \
	{ Int _i; Word *_b; \
		for (_i = 0, _b = (bits); _i < nWords(nbits); _i++) *_b++ = 0; }

/*
 * b_ones:
 *	Set all bits to ONE
 */
#define	b_ones(bits, nbits) \
	{ Int _i; Word *_b; \
		for (_i = 0, _b = (bits); _i < nWords(nbits); _i++) \
			*_b++ = ONES; \
		if (mod(nbits) != 0) \
			{_b--; *_b &= (ONES >> (WORDSIZE - mod(nbits))); }}

/*
 * b_mask:
 *	Set all bits in range LO..HI to ONE, all others to ZERO
 */
#define	b_mask(lo, hi, bits, nbits) \
	{ Int _i; \
		b_zero(bits, nbits); \
		for (_i = (lo); _i <= (hi); _i++) b_set(_i, bits, nbits); }

/*
 * b_and:
 *	Bitwise AND on two bit strings
 */
extern	void	b_and(/* bits1, bits2, result, nbits */);

/*
 * b_or:
 *	Bitwise OR on two bit strings
 */
extern	void	b_or(/* bits1, bits2, result, nbits */);

/*
 * b_xor:
 *	Bitwise XOR on two bit strings
 */
extern	void	b_xor(/* bits1, bits2, result, nbits */);

/*
 * b_not:
 *	Invert all bits in a bit string
 */
extern	void	b_not(/* bits, nbits */);

/*
 * b_next_perm:
 *	Determine next permutation of a single Word bit-string
 */
extern	void	b_next_perm(/* bits, gaps, counter */);

/*
 * b_display:
 *	Debugging function to display a string with a label
 */
#define	b_display(label,bits,nbits) \
	{fprintf(stderr,label);b_fprint(stderr,bits,nbits);putc('\n',stderr);}

#endif /*	BITS_H */
