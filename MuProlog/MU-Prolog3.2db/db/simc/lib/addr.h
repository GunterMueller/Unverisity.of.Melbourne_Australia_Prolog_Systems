/*
 * addr.c  -  deductive database package (address translation definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by James Thom, Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef ADDR_H
#define ADDR_H

/*
 * The maximum number of bits we use in converting hash codes to
 * segment addresses is one less than the number of bits in a word,
 * to avoid problems generating negative segment numbers
 */
#define MAXBITS	((WORDSIZE)-1)

/*
 * segmentOfHash
 *	Given hash value, work out address of corresponding
 *	segment at a given level
 *	Duplicates some functionality of set_segment()
 */
extern	Int	segmentOfHash(/* relation, hash, level */);

/*
 * ovflowSegment:
 *	Given a segment,level pair, work out the corresponding
 *	overflow segment on a higher level
 */
extern	Int	ovflowSegment(/* relation, segno, level, ovlevel */);

#endif ADDR_H
