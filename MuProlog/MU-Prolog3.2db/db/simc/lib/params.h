/*
 * params.h  -  deductive database package (configuration paramaters)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 */

#ifndef	PARAMS_H
#define	PARAMS_H

/*
 * WORDSIZE indicates how many bits are contained in one word of memory
 * in the underlying machine (i.e. the number of bits in a "C" "int").
 * It determines the size of hash values, and the addressing scheme for
 * dsimc databases.
 * If WORDSIZE is a power of two, then WORDPOWER should reflect this
 * (i.e. whenever WORDSIZE == 2^N, WORDPOWER == N).
 */
#ifndef WORDSIZE
#define WORDSIZE	32
#define WORDPOWER	5
#define	BYTESIZE	8
#endif

/*
 * PAGESIZE should be set to be equal to the block size of the
 * underlying file system (i.e. the number of bytes in a disk block).
 * It is used as a basis for rounding up segment sizes, offsets, etc.
 * so that they fit exactly into a number (usually one) of disk blocks.
 */
#ifndef PAGESIZE
#define	PAGESIZE	4096
#endif

#endif PARAMS_H
