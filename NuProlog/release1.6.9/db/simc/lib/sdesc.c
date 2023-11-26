/*
 * sdesc.c  -  deductive database package (segment descriptor operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

/*
 * sdesc_clear:
 *	Clear segment descriptors for splitting in DSIMC
 */
sdesc_clear(rel, hash)
Reln *rel;
Int hash;
{
	r_Int	i;
	r_Word	*slice;
	r_Int	slice_size;
	r_Word	*index_block;
	r_Int	which_block;
	r_Int	block_offset;
	r_Int	seg_within_block;
	r_Int	segno;

#if 0
fprintf(stderr,"sdesc_clear %s\n",dbrelname(rel));
#endif
	/*
	 * Allocate a buffer for the segment descriptor block
	 */
	if ((index_block = (Word *)malloc(block_size(rel))) == WordNULL)
		fatal("Can't clear segment descriptors for splitting");

	/*
	 * Work out which level 0 segment we are associated with
	 * Plus a couple of other useful constants
	 */
	segno = segmentOfHash(rel, hash, 0);
	slice_size = segs_per_block(rel);
	seg_within_block = segno & (segs_per_block(rel)-1);

	/*
	 * Determine which block for this segment and read it in
	 */
	which_block = segno >> log2_segs_per_block(rel);
#if 0
fprintf(stderr,"seg(%d,0) @ %d in %d\n",segno,seg_within_block,which_block);
#endif
	block_offset = (Faddr)(offset(rel) + which_block*block_size(rel));
	lseek(descfile(rel), block_offset, L_SET);
	read(descfile(rel), (char *)index_block, block_size(rel));

	/*
	 * For each slice, reset the bit for this segment
	 */
	for (i = 0; i < Ds(rel); i++) {
		slice = &(index_block[i * nWords(slice_size)]);
		b_reset(seg_within_block, slice, slice_size);
	}

	/*
	 * Write out the updated segment descriptor block
	 */
	lseek(descfile(rel), block_offset, L_SET);
	free((char *)index_block);
}
