/*
 * cache.c  -  deductive database package (file/relation cache operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */ 

#include "simc.h"

/*
 * search:
 *	Look up descriptor in cache hash table
 */
Bool
search(name, discrim, mode, table, table_size, pointer, location)
String name;
Int discrim;
Int mode;
Hash *table[];
Int table_size;
Hash ***pointer;
Hash **location;
{
	r_Char	*c;
	r_Int	hash;
	r_Hash	**ptr;
	r_Hash	*next;

	if (mode == STRING_MODE) {
		hash = 0;
		for (c = name; *c != ChNULL; c++)
			hash += *c;
		hash += discrim;
	}
	else
		hash = (Int)name + discrim;

	hash = (hash & 0x7fff) % table_size;

	ptr = &table[hash];
	next = table[hash];
	/*
	 * For STRING mode, we need to do full string comparison
	 * otherwise, we only need to compare pointers
	 */
	if (mode == STRING_MODE)
		while (next != NULL) {
			if (discrim == next->h_discrim &&
				streq(name,next->h_name)) {
				*location = next;
				*pointer = ptr;
				return(TRUE);
			}
			else {
				ptr = &(next->h_next);
				next = next->h_next;
			}
		}
	else
		while (next != NULL) {
			if (discrim == next->h_discrim &&
				name == next->h_name) {
				*location = next;
				*pointer = ptr;
				return(TRUE);
			}
			else {
				ptr = &(next->h_next);
				next = next->h_next;
			}
		}

	*location = NULL;
	*pointer = ptr;
	return(FALSE);
}

#ifdef TODO
/*
 * db_cleanup:
 *	Clean up caches and unlock files when unusual condition occurs
 */
void
db_cleanup()
{
	/*
	 * Traverse rdesc_table looking for open relation descriptor files
	 */

	/*
	 * Traverse dfile_table looking for open data segments
	 */
}
#endif /* TODO */
