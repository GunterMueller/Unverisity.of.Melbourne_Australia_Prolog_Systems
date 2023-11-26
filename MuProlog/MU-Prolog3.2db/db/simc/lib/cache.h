/*
 * cache.h  -  deductive database package (file/relation cache definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	CACHE_H
#define	CACHE_H

/*
 * Hash:
 *	Cache hash table chaining nodes
 *	DFile's, Segt's and Reln's must all begin with
 *	a sequence of fields identical to this structure
 */
typedef struct Hash {
	Char	*h_name;	/* ptr to "db/rel" string */
	Int	h_discrim;	/* used as length | seg# | data_file# */
	struct	Hash *h_next;	/* hash chain link */
} Hash;

#define	r_Hash		register Hash
#define	HashNULL	(Hash *)NULL

/*
 * Modes for doing cache hash table look-ups
 */
#define	STRING_MODE	0
#define	POINTER_MODE	1

/*
 * search:
 *	Look up database/relation in hash table
 */
extern	Bool	search(/* db_name, rfname, table, tab_size, ptr, loc */);

#ifdef TODO
/*
 * db_cleanup:
 *	Clean up database and unlock files when unusual condition occurs
 */
extern	void	db_cleanup();
#endif TODO

#endif CACHE_H
