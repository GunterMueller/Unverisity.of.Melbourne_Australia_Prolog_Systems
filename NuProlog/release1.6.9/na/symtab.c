/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * symtab.c  -  Nepolog assembler (symbol table functions)
 */

#include "na.h"

Symbol	symbols[SYMTABSIZE];

/*
 * hash:
 *	Hash an identifier into a (32-bit?) Word
 */
int
hash(ident)
char *ident;
{
	_char	*key;
	_int	i, j, knt;
	_Word	xyzzy;

	/*
	 * Generate seed for random numbers using key
	 */
	xyzzy = 0;
	key = ident;
	for (i = 0; *key != '\0'; i++, key++) {
		/* get rid of any sign extension */
		j = *key & 0377;
		/* knt is a sort of sliding window ... */
		knt = (i + ((i & 3) << 3)) & 037;
		/* rotate the stripped *key by knt */
		xyzzy ^= (j << (knt) | j >> (32 - knt));
	}
	return((int)(xyzzy&0x7fffffff));
}

/*
 * look_for:
 *	Return a pointer to a SymTab entry for a symbol in the table
 *	Returns NULL if there isn't one
 */
SymTab *
look_for(name)
char *name;
{
	_int	i;
	_Symbol	*l, *lprev;
	_SymTab	*s;

	i = hash(name) % SYMTABSIZE;

	if (symbols[i].sym_rec == -1)
		l = &symbols[i];
	else {
		for (l = &symbols[i]; l != SymbolNULL; lprev = l, l = l->sym_next) {
			s = symtab + l->sym_rec;
			if (streq(strings+s->sym_name, name))
				return(s);
		}
		if ((l = mkSymbol()) == SymbolNULL)
			fatal("run out of room to make new symbols");
		lprev->sym_next = l;
	}
	l->sym_rec = xsymtab - symtab;
	l->sym_next = SymbolNULL;
	return(xsymtab);
}

/*
 * add_atom:
 *	Find entry for an atom in symbol table;
 *	Return location of entry; add if not there already
 */
int
add_atom(name)
char *name;
{
	_int	i;
	_char	*n, *new;
	_SymTab	*sym;
	_Word	xsym;
	char	*save_string();

	sym = look_for(name);

	/*
	 * Name already exists in symbol table
	 */
	if (sym < xsymtab)
		return(sym-symtab);
	
	/*
	 * Copy "name" into string table
	 */
	new = save_string(name);

	/*
	 * Add new symbol table entry for "name"
	 */
	if ((xsymtab - symtab)*sizeof(SymTab) >= SYMTAB_SIZE)
		grow_work_area(symtab, xsymtab, SYMTAB_SIZE);
	xsym = xsymtab-symtab;
	xsymtab->sym_name = new - strings;
	xsymtab->sym_module = module_name - strings;
	xsymtab->sym_functors = lUNUSED;
	xsymtab->sym_atom = (Atom*)NULL;
	xsymtab++;
	return(xsym);
}


/*
 * init_symbols:
 *	Initialise symbol hash table
 */
void
init_symbols()
{
	_int	i;

	for (i = 0; i < SYMTABSIZE; i++)
	{
		symbols[i].sym_rec = -1;
		symbols[i].sym_next = SymbolNULL;
	}
}

