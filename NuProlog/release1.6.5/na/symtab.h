/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * symtab.h  -  Nepolog assembler (symbol table definitions)
 */

#ifndef SYMTAB_H
#define SYMTAB_H

#include "defs.h"

#define	SYMTABSIZE	4097

struct	Symbol {
	int	sym_rec;
	struct	Symbol	*sym_next;
};

typedef	struct Symbol	Symbol;
#define	_Symbol		register Symbol
#define	SymbolNULL	((Symbol *)NULL)

#define	_SymTab		register SymTab
#define	SymTabNULL	((SymTab *)NULL)

extern	Symbol	symbols[];	/* symbol hash table */

#define	mkSymbol()		((Symbol *)malloc(sizeof(Symbol)))

extern	SymTab	*look_for();
extern	void	clear_symbols();
extern	void	init_symbols();

#endif /* SYMTAB_H */

