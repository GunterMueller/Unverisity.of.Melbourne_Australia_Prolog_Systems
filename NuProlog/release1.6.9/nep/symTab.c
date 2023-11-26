/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"

static Word
hash(s)
register char *s;
{
	register Word h;

	for(h = 0; *s != '\0'; s++)
		h += (h << 1) + *s;
	h += h << 4;
	h += h << 8;
	h += h >> 5;

	return(h > 0 ? h : -h);
}

/*
 * Make a new Symbol Table with space for n entries at full load.
 */
SymbolTable *
newSymbolTable(n, name)
register Word n;
Atom *name;
{
	register SymbolTable *st;
	register Word i, sn;
	register Atom **a;

	if(n < SYMTABMIN)
		n = SYMTABMIN;
	else if(n >= SYMTABMAX)
		panic("Symbol table too big");
	sn = 1 << n;

	st = (SymbolTable *) malloc(sizeof(SymbolTable));
	st->st_name = StarToAtom(name);
	st->st_size = n;
	st->st_entries = 0;
	st->st_load = (Word) (sn * SYMTABLOAD);
	st->st_table
		= a
		= (Atom **) malloc(sn * sizeof(Atom *));
	for(i = 0; i < sn; i++)
		*a++ = (Atom *) NULL;

	return(st);
}

/*
 * Double the size of a Symbol Table.
 */
static void
growSymbolTable(st)
SymbolTable *st;
{
	register Word i, sn, osn;
	Word n, on;
	register Atom **a, **oa;

	on = st->st_size;
	oa = st->st_table;
	osn = 1 << on;

	n = on + 1;
	if(n >= SYMTABMAX)
		panic("Symbol table too big");
	sn = 1 << n;

	st->st_size = n;
	st->st_entries = 0;
	st->st_load = (Word) (sn * SYMTABLOAD);
	st->st_table
		= a
		= (Atom **) malloc(sn * sizeof(Atom *));
	for(i = 0; i < sn; i++)
		*a++ = (Atom *) NULL;

	for(i = 0; i < osn; i++, oa++)
		if(*oa != (Atom *) NULL)
			(void)enterAtom(eCharStar((*oa)->a_pname), st, *oa);
	free((char *) (oa - osn));
}

/*
 * Make a new Atom with pname s in module.
 */
static Atom *
newAtom(s, module)
char *s;
Object module;
{
	register Atom *a;

	a = (Atom *)Dalloc(sizeof(Atom));
	a->a_header = StrHeaderAtomHeader;
	a->a_pname = (s[0] == '\0') ? NIL : MakeString(storeString(s));
	a->a_module = module;
	a->a_functors = NIL;
	a->a_prop = NIL;
	a->a_prec = NIL;

	return(a);
}

/*
 * Look up a string in a SymbolTable, returning the address of its
 * entry, or of the place where it would be inserted if one of that
 * name is not already there, and expanding the table if necessary.
 *
 * Relys on our never completely filling a SymbolTable.
 */
static Atom **
lookupAtom(s, table)
char *s;
SymbolTable *table;
{
	register Atom **a;
	register Word h, size;

	start:
		size = 1 << table->st_size;
		h = hash(s) & (size - 1);

		a = table->st_table + h;
		while(	*a != (Atom *)NULL
				&& strcmp(eCharStar((*a)->a_pname), s) != 0) {
			if(++h >= size) {
				h = 0;
				a = table->st_table;
			} else
				a++;
		}

		if(*a == (Atom *) NULL && table->st_entries >= table->st_load) {
			growSymbolTable(table);
			goto start;
		}

	return(a);
}

/*
 * Look up a string in a Symbol Table, creating a new Atom
 * if one of that name is not already there, and expanding the
 * table if necessary.
 */
Atom *
enterAtom(s, table, existingAtom)
char *s;
SymbolTable *table;
Atom *existingAtom;
{
	register Atom **a;

	if(s == (char *) NULL)
		panic("Null pointer given to enterAtom()");
	a = lookupAtom(s, table);
	if(*a == (Atom *) NULL) {
		table->st_entries++;
		if(existingAtom == (Atom *) NULL)
			*a = newAtom(s, MakeBlock(table));	/* BUG.  Short-term fudge.  */
		else
			*a = existingAtom;
	}

	return(*a);
}

Instruction undefinedPredicate[] = {
	MakeIns2(cERROR, 1, 3)
};

/*
 * Make a new Functor.
 */
static Functor *
newFunctor(flags, atom, arity, next)
int flags, arity;
Atom *atom;
Object next;
{
	register Functor *f;

	/* Where should we put functors? */
	f = (Functor *)Talloc(sizeof(Functor));
	f->f_codeType = flags;
	f->f_sourceFile = NIL;
	f->f_arity = arity;
	f->f_code = undefinedPredicate;
	if(IsBlock(next)) 
		f->f_nextf = (Functor *) eRef(next);
	else
		f->f_nextf = (Functor *) NULL;
	f->f_functor = atom;

	return(f);
}

/*
 * Look up an arity in a Functor list.
 */
Functor *
lookupFunctor(arity, functors)
register int arity;
Object functors;
{
	register Functor *f;

	if(IsBlock(functors)) {
		for(	f = (Functor *) eRef(functors);
				f != (Functor *) NULL;
				f = f->f_nextf) {
			if(f->f_arity == arity)
				break;	
		}
		return(f);
	} else
		return((Functor *) NULL);
}

/*
 * Lookup an arity in the Functor list of an atom, making a new Functor
 * if it is not found.  The flags of a new functor are set all off.
 */
Functor *
enterFunctor(arity, atom)
int arity;
register Atom *atom;
{
	register Functor *f;

	f = lookupFunctor(arity, atom->a_functors);
	if(f == (Functor *) NULL) {
		f = newFunctor(0, atom, arity, atom->a_functors);
		atom->a_functors = StarToBlock(f);
	}
	return(f);
}

Object
p_predicateArities(atom)
Atom *atom;
{
	register Functor *f;
	register Object l;

	l = NIL; 
	if(!IsBlock(atom->a_functors))
		return(l);
	for(	f = (Functor *)eRef(atom->a_functors);
			f != (Functor *) NULL;
			f = f->f_nextf)
		l = pushCons(MakeSmallInt(f->f_arity), l);
	return(l);
}
