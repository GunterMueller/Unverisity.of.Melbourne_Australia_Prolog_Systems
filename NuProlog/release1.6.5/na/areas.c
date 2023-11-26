/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * areas.c  -  Nepolog assembler (work areas)
 */

#include "na.h"

Word	*code;			/* start of code segment for module */
Word	*data;			/* start of data segment for module */
Word	*heap;			/* start of junk heap for module */
char	*types;			/* start of code type-info segment for module */
char	*dtypes;		/* start of data type-info segment for module */
char	*htypes;		/* start of heap type-info segment for module */
SymTab	*symtab;		/* start of symbol table segment for module */
char	*strings;		/* start of string table segment for module */

Word	*xcode;			/* current loc in code segment */
Word	*xdata;			/* current loc in data segment */
Word	*xheap;			/* current loc in junk heap for module */
char	*xtypes;		/* current loc in code type-info segment */
char	*xdtypes;		/* current loc in data type-info segment */
char	*xhtypes;		/* current loc in heap type-info segment */
SymTab	*xsymtab;		/* current loc in symbol table segment */
char	*xstrings;		/* current loc in string table segment */

int	code_start;		/* start offset of pred in code area */
int	types_start;		/* start offset of pred types */
int	heap_start;		/* start offset of pred in heap */
int	htypes_start;		/* start offset of pred types in heap */

#undef	K
#define	K 1024
unsigned int	CODE_SIZE = 64*K;	/* default # bytes in code */
unsigned int	DATA_SIZE = 16*K;	/* default # bytes in data */
unsigned int	HEAP_SIZE = 16*K;	/* default # bytes in junk heap */
unsigned int	TYPES_SIZE = 16*K;	/* default # bytes in code type-info */
unsigned int	DTYPES_SIZE = 4*K;	/* default # bytes in data type-info */
unsigned int	HTYPES_SIZE = 4*K;	/* default # bytes in heap type-info */
unsigned int	SYMTAB_SIZE = 32*K;	/* default # bytes in symbol table */
unsigned int	STRINGS_SIZE = 32*K;	/* default # bytes in string table */


/*
 * init_work_areas:
 *	Allocate space for various segments of object file
 *	Assumes "malloc" always returns word-aligned address
 */
init_work_areas()
{
	if ((code = (Word *)malloc(CODE_SIZE)) == (Word *)NULL)
		fatal("can't allocate code area: %d bytes", CODE_SIZE);
	if ((data = (Word *)malloc(DATA_SIZE)) == (Word *)NULL)
		fatal("can't allocate data area: %d bytes", DATA_SIZE);
	if ((heap = (Word *)malloc(HEAP_SIZE)) == (Word *)NULL)
		fatal("can't allocate heap area: %d bytes", HEAP_SIZE);
	if ((types = (char *)malloc(TYPES_SIZE)) == (char *)NULL)
		fatal("can't allocate types area: %d bytes", TYPES_SIZE);
	if ((dtypes = (char *)malloc(DTYPES_SIZE)) == (char *)NULL)
		fatal("can't allocate dtypes area: %d bytes", DTYPES_SIZE);
	if ((htypes = (char *)malloc(HTYPES_SIZE)) == (char *)NULL)
		fatal("can't allocate htypes area: %d bytes", HTYPES_SIZE);
	if ((symtab = (SymTab *)malloc(SYMTAB_SIZE)) == (SymTab *)NULL)
		fatal("can't allocate symtab area: %d bytes", SYMTAB_SIZE);
	if ((strings = (char *)malloc(STRINGS_SIZE)) == (char *)NULL)
		fatal("can't allocate strings area: %d bytes", STRINGS_SIZE);
}

/*
 * _grow_work_area:
 *	If we run out of space in an area, make a bigger one
 */
_grow_work_area(base, top, size)
char **base;
char **top;
int *size;
{
	char	*new, *aname;
	Word	*nw, *ow, *ew;

#if 0
	/*
	 * Work out which are is being shifted for debugging
	 */
	if (base == &code) aname = "code";
	else if (base == &data) aname = "data";
	else if (base == &heap) aname = "heap";
	else if (base == &types) aname = "code type";
	else if (base == &dtypes) aname = "data type";
	else if (base == &htypes) aname = "heap type";
	else if (base == &symtab) aname = "symbol table";
	else if (base == &strings) aname = "string";
#endif

	/*
	 * Make a new area, twice as big as the old
	 */
	if ((new = (char*)malloc(*size*2)) == NULL)
		fatal("can't increase size of area");
	
#if 0
fprintf(stderr,"grow %s area from %d @ %x to %d @ %x\n",
		aname, *size, *base, *size*2, new);
#endif
#if 0
	bcopy(*base, new, *size);
#else
	nw = (Word *)new;
	ow = (Word *)*base;
	ew = ow + *size/sizeof(Word);
	while (ow < ew)
		*nw++ = *ow++;
#endif

	free(*base);

	*top = new + (*top-*base);
	*base = new;
	*size *= 2;
}
