/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * areas.h  -  Nepolog assembler (work area definitions)
 */

#ifndef AREAS_H
#define AREAS_H

#include "defs.h"

/*
 * Macros
 */

#define	grow_work_area(BASE,TOP,SIZE) \
	_grow_work_area((char **)(&BASE), (char **)(&TOP), (int *)(&SIZE))

/*
 * Global variables
 */

extern	Word	*code;		/* start of code segment for module */
extern	Word	*data;		/* start of data segment for module */
extern	Word	*heap;		/* start of junk heap for module */
extern	char	*types;		/* start of code type-info segment for module */
extern	char	*dtypes;	/* start of data type-info segment for module */
extern	char	*htypes;	/* start of heap type-info segment for module */
extern	SymTab	*symtab;	/* start of symbol table segment for module */
extern	char	*strings;	/* start of string table segment for module */
#if 0
extern	LabTab	*labels;	/* start of local labels segment for module */
#endif

extern	Word	*xcode;		/* current loc in code segment */
extern	Word	*xdata;		/* current loc in data segment */
extern	Word	*xheap;		/* current loc in junk heap for module */
extern	char	*xtypes;	/* current loc in code type-info segment */
extern	char	*xdtypes;	/* current loc in data type-info segment */
extern	char	*xhtypes;	/* current loc in heap type-info segment */
extern	SymTab	*xsymtab;	/* current loc in symbol table segment */
extern	char	*xstrings;	/* current loc in string table segment */

extern	int	code_start;	/* start offset of pred in code area */
extern	int	types_start;	/* start offset of pred types */
extern	int	heap_start;	/* start offset of pred in heap */
extern	int	htypes_start;	/* start offset of pred types in heap */

extern	unsigned int	CODE_SIZE;	/* # bytes in code segment */
extern	unsigned int	DATA_SIZE;	/* # bytes in data segment */
extern	unsigned int	HEAP_SIZE;	/* # bytes in junk heap segment */
extern	unsigned int	TYPES_SIZE;	/* # bytes in code type-info segment */
extern	unsigned int	DTYPES_SIZE;	/* # bytes in data type-info segment */
extern	unsigned int	HTYPES_SIZE;	/* # bytes in heap type-info segment */
extern	unsigned int	SYMTAB_SIZE;	/* # bytes in symbol table segment */
extern	unsigned int	STRINGS_SIZE;	/* # bytes in string table segment */

#endif /* AREAS_H */
