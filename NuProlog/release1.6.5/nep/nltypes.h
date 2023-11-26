/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

#ifndef NLTYPES_H
#define NLTYPES_H

#include "defs.h"

/*
 * Loader data types
 */

#define	nOPCODE		0	/* opcode */

#define	nSTRING		1	/* .STRING '...' */

#define	nNUMBER		2	/* $number */
#define	nATOM		3	/* $atom */
#define	nSTRUCT		4	/* $atom/arity */

#define	nLABEL		5	/* &number */
#define	nATLAB		6	/* &atom */
#define	nPROCLAB	7	/* &atom/arity */

#define	nNUMBER_NINE	8	/* unused, for obscure historical reasons */
#define nTAGREF		9	/* #(REF,...) */
#define nTAGDEL		10	/* #(DEL,...) */
#define nTAGLST		11	/* #(LST,...) */
#define nTAGCHR		12	/* #(CHR,...) */
#define nTAGSTR		13	/* #(STR,...) */
#define nTAGUREF	14	/* #(UREF,...) */
#define nTAGUDEL	15	/* #(UDEL,...) */

#define	nFUNCSYM	16	/* functor sym tab index */
#define	nFUNCARITY	17	/* functor arity */
#define	nFUNCCODE	18	/* functor code */
#define	nFUNCNEXT	19	/* functor next index */

#define	nHASHTAB	20	/* -> &($x:&L1, $x2:&L2, ... ) */
#define	nHASHNEXT	21	/* hash table next index */
#define	nHASHDEFAULT	22	/* hash table default label */

#define	nFUNCREF	23	/* &&foreign_function */

#define	nREGS		32	/* when ops have extra registers */

#define	nFLOAT		33	/* $float */
#define	nFVAL		34	/* float value stored in data area */
#define	nINT32		35	/* $32-bit-int */
#define	nINT32VAL	36	/* 32-bit-int value stored in data area */
#define	nBIT32		37	/* $$32-bit-int */

#define	nUNUSED		(0xFF)	/* unused arities */
#define	nUNDEF		(~0)	/* undefined locations */

#define	nMAGIC		42	/* magic number for nepolog load modules */

#define	lUNDEFINED	(Word)(0x80000000)
#define	lADDRESS	(Word)(0x7FFFFFFF)
#define	lUNUSED		(Word)(lUNDEFINED|lADDRESS)

/*
 * Header for Nepolog load modules
 */

typedef struct	Header {
	Word	nl_magic;	/* magic number for Nepolog load modules */
	Word	nl_version;	/* version number of Nepolog engine */
	Word	nl_code;	/* #bytes in code segment */
	Word	nl_data;	/* #bytes in data segment */
	Word	nl_heap;	/* #bytes in junk heap segment */
	Word	nl_types;	/* #bytes in code types segment */
	Word	nl_dtypes;	/* #bytes in data types segment */
	Word	nl_htypes;	/* #bytes in heap types segment */
	Word	nl_symtab;	/* #bytes in symtab segment */
	Word	nl_strings;	/* #bytes in strings segment */
} Header;

/*
 * Label table entries for Nepolog load modules
 */

struct LabTab {
	Word	lab_code;
	Word	lab_data;
	Word	lab_heap;
};

typedef struct LabTab	LabTab;

/*
 * Symbol table entries for Nepolog load modules
 */

struct	SymTab {
	Word	sym_name;	/* string table index for symbol name */
	Word	sym_module;	/* string table index for symbol module name */
	Word	sym_functors;	/* index to functor entries in junk heap */
	Atom	*sym_atom;	/* pointer to Atom for symbol at load time */
};

typedef struct SymTab	SymTab;

#endif /* NLTYPES_H */
