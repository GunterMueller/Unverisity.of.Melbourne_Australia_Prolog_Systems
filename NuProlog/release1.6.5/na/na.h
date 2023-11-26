/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * na.h  -  global definitions
 */

#ifdef PNP
#undef PNP
#endif /* PNP */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/file.h>
#include "version.h"
#include "machdep.h"
#include "defs.h"
#include "types.h"
#include "bytecodes.h"
#include "syms.h"
#include "nltypes.h"
#include "areas.h"
#include "labels.h"
#include "symtab.h"
#ifdef BSD4
#include <strings.h>
#else
#include <string.h>
#endif

/*
 * Macros
 */

#define	_int	register int
#define	_char	register char
#define	_float	register float
#define	_Word	register Word

/*
 * Assembler pseudo-ops
 */

#define	pWORD		0
#define	pCLAUSE		1
#define pSTRING		2

/*
 * Global variables
 */

extern	char	*my_name;	/* name by which "na" invoked */

extern	char	*src_name;	/* name of source file for current module */
extern	File	src_file;	/* source code file pointer */

extern	char	obj_name[];	/* name of object file for current module */
extern	int	obj_file;	/* object code file descriptor */

extern	Header	module_head;	/* header information for module */
extern	char	*module_name;	/* name of module being processed */

extern	int	naerrs;		/* how many assembler errors so far */
extern	int	mergeflg;	/* merge some instruction pairs */

/*
 * Parser & lexical analyser variables
 */

#ifdef LINE_BUFFER
extern	char	xxline[];	/* current input line buffer */
extern	char	*xxlineptr;	/* current char in "xxline" */
extern	int	xxnchars;	/* counts chars in current line */
extern	Bool	xxendline;	/* flag for end of source program line */
#endif /* LINE_BUFFER */
extern	Bool	xxendfile;	/* flag for end of source program file */

extern	File	yyin;		/* parser input file pointer */
extern	int	yylineno;	/* parser line counter in source program */
extern	int	yyerrs;		/* count of errors - no code if errs */
