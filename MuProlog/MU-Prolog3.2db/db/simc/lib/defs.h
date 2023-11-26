/*
 * defs.h  -  deductive database package (syntactic sugar for C)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	DEFS_H
#define	DEFS_H

#include <stdio.h>

/*
 *  Storage classes
 */

/*
 * For variables in functions
 */
#define	reg	register
#define	local	auto
#define	global	extern
#define	fixed	static

/*
 * For global variables & functions
 */
#define	public
#define	private static

/*
 *  Data types
 */

typedef	int		Int;
typedef	unsigned	Word;
typedef	long		Long;
typedef	short		Short;
typedef	float		Real;
typedef	char		Char;
typedef	char		*String;
typedef unsigned	Bool;
typedef	FILE		*FileP;
typedef	int		File;
#ifdef	BSD4
#define	Addr		caddr_t
#define	Faddr		off_t
#define	Void		void
#else	/*sys5?*/
typedef	char		*Addr;
typedef	long		Faddr;
typedef int		Void;
#endif	/*BSD4*/

#define	r_Int	register int
#define	r_Word	register unsigned
#define	r_Long	register long
#define	r_Short	register short
#define	r_Real	register float
#define	r_Char	register char
#define	r_Str	register String
#define	r_Bool	register Bool
#define	r_File	register File
#define	r_FileP	register File
#define	r_Addr	register Addr
#define	r_Faddr	register Faddr

#ifndef TRUE
#define	FALSE	0
#define	TRUE	1
#endif

#define	IntNULL		(Int *)NULL
#define	WordNULL	(Word *)NULL
#define	LongNULL	(Long *)NULL
#define	ShortNULL	(Short *)NULL
#define	RealNULL	(Real *)NULL
#define	ChNULL		('\0')
#define	StrNULL		(String)NULL
#define	BoolNULL	(Bool *)NULL
#define	FileNULL	(File)-1
#define FilePNULL	(FileP)NULL
#define	AddrNULL	(Addr)NULL
#define	FaddrNULL	(Faddr)-1

/*
 *  Macros
 */

/* #define	malloc(nbytes)	calloc(1,(nbytes)) */

#define	or		else if
#define	when		break; case
#define	otherwise	break; default
#define loop		for (;;)
#define streq(s1,s2)	(strcmp(s1,s2) == 0)
#define	mkstr(length)	((String)malloc((unsigned)(length)+1))
#define	mknew(Type)	((Type *)malloc((unsigned)sizeof(Type)))
#define	cfree(ptr,Type)	{ if ((ptr) != (Type *)NULL) \
				free((char *)(ptr)); \
			  (ptr) = (Type *)NULL; }
#define	isopen(fd)	((fd) > FileNULL)
#define	alignup(v,n)	((((v) + (n) - 1) / (n)) * (n))

#define	XX		/* mark something as unnecessary */

#endif	DEFS_H
