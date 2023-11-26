/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
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

#define	WordNULL	(Word *)NULL

typedef	char		*String;
#define	StrNULL		(char *)NULL

typedef unsigned	Bool;
#define	BoolNULL	(Bool *)NULL
#ifndef TRUE
#define	FALSE		0
#define	TRUE		1
#endif

typedef FILE		*File;
#define	FileNULL	(FILE *)NULL

#ifdef BSD4

typedef	caddr_t		Addr;
#define	AddrNULL	(Addr)NULL

typedef	off_t		Faddr;
#define	FaddrNULL	(Faddr)(-1)

#else /* !BSD4 */

typedef	char		*Addr;
#define	AddrNULL	(Addr)NULL

typedef	long		Faddr;
#define	FaddrNULL	(Faddr)(-1)

#endif /* BSD4 */

#define	ChNULL		('\0')

#define	K		1024


/*
 *  Macros
 */

#ifndef when
#define	when		break; case
#endif
#define	otherwise	break; default
#define streq(s1,s2)	(strcmp(s1,s2) == 0)
#define	mkstr(length)	((String)talloc((unsigned)(length)+1))
#define	mknew(Type)	((Type *)talloc((unsigned)sizeof(Type)))
#define	bytesin(first,last,Type) \
			(((last)-(first))*sizeof(Type))
#define	wordsin(first,last,Type) \
			(((last)-(first))*(sizeof(Type)/sizeof(Word)))
#define	aligned(bytes)	((bytes & (4-1))==0?(bytes):(((bytes) + 4) & (~(4-1))))

#endif /*	DEFS_H */
