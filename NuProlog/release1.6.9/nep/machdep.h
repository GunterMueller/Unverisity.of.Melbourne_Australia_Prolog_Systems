/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

/* NORMAL
A machine is NORMAL if MakeRef(x) == x for all values of Word *x.
This relies on such things as tREF == 0, of course, and is defined
here because it needs to be known early in types.h.
*/
#ifdef interdata
/*
 * Bug in cc(1) on 3240 stuffs up loading addresses
 * with large displacements in some circumstances.
 *
 * register Object *v;
 * *v = StarToAtom(&SymAtom);
 * is an example.
 */
#else /* interdata */
#	define NORMAL
#endif /* interdata */

#ifdef MACHINE_SGI
#	define _BSD_COMPAT
#endif /* MACHINE_SGI */

#ifdef LBT
#	undef NORMAL
#endif /* LBT */

#ifdef MACHINE_ENCORE_BSD_EMUL
#	ifndef MACHINE_ENCORE
#		define MACHINE_ENCORE
#	endif /* MACHINE_ENCORE */
#endif /* MACHINE_ENCORE_BSD_EMUL */

#ifdef MACHINE_SUN3
#	define MACHINE "sun3"
#	define MACHINE_SUN
#endif /* MACHINE_SUN3 */
#ifdef MACHINE_SUN4
#	define MACHINE "sun4"
#	define MACHINE_SUN
#endif /* MACHINE_SUN4 */
#ifdef MACHINE_SUN4_SOLARIS
#	define MACHINE "sun4"
#	define MACHINE_SUN
#endif /* MACHINE_SUN4_SOLARIS */
#ifdef MACHINE_VAX
#	define MACHINE "vax"
#endif /* MACHINE_VAX */
#ifdef MACHINE_MACII
#	define MACHINE "macII"
#endif /* MACHINE_MACII */
#ifdef MACHINE_ENCORE
#	define MACHINE "encore"
#endif /* MACHINE_ENCORE */
#ifdef MACHINE_386
#	define MACHINE "i386"
#endif /* MACHINE_386 */
#ifdef MACHINE_ELXSI
#	define MACHINE "elxsi"
#endif /* MACHINE_ELXSI */
#ifdef MACHINE_MIPS
#	define MACHINE "mips"
#endif /* MACHINE_MIPS */
#ifdef MACHINE_SGI
#	define MACHINE "sgi"
#endif /* MACHINE_SGI */
#ifdef MACHINE_DEC
#	define MACHINE "dec"
#endif /* MACHINE_DEC */
#ifdef MACHINE_ALPHA
/* OSF defines this!
/* #	define MACHINE "alpha" */
#endif

#ifdef BSD4
#	ifndef _PageSize
#		define _PageSize getpagesize()
		extern int PageSize;
#	endif /* _PageSize */
#endif /* BSD4 */

#ifndef _PageSize
#define _PageSize 4096
#define PageSize _PageSize
#endif /* _PageSize */

#if 0
#ifdef sparc
#define PROTOVERFLOW
#endif /* sparc */
#endif /* 0 */

#ifdef INLINE
#ifdef mc68020
#define INLINEmc68020
#endif /* mc68020 */
#ifdef sparc
#define INLINEsparc
#endif /* sparc */
#endif /* INLINE */

#ifndef LITTLEENDIAN
#if sequent && i386
#define LITTLEENDIAN
#endif
#if vax || ns32000 || ns32032 || ns32332 || ns32532
#define LITTLEENDIAN
#endif
#ifdef MACHINE_DEC
#define LITTLEENDIAN
#endif
#ifdef MACHINE_ALPHA
#	define LITTLEENDIAN
#endif
#endif /* LITTLEENDIAN */

#ifndef BSD4
#define index(s, c) strchr(s, c)
#define rindex(s, c) strrchr(s, c)
#define bcopy(s1, s2, n) memcpy(s2, s1, n)
#define bcmp(s1, s2, n) memcmp(s1, s2, n)
#define bzero(s, n) memset(s, 0, n)
#endif /* BSD4 */

#ifndef HZ
#	ifdef elxsi
#		define HZ 100
#	else
#		ifndef sgi
#			define HZ 60
#		endif
#	endif
#endif

#ifdef vax
#	define ALIGNMENT 1
#else
#	define ALIGNMENT 4
#endif

#if defined(__STDC__) || (MACHINE_SGI && defined(__EXTENSIONS__))
#	define HASH_CONCAT
#	define HASH_HASH_CONCAT
#endif

#ifdef MACHINE_SUN
#	define PROTO_UMASK
#	define PROTO_CHMOD
#	define PROTO_GETPWUID
#	define PROTO_GETPWNAM
#endif

#ifdef MACHINE_SGI
#	define PROTO_LOCALTIME
#	define PROTO_GETPWUID
#	define PROTO_GETPWNAM
#endif

#ifdef MACHINE_DEC
#	define PROTO_UMASK
#	define PROTO_CHMOD
#	define PROTO_GETPWUID
#	define PROTO_GETPWNAM
#endif

#ifdef MACHINE_ALPHA
#endif

#ifdef MACHINE_ALPHA
	BUG?  Maybe just use -taso option?
	typedef long Word;
	typedef unsigned long UWord;
#else
	typedef int Word;
	typedef unsigned int UWord;
#endif

/* Real
The type in which floating-point is stored.
*/
typedef double Real;

/* MAXALIGNMENTTYPE
Usually Real.
*/
#define MAXALIGNMENTTYPE Real

/* FLOATALIGNMENTTYPE
NU-Prolog does very little floating point and space efficiency is
greatest when this is Word.  If Real requires tighter alignment
then this should be changed.
*/
#ifdef MACHINE_SUN4_SOLARIS
#	define FLOATALIGNMENTTYPE Real
#endif

#ifndef FLOATALIGNMENTTYPE
#	define FLOATALIGNMENTTYPE Word
#endif

#define BYTESIZE 8
#ifdef MACHINE_ALPHA
#	define WORDSIZE 64
#else
#	define WORDSIZE 32
#endif
#define MAXCHAR 127

/* NWORDS
Round a number of bytes up to a number of Words.
*/
#if WORDSIZE / BYTESIZE == 4
#	define NWORDS(b) ((b + 3) >> 2)
#endif
#if WORDSIZE / BYTESIZE == 8
#	define NWORDS(b) ((b + 7) >> 3)
#endif
#ifndef NWORDS
#	define NWORDS(b) ((b + sizeof(Word) - 1) / sizeof(Word))
#endif

#define ALIGNANY(x, a) ((((Word)(x)) + a - 1) & ~(a - 1))

#define ALIGN(x) ALIGNANY(x, ALIGNMENT)
#define ALIGNW(x) ALIGNANY(x, sizeof(Word))
#define ALIGNMAX(x) ALIGNANY(x, sizeof(MAXALIGNMENTTYPE))
#define ALIGNFLOAT(x) \
	(sizeof(FLOATALIGNMENTTYPE) == sizeof(Word) \
		? ((Word)(x)) : ALIGNANY(x, sizeof(FLOATALIGNMENTTYPE)))
