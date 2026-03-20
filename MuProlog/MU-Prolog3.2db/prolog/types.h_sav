/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

/*
types.h
defines all global types, constants etc.
*/

				/* local definitions, including Int */
#include "local.h"

				/* temporary? definitions */
#define plerror(i) plerrnum = i
#define error(x) printf(x)
#define isalpha(x) ('a'<=x && x<='z' || 'A'<=x && x<='Z')


#define VERSION 3200

				/* for linked lists */
#define l_next(p) (*(Ptr)(p))
#define lnext(p) ((Ptr)*(Ptr)(p))
#define litem(p) (*((Ptr)(p)+1))

#define MAXFREE 32		/* number of memory free lists */

typedef Int *Ptr;

/*
TERMS
Atoms, variables and integers are represented by single words. The top two
bits tell what the word contains. Atoms and variables have an index into
the dictionary (to find the name, clauses etc) and either the variable number
or the number of arguments (zero for atoms). Compound terms are represented
by a pointer (with the top two bits zero!) to a block of words. The first
word is the functor (the same as an atom but the number of args is non-zero)
and then there is one word for each argument.
*/

#if 1
/*
 * If your machine can't handle tampering with 1 in the most
 * significant bit, change the "#if 1" to "#if 0" to get some
 * alternative defns (see below)
 */

#define TTYPE	(Int)0xC0000000         /* top two bits defin type */
#define TAGNUM  3 /* DS: needed because Altos cannot handle longs in switches */
#define TAGSHIFT 30 /* DS: need right shift of 30 for tag */
#define TINT	TTYPE			/* integers 11 */
#define TNUM	TTYPE			/* integers 11 */
#define TSIGN	(Int)0x20000000		/* sign bit for ints */
#define TVAR	(Int)0x80000000		/* vars    10 */
#define TAGVAR	2			/* DS: used in switches */
#define TATOM	(Int)0x40000000		/* atoms   01 */
#define TAGATOM	1			/* DS: used in switches */
#define TCOMP	(Int)0x00000000		/* compound terms 00 */
#define TAGCOMP	0			/* DS: used in switches */
#define TATOMIC	(Int)0x40000000		/* atomic terms have this set (jws) */
#define TARGBTS	(Int)0x00003FFF		/* nargs, varnum in low 14 bits */
#define TDBITS	(Int)0x3FFFC000		/* bits for dictionary index */
#define TDBIT	14			/* need left shift of 14 for dict indx*/


#define tagtype(x)      ((((Int)x)>>TAGSHIFT) & 3) /* DS: used in switches */
#define ConsInt(i)	((Int)(i) | TINT)
#define XtrInt(i)       (((Int)(i)) & TSIGN ? ((Int)(i)) : ((Int)(i)) & ~TINT)
#define ConsVar(d, vn)  ((((Int)d) << TDBIT) + (vn) | TVAR)
#define ConsFunc(d, na) ((((Int)d) << TDBIT) + (na) | TATOM)

#define IsInt(i)        ((((Int)(i)) & TTYPE) == TINT)
#define IsVar(i)        ((((Int)(i)) & TTYPE) == TVAR)
#define IsAtom(i)       ((((Int)(i)) & TTYPE) == TATOM)
#define IsComp(i)       ((((Int)(i)) & TTYPE) == TCOMP)

	/* these should be restricted to certain machines */
#define IsAtomic(i)	((((Int) (i)) & TATOMIC) != 0)
#define IsFunc(i)	(((Int) i) >= 0)
	/* old version */
/* #define IsAtomic(i)	(IsAtom(i) || IsInt(i)) */
/* #define IsFunc(i)	(IsComp(i) || IsAtom(i)) */

#else

/*
 * For machines which use most significant bit for something else
 * Avoid doing anything to the top bit in these definitions
 */
#define TTYPE	(Int)0x60000000         /* second top two bits define type */
#define TAGNUM  3 /* DS: needed because Altos cannot handle longs in switches */
#define TAGSHIFT 29 /* DS: need right shift of 29 for tag */
#define TINT	TTYPE			/* integers 11 */
#define TNUM	TTYPE			/* integers 11 */
#define TSIGN	(Int)0x10000000		/* sign bit for ints */
#define RSIGN	(Int)0x80000000		/* real sign bit for ints */
#define TVAR	(Int)0x40000000		/* vars    10 */
#define TAGVAR	2			/* DS: used in switches */
#define TATOM	(Int)0x20000000		/* atoms   01 */
#define TAGATOM	1			/* DS: used in switches */
#define TCOMP	(Int)0x00000000		/* compound terms 00 */
#define TAGCOMP	0			/* DS: used in switches */
#define TATOMIC	(Int)0x20000000		/* atomic terms have this set (jws) */
#define TARGBTS	(Int)0x00003FFF		/* nargs, varnum in low 14 bits */
/* there is one less bit than there used to be ... wonder what that breaks? */
#define TDBITS	(Int)0x1FFFC000		/* bits for dictionary index */
#define TDBIT	14			/* need left shift of 14 for dict indx*/

#define tagtype(x)      ((((Int)x)>>TAGSHIFT) & 3) /* DS: used in switches */
#define ConsInt(i)	((Int)(i) | TINT)
#define XtrInt(i)       (((Int)(i)) & TSIGN ? ((Int)(i)) : (((Int)(i)) & ~(TINT|RSIGN)))
#define ConsVar(d, vn)  ((((Int)d) << TDBIT) + (vn) | TVAR)
#define ConsFunc(d, na) ((((Int)d) << TDBIT) + (na) | TATOM)

#define IsInt(i)        ((((Int)(i)) & TTYPE) == TINT)
#define IsVar(i)        ((((Int)(i)) & TTYPE) == TVAR)
#define IsAtom(i)       ((((Int)(i)) & TTYPE) == TATOM)
#define IsComp(i)       ((((Int)(i)) & TTYPE) == TCOMP)

#define IsAtomic(i)	(IsAtom(i) || IsInt(i))
#define IsFunc(i)	(IsComp(i) || IsAtom(i))

#endif

#define atdict(at)      (((Int)at >> TDBIT) & (TDBITS >> TDBIT))
#define vtdict(at)      (((Int)at >> TDBIT) & (TDBITS >> TDBIT))
#define ctdict(t)       atdict(*(Int*)(t))
#define tdict(t)	(IsComp(t) ? ctdict(t) : atdict(t))
#define tnargs(t)       (*((Int*)t) & TARGBTS)

				/* functor and args of compound terms */
#define thead(t)	(*(Int*)t)
#define targ(i, t)	(*(i + (Int*) t))

#define ttype(t)        ((Int) t & TTYPE)
#define tnum(t)		XtrInt(t)

#define tvnum(t)        ((Int)t & TARGBTS)

#define MAXVARS	TARGBTS

/*
DICTIONARY
The dictionary is a simple hash table of pointers to structures. If a pointer
is NULL that space is free. The structures contain all info about all
identifiers used in the program, including pointers to the list of clauses.
The nargs field is zero for variables and 1 + the number of args for atoms
and functors.
*/

typedef union {
		Int dummy;
		struct {
			char nargs;
			char prot;
			short inprec;
		} chars;
		} dictchars;
typedef union {
		Int dummy;
		struct {
			short prprec;
			short psprec;
		} shorts;
		} dictshorts;

#ifdef DICT
#define DICTLEN 4087
#else
#define DICTLEN 1991
#endif

extern Int *dict[DICTLEN];
#define d_nargs(i)	(((dictchars *) dict[i])->chars.nargs)
#define d_prot(i)	(((dictchars *) dict[i])->chars.prot)
#define d_inprec(i)	(((dictchars *) dict[i])->chars.inprec)
#define d_name(i)	(dict[i][1])
#define d_wait(i)	(dict[i][2])
#define d_proc(i)	(dict[i][3])
#define d_nlinks(i)	(dict[i][4])
#define d_prprec(i)	(((dictshorts *) (dict[i]+5))->shorts.prprec)
#define d_psprec(i)	(((dictshorts *) (dict[i]+5))->shorts.psprec)
#ifdef CLINDEX
#define d_clindex(i)	(dict[i][6])	/* (jws) */
#define DENTRYSZ	7
#else
#define DENTRYSZ	6
#endif
#define DVNTRYSZ	2	/* vars only need first two words */

#define d_1clause(d)	d_proc(d)

#define dnargs(i)	((char) d_nargs(i))	/* #args+1 (0 for vars) */
#define dprot(i)	((char) d_prot(i))	/* protection/hide flags */
#define dname(i)	((char *) d_name(i))	/* ptr to name */
#define dprprec(i)	d_prprec(i)		/* prefix precedence */
#define dinprec(i)	d_inprec(i)		/* infix precedence */
#define dpsprec(i)	d_psprec(i)		/* postfix precedence */
#define dwait(i)	((Ptr) d_wait(i))	/* list of wait declarations */
#define dproc(i)	((Ptr) d_proc(i))	/* list of clauses */
#define dnlinks(i)	d_nlinks(i)		/* number of links (fudge) */
#ifdef CLINDEX
#define dclindex(i)	((Ptr) d_clindex(i))	/* clause index (jws) */
#endif
#define d1clause(d)	((Ptr) d_proc(d))	/* ptr to first clause */
#define d0clause(d)	((Ptr) &d_1clause(d))	/* 0th clause (fudge) */
						/* (assumes defn of cnextc) */

#define DHBITS 3
#define DVISIBLE 0		/* normal */
#define DHIDING 1		/* hide has been called but not hidden */
#define DHIDDEN 2		/* functor is hidden */
#define DPBITS 4
#define DPROT 4			/* functor protected (no listing or changing)*/
#define dfree(i)	(!dict[i])
#define dhide(i)	(dprot(i) & DHBITS)
#define dprotect(i)	(dprot(i) & DPBITS)


#define IDLEN 1024		/* max name length */
typedef char ident[IDLEN];

/*
CLAUSES
There is a linked list of clauses for each predicate. The number of links to
each one is stored and if it ever becomes zero the space is reclaimed. To
For system predicates written in C the nvars field is -1 and the clause
field points to the C function to call.
*/

#define c_nextc(c)	((c)[0])	/* next clause */
#define c_nlinks(c)	((c)[1])	/* number of links to this clause */
#define c_nvars(c)	((c)[2])	/* number of vars in this clause */
#define c_clause(c)	((c)[3])	/* ptr to clause (or C function) */
#define CCLSZ		4

typedef Int (*func)();

#define cnextc(c)	((Ptr) c_nextc(c))
#define cnlinks(c)	c_nlinks(c)
#define cnvars(c)	c_nvars(c)
#define cclause(c)	((Ptr) c_clause(c))
#define cfunc(c)	(*(func) cclause(c))
#define cisfunc(c)	(cnvars(c) == -1)
#define chead(c)	((Ptr) targ(1, cclause(c)))
/*
					bindings
The bindings are stored on the goalstack, after all the fixed length fields
(gcall, gproc etc). Each consists of a pointer to a term and its associated
level (Boyer & Moore structure sharing). Levels are represented as pointers
to the start of a group of bindings. For bindings to integers, atoms and
variables, the actual term is used, not a pointer to it (and the level is
irrelevant).
*/

struct bind {
	Ptr btermp;
	struct bind *blev;
	} ;
typedef struct bind *levtype;

#define bterm(v, l)	l[tvnum(v)].btermp
#define blevel(v, l)	l[tvnum(v)].blev

				/* goalstack */
/*
The goalstack uses an array of integers. The stack grows from one end and the
heap starts at the other end. Very simple memory management is used currently.
Conceptually the stack is made up of variable length records containing:
a pointer to the previous stack frame, a pointer to the current call, a pointer
to the current procedure being used for the call, a pointer to the parent call
(the call to the procedure that introduced the current call), the next
antecedent atom in the current procedure to call, a variable number of binding
locations (each has a ptr to a term and a level) - the number depends on the
current proc, a  variable length reset list (each element contains an address
and a previous value). If is is a resumed call, there is also a pointer to
the call to resume when the current call is completed, and the top bit of gprev
is set.
*/

#ifndef GSTKLEN
#define GSTKLEN 80000
#endif

struct rset {
		Int *addr, oldval;
		} ;
typedef struct rset *rsetp;

#define GPTRMASK	0x7fffffff

#define g_nextwake(top)	(*(((Ptr)top)-1))	/* continuation(resumed procs)*/
#define g_prev(top)	(*((Ptr)top))		/* previous frame */
#define g_call(top)	(*(((Ptr)top)+1))	/* current call */
#define g_proc(top)	(*(((Ptr)top)+2))	/* current proc (clause) */
#define g_parent(top)	(*(((Ptr)top)+3))	/* parent frame */
#define g_nextchild(top) (*(((Ptr)top)+4))	/* next untried subgoal */
#define g_rset(top)	(*(((Ptr)top)+5))	/* start of reset list */

#define gnextwake(top)	((Ptr)g_nextwake(top))
#define gprev(top)	((Ptr)(g_prev(top) & GPTRMASK))
#define gcall(top)	((Ptr)g_call(top))
#define gproc(top)	((Ptr)g_proc(top))
#define gparent(top)	((Ptr)g_parent(top))
#define gnextchild(top)	((Ptr)g_nextchild(top))
#define grset(top)	((Ptr)g_rset(top))
#define gbindings(top)	((levtype)(top+6))	/* start of bindings */
						/* start of reset list */
#define grsetlist(top,proc)	(top + 6 + (cnvars((Ptr)proc) > 0 ? \
				cnvars((Ptr)proc) * 2 : 0))

				/* gets real call if call points to comma */
#define gtcall(top)	(IsComp(gcall(top)) && ctdict(gcall(top)) == Dcomma ?\
				(Ptr) targ(1, gcall(top)) : gcall(top))
			/* there is a fudge or two to flag various things */
			/* such as delayed procs */

#define asleep(top)	(gnextchild(top) == (Ptr) top)
#define delayed(top)    ((unsigned Int)gnextchild(top) <= (Int) gtop && gnextchild(top))
#define resumed(top)	(g_prev(top) & ~GPTRMASK)

#define cut(gproc)      ((Int) (gproc) | ~GPTRMASK)	/* cut sets top bit */
#define beencut(gproc)  ((Int) (gproc) & ~GPTRMASK)	/* in gproc */
#define uncut(gproc)    ((Int) (gproc) & GPTRMASK)

				/* constants returned by system functions etc*/
#define FAIL 0
#define SUCCEED 1
#define DELAY 2
#define ERROR 3
#define CUTFAIL 4	/* fail, dont try next clause */
#define DELPREV 5	/* delay previous call (fudge for ngrnd- used in ~,if)*/
#define RESTORE	6	/* returned by restore */

#define NULLL (levtype)NULL	/* void* is better */
				/* non-int global functions */

extern Ptr tcopy();
extern Ptr newmem();
extern Ptr getterm();
extern Ptr stot();


		/* constants put in reset list for fudges done by reset */

#define RSTRUCT	0	/* reclaim single structure */
#define RTERM	1	/* reclaim list of structures (a term) */
#define RDB	2	/* call dbabort (database version only) */

		/* fudges for parameters for jws */
#ifdef PARAM
#define IsParam(i)	(IsAtom(i) && disparam(atdict(i)))

#define d_pterm(i)	d_wait(i)
#define d_plev(i)	d_proc(i)
#define dpterm(i)	((Ptr) d_pterm(i))
#define dplev(i)	((levtype) d_plev(i))

#define DPARAM		(DHIDDEN | DHIDING | DPROT)
#define disparam(i)	(dprot(i) == DPARAM)
#endif

#ifdef CLINDEX
/*
CLAUSE INDEX	(jws)
A clause index is a list of pairs of dictionary entries and clause lists.  The
first entry in the list is special and contains the number of the argument
indexed in the cln field and a list of all the clauses with a variable for the
indexed argument in the clproc field.
We do it this way to avoid the difficulty of chaining down different pointer
fields in mainloop().
Note that indexing a predicate makes it impossible to reclaim it.
*/

#define cl_next(p)	(p[0])	/* link field */
#define cl_pred(p)	(p[1])	/* key */
#define cl_n(p)		(p[1])	/* number of indexed argument */
#define cl_proc(p)	(p[2])	/* clause list */
#define CLSIZE		3

#define clnext(p)	((Ptr) cl_next(p))
#define clpred(p)	((Int) cl_pred(p))
#define cln(p)		((Int) cl_n(p))
#define clproc(p)	((Ptr) cl_proc(p))
#endif

/*
Info for save file header.  Various constants and variables are stored
in the header, followed by the stack, heap, dictionary and dynamically
loaded functions.  For details of what gets saved and how, look at the
code in predc.c, main.c, data.c and dload.c.
*/

#define S_CONSTS	0
#define S_MAIN		S_CONSTS + 4
#define S_DATA		S_MAIN + 8

#ifdef DLOAD
#define MAXDLOAD	10			/* max # dloads we save */
#define S_DLOAD		S_DATA + 7 + MAXFREE
#define S_LEN		S_DLOAD + MAXDLOAD*2
#else
#define S_LEN		S_DATA + 7 + MAXFREE
#endif

struct sbuffer{
	char header[32];
	long l[S_LEN];
} ;
extern struct sbuffer savebuf;

#define mainsbuf	&savebuf.l[S_MAIN]
#ifdef DLOAD
#define dloadsbuf	&savebuf.l[S_DLOAD]
#endif
