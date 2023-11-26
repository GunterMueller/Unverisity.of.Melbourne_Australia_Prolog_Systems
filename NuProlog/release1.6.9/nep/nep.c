/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"
#include <ctype.h>
#include <sys/types.h>
#include <sys/file.h>
#include <setjmp.h>
/*
 * The COFF a.out.h breaks struct nlist.
 * Note that the ELXSI has COFF even though it claims to be BSD.
 */
#include <nlist.h>
#include <sys/stat.h>
#ifdef BSD4
#	include <sys/uio.h>
#else
#	include <fcntl.h>
#endif
#include "nltypes.h"
#include <sys/time.h>
#ifdef LIMITRESOURCES
#	include <sys/resource.h>
#endif

#ifdef LITTLE
#	define CODESIZE (48 * 1024)
#	define DATASIZE (4 * 1024)
#	define TDATASIZE (4 * 1024)
#	define FREESIZE (4 * IOBUFSIZE / sizeof(Word) + 64 * 1024)
#	define TRAILSIZE (8 * 1024)
#	define HEAPSIZE (64 * 1024)
#	define STACKSIZE (48 * 1024)

#	define CODEGROW (16 * 1024)
#	define DATAGROW (4 * 1024)
#	define TDATAGROW (4 * 1024)
#else
#	define CODESIZE (48 * 1024)
#	define DATASIZE (8 * 1024)
#	define TDATASIZE (8 * 1024)
#	define FREESIZE (16 * IOBUFSIZE / sizeof(Word) + (128 + 16) * 1024)
#	define TRAILSIZE (32 * 1024)
#	define HEAPSIZE (96 * 1024)
#	define STACKSIZE (64 * 1024)

#	define CODEGROW (16 * 1024)
#	define DATAGROW (8 * 1024)
#	define TDATAGROW (8 * 1024)
#endif

#ifndef PageSize
	int PageSize;			/*See machdep.h to understand this. */
#endif /* PageSize */

/* HeapBreathingSpace, TrailBreathingSpace, StackBreathingSpace
Minimum areas on top of the heap, trail, and stack that must be
free when calling non-overflow checking functions.
This arrangement is needed because it can be very difficult to
shift stacks after some operations have begun.

If we are using memory protection to detect overflow on the stacks
then the breathing-spaces must be multiples of PageSize and at least
two pages each.
*/
#ifdef PROTOVERFLOW
#	define BREATHINGSPACE (PageSize / sizeof(Word))
#else
#	define BREATHINGSPACE 0
#endif

int HeapBreathingSpace, StackBreathingSpace, TrailBreathingSpace;

/*
 * Limit resources used.  Mostly of use in teaching environments.
 *
 * BUG!  Hardly very flexible, but about as much as is justified.
*/
#ifdef LIMITRESOURCES
#	define CPU_LIMIT		600
#	define FSIZE_LIMIT		1000000
#	define DATA_LIMIT		4000000
#	define STACK_LIMIT		1000000
#	define CORE_LIMIT		0
#	define RSS_LIMIT		2000000
#endif

Memory *codeArenas;
Memory *dataArenas;
Memory *tDataArenas;
Word programSize;		/* Total size of the three arenas in Words. */
Object freeList;		/* List of free $BMTs. */

/*
 * The prologFlags array.
 * It is used as both a Prolog term and a C array.
 */
Object Flags[NFLAGS];

SymbolTable *stab = (SymbolTable *)NULL;  /* global symbol table */

/* Memory for the heap, trail and stack is allocated at startup. */
Word *memory, *memoryMax;
TrailRecord *trailBase, *trailTop, *trailMax;
Word *heapBase, *heapTop, *heapMax, *stackBase, *stackTop, *stackMax;

/*
 * The table of C Functions for the twisted purposes of the database sytems.
 */
#define NFUNCTDUMMIES 5
Object cFunctDummy[NFUNCTDUMMIES];
PFInt cFunctTable[NFUNCTDUMMIES];
Object *lastCFunctDummy = cFunctDummy + NFUNCTDUMMIES;

/*
 * Tables of arithmetic functions.
 */

PFVoid ArithFuncTable[] = {
	f_add,		/*  0	&SymAdd */
	f_sub,		/*  1	&SymMinus */
	f_mult,		/*  2	&SymMult */
	f_div,		/*  3	&SymDiv */
	f_intdiv,	/*  4	&SymIntDiv */
	f_mod,		/*  5	&SymMod */
	f_bitand,	/*  6	&SymBitAnd */
	f_bitor,	/*  7	&SymBitOr */
	f_bitxor,	/*  8	&SymBitXOr */
	f_lsh,		/*  9	&SymLSH */
	f_rsh,		/* 10	&SymRSH */
	f_logand,	/* 11	&SymAnd */
	f_logor,	/* 12	&SymOr */
	f_lognot,	/* 13	&SymNot */
	f_minus,	/* 14	&SymMinus */
	f_bitcomp,	/* 15	&SymBackslash */
	f_sin,		/* 16	&SymSin */
	f_cos,		/* 17	&SymCos */
	f_tan,		/* 18	&SymTan */
	f_asin,		/* 19	&SymAsin */
	f_acos,		/* 20	&SymAcos */
	f_atan,		/* 21	&SymAtan */
	f_atan2,	/* 22	&SymAtan2 */
	f_exp,		/* 23	&SymExp */
	f_log,		/* 24	&SymLog */
	f_log10,	/* 25	&SymLog10 */
	f_power,	/* 26	&SymStarStar */
	f_sqrt,		/* 27	&SymSqrt */
	f_integer,	/* 28	&SymInteger */
	f_float,	/* 29	&SymFloat */
	f_round,	/* 30	&SymRound */
	f_lt,		/* 31	&SymLT */
	f_le,		/* 32	&SymLE */
	f_gt,		/* 33	&SymGT */
	f_ge,		/* 34	&SymGE */
	f_eq,		/* 35	&SymEQ */
	f_ne,		/* 36	&SymNE */
	f_eval,		/* 37	&SymAdd */
	f_sub_r,	/* 38	&SymSubR */
	f_div_r,	/* 39	&SymDivR */
	f_intdiv_r,	/* 40	&SymIntDivR */
	f_mod_r,	/* 41	&SymModR */
	f_lsh_r,	/* 42	&SymLSHR */
	f_rsh_r,	/* 43	&SymRSHR */
	f_atan2_r,	/* 44	&SymAtan2R */
	f_power_r,	/* 45	&SymStarStarR */
	f_add1,		/* 46	&SymAdd1 */
	f_sub1,		/* 47	&SymMinus1 */
	f_byte,		/* 48	&SymInteger8At */
	f_ubyte,	/* 49	&SymUnsigned8At */
	f_half,		/* 50	&SymInteger16At */
	f_uhalf,	/* 51	&SymUnsigned16At */
	f_word,		/* 52	&SymIntegerAt */
	f_address,	/* 53	&SymAddressAt */
	f_single,	/* 54	&SymSingleAt */
	f_double	/* 55	&SymDoubleAt */
};

Atom *ArithFuncAtoms[] = {
	&SymAdd,
	&SymMinus,
	&SymMult,
	&SymDiv,
	&SymIntDiv,
	&SymMod,
	&SymBitAnd,
	&SymBitOr,
	&SymBitXOr,
	&SymLSH,
	&SymRSH,
	&SymAnd,
	&SymOr,
	&SymNot,
	&SymMinus,
	&SymBackslash,
	&SymSin,
	&SymCos,
	&SymTan,
	&SymAsin,
	&SymAcos,
	&SymAtan,
	&SymAtan2,
	&SymExp,
	&SymLog,
	&SymLog10,
	&SymStarStar,
	&SymSqrt,
	&SymInteger,
	&SymFloat,
	&SymRound,
	&SymLT,
	&SymLE,
	&SymGT,
	&SymGE,
	&SymEQ,
	&SymNE,
	&SymAdd,		/* Identity function */
	&SymMinusR,
	&SymDivR,
	&SymIntDivR,
	&SymModR,
	&SymLSHR,
	&SymRSHR,
	&SymAtan2R,
	&SymStarStarR,
	&SymAdd1,
	&SymMinus1,
	&SymInteger8At,
	&SymUnsigned8At,
	&SymInteger16At,
	&SymUnsigned16At,
	&SymIntegerAt,
	&SymAddressAt,
	&SymSingleAt,
	&SymDoubleAt
};

char ArithFuncArity[] = {
	2,	/*  0	&SymAdd */
	2,	/*  1	&SymMinus */
	2,	/*  2	&SymMult */
	2,	/*  3	&SymDiv */
	2,	/*  4	&SymIntDiv */
	2,	/*  5	&SymMod */
	2,	/*  6	&SymBitAnd */
	2,	/*  7	&SymBitOr */
	2,	/*  8	&SymBitXOr */
	2,	/*  9	&SymLSH */
	2,	/* 10	&SymRSH */
	2,	/* 11	&SymAnd */
	2,	/* 12	&SymOr */
	1,	/* 13	&SymNot */
	1,	/* 14	&SymMinus */
	1,	/* 15	&SymBackslash */
	1,	/* 16	&SymSin */
	1,	/* 17	&SymCos */
	1,	/* 18	&SymTan */
	1,	/* 19	&SymAsin */
	1,	/* 20	&SymAcos */
	1,	/* 21	&SymAtan */
	2,	/* 22	&SymAtan2 */
	1,	/* 23	&SymExp */
	1,	/* 24	&SymLog */
	1,	/* 25	&SymLog10 */
	2,	/* 26	&SymStarStar */
	1,	/* 27	&SymSqrt */
	1,	/* 28	&SymInteger */
	1,	/* 29	&SymFloat */
	1,	/* 30	&SymRound */
	2,	/* 31	&SymLT */
	2,	/* 32	&SymLE */
	2,	/* 33	&SymGT */
	2,	/* 34	&SymGE */
	2,	/* 35	&SymEQ */
	2,	/* 36	&SymNE */
	1,	/* 37	&SymAdd */
	2,	/* 38	&SymSubR */
	2,	/* 39	&SymDivR */
	2,	/* 40	&SymIntDivR */
	2,	/* 41	&SymModR */
	2,	/* 42	&SymLSHR */
	2,	/* 43	&SymRSHR */
	2,	/* 44	&SymAtan2R */
	2,	/* 45	&SymStarStarR */
	1,	/* 46	&SymAdd1 */
	1,	/* 47	&SymMinus1 */
	1,	/* 48	&SymInteger8At */
	1,	/* 49	&SymUnsigned8At */
	1,	/* 50	&SymInteger16At */
	1,	/* 51	&SymUnsigned16At */
	1,	/* 52	&SymIntegerAt */
	1,	/* 53	&SymAddressAt */
	1,	/* 54	&SymSingleAt */
	1	/* 55	&SymDoubleAt */
};

PFInt ArithPredTable[] = {
	p_and,		/*  0	&SymAnd */
	p_or,		/*  1	&SymOr */
	p_lt,		/*  2	&SymLT */
	p_le,		/*  3	&SymLE */
	p_gt,		/*  4	&SymGT */
	p_ge,		/*  5	&SymGE */
	p_eq,		/*  6	&SymEQ */
	p_ne		/*  7	&SymNE */
};

Atom *ArithPredAtoms[] = {
	&SymAnd,
	&SymOr,
	&SymLT,
	&SymLE,
	&SymGT,
	&SymGE,
	&SymEQ,
	&SymNE
};

char ArithPredArity[] = {
	2,	/*  0	&SymAnd */
	2,	/*  1	&SymOr */
	2,	/*  2	&SymLT */
	2,	/*  3	&SymLE */
	2,	/*  4	&SymGT */
	2,	/*  5	&SymGE */
	2,	/*  6	&SymEQ */
	2	/*  7	&SymNE */
};

jmp_buf re_entry_point;	/* place for ABORT to longjmp to */

Word	*top;		/* top of code+data segment */

Word	*code;		/* start of code segment for module */
Word	*data;		/* start of data segment for module */
Word	*heap;		/* start of junk heap segment for module */
char	*types;		/* start of code type-info segment for module */
char	*dtypes;	/* start of data type-info segment for module */
char	*htypes;	/* start of heap type-info segment for module */
SymTab	*symtab;	/* start of symbol table segment for module */
char	*strings;	/* start of string table segment for module */

Instruction notInitializing[] = {
	MakeIns0(cFAIL)
};

Instruction initializing[] = {
	MakeIns0(cPRO)
};

Instruction ret[] = {
	MakeIns0(cRET)
};

/*
 * Precomputed StrHeaders used frequently enough to be worth the effort.
 *
 * They will go away if the representation of the Functor list is improved.
 */
Structure StrHeader6_BMT, StrHeader2_Ref, StrHeader2Stream;
Structure StrHeader2Cons, StrHeader2Equal, StrHeader1_VAR;
Structure StrHeader1Write_Var, StrHeader1Parentheses;
Structure StrHeaderAtomHeader, StrHeader3_Prop;

#ifdef DEBUG

SpyStruct SpyPoints[100];

Program PastPCs[NPASTPCS];
Choice *PastBs[NPASTPCS];
int NextPCSlot = 0;

#endif /* DEBUG */

#ifdef FLOAD
	/* NIL is often needed in foreign code that handles NU-Prolog terms. */

	/* Some, broken, compilers can't handle NIL as an initializer. */
	Object NUNIL;
#endif /* FLOAD */

#ifdef DEBUG3
	int callflg = 0;
#endif
#ifdef DEBUG
	int regflg = 0;
	int spyflg = 0;
	int traceflg = 0;
#endif
int argflg = 0;
int dumpflg = 0;
int entryflg = 0;
int initflg = 0;
int listflg = 0;
int loadflg = 0;
int hashflg = 0;
int hashdflg = 0;
int nameflg = 0;
int restflg = 0;
int saveflg = 0;
int sysflg = 0;
int timeflg = 0;
int coreflg = 0;
int freesize = FREESIZE;
int heapsize = HEAPSIZE;
int stacksize = STACKSIZE;
int trailsize = TRAILSIZE;

static Object
commandArgs(argn, argc, argv)
register int argn, argc;
char **argv;
{
	register int i;
	register Object args, *l;

	args = NIL;

	if(argn != 0)
		for(i = argc - 1; argn <= i; i--) {
			l = (Object *) Dalloc(2 * sizeof(Object));
			l[0] = StarToAtom(enterAtom(argv[i], stab, (Atom *) NULL));
			l[1] = args;
			args = StarToList(l);
		}

	l = (Object *) Dalloc(2 * sizeof(Object));
	l[0] = StarToAtom(enterAtom(argv[nameflg], stab, (Atom *) NULL));
	l[1] = args;
	return(StarToList(l));
}

char currentDirectory[MAXPATHLEN];	/* Place to store the cwd. */
char nepologFileName[MAXPATHLEN];	/* Name of a file with symbol table. */

char	*my_name;

char **environment;

static void
saveBlock(fd, base, len)
register int fd;
char *base;
Word len;
{
	write(fd, (char *) &base, sizeof(char *));
	write(fd, (char *) &len, sizeof(Word));
	write(fd, base, sizeof(char) * len);
if(dumpflg)
  printf("Base = 0x%x, len = %d\n", base, len);
}

static void
saveMemory(fd, m)
register int fd;
register Memory *m;
{
	while(m != (Memory *) NULL) {
		saveBlock(fd, (char *) m, sizeof(Memory));
		saveBlock(
			fd,
			(char *) m->mem_base,
			sizeof(Word) * (m->mem_top - m->mem_base));
		m = m->mem_next;
	}
}

static int
IsPrologArena(m)
register Memory *m;
{
	register Memory *mp;

	for(mp = codeArenas; mp != (Memory *) NULL; mp = mp->mem_next)
		if(m == mp)
			return(1);
	for(mp = dataArenas; mp != (Memory *) NULL; mp = mp->mem_next)
		if(m == mp)
			return(1);
	for(mp = tDataArenas; mp != (Memory *) NULL; mp = mp->mem_next)
		if(m == mp)
			return(1);
	return(0);
}

/*
 * This routine is based on bits of William Sebok's tstmalloc routine.
 */
static void
saveMallocArena(fd)
register int fd;
{
	register struct overhead *p, *q;
	register struct qelem *qp;

	for(qp = adjhead.q_forw; qp != &adjhead; qp = qp->q_forw) {
		p = FROMADJ(qp);
		switch(p->ov_magic) {
		when MAGIC_FREE:
			saveBlock(fd, (char *) p, sizeof(struct overhead));
		when MAGIC_BUSY:
			/* The contents of the Prolog arenas are saved elsewhere. */
			if(IsPrologArena((Memory *)((char *) p + sizeof(struct overhead))))
				saveBlock(fd, (char *) p, sizeof(struct overhead));
			else
				saveBlock(fd, (char *) p, p->ov_length);
		break;
		default:
			panic("Bad block found in malloc chain.");
		}
	}
}

caddr_t lowWater;	/* Base of dynamicly allocated memory. */
caddr_t highWater;	/* Highest address that needs to be saved by save(). */

/* The version of the interpreter that saved or is saving a saved state. */
static Word savedVersion;

static void
save(fname)
char *fname;
{
	register int fd;
	register struct overhead *p;

	fd = open(fname, O_TRUNC|O_WRONLY|O_CREAT, 0755);
	if(fd < 0) {
		fprintf(stderr, "Can't open %s to save to\n", fname);
		exit(1);
	}

	/* showMallocArena(); */
	write(fd, PATHheader, strlen(PATHheader));

	/*
	 * restore() expects savedVersion and highWater to be first.
	 */
	savedVersion = NUVERSION;
	/* saveBlock(fd, (char *)&savedVersion, sizeof(savedVersion)); */
	write(fd, (char *)&savedVersion, sizeof(savedVersion));
	p = FROMADJ(adjhead.q_back);
	highWater = (char *) p + p->ov_length;
	/* saveBlock(fd, (char *)&highWater, sizeof(highWater)); */
	write(fd, (char *)&highWater, sizeof(highWater));

	/*
	 * The rest of the saved state is a sequence of blocks.
	 */
	saveBlock(fd, (char *)&endfree, sizeof(endfree));
	saveBlock(fd, (char *)&adjhead, sizeof(adjhead));
	saveBlock(fd, (char *)buckets, sizeof(buckets));
	saveBlock(fd, (char *)&stab, sizeof(stab));
	saveBlock(fd, (char *)systemAtoms, sizeof(Atom) * nSystemAtoms);
	saveBlock(fd, (char *)Flags, sizeof(Flags));
	saveBlock(fd, (char *)&programSize, sizeof(programSize));
	saveBlock(fd, (char *)&freeList, sizeof(freeList));
	saveMallocArena(fd);
	saveBlock(fd, (char *)&codeArenas, sizeof(codeArenas));
	saveMemory(fd, codeArenas);
	saveBlock(fd, (char *)&dataArenas, sizeof(dataArenas));
	saveMemory(fd, dataArenas);
	saveBlock(fd, (char *)&tDataArenas, sizeof(tDataArenas));
	saveMemory(fd, tDataArenas);

	close(fd);
}

static int
restoreBlock(fd)
int fd;
{
	char *base;
	Word len;
	register int bytes;

	bytes = read(fd, (char *) &base, sizeof(char *));
	if(bytes == 0)
		return(0);
	else if(bytes != sizeof(char *)) {
		perror("A");
		printf("A: bytes = %d, base = 0x%x\n", bytes, base);
		fflush(stdout);
		panic("Restoring from truncated file.");
	}
	bytes = read(fd, (char *) &len, sizeof(Word));
	if(bytes != sizeof(Word)) {
		perror("B");
		printf("B: bytes = %d, base = 0x%x, len = 0x%x\n", bytes, base, len);
		fflush(stdout);
		panic("Restoring from truncated file.");
	}
	bytes = read(fd, base, sizeof(char) * len);
	if(bytes != sizeof(char) * len) {
		perror("C");
		printf("C: bytes = %d, base = 0x%x, len = 0x%x\n", bytes, base, len);
		fflush(stdout);
		panic("Restoring from truncated file.");
	}
	return(1);
}

static void
restore(fname)
char *fname;
{
	register int fd;
	register int i;
	char c;
	int  outOfDate();
	Word mallocedMemory();

	if(mallocedMemory() != 0) {
		fprintf(stderr, "Memory malloced: %d\n", mallocedMemory());
		panic("Can't restore malloc() arena with allocations outstanding");
	}
	fd = open(fname, O_RDONLY, 0644);
	if(fd < 0) {
		fprintf(stderr, "Can't open %s to restore from\n", fname);
		exit(1);
	}
/* JUNK
 * No longer necessary because saved states now have version numbers.
	if(outOfDate(fd, "nep")) {
		fprintf(stderr, "Save file \"%s\" out of date\n", fname);
		exit(1);
	}
JUNK */


	/* Skip the header line */
	do {
		i = read(fd, &c, sizeof(char));
	} while(i == 1 && c != '\n');

	/*
	 * Check engine version number in saved state.
	 */
	/* restoreBlock(fd); */
	read(fd, (char *)&savedVersion, sizeof(savedVersion));
	if(savedVersion != NUVERSION) {
		fprintf(stderr,
			"Warning: Save file %s is from version %d.%d.%d of NU-Prolog.  Not restored.\n",
			fname,
			savedVersion / 10000,
			(savedVersion % 10000) / 100,
			savedVersion % 100);
		exit(1);
	}

	/*
	 * Make room for the areas to be restored.
	 */
	/* restoreBlock(fd); */
	read(fd, (char *)&highWater, sizeof(highWater));
	if(BRK(highWater))
		panic("Can't allocate enough memory to restore saved image.");

	while(restoreBlock(fd))
		;
	/* showMallocArena(); */
	close(fd);
}

static void
initSymTab() {
	register int i;
	register Atom *a;

	stab = newSymbolTable(12, &SymUser);

	for(i = 0; i < nSystemAtoms; i++) {
		a = systemAtoms + i;
		if(systemAtomNames[i][0] == '\0')
			a->a_pname = NIL;
		else
			a->a_pname = StarToString(systemAtomNames[i]);
		a->a_module = StarToBlock(stab);
		a->a_header = StrHeaderAtomHeader;
		/*
		 * Set functors to something intuitively sensible.
		 * Should be done in syms.c, but some compilers can't cope.
		 * Can you say ELXSI?  I thought you could!
		 */
		if(a->a_functors == MakeSmallInt(0))
			a->a_functors = NIL;
		a->a_prop = NIL;
		a->a_prec = NIL;
		(void) enterAtom(systemAtomNames[i], stab, a);
	}

	addprop(&SymUser, StarToAtom(&Sym_SymbolTable), StarToBlock(stab));
	addprop(&Sym_Modules, StarToAtom(&Sym_Module), StarToAtom(&SymUser));
}

static void
initStrHeaders() {
	StrHeader6_BMT = StarToStrHeader(6, &Sym_BMT);
	StrHeader2_Ref = StarToStrHeader(2, &Sym_Ref);
	StrHeader3_Prop = StarToStrHeader(3, &Sym_Prop);
	StrHeader2Stream = StarToStrHeader(2, &SymStream);
	StrHeader2Cons = StarToStrHeader(2, &SymCons);
	StrHeader2Equal = StarToStrHeader(2, &SymEqual);
	StrHeader1_VAR = StarToStrHeader(1, &Sym_VAR);
	StrHeader1Write_Var = StarToStrHeader(1, &SymWrite_Var);
	StrHeader1Parentheses = StarToStrHeader(1, &SymParentheses);
	StrHeaderAtomHeader = StarToStrHeader(wordsof(Atom) - 1, &SymAtom);
}

static int
uDisplayTerm(t)
Object t;
{
	displayTerm(stdout, t);
	printf("\n");
	return 0;
}

static int
uWriteTerm(t)
Object t;
{
	writeTerm(stdout, t);
	printf("\n");
	return 0;
}

static void
initCFunct()
{
	register int n;

	for(n = 0; n < NFUNCTDUMMIES; n++)
		cFunctDummy[n] = NIL;

	cFunctTable[0] = uDisplayTerm;
	cFunctTable[1] = uWriteTerm;
	cFunctTable[2] = p_simc_abort;
	cFunctTable[3] = p_sql_abort;
	/*cFunctTable[4] = p_dsimc_abort;		BUG!  Not defined. */
	cFunctTable[4] = uDisplayTerm;
}

static void
initFlags(restflg)
int restflg;
{
	register int n;

	Flags[flgNSTREAMS] = MakeSmallInt(nstreams);

	/*
	 * As yet, there are no other flags that need to be reset on a restore.
	 */
	if(restflg)
		return;

	Flags[0] = StarToStrHeader(NFLAGS - 1, &SymFlags);
	for(n = 1; n < NFLAGS; n++)
		Flags[n] = NIL;			/* For safety */
	Flags[flgCHAR] = StarToAtom(&SymOn);
	Flags[flgFILE] = StarToAtom(&SymOff);
	Flags[flgDEBUG] = StarToAtom(&SymDebug);
	Flags[flgLOCALDEBUG] = StarToAtom(&SymOff);
	Flags[flgLEASHMODE] = MakeSmallInt(leashALL);
	Flags[flgCALLNUMBER] = MakeSmallInt(0);
	Flags[flgDELAYED] = StarToAtom(&SymFail);
	Flags[flgWIZARD] = StarToAtom(&SymOff);
	Flags[flgLIBRARIES]
		= StarToAtom(enterAtom(PATHnulib, stab, (Atom *) NULL));
	Flags[flgBINARIES] = StarToAtom(enterAtom(PATHnubin, stab, (Atom *) NULL));
	Flags[flgCOMMANDS] = StarToAtom(enterAtom(PATHbin, stab, (Atom *) NULL));
	Flags[flgDATAGROW] = MakeSmallInt(DATAGROW);
	Flags[flgTDATAGROW] = MakeSmallInt(TDATAGROW);
	Flags[flgCODEGROW] = MakeSmallInt(CODEGROW);
	Flags[flgVARS] = StarToAtom(&SymOn);
	Flags[flgREDEFWARN] = StarToAtom(&SymOn);
	Flags[flgPRINTDEPTH] = MakeSmallInt(10);
	Flags[flgOPTIMIZEQUERIES] = StarToAtom(&SymOff);
	Flags[flgMAXDEPTH] = MakeSmallInt(100000);
#ifdef FLOAD
	Flags[flgFLOAD] = StarToAtom(&SymTrue);
#else /* FLOAD */
	Flags[flgFLOAD] = StarToAtom(&SymFail);
#endif /* FLOAD */
	Flags[flgMACHINE] =
		StarToAtom(enterAtom(MACHINE, stab, (Atom *) NULL));
	Flags[flgVERSION] = MakeSmallInt(NUVERSION);
	Flags[flgNSTREAMS] = MakeSmallInt(nstreams);
	Flags[flgMAXARITY] = MakeSmallInt(MAXARITY);
}

#ifdef BSD4

main(argc, argv, env)
int argc;
char *argv[], *env[];

#else /* BSD4 */

extern char **environ;

main(argc, argv)
int argc;
char *argv[];

#endif /* BSD4 */
{
	register int i;
	register char *c;
	Functor *init, *inf;
	unsigned int size;
	int restOfArgs;
	Machine MR;
	char *buffers;
	char *barrier;
	char stdinBuffer[IOBUFSIZE], stdoutBuffer[IOBUFSIZE];
	char stderrBuffer[IOBUFSIZE];
	struct stat sb;

#ifdef FLOAD
	/* Some, broken, compilers can't handle NIL as an initializer. */
	Object NUNIL = NIL;
#endif

	my_name = argv[0];
#ifdef BSD4
	environment = env;
#else /* BSD4 */
	environment = environ;
#endif /* BSD4 */

	initMalloc();

#ifndef PageSize
	PageSize = _PageSize;			/*See machdep.h to understand this. */
#endif /* PageSize */
	HeapBreathingSpace
		= BREATHINGSPACE
		+ ALIGNANY((1024 + MAXARITY * 2)*sizeof(Word), PageSize)/sizeof(Word);
	StackBreathingSpace
		= BREATHINGSPACE
		+ ALIGNANY(1024 * sizeof(Word), PageSize) / sizeof(Word);
	TrailBreathingSpace
		= StackBreathingSpace;

#ifdef LIMITRESOURCES
#define Limit(res, val, rl) { \
	rl.rlim_cur = rl.rlim_max = val; setrlimit(res, &rl); \
	}
	{
		struct rlimit rl;

		Limit(RLIMIT_CPU, CPU_LIMIT, rl);
		Limit(RLIMIT_FSIZE, FSIZE_LIMIT, rl);
		Limit(RLIMIT_DATA, DATA_LIMIT, rl);
		Limit(RLIMIT_STACK, STACK_LIMIT, rl);
		Limit(RLIMIT_CORE, CORE_LIMIT, rl);
		Limit(RLIMIT_RSS, RSS_LIMIT, rl);
	}
#endif /* LIMITRESOURCES */

	/*
	 * Some systems, including ELXSIs, call a built-in,
	 * unchangeable, malloc to get io buffers.  This stuffs up
	 * heap expansion, so we allocate io buffers by hand.
	 *
	 * We also do this for stderr in case we are on a system that
	 * line buffers it, or someone re-directs it from the shell.
	 *
	 * This doesn't fix everything, unfortunately, but nothing
	 * short of distributing our own version of stdio would.
	 *
	 * One advantage of doing this is that it allows stdio to be
	 * used within malloc().
	 */
	/*
	setvbuf(stdin, stdinBuffer, _IOFBF, IOBUFSIZE);
	setvbuf(stdout, stdoutBuffer, _IOFBF, IOBUFSIZE);
	setvbuf(stderr, stderrBuffer, _IOFBF, IOBUFSIZE);
	*/

	strcpy(nepologFileName, PATHnep);

	/* Set up enough of *CMR for copy() to work */
	MR.mr_tr = (TrailRecord *) NULL;
	CMR = &MR;

	for(i = 1; i < argc && argv[i][0] == '-'; i++) {
		for(c = argv[i]; *c != '\0'; c++)
			switch(*c) {
			when 'a':			/* The rest of the arguments are command args */
								/* Sometimes also processed in the load loop */
				argflg = i + 1;
				restOfArgs = argc;
				goto argsProcessed;		/* break 3 */
#ifdef DEBUG3
			when 'c':			/* Debugging */
				callflg = 1;
#endif /* DEBUG3 */
			when 'C':
				coreflg = 1;
			when 'd':			/* Debugging */
				dumpflg = 1;
			when 'e':
				entryflg = ++i;
			when 'h':
				hashflg = 1;
			when 'H':
				hashdflg = 1;
			when 'i':
				initflg = ++i;	/* Initialization file */
			when 'l':			/* Debugging */
				listflg = 1;
				dumpflg = 1;
			when 'L':			/* Debugging */
				loadflg = 1;
#ifdef DEBUG
			when 'p': {			/* Debugging */
				register char *c;
				register Word n;

				i++;
				n = 0;
				for(c = argv[i]; *c != '\0'; c++) {
					n = (n << 4) + *c;
					if(isdigit(*c))
						n -= '0';
					else if('a' <= *c && *c <= 'f')
						n += 10 - 'a';
					else if('A' <= *c && *c <= 'F')
						n += 10 - 'A';
					else
						panic("Non-hex digit in arg to -p.");
				}
				SpyPoints[spyflg].sp_address = (Word *)n;
				SpyPoints[spyflg].sp_value = 0;
				spyflg++;
			}
#endif /* DEBUG */
			when 'P':
				nameflg = ++i;
#ifdef DEBUG
			when 'r':			/* Debugging */
				regflg = 1;
				traceflg = 1;
#endif /* DEBUG */
			when 'R':			/* Restart from file */
				restflg = ++i;
			when 's':			/* System is being built */
				sysflg = 1;
			when 'S':			/* Save to file */
				saveflg = ++i;
#ifdef DEBUG
			when 't':			/* Debugging */
				traceflg = 1;
#endif /* DEBUG */
			when 'T':			/* Timing messages */
				timeflg = 1;
			when 'u':			/* FREESIZE */
				freesize = atoi(argv[++i]) * 1024;
			when 'v':			/* HEAPSIZE */
				heapsize = atoi(argv[++i]) * 1024;
			when 'w':			/* STACKSIZE */
				stacksize = atoi(argv[++i]) * 1024;
			when 'x':			/* TRAILSIZE */
				trailsize = atoi(argv[++i]) * 1024;
			}
	}
	restOfArgs = i;

	argsProcessed:

	if(restflg) {
		restore(argv[restflg]);
		/*
		 * Probably quicker to do these each time rather than save
		 * and restore them.
		 */
		initStrHeaders();
	} else {
		codeArenas =
			(Memory *) malloc(sizeof(Memory) + sizeof(Word) * CODESIZE);
		codeArenas->mem_base = wordsof(Memory) + (Word *) codeArenas;
		codeArenas->mem_top = codeArenas->mem_base;
		codeArenas->mem_max = codeArenas->mem_base + CODESIZE;
		codeArenas->mem_size = CODESIZE;
		codeArenas->mem_freed = 0;
		codeArenas->mem_next = (Memory *) NULL;
		dataArenas =
			(Memory *) malloc(sizeof(Memory) + sizeof(Word) * DATASIZE);
		dataArenas->mem_base = wordsof(Memory) + (Word *) dataArenas;
		dataArenas->mem_top = dataArenas->mem_base;
		dataArenas->mem_max = dataArenas->mem_base + DATASIZE;
		dataArenas->mem_size = DATASIZE;
		dataArenas->mem_freed = 0;
		dataArenas->mem_next = (Memory *) NULL;
		tDataArenas =
			(Memory *) malloc(sizeof(Memory) + sizeof(Word) * TDATASIZE);
		tDataArenas->mem_base = wordsof(Memory) + (Word *) tDataArenas;
		tDataArenas->mem_top = tDataArenas->mem_base;
		tDataArenas->mem_max = tDataArenas->mem_base + TDATASIZE;
		tDataArenas->mem_size = TDATASIZE;
		tDataArenas->mem_freed = 0;
		tDataArenas->mem_next = (Memory *) NULL;

		programSize = CODESIZE + DATASIZE + TDATASIZE;
		freeList = NIL;
		SourceFile = NIL;		/* Just in case. */

		initStrHeaders();
		initSymTab();
		if(!load((initflg ? argv[initflg] : PATHwake)))
			panic("Can't load system code");
	}

	/*
	 * For some reason or other, Sun's setlinebuf() calls malloc() too!
	 * It seems to want a 128 byte block for something, perhaps to
	 * buffer the output.  If so, it does this even if it's already been
	 * given a bigger buffer.  This is stupid!
	 */
	setlinebuf(stdout);
	setlinebuf(stderr);

	if(getwd(currentDirectory) == (char *) NULL) {
		strcpy(currentDirectory, ".");
		warning("Unable to get current working directory.");
	}

	/*
	 * Malloc enough space for a few io buffers first to prevent
	 * stdio from blocking heap expansion as soon as a file
	 * is opened.  The space is returned immediately.
	 */
	if(freesize > 0)
		buffers = malloc(freesize * sizeof(Word));
	else
		buffers = (char *) NULL;

	if(heapsize < 2 * PageSize / sizeof(Word) + HeapBreathingSpace)
		heapsize = 2 * PageSize / sizeof(Word) + HeapBreathingSpace;
	if(stacksize < 2 * PageSize / sizeof(Word) + StackBreathingSpace)
		stacksize = 2 * PageSize / sizeof(Word) + StackBreathingSpace;
	if(trailsize < 2 * PageSize / sizeof(Word) + TrailBreathingSpace)
		trailsize = 2 * PageSize / sizeof(Word) + TrailBreathingSpace;

#ifdef PROTOVERFLOW
	heapsize = ALIGNANY(heapsize * sizeof(Word), PageSize) / sizeof(Word);
	stacksize = ALIGNANY(stacksize * sizeof(Word), PageSize) / sizeof(Word);
	trailsize = ALIGNANY(trailsize * sizeof(Word), PageSize) / sizeof(Word);

	size = PageSize / sizeof(Word) + heapsize + stacksize + trailsize;
#else /* PROTOVERFLOW */
	size = heapsize + stacksize + trailsize;
#endif /* PROTOVERFLOW */

	/*
	 * We also take and keep a small block to prevent memory
	 * reallocation from moving the heap backwards to make use of the
	 * freed buffers.
	 * There may be something free at this point, so we can't just
	 * malloc the barrier.
	 */
	barrier = balloc(4);

	memory = (Word *) balloc(size * sizeof(Word));
	if(memory == (Word *) NULL)
		panic("Can't allocate NU-Prolog working areas.");
	memoryMax = memory + size;
	
	if(barrier < buffers || (char *) memory < barrier)
		panic("Reallocation barrier not set properly.");

#ifdef PROTOVERFLOW
	heapBase = (Word *) ALIGNANY(memory, PageSize);
#else /* PROTOVERFLOW */
	heapBase = memory;
#endif /* PROTOVERFLOW */

	heapMax = heapBase + heapsize;
	heapTop = heapMax - HeapBreathingSpace;

	trailMax = (TrailRecord *) (heapBase + size);		/* Alignment? */
	trailTop = (TrailRecord *) (heapBase + size - TrailBreathingSpace);
	trailBase = (TrailRecord *) (heapBase + size - trailsize);
	stackMax = (Word *) trailBase;
	stackTop = stackMax - StackBreathingSpace;
	stackBase = stackMax - stacksize;

	if(freesize > 0)
		free(buffers);

	WakeUp = enterFunctor(1, &Sym_WakeUp)->f_code;
	PureWakeUp = enterFunctor(1, &Sym_PureWakeUp)->f_code;
	WakeDebug = enterFunctor(2, &SymSpy_WakeDebug)->f_code;

	initCFunct();
	initIO();
	initFlags(restflg);
#ifdef BSD4
	srandom(getpid() ^ (int) time((long *) 0));
#else /* BSD4 */
	srand48(getpid() ^ time((long *) 0));
#endif /* BSD4 */

	inf = enterFunctor(0, &SymInitializing);
	inf->f_code = initializing;
	inf->f_codeType = fCOMPILED;
	inf->f_sourceFile = StarToAtom(&SymUser);
	init = enterFunctor(0, &Sym_Init);
	for(i = restOfArgs; i < argc; i++) {
		if(argv[i][0] == '-' && argv[i][1] == 'a') {
			argflg = i + 1;
			break;
		}
		init->f_code = undefinedPredicate;
		init->f_codeType = fCOMPILED;
		init->f_sourceFile = NIL;
		if(!load(argv[i]))
			warning2("Unable to load %s", argv[i]);
		if(init->f_sourceFile != NIL)
			execute("$init", 0, NIL);
	}
	init->f_code = undefinedPredicate;
	init->f_sourceFile = NIL;
	inf->f_code = notInitializing;
	SourceFile = StarToAtom(&SymUser);

	dump();
	InterruptAction = intCOUNT;

	if(saveflg) {
		endfree = 1;
		free((char *) streams);
		free(barrier);
		free(memory);
		endfree = 0;
		save(argv[saveflg]);

		exit(0);
	}

	/* Finished with the dummy Machine. */
	CMR = (Machine *) NULL;

	execute(
		entryflg ? argv[entryflg] : NULL,
		argflg,
		commandArgs(argflg, argc, argv));

	exit(0);
}

#ifdef MEASURE
extern long LI;
extern long ReadModeOps[];
extern long WriteModeOps[];
extern long ShallowReadModeOps[];
extern long ShallowWriteModeOps[];
extern long ShallowModeExits[];
extern long ShallowEntries;
extern long ShallowFrames;
#endif /* MEASURE */

execute(entryPoint, argflg, args)
char *entryPoint;
register int argflg;
Object args;
{
	Object X[256];
	Machine MR;				/* Initial Machine */
	Machine *OldMR;			/* Possible old Machine */
	register Choice *B;
	register Environment *E;
	Word utime1, utime2, stime1, stime2;
	float time;
	int entryArity;
	Functor *main_func;
	Program main_code;

	if(entryPoint == NULL) {
		entryPoint = "main";
	}
	entryArity = (argflg ? 1 : 0);
	main_func =
		enterFunctor(entryArity, enterAtom(entryPoint, stab, (Atom *)NULL));
	main_code = main_func->f_code;
	if(main_code == (Program)NULL) {
		fprintf(stderr,"nep: no main program\n");
		exit(1);
	}

#ifdef PROTOVERFLOW
	(void) signal(SIGSEGV, receiveSignal);
	if(setRedZones()) {
		perror("");
		panic("Can't set red-zones.");
	}
#endif /* PROTOVERFLOW */

	if(timeflg)
		cputime(&utime1, &stime1);

	(void) setjmp(re_entry_point);

	Flags[flgLOCALDEBUG] = StarToAtom(&SymOff);
	Flags[flgDELAYED] = StarToAtom(&SymFail);

	if(argflg)
		X[0] = args;

	OldMR = CMR;
	CMR = &MR;

#ifdef ENCORE_JUNK
	{
		register Word *s;
		for(s = stackMax; s >= stackBase; s--)
			*s = processId;
		for(s = stackMax; s >= stackBase; s--)
			if(*s != processId) {
				fprintf(stderr, "*%x == %d\n", s, *s);
				panic("Stacks shared.");
			}
	}
#endif /* ENCORE_JUNK */

	E = (Environment *) stackBase;
	E->e_cp = ret;
	E->e_ce = (Environment *)0;
	E++;
	B = (Choice *) E;
	B->c_e = E;
	B->c_b = B;				/* Kludge for any top-level cut operators. */
	B->c_cp = ret;
	B->c_retry = ret;
	B->c_tr = trailBase;
	B->c_h = heapBase;
	B++;
	MR.mr_p = main_code;
	MR.mr_e = E;
	MR.mr_b = B;
	MR.mr_a = (Word *) B;
	MR.mr_h = heapBase;
	MR.mr_x = X;
	MR.mr_tr = trailBase;
	MR.mr_cp = ret;
	MR.mr_hb = heapBase;
	MR.mr_w = 0;
	MR.mr_suspended = (Machine *) NULL;

	interpret(main_func);
	if(timeflg) {
		cputime(&utime2, &stime2);
		time = (utime2 - utime1) / 1000.0;
		printf("%%\tTime was %8.3fs", time);
#ifdef MEASURE
		printf(" to perform %dLI.\n", LI);
		if(time != 0.0)
			printf("%%\tGiving %.0fLIPS\n", LI/time);
		{
			register int i;
			long DirectChoiceOps, IndirectChoiceOps;
			long ShallowExits = 0;
			long TotalReadOps = 0, TotalWriteOps = 0;

			for(i = 0; i < cLAST; i++) {

				ShallowExits += ShallowModeExits[i];
				TotalReadOps += ReadModeOps[i] + ShallowReadModeOps[i];
				TotalWriteOps += WriteModeOps[i] + ShallowWriteModeOps[i];
				printf("%9s: %7d = %7dr +%7dR +%7dw +%7dW.  %7de\n",
					bytecodes[i].op_name,
					ReadModeOps[i] + WriteModeOps[i]
					+ ShallowReadModeOps[i] + ShallowWriteModeOps[i],
					ReadModeOps[i], ShallowReadModeOps[i], 
					WriteModeOps[i], ShallowWriteModeOps[i],
					ShallowModeExits[i]);
			}
			DirectChoiceOps =
				ReadModeOps[cT] + WriteModeOps[cT];
			IndirectChoiceOps =
				ReadModeOps[cTE] + WriteModeOps[cTE];
			printf("Total Ops: %d = %dr + %dw\n",
				TotalReadOps + TotalWriteOps,
				TotalReadOps, TotalWriteOps);
			printf("Choices: %d = %d(T) + %d(TE)\n",
				DirectChoiceOps + IndirectChoiceOps,
				DirectChoiceOps, IndirectChoiceOps);
			printf("Shallow Entries: %d\n", ShallowEntries);
			printf("Shallow Frames: %d\n", ShallowFrames);
			printf("Shallow Exits: %d\n", ShallowExits);
		}
#else /* MEASURE */
		printf("\n");
#endif /* MEASURE */
	}

#ifdef PROTOVERFLOW
	(void) signal(SIGSEGV, SIG_DFL);
	if(clearRedZones()) {
		perror("");
		panic("Can't clear red-zones.");
	}
#endif /* PROTOVERFLOW */

	CMR = OldMR;
}

dump()
{
	register Word *loc;
	register int k;
	register Atom **ap;

	if(listflg)
		for(loc = code; loc < top; loc++) 
			loc += displayInstruction(stdout, code, loc);

	if(dumpflg)
		for(ap = stab->st_table, k = 0; k < (1 << stab->st_size); ap++, k++)
			if(*ap != (Atom *) NULL) {
				register Functor *f;

				fprintf(stdout, "%s:", eCharStar((*ap)->a_pname));
				f = (Functor *) (*ap)->a_functors;
				while(IsBlock((Word) f)) {
					f = (Functor *) eCValue((Word)f);
					fprintf(stdout,
						" %d(%x) @ 0x%x",
						f->f_arity,
						f->f_codeType,
						f->f_code);
					f = (Functor *) f->f_nextf;
				}
				fprintf(stdout, "\n");
			}
}

PFInt
addressOfFunction(f)
char *f;
{
	struct nlist entries[2];
	 
	entries[0].n_name = f;
	entries[1].n_name = "";
	if(nlist(nepologFileName, entries) == -1) {
		warning("nlist() failed in addressOfFunction().");
		return((PFInt) NULL);
	}
	return((PFInt) (entries[0].n_value));
}
