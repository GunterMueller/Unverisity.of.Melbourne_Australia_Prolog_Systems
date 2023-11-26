/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#define wordsof(x) (sizeof(x)/sizeof(Word))

#define ExtFieldVar(x, base, len) ((((UWord)(x)) >> (base)) & ~(-1<<(len)))
#define ExtField(x, base, len) \
	((base)+(len)==WORDSIZE ? ((UWord)(x)) >> (base) : ExtFieldVar(x,base,len))
#define ExtSignedField(x, base, len) \
	((Word)((x) << (WORDSIZE-(base)-(len))) >> (WORDSIZE-(len)))
#define FormField(x, base, len) /* (((x) & ~(-1<<(len))) << (base)) */ \
	((base)+(len)==WORDSIZE \
		? ((UWord)(x)) << (base) \
		: ((x) & ~(-1<<(len))) << (base))
#define MakeMask(base, len) (~(-1<<(len)) << (base))

#include "tags.h"

/* Object Word
typedef union {
	struct {
		Indirect: 1;
		Type: 3;
		CType: 2;
		Ref: 26;
	} v1;
	struct {
		dummy1: 1;
		SType: 2;
		DType: 1;
		Value: 28;
	} v2;
	struct {
		Tag: 4;
		dummy2: 2;
		CValue: 26;
	} v3;
	struct {
		dummy3: 1;
		TypedValue: 31;
	} v4;
	struct {
		dummy4: 3;
		UType: 3;
		dummy5: 26;
	} v5;
	struct {
		FType: 6;
		dummy6: 26;
	} v6;
} Object;
*/
typedef Word Object;

/* In tags.h
#ifdef LARGEMODEL
#define lofIndirect 0
#else
#define lofIndirect 1
#endif
#define bofIndirect (WORDSIZE-lofIndirect)
#define lofTypedValue bofIndirect
#define bofTypedValue 0
#define lofType 3
#define bofType (bofIndirect-lofType)
#define lofTag (lofIndirect+lofType)
#define bofTag (WORDSIZE-lofTag)
#define lofDType 1
#define bofDType bofType
#define lofSType 2
#define bofSType (bofDType+lofDType)
#define lofCType 2
#define bofCType (bofType-lofCType)
#define lofUType 3
#define bofUType bofCType
#define lofFType (lofIndirect + lofType + lofCType)
#define bofFType bofCType
#define lofRef bofCType
#define bofRef 0
#define lofValue bofType
#define bofValue 0
#define lofCValue bofCType
#define bofCValue 0

#define eIndirect(x) ExtField(x, bofIndirect, lofIndirect)
#define eTypedValue(x) ExtField(x, bofTypedValue, lofTypedValue)
#define eType(x) ExtField(x, bofType, lofType)
#define eSType(x) ExtField(x, bofSType, lofSType)
#define eDType(x) ExtField(x, bofDType, lofDType)
#define eCType(x) ExtField(x, bofCType, lofCType)
#define eUType(x) ExtField(x, bofUType, lofUType)
#define eFType(x) ExtField(x, bofFType, lofFType)
#define eTag(x) ExtField(x, bofTag, lofTag)
#define eRef(x) ((Object *)ExtField(x, bofRef, lofRef))
#define eValue(x) ExtField(x, bofValue, lofValue)
#define eCValue(x) ExtField(x, bofCValue, lofCValue)
*/
#define fIndirect(x) FormField(x, bofIndirect, lofIndirect)
#define fTypedValue(x) FormField(x, bofTypedValue, lofTypedValue)
#define fType(x) FormField(x, bofType, lofType)
#define fSType(x) FormField(x, bofSType, lofSType)
#define fDType(x) FormField(x, bofDType, lofDType)
#define fCType(x) FormField(x, bofCType, lofCType)
#define fUType(x) FormField(x, bofUType, lofUType)
#define fFType(i, t, c) \
	FormField(((i) << lofType | (t)) << lofCType | (c), bofFType, lofFType)
#define fTag(i, x) FormField((i) << lofType | (x), bofTag, lofTag)
#define fRef(x) FormField(x, bofRef, lofRef)
#define fValue(x) FormField(x, bofValue, lofValue)
#define fCValue(x) FormField(x, bofCValue, lofCValue)

/* Check that an address can be represented within the Object datatype. */
#define LegalAddress(x) (eRef(x) == (Object *) (x))

/* Get a char * from a Prolog string. */
#define eCharStar(x) ((x) == NIL ? "" : (char *) eRef(x))

/* Various faster macros for special situations

eRefStar(x) ((Object *)(x))	-- x is a pointer || IsRef(x) && IsIndirect(x)
*/
#ifdef NORMAL
#ifdef LBT
#define eRefStar(x) eRef(x)
#else /* LBT */
#define eRefStar(x) ((Object *)(x))
#endif /* LBT */
#else /* NORMAL */
#define eRefStar(x) eRef(x)
#endif /* NORMAL */

#define iIND 0
#define iIMM 1

/* WARNING:
 * The term comparison functions rely on the relative ordering of
 * the twelve t... constants.  If this changes, the function
 * p_compare must be re-written.
 */

#define tVAR 00
#define tCON 01
#define tSEQ 02
#define tREC 03

#define dt0 0
#define dt1 1

#define tREF (tVAR << lofDType | dt0)	/* 0 */
#define tDEL (tVAR << lofDType | dt1)	/* 1 */
#define tICN (tCON << lofDType | dt0)	/* 2 */
#define tUCN (tCON << lofDType | dt1)	/* 3 */
#define tLST (tSEQ << lofDType | dt0)	/* 4 */
#define tCHR (tSEQ << lofDType | dt1)	/* 5 */
#define tSTR (tREC << lofDType | dt0)	/* 6 */
#define tBMV (tREC << lofDType | dt1)	/* 7 */

#define MakeVar(i, t, x) (fTag((iIND), (t)) | fRef((Word)(x)))

#ifdef NORMAL
#define MakeRef(x) ((Object)(x))
#define MakeIndirect(x) (MakeMask(bofTypedValue, lofTypedValue) & (Word)(x))
#define MakeImmediate(x) (~MakeMask(bofTypedValue, lofTypedValue) | (Word)(x))
#else /* NORMAL */
#define MakeRef(x) MakeVar(iIND, tREF, x)
#define MakeIndirect(x) (fIndirect(iIND) | fTypedValue((Word)(x)))
#define MakeImmediate(x) (fIndirect(iIMM) | fTypedValue((Word)(x)))
#endif /* NORMAL */

#define MakeUnboundRef(x) MakeVar(iIMM, tREF, x)
#define MakeUnboundDel(x) MakeVar(iIMM, tDEL, x)
#define MakeObject(t, ct, x) (fTag(iIMM, t) | fCType(ct) | fRef((Word)(x)))
#define MakeStr(x) (fTag(iIMM, tSTR) | fRef((Word)(x)))
#define MakeBMV(x) (fTag(iIMM, tBMV) | fRef((Word)(x)))

/* Lists
Lists are represented as pairs of words.
Strings of characters are represented by null-terminated
character arrays.
*/
#define MakeList(x) (fTag(iIMM, tLST) | fRef((Word)(x)))
#define MakeString(x) (fTag(iIMM, tCHR) | fRef((Word)(x)))

/* WARNING:
 * The term comparison functions rely on the relative ordering of
 * the ct... constants.  If this changes, the function
 * p_compare must be re-written.
 */

#define ctINT 00
#define ctATM 01
#define ctIM0 02			/* Unused value */
#define ctBLK 03

#define ctI32 00
#define ctFLT 01

#define utINT (dt0 << lofCType | ctINT)	/* 0 */
#define utATM (dt0 << lofCType | ctATM)	/* 1 */
#define utBLK (dt0 << lofCType | ctBLK)	/* 3 */
#define utI32 (dt1 << lofCType | ctI32)	/* 4 */
#define utFLT (dt1 << lofCType | ctFLT)	/* 5 */

#define MakeConstant(ct, x) (fTag(iIMM, tICN) | fCType(ct) | fCValue((Word)(x)))
#define MakeUConstant(ct, x) (fTag(iIMM, tUCN)|fCType(ct)|fCValue((Word)(x)))

#define MakeAtom(x) MakeConstant(ctATM, x)
#define MakeInt(x, h) \
	(SmallInt(x) \
		? MakeSmallInt(x) \
		: (*(h) = x, /* BUG!  Heap Overflow */ StarToInt32((h)++)) \
	)
#define MakeSmallInt(x) MakeConstant(ctINT, x)
#define MakePositiveInt(x) \
	MakeConstant(ctINT, MakeMask(bofCValue, lofCValue - 1) & x)
#define AddToSmallInt(x, i) ((x) + ((i) << bofCValue))
#define IncrementSmallInt(x, i) ((x) += (i) << bofCValue)
#define MakeFloat(x) MakeUConstant(ctFLT, x)
#define MakeInt32(x) MakeUConstant(ctI32, x)
#define NIL	StarToAtom(&SymNIL)
#define MakeBlock(x) MakeConstant(ctBLK, x)

#define ttREF eTag(fTag(iIND, tREF))
#define ttDEL eTag(fTag(iIND, tDEL))
#define ttICN eTag(fTag(iIMM, tICN))
#define ttUCN eTag(fTag(iIMM, tUCN))
#define ttLST eTag(fTag(iIMM, tLST))
#define ttCHR eTag(fTag(iIMM, tCHR))
#define ttSTR eTag(fTag(iIMM, tSTR))
#define ttBMV eTag(fTag(iIMM, tBMV))

#define ftREF eFType(fFType(iIND, tREF, 0))
#define ftDEL eFType(fFType(iIND, tDEL, 0))
#define ftINT eFType(fFType(iIMM, tICN, ctINT))
#define ftATM eFType(fFType(iIMM, tICN, ctATM))
#define ftBLK eFType(fFType(iIMM, tICN, ctBLK))
#define ftFLT eFType(fFType(iIMM, tUCN, ctFLT))
#define ftI32 eFType(fFType(iIMM, tUCN, ctI32))
#define ftLST eFType(fFType(iIMM, tLST, 0))
#define ftCHR eFType(fFType(iIMM, tCHR, 0))
#define ftSTR eFType(fFType(iIMM, tSTR, 0))
#define ftBMV eFType(fFType(iIMM, tBMV, 0))

/* Maximum and minimum integers representable by Prolog. */
#define PMAXSMALLINT ((1 << (lofCValue - 1)) - 1)
#define PMINSMALLINT (-(1 << (lofCValue - 1)))
#define PMAXINT (~(-1 << (WORDSIZE - 1)))
#define PMININT (1 << (WORDSIZE - 1))

#define SmallInt(x) (x == ExtSignedField(x, 0, lofCValue))

#ifdef NORMAL
#ifdef LARGEMODEL
#define IsIndirect(x) (IsVar(x) && (x) != *eRef(x))
#define IsUnbound(x) (IsVar(x) && (x) == *eRef(x))
#define IsVar(x) (eSType(x) == tVAR)
#else /* LARGEMODEL */
#define IsIndirect(x) ((Word)(x) >= 0 && (x) != *eRef(x))
						/* Relies on adjacency of Indirect and SType fields */
						/* and only works if x still has its tag */
#define IsUnbound(x) ((Word)(x) >= 0 && (x) == *eRef(x))
#define IsVar(x) ((Word)(x) >= 0)
#endif /* LARGEMODEL */
#else /* NORMAL */
#define IsIndirect(x) (IsVar(x) && (x) != *eRef(x))
#define IsUnbound(x) (IsVar(x) && (x) == *eRef(x))
#define IsVar(x) (eSType(x) == tVAR)
#endif /* NORMAL */
#define IsVarUnbound(x) ((x) == *eRef(x))	/* x known to be a var */

#define IsRef(x) (eTag(x) == ttREF)
#define IsDel(x) (eTag(x) == ttDEL)
#define IsImmediate(x) (!IsIndirect(x))
#define IsBound(x) !IsUnbound(x)
#define IsConst(x) (eSType(x) == tCON)
#define IsIConst(x) (eTag(x) == ttICN)
#define IsUConst(x) (eTag(x) == ttUCN)
#define IsStr(x) (eTag(x) == ttSTR)
#define IsBMV(x) (eTag(x) == ttBMV)
#define IsSequence(x) (eSType(x) == tSEQ)
#define IsList(x) (eTag(x) == ttLST)
#define IsString(x) (eTag(x) == ttCHR)

#define IsAtom(x) (eFType(x) == ftATM)
#define IsInt(x) (IsSmallInt(x) || IsInt32(x))
#define IsSmallInt(x) (eFType(x) == ftINT)
#define IsNIL(x) ((x) == NIL)
#define IsBlock(x) (eFType(x) == ftBLK)
#define IsFloat(x) (eFType(x) == ftFLT)
#define IsInt32(x) (eFType(x) == ftI32)
#define IsNumber(x) (IsSmallInt(x) || IsFloat(x) || IsInt32(x))

#define eInt(x) (IsSmallInt(x) ? eSmallInt(x) : eInt32(x))
#define eSmallInt(x) ExtSignedField(x, bofCValue, lofCValue)
#define eFloat(x) (* (double *) eRef(x))
#define eInt32(x) (* (Word *) eRef(x))

typedef Word Structure;

#define MakeStrHeader(arity, x) \
	((Structure) enterFunctor(arity, (Atom *) eRef(x)))

#define eNArgs(x) (((Functor *) (x))->f_arity)
#define eFunctor(x) (((Functor *) (x))->f_functor)

#define MAXARITY 4096
#define MINARITY 5

/* StarTo....
Macros that ld(1) can usually be persuaded to accept as initializers.
They are also somewhat faster, particularly with dumb compilers that
emit zero shifts.

Syms.c and various other files may need a lot of fiddling on nonNORMAL
machines.
*/
#ifdef NORMAL
#define StarToVar(i, t, x) (fTag((iIND), (t)) + (Word)(x))
#define StarToRef(x) ((Object)(x))
#define StarToIndirect(x) (fIndirect(iIND) + (Word)(x))
#define StarToImmediate(x) (fIndirect(iIMM) + (Word)(x))
#define StarToUnboundRef(x) StarToVar(iIND, tREF, x)
#define StarToUnboundDel(x) StarToVar(iIND, tDEL, x)
#define StarToStr(x) (fTag(iIMM, tSTR) + (Word)(x))
#define StarToList(x) (fTag(iIMM, tLST) + (Word)(x))
#define StarToString(x) (fTag(iIMM, tCHR) + (Word)(x))
#define StarToConstant(ct, x) ((fTag(iIMM, tICN) + fCType(ct)) + (Word)(x))
/*
#define StarToConstant(ct, x) \
	((Word)(fTag(iIMM, tICN) + fCType(ct) + (char *)(x)))
*/
#define StarToUConstant(ct, x) (fTag(iIMM, tUCN) + fCType(ct) + (Word)(x))
#define StarToAtom(x) StarToConstant(ctATM, x)
#define StarToBlock(x) StarToConstant(ctBLK, x)
#define StarToFloat(x) StarToUConstant(ctFLT, x)
#define StarToInt32(x) StarToUConstant(ctI32, x)
#else /* NORMAL */
#define StarToVar(i, t, x) MakeVar(i, t, x)
#define StarToRef(x) MakeRef(x)
#define StarToIndirect(x) MakeIndirect(x)
#define StarToImmediate(x) MakeImmediate(x)
#define StarToUnboundRef(x) MakeUnboundRef(x)
#define StarToUnboundDel(x) MakeUnboundDel(x)
#define StarToStr(x) MakeStr(x)
#define StarToList(x) MakeList(x)
#define StarToString(x) MakeString(x)
#define StarToUConstant(ct, x) MakeUConstant(ct, x)
#define StarToAtom(x) MakeAtom(x)
#define StarToBlock(x) MakeBlock(x)
#define StarToFloat(x) MakeFloat(x)
#define StarToInt32(x) MakeInt32(x)
#endif /* NORMAL */
#define StarToStrHeader(arity, x) MakeStrHeader(arity, x)

/* Instruction Word
*/
typedef Word Instruction;
typedef Instruction *Program;

#define INSTSPERWORD (sizeof(Word) / sizeof(Instruction))

/*
#define lofOpCode 8
#define bofOpCode 0
#define lofArg1 8
#define bofArg1 lofOpCode
#define lofArg2 8
#define bofArg2 (lofArg1+bofArg1)
*/
/*
#define lofDisp 18
#define bofDisp lofOpCode
*/

#ifdef LITTLEENDIAN
#define lofOpCode 8
#define bofOpCode 0
#define lofArg1 8
#define bofArg1 8
#define lofArg2 8
#define bofArg2 16
#define lofArg3 8
#define bofArg3 24
#else
#define lofOpCode 8
#define bofOpCode 24
#define lofArg1 8
#define bofArg1 16
#define lofArg2 8
#define bofArg2 8
#define lofArg3 8
#define bofArg3 0
#endif

#define eOpCode(x) ExtField(x, bofOpCode, lofOpCode)
#define eArg1(x) ExtField(x, bofArg1, lofArg1)
#define eArg2(x) ExtField(x, bofArg2, lofArg2)
#define eArg3(x) ExtField(x, bofArg3, lofArg3)
/*
#define eDisp(x) ExtSignedField(x, bofDisp, lofDisp)
*/
#define fOpCode(x) FormField(x, bofOpCode, lofOpCode)
#define fArg1(x) FormField(x, bofArg1, lofArg1)
#define fArg2(x) FormField(x, bofArg2, lofArg2)
#define fArg3(x) FormField(x, bofArg3, lofArg3)
/*
#define fDisp(x) FormField(x, bofDisp, lofDisp)
*/

#define MakeIns0(op) fOpCode(op)
#define MakeIns1(op, a1) (fOpCode(op) | fArg1(a1))
#define MakeIns2(op, a1, a2) (fOpCode(op) | fArg1(a1) | fArg2(a2))
#define MakeIns3(op, a1, a2, a3) (fOpCode(op)|fArg1(a1)|fArg2(a2)|fArg3(a3))

/* Atom Structure
*/
typedef struct {
	Object a_header;	/* The constant MakeStrHeader(5, &SymAtom) */
	Object a_pname;
	Object a_module;
	Object a_functors;	/* A BLK pointer to a list of Functors */
	Object a_prop;
	Object a_prec;
} Atom;

#define systemAtomIndex(x) ((char *) (x) - (char *) systemAtoms)

#define fINTERPRETED	001
#define fCOMPILED		002
#define fNATIVE			003
#define fSPYPT			004

/* Functor Structure
*/
typedef struct Functor {
	Program f_code;
	struct Functor *f_nextf;	/* Next Functor, or the parent Atom */
	Object f_sourceFile;
	Atom *f_functor;
	short f_arity;
	char f_codeType;
} Functor;

/* Environment Structure
An environment is represented by an Environment struct on the stack,
followed by a vector of Words containing the values of the saved
local variables.  The length of this vector is determined by following
the CP register to the CALL instruction.
*/
typedef struct Environment {
	Program e_cp;
	struct Environment *e_ce;
} Environment;

/* Delay Structure
A delayed call is represented by a Delay struct on the heap,
followed by a vector of d_n Words containing the values of the
saved temporary registers.  This is effectively a closure.

A Delay has been woken if d_sleeping == 0.  If the delayed clause
is being spied on, then d_sleeping will be the term used to display
the goal and CP is saved as an extra register value after the other
saved registers so that the spypoint prefix can be properly spliced
into the instruction stream on waking.  (In this case d_n is increased
by one.) Otherwise d_sleeping == 1.
*/
typedef struct Delay {
	Program d_p;
	Word d_sleeping;
	Word d_n;
} Delay;

/* DelayHeader Structure
*/
typedef struct {
	Object *h_self;
	Object h_marks;
} DelayHeader;

/* Mark Structure
A delayed variable is bound to a DelayHeader which points to a list of
Delays waiting on it.  This is arranged as a Prolog list so that
it may be handed direct to the Prolog routine that wakes calls.

For convenience, a Mark struct is also defined.
*/
typedef struct Mark {
	Object m_delay;
	Object m_nextMark;
} Mark;

/* Trail
The trail is a stack of (address, value) pairs which
indicate addresses to be restored to specific values on failure.

Some inlining may depend on the order of fields in the record.
*/

typedef struct {
	Word *tr_address;
	Word tr_value;
} TrailRecord;

/* Choice Point Structure
A choice point is represented by a Choice struct on the stack,
followed by a vector of Words containing the values of the saved
temporary registers.

How do we tell if c_retry is interpreted?  Answer: we just assume that
it's the same type of code as is currently running.  Calls between
different types of code must put code stubs in the appropriate places
to make the transition.

Some inlining may depend on the order of fields in the record.
*/
typedef struct Choice {
	Environment *c_e;
	struct Choice *c_b;
	Program c_cp;
	Program c_retry;
	TrailRecord *c_tr;
	Word *c_h;
	/*Word c_n;*/
} Choice;

/* Symbol Tables
Symbol tables are variable sized arrays of pointers to Atoms.
At least, that is what they are now.

We require SYMTABLOAD * (1 << SYMTABMIN) + 2 <= SYMTABMIN.
*/

#define SYMTABLOAD .75		/* load factor */
#define SYMTABMIN 5 		/* minimum size (32 entries) */
#define SYMTABMAX 18 		/* maximum size (256k entries) */

typedef struct SymbolTable {
	Object st_name;			/* which atom owns the table */
	Word st_size;			/* how big it is */
	Word st_entries;		/* how much of it is used */
	Word st_load;			/* how much of it to use */
	Atom **st_table;
} SymbolTable;

#define MAXPUSHEDCHAR 2		/* Maximum characters pushed back on input */
#define EXTRASTREAMS 4		/* Number of new Streams to make on overflow */

/* Stream
The character and line position counting code is a gross hack.  It gets
lost rather easily, and there is no way to make C code keep the counts
up to date.

There are many mutually exclusive fields here.  The only reason for not
putting them in a union is the two extra levels of naming then required.
Streams are few enough that a little wasted space doesn't matter.
*/
typedef struct Stream {
	/* Prolog part */
	Object s_header;		/* The constant MakeStrHeader(7, &SymStream) */
	Object s_name;			/* The atom used to name the file */
	Object s_mode;			/* One of read, write, append, */
							/* string{Read,Write}, NIL */
	Object s_identifier;	/* The term $stream(fd, s_instance) */
							/* or user_{input,output,error} */
	Object s_iseof;			/* EOF reported on read */
	Object s_chars;			/* Characters transfered */
	Object s_lines;			/* Lines transfered */
	Object s_linepos;		/* Position on line */
	/* C part */
	Word s_identArray[3];	/* Space for $stream(fd, s_instance) */
							/* It must usually be copied onto the heap */
	int (*s_get)();			/* Get a char */
	int (*s_unget)();		/* Push back a char */
	int (*s_put)();			/* Put a char */
	void (*s_puts)();		/* Put a string */
	int (*s_eof)();			/* Eof */
	FILE *s_fp;
	Word s_instance;		/* How many times has this stream been closed */
	Word s_npushed;			/* Number of characters pushed back on input */
	Word s_pushed[MAXPUSHEDCHAR];	/* Characters pushed */
	struct Stream *s_counts;/* The struct that is keeping the counts */
	Word s_pid;				/* Pid of owner if pipe else -1 */
	char *s_base;			/* Base if a string else NULL */
	char *s_c;				/* Current char in string */
	char *s_end;			/* Last char in string */
	char *s_max;			/* End of string */
} Stream;

/* Machine
Contains all the Prolog Machine Registers.  Most of them are copied
into register variables and written back to the Machine as necessary.

The global variable CMR points to the current Machine.
The stack of suspended Machines is linked together.
*/
typedef struct Machine {
	Program mr_p;
	Environment *mr_e;
	Choice *mr_b;
	Word *mr_a;
	Word *mr_h;
	Object *mr_x;
	TrailRecord *mr_tr;
	Program mr_cp;
	Word *mr_hb;
	Object mr_w;
	struct Machine *mr_suspended;
} Machine;

/* Data Areas
Names for the data areas involved in copying and uncopying.
*/
#define iDATA 0
#define iTDATA 1
#define iHEAP 2

/* Memory Structure
Structure used to maintain lists of the memory chunks used
for data, tData, and code.
*/
typedef struct Memory {
	Word *mem_base;
	Word *mem_top;
	Word *mem_max;
	Word mem_size;
	Word mem_freed;
	struct Memory *mem_next;
} Memory;

/* Arith Structure
Structure used for the arithmetic stacks.
Not all the fields are always used.

ARITHSIZE is the maximum size of the arithmetic stack.  This is built
on the local stack, primarily to ensure that it is at a LegalAddress.

The ar* constants are sometimes anded together for decision making.
Consequently, they shouldn't be altered without considering their use.

The are* constants are error conditions set by the arithmetic routines.
*/
#define ARITHSIZE 128

#define arINT	03
#define arFLT	01
#define arNAN	00			/* Not a number.  I.E. tVAR or tSTR */

#define areOK			0		/* No error */
#define areNON			1		/* Not an expression */
#define areDIVZERO		2		/* Divide by zero */
#define areINT			3		/* Integer arguments needed for operation */
#define areNONGROUND	4		/* Must be ground */
#define areMISC			5		/* Unspecified error */

typedef struct {
	double ar_float;		/* Value if float */
	Word ar_int;			/* Value if integer */
	Object ar_value;		/* Prolog representation of the object */
	int ar_type;			/* Type of the object */
} Arith;

/* CFunctions
A few of the stranger parts of the system (only the database at present)
need to be informed when they are cut off.

This is done with C functions,
which are represented by reserve indexes on the trail.
They are called by trimtrail when found.
*/
typedef int (*PFInt)();

/* Another handy type
*/
typedef void (*PFVoid)();

#define IsCFunctNumber(n) ((n) < lastCFunctDummy && cFunctDummy <= (n))

/* Interrupt Actions
*/
#define intCOUNT 0
#define intABORT 1
#define intRETURN 2

/* Prolog Flags
N.B. Flag 0 is the StrHeader.
*/
#define flgCHAR					1
#define flgFILE					2
#define flgDEBUG				3
#define flgWIZARD				4
#define flgLIBRARIES			5
#define flgBINARIES				6
#define flgDATAGROW				7
#define flgLOCALDEBUG			8
#define flgLEASHMODE			9
#define flgCALLNUMBER			10
#define flgDELAYED				11
#define flgVARS					12
#define flgREDEFWARN			13
#define flgPRINTDEPTH			14
#define flgTDATAGROW			15
#define flgCOMMANDS				16
#define flgCODEGROW				17
#define flgOPTIMIZEQUERIES		18
#define flgMAXDEPTH				19
#define flgFLOAD				20
#define flgMACHINE				21
#define flgVERSION				22
#define flgNSTREAMS				23
#define flgMAXARITY				24

#define NFLAGS 25

#define testFlag(n, atom) (Flags[n] == StarToAtom(&atom))
#define testFlagOn(n) testFlag(n, SymOn)
#define testFlagOff(n) testFlag(n, SymOff)

/* Leash Mode Flags
*/
#define leashCALL		001
#define leashEXIT		002
#define leashREDO		004
#define leashFAIL		010
#define leashWAKE		020
#define leashDELAY		040
#define leashALL		077

/* Unix System Calls Numbers
*/
#define sysACCESS		 0
#define sysCHDIR		 1
#define sysCHMOD		 2
#define sysENVIRONMENT	 3
#define sysGETEGID		 4
#define sysGETGID		 5
#define sysGETGROUPS	 6
#define sysGETLOGIN		 7
#define sysGETPID		 8
#define sysGETPPID		 9
#define sysGETPWUID		10
#define sysGETPWNAM		11
#define sysGETUID		12
#define sysGETEUID		13
#define sysGETWD		14
#define sysHOSTNAME		15
#define sysKILL			16
#define sysLINK			17
#define sysMKDIR		18
#define sysRANDOM		19
#define sysRENAME		20
#define sysRMDIR		21
#define sysSLEEP		22
#define sysSTAT			23
#define sysTIME			24
#define sysTRUNCATE		25
#define sysUMASK		26
#define sysUNLINK		27
#define sysWAIT			28
#define sysREADDIR		29
#define sysFORK			30
#define sysSYSTEM		31
#define sysEXEC			32
#define sysSTATISTICS	33
#define sysFSEEK		34
#define sysFTELL		35
#define sysHOSTID		36
#define sysSIGNAL		37

#define NSYSCALLS		38

/* Data For System Calls
The SYSCALL builtin uses an array of SysCall structs to handle the
various system calls.

Types are ctINT or ctATM.
*/
typedef struct {
	PFInt sys_f;				/* Function to call */
	char sys_value;				/* Type of return value */
	char sys_n;					/* Number of input args */
	char sys_args[3];			/* Types of input args */
} SysCall;

/* Write Flags
Flags given to writev.
*/

#define wflgLIST		0
#define wflgSTRING		1
#define wflgQUOTE		2
#define wflgQUOTEALL	3
#define wflgOPS			4

#define NWFLAGS			5

#define lofBase 6
#define bofBase 0
#define lofFlags NWFLAGS
#define bofFlags lofBase
#define lofPrec 11
#define bofPrec (bofFlags + lofFlags)

#define eBase(x) ExtField(x, bofBase, lofBase)
#define eFlags(x) ExtField(x, bofFlags, lofFlags)
#define ePrec(x) ExtField(x, bofPrec, lofPrec)
#define fBase(x) FormField(x, bofBase, lofBase)
#define fFlags(x) FormField(x, bofFlags, lofFlags)
#define fPrec(x) FormField(x, bofPrec, lofPrec)

#define MakeFlags(base, prec, flags) (fBase(base)|fFlags(flags)|fPrec(prec))

#ifdef PROTOVERFLOW
/*
Current state of the red-zones at the ends of the stacks.
*/
#define rzCLEAR 0
#define rzCHECK 1
#define rzSET 2
#endif /* PROTOVERFLOW */

#ifdef DEBUG

typedef struct {
	Word *sp_address;
	Word sp_value;
} SpyStruct;

#define NPASTPCS 100

#endif /* DEBUG */
