/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#define ot0ARGS	0000
#define ot1ARGS	0001
#define ot2ARGS	0002
#define ot3ARGS	0003
#define ot4ARGS	0004
#define ot5ARGS	0005
#define ot6ARGS	0006
#define otLABEL	0010
#define otIMMED	0020
#define otSTRUC	0040
#define otTABLE	0100
#define otPROC	0200

#define otNARGS 0007

#define ot0		ot0ARGS
#define ot1		ot1ARGS
#define ot2		ot2ARGS
#define ot3		ot3ARGS
#define ot4		ot4ARGS
#define ot5		ot5ARGS
#define ot6		ot6ARGS
#define ot0L	ot0ARGS|otLABEL
#define ot1L	ot1ARGS|otLABEL
#define ot2L	ot2ARGS|otLABEL
#define ot3L	ot3ARGS|otLABEL
#define ot1P	ot1ARGS|otIMMED|otPROC
#define ot2P	ot2ARGS|otIMMED|otPROC
#define ot0I	ot0ARGS|otIMMED
#define ot1I	ot1ARGS|otIMMED
#define ot2I	ot2ARGS|otIMMED
#define ot3I	ot3ARGS|otIMMED
#define ot1IL	ot1ARGS|otIMMED|otLABEL
#define ot0S	ot0ARGS|otIMMED|otSTRUC
#define ot1S	ot1ARGS|otIMMED|otSTRUC
#define ot2S	ot2ARGS|otIMMED|otSTRUC
#define ot3S	ot3ARGS|otIMMED|otSTRUC
#define ot1SL	ot1ARGS|otIMMED|otSTRUC|otLABEL
#define ot1T	ot1ARGS|otIMMED|otTABLE
#define ot2T	ot2ARGS|otIMMED|otTABLE

typedef struct {
	char *op_name;
	short op_type;
} opcode;

/* Bytecode Table
 * The table must be in exactly the format below.  It is processed
 * by awk(1) to make bytecodes.h and sys/bytecodes.nl, and by
 * na/lex.make to make na/lex.c.  There may be others!
 *
 * Do not nest #ifdefs or use forms more complicated than #ifdef THINGY
 * without examining the scripts above.
*/

opcode bytecodes[] = {
{"GVARX",	ot2},	/* Used for PVALX too. */
{"GVARA",	ot2},	/* Used for PVALX too. */
{"GVARY",	ot2},
{"GLVARY",	ot2},	/* Get Local Variable */
{"GVALX",	ot2},
{"GVALY",	ot2},
{"GC",		ot1I},
{"GNIL",	ot1},
{"GS",		ot1S},
{"GSX1",	ot2S},	/* GS X1,$Pred/Arity;  UVARX X2 */
{"GSX2",	ot3S},	/* GS X1,$Pred/Arity;  UVARX X2;  UVARX X3 */
{"GL",		ot1},
{"GLVX2",	ot3},	/* GL X1;  UVARX X2;  UVARX X3 */
{"PVARX",	ot2},
{"PVARY",	ot2},
{"PVALY",	ot2},
{"PVALY2",	ot3},	/* X3 := Y1; X[A3+1] := Y2 */
{"PUVAL",	ot2},
{"PC",		ot1I},
{"PNIL",	ot1},
{"PS",		ot1S},
{"PL",		ot1},
{"UVOID",	ot1},	/* Unify A1 void variables */
{"UVOID1",	ot0},	/* Unify 1 void variable */
{"UVARX",	ot1},	/* Unify Variable */
{"UVARA",	ot1},	/* Unify Variable */
{"UVARY",	ot1},	/* Unify Variable */
{"UVALX",	ot1},	/* Unify Value */
{"UVALY",	ot1},	/* Unify Value */
{"ULVX",	ot1},	/* Unify local Value */
{"ULVY",	ot1},	/* Unify local Value */
{"UC",		ot0I},	/* Unify Constant */
{"UL",		ot1},	/* Unify List (X1 dummy) */
{"US",		ot1S},	/* Unify Struct (X1 dummy) */
{"UVARXVARX", ot2},	/* UVARX X1; UVARX X2 */
{"UVARAVARA", ot2},	/* UVARX [AX]1; UVARX [AX]2 (at least one A) */
{"UVARYVARY", ot2},	/* UVARY Y1; UVARY Y2 */
{"UVARXLVX", ot2},	/* UVARX X1; ULVX X2 */
{"UVARALVX", ot2},	/* UVARX A1; ULVX X2 */
{"ULVXVARX", ot2},	/* ULVX X1; UVARX X2 */
{"ULVXVARA", ot2},	/* ULVX X1; UVARX A2 */
{"ULVXLVX", ot2},	/* ULVX X1; ULVX X2 */
{"ARITH",	ot0},	/* Set up for arithmetic -- very rare */
{"APUSHX",	ot1},	/* ARITH, PUSHX */
{"APUSHY",	ot1},	/* ARITH, PUSHY */
{"APUSHI",	ot0I},	/* ARITH, PUSHI */
{"APUSHF",	ot0I},	/* ARITH, PUSHF */
{"PUSHX",	ot1},	/* *A++ = X1 */
{"PUSHY",	ot1},	/* *A++ = Y1 */
{"PUSHVX",	ot1},	/* *A++ = X1 = new variable */
{"PUSHVY",	ot1},	/* *A++ = Y1 = new variable */
{"PUSHI",	ot0I},	/* *A++ = Integer */
{"PUSHF",	ot0I},	/* *A++ = Float */
{"POPX",	ot1},	/* X1 = *--A */
{"POPY",	ot1},	/* Y1 = *--A */
{"POPVX",	ot1},	/* unify(X1, *--A) */
{"POPVY",	ot1},	/* unify(Y1, *--A) */
{"POPC",	ot0I},	/* if(*--A != Constant) goto fail; */
{"AFUNC",	ot1},	/* arithmetic on top of stack */
{"APRED",	ot1},	/* arithmetic predicate on top of stack */
{"JPRED",	ot1L},	/* APRED, if fails goto Label */
{"WEVAL",	ot0},	/* Wake an evaluation if ground.  Bind X[0] to eval(X[1]).*/
{"OCCURS",	ot2},	/* occurs(X1, X2) */
{"NAME",	ot2},	/* name(X1, X2) */ /* To be improved */
{"LTOS",	ot2},	/* listToString(X1, X2) */
{"UNIV",	ot2},	/* X1 =.. X2 */ /* To be improved */
{"FUNCTOR",	ot3},	/* functor(X1, X2, X3) */
{"ARG",		ot3},	/* arg(X1, X2, -X3) */
{"ISEQ",	ot3},	/* $is_eq(X1, X2, X3) */
{"WISEQ",	ot0},	/* Wake an inequality. */
{"CVTTOH",	ot1},	/* Arcane op to Copy Variables To Top Of Heap */
{"PLUS",	ot3},	/* plus(X1, X2, X3) */
{"DEFINED",	ot4},	/* $defined(X1, X2, -X3, -X4) */
{"ARITIES",	ot2},	/* $predicateArities(X1, -X2) */
{"FLAGS",	ot1},	/* put the flags struct into X1 */
{"EXECS",	ot2},	/* exec the spypoint X1 with callNumber X2 */
{"SYMBOL",	ot2},	/* X2 = MakeStr(eRef(X1)) if IsAtom(X1) */
{"COPY",	ot4},	/* X3,X4 = copy(A1, X2) */
{"UNCOPY",	ot3},	/* X3 = uncopy(X1, X2) */
{"MAKEBMT",	ot4},	/* X3,X4 = makeBMT(X1, X2) (X3 = $bmt(...), $block(X4)) */
{"LINKBMT",	ot3},	/* linkBMT(A1, X2, X3) */
{"INST",	ot3},	/* X2,X3 = instance(X1) (X2 key, X3 value) */
{"ERASE",	ot1},	/* erase(X1) */
{"PROPS",	ot3},	/* $proplist(X1, X2, -X3) */
{"ABOLISH",	ot2},	/* abolish(X1, X2) */
{"REPLACN",	ot3},	/* $replacn(X1, X2, X3) */
{"REPLACV",	ot3},	/* $replacn(X1, X2, -X3) For fjh */
{"SETARG",	ot3},	/* setarg(X1, X2, +X3) */
{"SETARGV",	ot3},	/* setarg(X1, X2, -X3) For fjh */
{"AREF",	ot4},	/* X3 = $aref(X1, X2, A4) */
{"ASET",	ot5},	/* X4 = $aset(X1, X2, X3, A5) */
{"ID",		ot2},	/* X1 == X2 */
{"NOTID",	ot2},	/* X1 \== X2 */
{"IDTEST",	ot2},	/* Mode := X1 == X2 */
{"COMPARE",	ot3},	/* compare(X1, X2, X3) */
{"SORT",	ot3},	/* sort(X2, X3) with key controlled by X1 */
{"ALL",		ot1},	/* Allocate environment of size A1 */
{"DALL",	ot0},	/* Deallocate environment */
{"CALL",	ot2P},	/* Call (interpreted) Proc */
{"PRO",		ot0},	/* Proceed */
{"DALLPRO",	ot0},	/* Deallocate, Proceed */
{"EXEC",	ot1P},	/* Execute (interpreted) Proc */
{"DALLEXE",	ot1P},	/* Deallocate, Execute (interpreted) Proc */
{"EXECSOT",	ot2T},	/* Execute (interpreted) SOT X2, Table */
{"FRUN",	ot1P},	/* Run the foreign function Proc/A1 */
{"APPLY",	ot1},	/* Apply (interpreted) X[A1-1] */
{"FUNCALL",	ot1},	/* Funcall (interpreted) X[A1-1] */
{"CATCH",	ot2P},	/* Call the goal X2, catching Y[0] */
{"THROW",	ot1},	/* Throw X1 */
{"TE",		ot3L},	/* TryMeElse Label */
{"RE",		ot2L},	/* RetryMeElse Label */
{"TRE",		ot2},	/* TrustMeElse Fail */
{"T",		ot2L},	/* Try Label */
{"R",		ot2L},	/* Retry Label */
{"TR",		ot2L},	/* Trust Label */
{"RET",		ot0},	/* Return from interpreter */
{"ABORT",	ot0},	/* Start from scratch at original entrypoint */
{"EXIT",	ot1},	/* exit(X1) */
{"FAIL",	ot0},	/* Fail */
{"J",		ot0L},	/* Jump to Label */
{"JTRUE",	ot0L},	/* Jump to Label if ReadMode */
{"JFAIL",	ot0L},	/* Jump to Label if WriteMode */
{"JVX",		ot1L},	/* Jump if variable to Label */
{"JNVX",	ot1L},	/* Jump if non-variable to Label */
{"JC",		ot1IL},	/* Jump if == const to Label */
{"JS",		ot1SL},	/* Jump if == str to Label */
{"SOT",		ot1T},	/* SwitchOnTerm X1, Table */
{"SOC",		ot3L},	/* SwitchOnConstant X1, A2, A3, Label */
{"SOCE",	ot3L},	/* SwitchOnConstantElse X1, A2, A3, Label */
{"SOS",		ot3L},	/* SwitchOnStructure X1, A2, A3, Label */
{"SOSE",	ot3L},	/* SwitchOnStructureElse X1, A2, A3, Label */
{"SPYMDEL",	ot1L},	/* Spypoint version of MKDEL */
{"MKDEL",	ot1L},	/* Make a Delay with A1 temporaries and resuming at Label */
{"FVAR",	ot2},	/* X2 is first var in X1 or NIL */
{"MARK",	ot1},	/* Mark deRef(X1) as delayed, resuming through CD */
{"DELAY",	ot0},	/* Delay CD */
{"WAKE",	ot1},	/* Wake a call through X[0], don't spawn if A1 */
{"NECK",	ot1},	/* Wake any calls in W, saving A1 temporaries */
{"WOKEN",	ot0},	/* Restore temporaries after waking goals */
{"TYPX",	ot2},	/* (1 << eType(X1)) & A2 */
{"TYPY",	ot2},	/* (1 << eType(Y1)) & A2 */
{"CTYPX",	ot2},	/* (1 << eCType(X1)) & A2 */
{"CTYPY",	ot2},	/* (1 << eCType(Y1)) & A2 */
{"JTYPX",	ot2L},	/* Jump to Label if (1 << eType(X1)) & A2 */
{"JTYPY",	ot2L},	/* Jump to Label if (1 << eType(Y1)) & A2 */
{"JCTYPX",	ot2L},	/* Jump to Label if (1 << eCType(X1)) & A2 */
{"JCTYPY",	ot2L},	/* Jump to Label if (1 << eCType(Y1)) & A2 */
{"EREF",	ot2},	/* X2 := MakeInt(eRef(X1)) */
{"MKOBJ",	ot4},	/* X4 := MakeObject(eInt(X1), eInt(X2), eRef(X3)) */
{"LABELX",	ot1},	/* X1 := B */
{"LABELY",	ot1},	/* Y1 := B */
{"CUTX",	ot1},	/* Cut back to choicepoint in X1 */
{"CUTY",	ot1},	/* Cut back to choicepoint in Y1 */
{"SOFTCUT",	ot1},	/* Cut off choicepoint in X1 */
{"ONCUT",	ot2},	/* On cut, call C Function X1 with argument X2 */
{"ERROR",	ot2},	/* Error.  Message A1, Action A2 */
{"PRINTF",	ot3},	/* Fprintf(stream X1, string X2, list X3) */
{"PRTNUM",	ot4},	/* Print the number X3 with format <X1, X2> into X4 */
{"SPRT",	ot3},	/* Put printable representation of X2 with flags X1 in X3 */
{"DISPX",	ot1},	/* Display Value, showing variable chains */
{"TRACE",	ot1},	/* Set traceflg = regflg = callflg = A1 */
{"GET",		ot2},	/* Put next (non-space if A2) ascii on user_input into X1 */
{"PUT",		ot1},	/* Put character X1 on user_output */
{"SGET",	ot3},	/* Put next (non-space if A3) ascii on stream X1 into X2 */
{"SGETTOK",	ot3},	/* Put next token on stream X1 into X2, its type into X3 */
{"SPUT",	ot2},	/* Put character X2 on stream X1 */
{"SPUTL",	ot2},	/* Put string X2 on stream X1 */
{"TOKEN",	ot4},	/* Ditto, but from string X1, putting rest of X1 into X4 */
{"OPEN",	ot3},	/* Open X1 with mode X2 giving stream X3 */
{"CLOSE",	ot1},	/* Close stream */
{"GETSTR",	ot2},	/* A block pointer to Streams[X1] is placed in X2 */
{"FLUSH",	ot1},	/* Flush stream */
{"CLRERR",	ot1},	/* Clear error on stream */
{"SETSTR",	ot2},	/* Set input stream */
{"CURRSTR",	ot2},	/* Current input stream */
{"FORK",	ot5},	/* Fork, creating pipe between parent and child */ 
{"SYSCALL",	ot5},	/* Unix system call, A1.  Data in X[234], results in X5 */
{"LOAD",	ot1},	/* Load the .no file with path name X1 */
{"ILOAD",	ot3},	/* Incremental in-core load of predicate */
{"FLOAD",	ot4},	/* Load functions from foreign files */
{"SPYPT",	ot0L},	/* Put <goal,callLevel> into <0,1> or dcall Label */
{"SPY",		ot2},	/* Install a spypoint on X1/X2 */
{"NOSPY",	ot2},	/* Remove the spypoint on X1/X2 */
{"SIMC",	ot5},	/* Simc operation */
{"DSIMC",	ot6},	/* Dsimc operation */
{"SQL",		ot4},	/* Sql operation */
{"PSTOT",	ot2},	/* Parse prefix-format string into term structure */
{"LAST",	ot0},	/* Last Instruction in a predicate's code section */
};
