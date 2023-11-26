/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1990 by it.
 *
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"
#include <setjmp.h>
#include <math.h>

extern jmp_buf re_entry_point, interrupt_return_point;

Machine *CMR;

#define SaveForUnify { \
	SaveTR; \
	SaveB; \
	SaveH; \
	SaveHB; }
#define RestoreAfterUnify {RestoreTR; RestoreH; }
#define SaveForBuilding { \
	SaveTR; \
	SaveB; \
	SaveH; \
	SaveHB; }
#define RestoreAfterBuilding {RestoreTR; RestoreH;}
#define SaveForShift { \
	SaveTR; \
	SaveE; \
	SaveB; \
	SaveA; \
	SaveH; }
#define RestoreAfterShift { \
	RestoreTR; \
	RestoreE; \
	RestoreB; \
	RestoreA; }
/* SaveForInterrupt must include SaveForShift */
#define SaveForInterrupt SaveForShift
/* RestoreAfterInterrupt must include RestoreAfterShift */
#define RestoreAfterInterrupt RestoreAfterShift
#define SaveForCopying {SaveTR; }
#define RestoreAfterCopying {/* Do nothing. */}
#define SaveForUncopying SaveForShift
#define RestoreAfterUncopying {RestoreAfterShift; RestoreH;}
#define SaveForEverything { \
	SaveP; \
	SaveTR; \
	SaveE; \
	SaveB; \
	SaveA; \
	SaveH; \
	SaveHB; }
#define RestoreAfterEverything { \
	RestoreTR; \
	RestoreE; \
	RestoreB; \
	RestoreA; \
	RestoreH; \
	RestoreHB; }

/*
 * We use the argument to the overflow macros for documentation only
 * because the BREATHINGSPACEs above the stacks make 0 good enough.
 */
#define heapOverflow(inc) heapOverflowSignal(0, H)
#define stackOverflow(inc) stackOverflowSignal(0, A)
#define trailOverflow(inc) trailOverflowSignal(0, TR)
#undef HEAPOVERFLOWSIGNAL
#define HEAPOVERFLOWSIGNAL 1
#undef TRAILOVERFLOWSIGNAL
#define TRAILOVERFLOWSIGNAL 1

/*
 * Sometimes need to embed #ifdef NO_LOCAL_VARS in other macro definitions.
 */
#ifdef NO_LOCAL_VARS
#define IF_LOCAL_VARS(x)
#else /* NO_LOCAL_VARS */
#define IF_LOCAL_VARS(x) x
#endif /* NO_LOCAL_VARS */

#ifdef HASH_HASH_CONCAT
#	define CONCAT(x,y) x##y
#else
#	define IDENT(x) x
#	define CONCAT(x,y) IDENT(x)y
#endif

/*
 * Promote an int or float on the arithmetic stack to a term.
 * Note that the stack cell still looks like the int or float --
 * no code should depend on this.
 */
#define ArithNAN(a) \
	switch((a)->ar_type) { \
	when arINT: (a)->ar_value = MakeInt((a)->ar_int, H); \
	when arFLT: \
		H = (Word *) ALIGNFLOAT(H); \
		*(Real *)H = (a)->ar_float; \
		(a)->ar_value = StarToFloat(H); \
		H += wordsof(Real); \
	}

#define evalObject(x, ev__v, retryLabel) { \
	register Object ev__v; \
	ev__v = x; \
	retryLabel: \
	DeRef(ev__v); \
	switch(eType(ev__v)) { \
	when tREF: \
		ArithStack->ar_type = arNAN; DelayArith = ev__v; \
		IF_LOCAL_VARS( \
			if((Word*)eRefStar(ev__v) >= stackBase) { \
				/* Is it on the stack? */ \
				*H = StarToUnboundRef(H); \
				ArithStack->ar_value = StarToRef(H); \
				bindS(eRefStar(ev__v), StarToRef(H), TR); \
				H++; \
				binding(ev__v); \
				/*trailOverflow(0);*/ \
		} else ) \
		{ \
			ArithStack->ar_value = ev__v; \
		} \
	when tDEL:			/* Delays are already on the heap. */ \
		ArithStack->ar_type = arNAN; ArithStack->ar_value = ev__v; \
		DelayArith = ev__v; \
	when tICN: \
		if(IsSmallInt(ev__v)) { \
			ArithStack->ar_type = arINT; \
			ArithStack->ar_int = eSmallInt(ev__v); \
		} else if(IsAtom(ev__v)) { \
			(void) eval(ev__v); \
		} else { \
			ArithError = areNON; \
			ArithStack->ar_type = arNAN; ArithStack->ar_value = ev__v; \
		} \
	when tUCN: \
		if(IsFloat(ev__v)) { \
			ArithStack->ar_type = arFLT; ArithStack->ar_float = eFloat(ev__v); \
		} else if(IsInt32(ev__v)) { \
			ArithStack->ar_type = arINT; ArithStack->ar_int = eInt32(ev__v); \
		} else { \
			ArithError = areNON; \
			ArithStack->ar_type = arNAN; ArithStack->ar_value = ev__v; \
		} \
	when tLST: case tCHR: case tSTR: \
		ev__v = eval(ev__v); \
		if(IsVar(ev__v)) /* BUG!  Redundant! */ \
			DelayArith = ev__v; \
	when tBMV: \
		ArithError = areNON; \
		ArithStack->ar_type = arNAN; ArithStack->ar_value = ev__v; \
	} \
	ArithStack++; }

/* Macro used by the various POP instructions. */
/* Care is needed with the placement of Restores. */
#define MarkVarOrReevaluateArith(x, Answer) \
	if(!mark(x, CD)) { \
		do { \
			x = firstVar(X[1]); \
		} while(x != NIL && !mark(x, CD)); \
		RestoreAfterBuilding; /* !!!!! */ \
		if(x == NIL) { \
			SaveForUnify; \
			eval(X[1]); /* BUG!  What about failure elsewhere? */ \
			RestoreAfterUnify; \
			if(ArithError != areOK) { \
				arithError(X[1]); \
				goto fail; \
			} else { \
				ArithNAN(ArithStack);	/* This may use heap space. */ \
				Answer; \
			} \
		} \
	} else { \
		RestoreAfterBuilding; \
	}

#ifdef NO_LOCAL_VARS
#define unify_value_write(v, s, t, retry) { DeRef(v); *H++ = (v); }
#else /* NO_LOCAL_VARS */
#define unify_value_write(v, s, t, retry) { \
	DeRef3((v), \
		{ \
			if(!IsRef(v) || (Word *)eRefStar(v) < H)    /* Grunge */ \
				*H++ = (v); \
			else { \
				*H = StarToUnboundRef(H); \
				bindS(eRefStar(v), StarToRef(H), TR); \
				if(s) \
					(t) = StarToRef(H); \
				H++; \
				binding(v); \
			} \
		}, \
		{ \
			*H++ = (v); \
		} \
	) \
	}
#endif /* NO_LOCAL_VARS */

#define Y ((Object *) E)
#define W MR.mr_w
#define cop(op) case op: CONCAT(L,op)
#define wop(op) when op: CONCAT(L,op)
#define wopop(op1, op2) when op1: case op2: CONCAT(L,op1): CONCAT(L,op2)
#define retryTarget(n) CONCAT(retry_label_,n)
#define PendingBind(t, var, val)
#define OpCode (*(unsigned char *) P)
#define OpCodeOf(p) (*(unsigned char *) (p))
#define ArgOf(n, p) (*((n) + (unsigned char *) (p)))
#define Arg(n) (*((n) + (unsigned char *) P))
#define Arg1 (*(1 + (unsigned char *) P))
#define Arg2 (*(2 + (unsigned char *) P))
#define Arg3 (*(3 + (unsigned char *) P))
#define Arg4 (*(4 + (unsigned char *) P))
#define Arg5 (*(5 + (unsigned char *) P))
#define Arg6 (*(6 + (unsigned char *) P))
#define EnvSize (*(1 + (unsigned char *) (CP - 2)))
#define ChoiceSize(b) (*(1 + (unsigned char *) (((b)-1)->c_retry)))

#ifdef MEASURE
long LI;                    /* Inference Counter */
#define CountLI LI++
long ReadModeOps[256];
long WriteModeOps[256];
long ShallowReadModeOps[256];
long ShallowWriteModeOps[256];
long ShallowModeExits[256];
long ShallowEntries;
long ShallowFrames;
#define ExitShallowMode {ShallowModeExits[OpCode]++;}
#define EnterShallowMode { ShallowFrames++; ShallowEntries++; }
#define RetryShallowMode {ShallowFrames++;}
#define TrustShallowMode {ShallowFrames++;}
#else /* MEASURE */
#define CountLI
#define ExitShallowMode
#define EnterShallowMode
#define RetryShallowMode
#define TrustShallowMode
#endif /* MEASURE */

#define extra_reg0

#ifdef elxsi
#define extra_reg1
#define extra_reg2
#endif /* elxsi */

#ifdef sparc
#define extra_reg1
#define extra_reg2
#define extra_reg3
#define extra_reg4
#endif /* sparc */

#if defined(mips) || defined(MACHINE_SGI) || defined(MACHINE_DEC)
#define extra_reg1
#endif /* defined(mips) || defined(MACHINE_SGI) || defined(MACHINE_DEC) */

#ifdef mc68000
#undef extra_reg0
#endif /* mc68000 */

#ifdef ns32000
#undef extra_reg0
#endif /* ns32000 */

static Instruction pro[] = {
	MakeIns0(cPRO)
};

/*
 * This is less efficient than it could be, but keeping c_retry pointing
 * to a retry or trust instruction is a good defence against future need.
 */
static Instruction noChoice[] = {
	MakeIns2(cTRE, 0, 0),		/* Remember the args needed by ChoiceSize. */
	MakeIns0(cFAIL)
};

static Object Call, CallNumber;	/* Communication between EXECS and SPYMDEL */

#ifdef DEBUG
#define RecordPC(p) { \
	PastPCs[NextPCSlot] = (p); \
	PastBs[NextPCSlot++] = B; \
	if(NextPCSlot >= NPASTPCS) \
		NextPCSlot = 0; \
	}
#else /* DEBUG */
#define RecordPC(p)
#endif /* DEBUG */

void
interpret(Self)
Functor *Self;				/* Called predicate (for undefineds) */
{
	register Program P;
#define SaveP MR.mr_p = P

	register Environment *E;
#define SaveE MR.mr_e = E
#define RestoreE E = MR.mr_e

	register Word *H;
#define SaveH MR.mr_h = H
#define RestoreH H = MR.mr_h

	register Object *X;

#ifdef extra_reg0
	register Choice *B;
#define SaveB MR.mr_b = B
#define RestoreB B = MR.mr_b
#else /* extra_reg0 */
#define B (MR.mr_b)
#define SaveB
#define RestoreB
#endif /* extra_reg0 */

#ifdef extra_reg1
	register TrailRecord *TR;
#define SaveTR MR.mr_tr = TR
#define RestoreTR TR = MR.mr_tr
#else /* extra_reg1 */
#define TR (MR.mr_tr)
#define SaveTR
#define RestoreTR
#endif /* extra_reg1 */

#ifdef extra_reg2
	register Word *A;
#define SaveA MR.mr_a = A
#define RestoreA A = MR.mr_a
#else /* extra_reg2 */
#define A (MR.mr_a)
#define SaveA
#define RestoreA
#endif /* extra_reg2 */

	Machine MR;
	Program CP;

#define HB (MR.mr_hb)
#define SaveHB
#define RestoreHB

#ifdef extra_reg3
	register Object *S;
#else /* extra_reg3 */
	Object *S;
#endif /* extra_reg3 */

	Object StringConsCell[2];
	Delay *CD;					/* Current Delay */
	TrailRecord *OldTR;			/* Save location for WAKE -> WEVAL/WISEQ */
	int SuspendedVars;			/* Number of vars to save when waking a goal */
	Object *LHS;				/* Where to put result of arithmetic exp */
	Object LHSV;				/* What to unify result of arithmetic exp */

	MR = *CMR;
	CMR = &MR;
	P = MR.mr_p;
	RestoreE;
	RestoreB;
	RestoreA;
	RestoreH;
	X = MR.mr_x;
	RestoreTR;
	CP = MR.mr_cp;
	RestoreHB;

/* Read Mode */
ReadMode:
	for(;;) {
		register Object v;

#ifdef DEBUG
if(regflg || traceflg) {
	register int i;

if(regflg)
	fprintf(stdout,
	"%5x %5x %5x %5x %5x ",
	(Word*)E, (Word*)B, (Word*)A, TR, (Word*)H);
if(traceflg) {
	fprintf(stdout, " ");
	(void) displayInstruction(stdout, 0, P);
}
for(i = 0; i < spyflg; i++) {
	Word *address, old;

	address = SpyPoints[i].sp_address;
	old = SpyPoints[i].sp_value;
	if(*address != old) {
		fprintf(stdout, "*0x%x changed from 0x%x(%d) to 0x%x(%d)\n",
			address,
			old, old,
			*address, *address);
		SpyPoints[i].sp_value = *address;
	}
}
}
#endif /* DEBUG */
#ifdef MEASURE
ReadModeOps[OpCode]++;
#endif /* MEASURE */
		switch(OpCode) {
		wop(cGVARX):
		cop(cGVARA):		/* Fall Through */
			X[Arg1] = X[Arg2];
			P += niGVARX;
		wop(cGVARY):
			Y[Arg1] = X[Arg2];
			P += niGVARY;
		wop(cGLVARY): {
#ifdef NO_LOCAL_VARS
			Y[Arg1] = X[Arg2];
			P += niGLVARY;
#else /* NO_LOCAL_VARS */
			/* register Object v; */
			register Object *y;

			y = Y + Arg1;
			v = X[Arg2];
			P += niGLVARY;
			DeRefPure(v);
			if(IsRef(v) && eRefStar(v) > y) {
				*eRefStar(v) = StarToRef(y);
				*y = StarToUnboundRef(y);
				trailS(eRefStar(v), TR);
				binding(eRefStar(v));
			} else
				*y = v;
#endif /* NO_LOCAL_VARS */
		}
		wop(cGVALX): {
			register Object *x;

			x = X + Arg2;
			SaveForUnify;
			if(*x = unify(*x, X[Arg1])) {
				RestoreAfterUnify;
				P += niGVALX;
				/* BUG?  heapOverflow(0); */
			} else {
				RestoreTR;
				goto fail;
			}
		}
		wop(cGVALY):
			SaveForUnify;
			if(!unify(X[Arg2], Y[Arg1])) {
				RestoreTR;
				goto fail;
			}
			RestoreAfterUnify;
			P += niGVALY;
			/* BUG?  heapOverflow(0); */
		wop(cGC): {
			/* register Object v; */

			v = X[Arg1];
			P += niGC;
			DeRef3(v,
				{
					if(IsRef(v)) {
						bind(eRefStar(v), ((Object *)P)[-1], TR);
						binding(v);
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, ((Object *)P)[-1], H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						binding(v);
					}
				},
				if(IsIConst(v)) {
					if(((Object *)P)[-1] != v)
						goto fail;
				} else if(IsUConst(v)) {					/* BUG! */
					if(IsFloat(v)) {
						if(		!IsFloat(((Object *)P)[-1])
							||	eFloat(v) != eFloat(((Object *)P)[-1]))
							goto fail;
					} else if(IsInt32(v)) {
						if(		!IsInt32(((Object *)P)[-1])
							||	eInt32(v) != eInt32(((Object *)P)[-1]))
							goto fail;
					} else
						panic("Impossible UCN type in GC");
				} else
					goto fail;
			);
		}
		wop(cGNIL): {
			/* register Object v; */

			v = X[Arg1];
			P += niGNIL;
			DeRef3(v,
				{
					if(IsRef(v)) {
						bind(eRefStar(v), NIL, TR);
						binding(v);
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, NIL, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						binding(v);
					}
				},
				{ if(NIL != v)
					goto fail; }
			);
		}
		wop(cGS): {
			/* register Object v; */

			v = X[Arg1];
		GetStruct:
			DeRef3(v,
				{
					heapOverflow(1 + MAXARITY + wordsof(DelayHeader));
					if(IsRef(v)) {
						*eRefStar(v) = StarToStr(H);
						trail(eRefStar(v), TR);
						*H++ = ((Object *)P)[1];
						P += niGS;
						goto WriteMode;
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, StarToStr(H),
							H, A, TR, W, SaveForShift, RestoreAfterShift);
						*H++ = ((Object *)P)[1];
						P += niGS;
						goto WriteMode;
					}
				},
				{
					if(IsStr(v)) {
						S = (Object *) eRef(v);
						if(*S++ != ((Object *)P)[1])
							goto fail;
						P += niGS;
					} else
						goto fail;
				}
			);
		}
		wop(cGSX1): {
			/* register Object v; */

			v = X[Arg1];
			DeRef3(v,
				{
					heapOverflow(2 + wordsof(DelayHeader));
					*H++ = ((Object *)P)[1];
					X[Arg2] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					P += niGSX1;
					if(IsRef(v)) {
						bind(eRefStar(v), StarToStr(H-2), TR);
						goto WriteMode;
					} else /* if(IsDel(v)) */ {
						register Object p;

						p = StarToStr(H - 2);
						v = (Object) eRef(v);
						bindDel(v, p, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						goto WriteMode;
					}
				},
				{
					if(IsStr(v)) {
						S = (Object *) eRef(v);
						if(S[0] != ((Object *)P)[1])
							goto fail;
						v = S[1];
						if(IsVar(v))
							v = MakeIndirect(v);
						X[Arg2] = v;
						P += niGSX1;
					} else
						goto fail;
				}
			);
		}
		wop(cGSX2): {
			/* register Object v; */

			v = X[Arg1];
			DeRef3(v,
				{
					heapOverflow(3 + wordsof(DelayHeader));
					*H++ = ((Object *)P)[1];
					X[Arg2] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					X[Arg3] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					P += niGSX2;
					if(IsRef(v)) {
						bind(eRefStar(v), StarToStr(H-3), TR);
						goto WriteMode;
					} else /* if(IsDel(v)) */{
						register Object p;

						p = StarToStr(H - 3);
						v = (Object) eRef(v);
						bindDel(v, p, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						goto WriteMode;
					}
				},
				{
					if(IsStr(v)) {
						S = (Object *) eRef(v);
						if(S[0] != ((Object *)P)[1])
							goto fail;
						X[Arg2] = S[1];
						X[Arg3] = S[2];
						P += niGSX2;
					} else
						goto fail;
				}
			);
		}
		wop(cGL): {
			/* register Object v; */

			v = X[Arg1];
		GetList:
			DeRef3(v,
				{
					heapOverflow(2 + wordsof(DelayHeader));
					P += niGL;
					if(IsRef(v)) {
						v = (Object) eRef(v);
						*(Object *)v = StarToList(H);
						trail(v, TR);
						goto WriteMode;
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, StarToList(H),
							H, A, TR, W, SaveForShift, RestoreAfterShift);
						goto WriteMode;
					}
				},
				{
					if(IsList(v)) {
						S = (Object *) eRef(v);
						P += niGL;
					} else if(IsString(v)) {
						register char *s;

						S = StringConsCell;
						P += niGL;
						s = (char *) eRef(v);
						StringConsCell[0] = MakeSmallInt(s[0]);
						if(s[1] == '\0')
							StringConsCell[1] = NIL;
						else
							StringConsCell[1] = StarToString(s + 1);
					} else
						goto fail;
				}
			);
		}
		wop(cGLVX2): {
			/* register Object v; */

			v = X[Arg1];
			DeRef3(v,
				{
					heapOverflow(2 + wordsof(DelayHeader));
					X[Arg2] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					X[Arg3] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					P += niGLVX2;
					if(IsRef(v)) {
						bind(eRefStar(v), StarToList(H-2), TR);
					} else /* if(IsDel(v)) */ {
						register Object p;
	
						p = StarToList(H - 2);
						v = (Object) eRef(v);
						bindDel(v, p, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
					}
				},
				{
					if(IsList(v)) {
						S = (Object *) eRef(v);
						X[Arg2] = S[0];
						X[Arg3] = S[1];
						P += niGLVX2;
					} else if(IsString(v)) {
						S = (Object *) eRef(v);
						X[Arg2] = MakeSmallInt(*(char *)S);
						S = (Object *) (sizeof(char) + (Word) S);
						if(*(char *)S == '\0')
							X[Arg3] = NIL;
						else
							X[Arg3] = StarToString(S);
						P += niGLVX2;
					} else
						goto fail;
				}
			);
		}
		wop(cPVARX):
			X[Arg2]
			= X[Arg1]
			= StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;		/* BUG!  Need to check heap overflow. */
			P += niPVARX;
		wop(cPVARY): {
#ifdef NO_LOCAL_VARS
			X[Arg2]
			= Y[Arg1]
			= StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
#else /* NO_LOCAL_VARS */
			/* register Object v; */

			v = (Object)&Y[Arg1];
			X[Arg2] = StarToRef(v);
			*(Object *)v = StarToUnboundRef(v);
#endif /* NO_LOCAL_VARS */
			P += niPVARY;
		}
		wop(cPVALY): {
			X[Arg2] = Y[Arg1];
			P += niPVALY;
		}
		wop(cPVALY2): {
			register Word *x;

			x = X + Arg3;
			x[0] = Y[Arg1];
			x[1] = Y[Arg2];
			P += niPVALY2;
		}
		wop(cPUVAL): {
			/* register Object v; */

			v = Y[Arg1];
			DeRefPure(v);
#ifdef NO_LOCAL_VARS
			X[Arg2] = v;
#else /* NO_LOCAL_VARS */
			if(IsRef(v)
				&& (Word*)eRefStar(v) >= (Word*)E
				&& (Word*)eRefStar(v) < (Word*)A)
			{
				*H = StarToUnboundRef(H);
				bindS(eRefStar(v), StarToRef(H), TR);
				X[Arg2] = StarToRef(H);
				H++;
				binding(v);
			/*  Redundant
			} else if(IsDel(v)) {
				X[Arg2] = v;
			*/
			} else
				X[Arg2] = v;
#endif /* NO_LOCAL_VARS */
			P += niPUVAL;
		}
		wop(cPC):
			X[Arg1] = (Object) ((Object *)P)[1];
			P += niPC;
		wop(cPNIL):
			X[Arg1] = NIL;
			P += niPNIL;
		wop(cPS):
			heapOverflow(1 + MAXARITY);
			X[Arg1] = StarToStr(H);
			P += niPS;
			*H++ = ((Object *)P)[-1];
			goto WriteMode;
		wop(cPL):
			heapOverflow(2);
			X[Arg1] = StarToList(H);
			P += niPL;
			goto WriteMode;
		wop(cUVOID):
			S += Arg1;
			P += niUVOID;
		wop(cUVOID1):
			S += 1;
			P += niUVOID1;
		wop(cUVARX):
		cop(cUVARA):		/* Fall Through */
			X[Arg1] = *S++;
			P += niUVARX;
		wop(cUVARY):
			Y[Arg1] = *S++;
			P += niUVARY;
		wop(cUVARXVARX):
		cop(cUVARAVARA):	/* Fall Through */
			X[Arg1] = *S++;
			X[Arg2] = *S++;
			P += niUVARXVARX;
		wop(cUVARYVARY):
			Y[Arg1] = *S++;
			Y[Arg2] = *S++;
			P += niUVARYVARY;
		wop(cUVARXLVX):
		cop(cUVARALVX): {	/* Fall Through */
			register Object *x;

			X[Arg1] = *S++;
			x = X + Arg2;
			SaveForUnify;
			if(*x = unify(*S++, *x)) {
				RestoreAfterUnify;
				P += niUVARXLVX;
			} else {
				RestoreTR;
				goto fail;
			}
		}
		wop(cULVXVARX):
		cop(cULVXVARA): {	/* Fall Through */
			register Object *x;

			x = X + Arg1;
			SaveForUnify;
			if(*x = unify(*S++, *x)) {
				RestoreAfterUnify;
			} else {
				RestoreTR;
				goto fail;
			}
			X[Arg2] = *S++;
			P += niULVXVARX;
		}
		wop(cULVXLVX): {
			register Object *x;

			x = X + Arg1;
			SaveForUnify;
			if((*x = unify(*S++, *x)) && (x = X + Arg2, *x = unify(*S++, *x))) {
				RestoreAfterUnify;
				P += niULVXLVX;
			} else {
				RestoreTR;
				goto fail;
			}
		}
		wopop(cUVALX,cULVX): {
			register Object *x;

			x = X + Arg1;
			SaveForUnify;
			if(*x = unify(*S++, *x)) {
				RestoreAfterUnify;
				P += niUVALX;
			} else {
				RestoreTR;
				goto fail;
			}
		}
		wopop(cUVALY,cULVY):
			SaveForUnify;
			if(unify(*S++, Y[Arg1])) {
				RestoreAfterUnify;
				P += niUVALY;
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cUC): {
			/* register Object v; */

			v = *S++;
			P += niUC;
			DeRef3(v,
				{
					if(IsRef(v)) {
						bindH(eRefStar(v), ((Object *)P)[-1], TR);
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, ((Object *)P)[-1], H, A, TR, W,
								SaveForShift, RestoreAfterShift);
					}
					binding(v);
				},
				{
					if(IsIConst(v)) {
						if(v != ((Object *)P)[-1])
							goto fail;
					} else if(IsUConst(v)) {					/* BUG! */
						if(IsFloat(v)) {
							if(		!IsFloat(((Object *)P)[-1])
								||	eFloat(v) != eFloat(((Object *)P)[-1]))
								goto fail;
						} else if(IsInt32(v)) {
							if(		!IsInt32(((Object *)P)[-1])
								||	eInt32(v) != eInt32(((Object *)P)[-1]))
								goto fail;
						} else
							panic("Impossible UCN type in UC");
					} else
						goto fail;
				}
			);
		}
		wop(cUL):
			v = *S;
			goto GetList;
		wop(cUS):
			v = *S;
			goto GetStruct;
		wop(cARITH):
			/*
			 * Note that it is important that only the POP instructions
			 * check for overflow.  Otherwise, stack relocation may make
			 * DelayArith invalid.  Apart from the POP instructions, heap
			 * and trail are only consumed if the arithmetic goal is going
			 * to delay.
			 *
			 * This instruction is almost never generated.  One of the
			 * APUSH* is used instead.
			 */

			CountLI;
			ArithError = areOK;
			DelayArith = 0;
			/*
			 * Leave room for an environment to be allocated
			 * during evaluation of the expression.
			 */
			ArithStack = (Arith *) ALIGNMAX(A + wordsof(Environment) + 256);
			ArithStackMax = ArithStack + ARITHSIZE;
			P += niARITH;
		wop(cAPUSHX):
			CountLI;
			ArithError = areOK;
			DelayArith = 0;
			ArithStack = (Arith *) ALIGNMAX(A + wordsof(Environment) + 256);
			ArithStackMax = ArithStack + ARITHSIZE;
			goto PushX;
		wop(cAPUSHY):
			CountLI;
			ArithError = areOK;
			DelayArith = 0;
			ArithStack = (Arith *) ALIGNMAX(A + wordsof(Environment) + 256);
			ArithStackMax = ArithStack + ARITHSIZE;
			goto PushY;
		wop(cAPUSHI):
			CountLI;
			ArithError = areOK;
			DelayArith = 0;
			ArithStack = (Arith *) ALIGNMAX(A + wordsof(Environment) + 256);
			ArithStackMax = ArithStack + ARITHSIZE;
			goto PushI;
		wop(cAPUSHF):
			CountLI;
			ArithError = areOK;
			DelayArith = 0;
			ArithStack = (Arith *) ALIGNMAX(A + wordsof(Environment) + 256);
			ArithStackMax = ArithStack + ARITHSIZE;
			goto PushF;
		wop(cPUSHX):
		PushX:
			if(ArithStack >= ArithStackMax)
				panic("Arithmetic stack overflow");
			evalObject(X[Arg1], v, retryTarget(cPUSHX));
			P += niPUSHX;
		wop(cPUSHY):
		PushY:
			if(ArithStack >= ArithStackMax)
				panic("Arithmetic stack overflow");
			evalObject(Y[Arg1], v, retryTarget(cPUSHY));
			P += niPUSHY;
		wop(cPUSHVX):
			if(ArithStack >= ArithStackMax)
				panic("Arithmetic stack overflow");
			*H = StarToUnboundRef(H);
			X[Arg1]
				= DelayArith
				= ArithStack->ar_value
				= StarToRef(H);
			H++;
			ArithStack->ar_type = arNAN;
			ArithStack++;
			P += niPUSHVX;
		wop(cPUSHVY):
			if(ArithStack >= ArithStackMax)
				panic("Arithmetic stack overflow");
			*H = StarToUnboundRef(H);
			Y[Arg1]
				= DelayArith
				= ArithStack->ar_value
				= StarToRef(H);
			H++;
			ArithStack->ar_type = arNAN;
			ArithStack++;
			P += niPUSHVY;
		wop(cPUSHI):
		PushI:
		{
			if(ArithStack >= ArithStackMax)
				panic("Arithmetic stack overflow");
			ArithStack->ar_int = ((Word *)P)[1];
			ArithStack->ar_type = arINT;
			ArithStack++;
			P += niPUSHI;
		}
		wop(cPUSHF):
		PushF:
		{
			/* register Object v; */

			if(ArithStack >= ArithStackMax)
				panic("Arithmetic stack overflow");
			v = ((Object *)P)[1];
			/* BUG!  Should check the type of v properly. */
			ArithStack->ar_float = eFloat(v);
			ArithStack->ar_type = arFLT;
			ArithStack++;
			P += niPUSHF;
		}
		wop(cPOPX):
			LHS = X + Arg1;
			P += niPOPX;
		PopX:
			ArithStack--;
			ArithNAN(ArithStack);
			if(ArithError != areOK) {
				arithError(ArithStack->ar_value);
				goto fail;
			} else if(DelayArith) {
				register Object X0, X1, y;

				X0 = X[0]; X1 = X[1];
				*H = StarToUnboundRef(H);
				X[0]
					= y
					= StarToRef(H);
				H++;
				X[1] = ArithStack->ar_value;
				SaveForBuilding;
				CD = makeDelay(2, WEvaluate);
				mark(DelayArith, CD);
				RestoreAfterBuilding;
				X[0] = X0; X[1] = X1;
				*LHS = y;
				trailOverflow(0);
			} else
				*LHS = ArithStack->ar_value;
			heapOverflow(0);
		wop(cPOPY):
			LHS = Y + Arg1;
			P += niPOPY;
			goto PopX;
		wop(cPOPVX):
			LHSV = X[Arg1];
			P += niPOPVX;
		PopVX:
			ArithStack--;
			ArithNAN(ArithStack);
			if(ArithError != areOK) {
				arithError(ArithStack->ar_value);
				goto fail;
			} else if(DelayArith) {
				register Object X0, X1;

				X0 = X[0]; X1 = X[1];
				X[0] = LHSV;
				X[1] = ArithStack->ar_value;
				SaveForBuilding;
				CD = makeDelay(2, WEvaluate);
				mark(DelayArith, CD);
				RestoreAfterBuilding;
				X[0] = X0; X[1] = X1;
			} else {
				SaveForUnify;
				if(!unify(LHSV, ArithStack->ar_value)) {			/* Lazy! */
					RestoreTR;
					goto fail;
				}
				RestoreAfterUnify;
			}
			trailOverflow(0);
			heapOverflow(0);
		wop(cPOPVY):
			LHSV = Y[Arg1];
			P += niPOPVY;
			goto PopVX;
		wop(cPOPC):
			ArithStack--;
			ArithNAN(ArithStack);
			if(ArithError != areOK) {
				arithError(ArithStack->ar_value);
				goto fail;
			} else if(DelayArith) {
				register Object X0, X1;

				X0 = X[0]; X1 = X[1];
				X[0] = *(Object *)(P + 1);
				X[1] = ArithStack->ar_value;
				SaveForBuilding;
				CD = makeDelay(2, WEvaluate);
				mark(DelayArith, CD);
				RestoreAfterBuilding;
				X[0] = X0; X[1] = X1;
				trailOverflow(0);
			} else {
				/* register Object v; */

				v = *(Object *)(P + 1);
				/* BUG!  Should check the type of v properly. */
				switch(eType(v)) {
				when tICN:
					if(ArithStack->ar_value != v)
						goto fail;
				when tUCN:
					if(IsFloat(ArithStack->ar_value)) {
						if(ArithStack->ar_float != eFloat(v))
							goto fail;
					} else if(IsInt32(ArithStack->ar_value)) {
						if(ArithStack->ar_int != eInt32(v))
							goto fail;
					} else
						goto fail;
				}
			}
			P += niPOPC;
			heapOverflow(0);
		wop(cAFUNC):
			if(!DelayArith && ArithError == areOK)
				ArithFuncTable[Arg1]();
			else
				ArithStack -= ArithFuncArity[Arg1] - 1;
			if(DelayArith || ArithError != areOK) {
				register Object *t;
				/* register Object v; */

				v = ArithFuncArity[Arg1];
				t = H;
				H += 1 + v;
				t[0] = StarToStrHeader(v, ArithFuncAtoms[Arg1]);
				ArithNAN(ArithStack - 1);		/* May use heap! */
				t[1] = ArithStack[-1].ar_value;
				if(v == 2) {
					ArithNAN(ArithStack + 0);
					t[2] = ArithStack[0].ar_value;
				}
				ArithStack[-1].ar_value = StarToStr(t);
				ArithStack[-1].ar_type = arNAN;
			}
			P += niAFUNC;
		wop(cAPRED):
			if(!DelayArith && ArithError == areOK) {
				if(!ArithPredTable[Arg1]())
					goto fail;	/* Only report further errors if succeeds! */
			} else
				ArithStack -= ArithPredArity[Arg1] - 1;
			if(DelayArith || ArithError != areOK) {
				register Object *t;
				/* register Object v; */

				v = ArithPredArity[Arg1];
				t = H;
				H += 1 + v;
				t[0] = StarToStrHeader(v, ArithPredAtoms[Arg1]);
				ArithNAN(ArithStack - 1);		/* May use heap! */
				t[1] = ArithStack[-1].ar_value;
				if(v == 2) {
					ArithNAN(ArithStack + 0);
					t[2] = ArithStack[0].ar_value;
				}
				ArithStack[-1].ar_value = StarToStr(t);
				ArithStack[-1].ar_type = arNAN;
				if(DelayArith) {
					register Object X0, X1;

					X0 = X[0]; X1 = X[1];
					X[0] = MakeSmallInt(1);
					X[1] = ArithStack[-1].ar_value;
					SaveForBuilding;
					CD = makeDelay(2, WEvaluate);
					mark(DelayArith, CD);
					RestoreAfterBuilding;
					X[0] = X0; X[1] = X1;
					trailOverflow(0);
				} else {
					arithError(ArithStack[-1].ar_value);
					goto fail;
				}
			}
			P += niAPRED;
		wop(cJPRED):
			if(!DelayArith && ArithError == areOK) {
				if(!ArithPredTable[Arg1]()) {
					RecordPC(P);
					P = ((Program *)P)[1];
					RecordPC(P);
					goto ReadMode;/* Only report further errors if succeed! */
				}
			} else
				ArithStack -= ArithPredArity[Arg1] - 1;
			if(DelayArith || ArithError != areOK) {
				/* register Object v; */

				v = ArithPredArity[Arg1];
				ArithNAN(ArithStack - 1);
				if(v == 2) {
					ArithNAN(ArithStack + 0);
				}
				if(DelayArith)
					ArithError = areNONGROUND;
				arithErrorN(
					ArithPredAtoms[Arg1],
					v,
					ArithStack[-1].ar_value,
					ArithStack[0].ar_value);
				goto fail;
			}
			P += niJPRED;
		wop(cWEVAL): {
			/* register Object v; */

#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "Evaluating ");
	displayTerm(stdout, X[1]); fprintf(stdout, "\n");
}
#endif /* DEBUG */
			v = firstVar(X[1]);		/* BUG!  Let eval() do the work. */
			if(v == NIL) {
				ArithError = areOK;
				DelayArith = 0;
				ArithStack = (Arith *) ALIGNMAX(A + wordsof(Environment) + 256);
				ArithStackMax = ArithStack + ARITHSIZE;
				(void) eval(X[1]);
				if(ArithError != areOK) {
					arithError(X[1]);
					goto fail;
				} else {
					switch(ArithStack[0].ar_type) {
						when arINT:
							ArithStack[0].ar_value
								= MakeInt(ArithStack[0].ar_int, H);
						when arFLT:
							H = (Word *) ALIGNFLOAT(H);
							*(Real *)H = ArithStack[0].ar_float;
							ArithStack[0].ar_value = StarToFloat(H);
							H += wordsof(Real);
						when arNAN:
#ifdef DEBUG
displayTerm(stdout, X[0]); printf("\n");
displayTerm(stdout, X[1]); printf("\n");
displayTerm(stdout, ArithStack[0].ar_value); printf("\n");
#endif /* DEBUG */
							panic("Urk! in WEVAL");
					}
					SaveForUnify;
					if(!unify(X[0], ArithStack[0].ar_value)) {		/* Lazy! */
						RestoreTR;
						goto fail;
					}
					RestoreAfterUnify;
				}
			} else {
				/*
				 * We can't be spying on this Delay, so the value
				 * of d_sleeping must have been 1.
				 */
				(CD-1)->d_sleeping = 1;
				TR = OldTR;	/* Remove the trailing of d_sleeping, if done */
				SaveForBuilding;
				mark(v, CD);
				RestoreAfterBuilding;
			}
			heapOverflow(0);
			trailOverflow(0);
			RecordPC(P);
			if(W != 0)
				P = pro;			/* Wake goals */
			else
				P = CP;
			RecordPC(P);
		}
		wop(cOCCURS):
			CountLI;
			/* SaveForUnify; p_occurs() doesn't unify() anything */
			if(p_occurs(X[Arg1], X[Arg2])) {
				P += niOCCURS;
			} else {
				/* RestoreTR; not needed */
				goto fail;
			}
		wop(cNAME):
			CountLI;
			SaveForUnify;
			if(p_name(X[Arg1], X[Arg2])) {
				RestoreAfterUnify;
				P += niNAME;
				heapOverflow(0);
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cLTOS):
			CountLI;
			SaveForUnify;
			if(p_listToString(X[Arg1], X[Arg2])) {
				RestoreAfterUnify;
				P += niLTOS;
				heapOverflow(0);
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cUNIV):
			CountLI;
			SaveForUnify;
			if(p_univ(X[Arg1], X[Arg2])) {
				RestoreAfterUnify;
				P += niUNIV;
				heapOverflow(0);
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cFUNCTOR):
			CountLI;
			SaveForUnify;
			if(p_functor(X[Arg1], X[Arg2], X[Arg3])) {
				RestoreAfterUnify;
				P += niFUNCTOR;
				trailOverflow(0);
				heapOverflow(0);
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cARG):
			CountLI;
			SaveForUnify;
			if(p_arg(X[Arg1], X[Arg2], X + Arg3)) {
				RestoreAfterUnify;
				P += niARG;
				trailOverflow(0);
				heapOverflow(0);
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cISEQ): {
			register TrailRecord *t, *TRB;

#undef TRAILOVERFLOWSIGNAL
#define TRAILOVERFLOWSIGNAL 0

#ifdef DEBUG4
displayTerm(stdout, X[Arg1]); printf("\n");
displayTerm(stdout, X[Arg2]); printf("\n");
#endif /* DEBUG4 */
			TRB = TR;			/* Note that TR == (B-1)->c_tr */
			SaveForUnify;
			if(unify(X[Arg1], X[Arg2])) {
				/*
				 * Unify() may try to wake some goals.  Because unify()
				 * is only used as a test here, we ignore these and
				 * remove any records that it puts on the heap.
				 * Later on we restore any bindings that it makes.
				 */
				RestoreTR;	/* RestoreAfterUnify, but don't restore H; */
				W = 0;		/* W must be 0 when ISEQ is used. */
#ifdef DEBUG4
printf("TR = 0x%x, TRB = 0x%x\n", TR, TRB);
#endif /* DEBUG4 */
				if(TR == TRB)
					goto fail;
				else {
					Object TempX[6];
					register TrailRecord *TRT;

					/*
					 * First bind all the global variables that have been
					 * bound to without themselves being bound to a marker.
					 */
					t = TR;
					while(t > TRB) {
						/* register Object v; */

						t--;
						v = *t->tr_address;
						DeRef(v);
						if(!IsVar(v))
							continue;
#ifdef DEBUG4
printf("BINDING 0x%x IN ISEQ\n", v);
#endif /* DEBUG4 */
						v = (Object) eRef(v);
						trail2(*(Object *)v, v, TR);
						*(Object *)v = StarToAtom(&SymEqual);
						/* BUG!  trail() macro checks overflow too. */
						trailOverflowCheck(TR, 0);	/* Can't move Trail here */
					}

					/* Now unbind all the global variables. */
					TRT
						= t
						= TR;
					while(t > TRB) {
						t--;
#ifdef DEBUG4
printf("RESTORING 0x%x to 0x%x IN ISEQ\n",
	(int)t->tr_address,
	(int)t->tr_value);
#endif /* DEBUG4 */
						*t->tr_address = t->tr_value;
					}

					/* make a delay record */
					TempX[0] = MakeSmallInt(cISEQ);		/* Now redundant */
					TempX[1] = X[Arg1];
					TempX[2] = X[Arg2];
					/* take care to get the right HB! */
					TempX[4] = MakeSmallInt(HB); /*(B-1)->c_h);*/
					TempX[5] = MakeSmallInt(H);

					/*
					 * Clobber the choicepoint surrounding the ISEQ,
					 * but leave anything that was built on the heap
					 * for use when the goal wakes up.
					 */
					A = (Word *) --B;
					B = B->c_b;
					HB = (B-1)->c_h;

					/*
					 * Make a variable for the result.  In most uses of
					 * cISEQ, this will be bound to SymFail by the
					 * following WAM instruction.
					 */
					*H = StarToUnboundRef(H);
					TempX[3]
						= X[Arg3]
						= StarToRef(H);
					H++;
					SaveForBuilding;
					MR.mr_x = TempX;
					CD = makeDelay(6, WIsEq);
					MR.mr_x = X;

					/* mark all the variables between (B-1)->c_tr and TRT */
					t = TRT;
					while(t > TRB) {
						t--;
#ifdef DEBUG4
printf("MARKING 0x%x IN ISEQ\n", (int)t->tr_address);
#endif /* DEBUG4 */
						mark(StarToRef(t->tr_address), CD);
					}

					RestoreAfterBuilding;

					/*
					 * Remove the trail segment between TRB and TRT.
					 * This information is no longer needed.
					 * (Clever programming could save this effort,
					 * but I haven't bothered yet.)
					 */
					t = TRT;
					while(t < TR)
						*TRB++ = *t++;
					TR = TRB;

					P += niISEQ;
					heapOverflow(0);
				}
			} else {
				/*
				 * Clean up the trail and clobber the choicepoint
				 * surrounding the ISEQ, taking care to leave any
				 * variables globalized by CVTTOH alone.
				 */
				RestoreTR;
				while(TR > TRB) {
					TR--;
					*TR->tr_address = TR->tr_value;
				}
				H = (B-1)->c_h;		/* CVTTOH may have modified this */
				A = (Word *) --B;
				B = B->c_b;
				HB = (B-1)->c_h;

				X[Arg3] = StarToAtom(&SymFail);
				P += niISEQ;
			}

#undef TRAILOVERFLOWSIGNAL
#define TRAILOVERFLOWSIGNAL 1
		}
		wop(cWISEQ):
			/*
			 * Remove the trailing of d_sleeping, if done.
			 * May have to put it back later if we succeed.
			 */
			TR = OldTR;

			SaveForUnify;
			/*
			 * We can't be spying on this Delay, so the value
			 * of d_sleeping must have been 1.
			 */
			switch(is_eq(X, CD)) {
			when 0:
				(CD-1)->d_sleeping = 1;	/* We untrailed this above */
				RestoreTR;
				goto fail;
			when 1:
				RestoreAfterUnify;
				trail2(1, &(CD-1)->d_sleeping, TR);
			when 2:
				RestoreAfterUnify;
				(CD-1)->d_sleeping = 1;
			}
			trailOverflow(0);
			heapOverflow(0);
			RecordPC(P);
			if(W != 0)
				P = pro;			/* Wake goals */
			else
				P = CP;
			RecordPC(P);
		wop(cCVTTOH):
			/*
			 * CVTTOH is a very specialized instruction that MUST be used
			 * immediately after the choice-point that is wrapped around
			 * ISEQ is created and before any structures are built on the
			 * heap.  This is because it modifies the c_h field to
			 * protect any variables that it globalizes from ISEQ's
			 * later destruction of the choice-point.
			 */
			SaveForBuilding;
			copyVariablesToTopOfHeap(X[Arg1]);
			RestoreAfterBuilding;
			(B-1)->c_h = H;
			heapOverflow(0);
			trailOverflow(0);
			P += niCVTTOH;
		wop(cPLUS):
			CountLI;
			SaveForUnify;
			if(p_plus(X[Arg1], X[Arg2], X[Arg3])) {
				RestoreAfterUnify;
				P += niPLUS;
				trailOverflow(0);
				heapOverflow(0);
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cDEFINED): {
			register Object atom, arity;
			register Functor *f;

			/* No error checking */
			atom = X[Arg1]; DeRef(atom);
			arity = X[Arg2]; DeRef(arity);
			f = lookupFunctor(eSmallInt(arity),
					((Atom *) eRef(atom))->a_functors);
			if(f == (Functor *) NULL || f->f_sourceFile == NIL)
				goto fail;
			X[Arg3] = f->f_sourceFile;
			X[Arg4] = MakeSmallInt(f->f_codeType);
			P += niDEFINED;
		}
		wop(cARITIES): {
			/* register Object v; */

			/* No error checking */
			v = X[Arg1]; DeRef(v);
			SaveH;
			X[Arg2] = p_predicateArities((Atom *) eRef(v));
			RestoreH;
			P += niARITIES;
		}
		wop(cFLAGS):
			X[Arg1] = StarToStr(Flags);
			P += niFLAGS;
		wop(cEXECS): {
			/* register Object v; */

			CountLI;
			/*
			 * Note that it is not necessary to check ActionPending,
			 * although it wouldn't hurt to do so.
			 */
			v = X[Arg1]; DeRef(v);
			Call = v;
			CallNumber = X[Arg2]; DeRef(CallNumber);
			Flags[flgDELAYED] = StarToAtom(&SymFail);
			switch(eType(v)) {
			when tICN:
				if(IsAtom(v))
					Self = enterFunctor(0, (Atom *) eRef(v));
				else
					panic("Uncallable object to EXECS");
			when tLST:
				v = (Object)eRef(v);
				Self = enterFunctor(2, &SymCons);
				X[0] = ((Object *) v)[0];
				X[1] = ((Object *) v)[1];
			when tCHR:
				panic("Unimplemented type in EXECS");
			when tSTR: {
				register int n;

				v = (Object)eRef(v);
				n = eNArgs(*(Structure *)v);
				Self = enterFunctor(n, eFunctor(*(Structure *)v));
				v += sizeof(Object);
				for(n--; n >= 0; n--)
					X[n] = ((Object *) v)[n];
			}

			break;
			default:
				panic("Uncallable object to EXECS");
			}
			RecordPC(P);
			if(Self->f_codeType == fSPYPT)
				P = *(Program *)(Self->f_code + 1);
			else
				P = Self->f_code;
			RecordPC(P);
		}
		wop(cSYMBOL): {
			/* register Object v; */

			v = X[Arg1]; DeRef(v);
			if(IsAtom(v))
				X[Arg2] = StarToStr(eRef(v));
			else				/* Should this be an error? */
				goto fail;
			P += niSYMBOL;
		}
		wop(cCOPY): {
			/* register Object v; */
			register Object v1;
			int nvars;

			/* WARNING!  No error checking.  Done in Prolog. */
			v1 = X[Arg1]; DeRef(v1);
			v = X[Arg2]; DeRef(v);
			SaveForCopying;		/* N.B. Only saves TR */
			X[Arg4] = copy(eSmallInt(v1), (Object *) NULL, v, &nvars);
			RestoreAfterCopying;
			X[Arg3] = MakeSmallInt(nvars);
			P += niCOPY;
		}
		wop(cUNCOPY): {
			/* register Object v; */

			v = X[Arg2]; DeRef(v);
			if(IsSmallInt(v)) {
				SaveForUncopying;
				X[Arg3] = uncopy(X[Arg1], eSmallInt(v));
				RestoreAfterUncopying;
				heapOverflow(0);
			} else
				panic("Integer expected in UNCOPY");
			P += niUNCOPY;
		}
		wop(cMAKEBMT): {
			int instance;

			SaveForCopying;		/* N.B. Only saves TR */
			if(p_makeBMT(X[Arg1], X[Arg2], &instance, X+Arg3, X+Arg4) == 0) {
				RestoreAfterCopying;
				goto fail;
			}
			RestoreAfterCopying;
			P += niMAKEBMT;
		}
		wop(cLINKBMT):
			if(p_linkBMT(Arg1, X[Arg2], X[Arg3]))
				P += niLINKBMT;
			else
				goto fail;
		wop(cINST): {
			SaveForUncopying;
			if(p_instance(X[Arg1], X + Arg2, X + Arg3)) {
				RestoreAfterUncopying;
				P += niINST;
			} else
				goto fail;
		}
		wop(cERASE):
			if(p_erase(X[Arg1]))
				P += niERASE;
			else
				goto fail;
		wop(cPROPS):
			SaveForBuilding
			if(p_proplist(X[Arg1], X[Arg2], X + Arg3)) {
				RestoreAfterBuilding;
				P += niPROPS;
			} else
				goto fail;
		wop(cABOLISH):
			if(p_abolish(X[Arg1], X[Arg2]))
				P += niABOLISH;
			else
				goto fail;
		wop(cREPLACN):
			p_replacn(X[Arg1], X[Arg2], X[Arg3]);
			P += niREPLACN;
		wop(cREPLACV):
			p_replacn_var(X[Arg1], X[Arg2], X + Arg3);
			P += niREPLACV;
		wop(cSETARG):
			SaveForBuilding;
			if(!p_setarg(X[Arg1], X[Arg2], X[Arg3])) {
				RestoreTR;
				goto fail;
			}
			RestoreAfterBuilding;
			P += niSETARG;
		wop(cSETARGV):
			SaveForBuilding;
			if(!p_setarg_var(X[Arg1], X[Arg2], X + Arg3)) {
				RestoreTR;
				goto fail;
			}
			RestoreAfterBuilding;
			P += niSETARGV;
		wop(cAREF):
			if(p_aref(X[Arg1], X[Arg2], X + Arg3, Arg4))
				P += niAREF;
			else
				goto fail;
		wop(cASET):
			SaveForUnify;
			if(p_aset(X[Arg1], X[Arg2], X[Arg3], X + Arg4, Arg5)) {
				RestoreAfterUnify;
				P += niASET;
				trailOverflow(0);
				heapOverflow(0);
			} else
				goto fail;
		wop(cID):
			if(!identical(X[Arg1], X[Arg2]))
				goto fail;
			P += niID;
		wop(cNOTID):
			if(identical(X[Arg1], X[Arg2]))
				goto fail;
			P += niNOTID;
		wop(cIDTEST):
			if(identical(X[Arg1], X[Arg2])) {
				P += niIDTEST;
			} else {
				P += niIDTEST;
				goto WriteMode;
			}
		wop(cCOMPARE): {
			/* register Object v; */

			CountLI;
			v = p_compare(X[Arg2], X[Arg3]);
			if(v < 0)
				X[Arg1] = StarToAtom(&SymLT);
			else if(v == 0)
				X[Arg1] = StarToAtom(&SymEqual);
			else
				X[Arg1] = StarToAtom(&SymGT);
			P += niCOMPARE;
		}
		wop(cSORT):
			SaveForBuilding;
			if(p_sort(X[Arg1], X[Arg2], X + Arg3)) {
				RestoreAfterBuilding;
				P += niSORT;
			} else
				goto fail;
		wop(cDALLPRO):
			CP = (E-1)->e_cp;
			E = (E-1)->e_ce;
			if((Word *)E > (Word *)B)
				A = (Word *) E + EnvSize;
			else
				A = (Word *) B + ChoiceSize(B);
		cop(cPRO):			/* FALL THROUGH! */
			if(W != 0) {
#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "W != 0\n");
	displayTerm(stdout, W); fprintf(stdout, "\n");
}
#endif /* DEBUG */

				X[0] = W;
				W = 0;
				RecordPC(P);
				P = PureWakeUp;
			} else {
				RecordPC(P);
				P = CP;
			}
			RecordPC(P);
		wop(cDALLEXE): {
			CountLI;
#ifdef DEBUG3
if(callflg) {
	LockIO;
	displayCall(stdout,
		"CALLING %s(",
		eCharStar((*(Functor **)(P+1))->f_functor->a_pname),
		Arg1,
		X);
	UnlockIO;
}
#endif /* DEBUG3 */
			if(ActionPending) {
				ActionPending = 0;
				if(SignalPending) {
					SaveForInterrupt;
					processSignals();
					RestoreAfterInterrupt;
				}
				if(W != 0) {
					SuspendedVars = Arg1;
/* No point coming back here later.
 * BUG!  Yes there is!  The debugger gets awfully confused if Self isn't set
 * right.  Occurs if target is a spypoint.
 * Worse, because we do come back we must take care not to deallocate *two*
 * environments!
					Self = *(Functor **)(P+1);
					RecordPC(P);
					P = Self->f_code;
					RecordPC(P);
*/
					goto wake;
				}
			}
			CP = (E-1)->e_cp;	/* Deallocate */
			E = (E-1)->e_ce;
			if((Word *)E > (Word *)B)
				A = (Word *) E + EnvSize;
			else
				A = (Word *) B + ChoiceSize(B);
			Self = *(Functor **)(P+1);
			RecordPC(P);
			P = Self->f_code;
			RecordPC(P);
		}
		wop(cEXEC):	{
			CountLI;
#ifdef DEBUG3
if(callflg) {
	displayCall(stdout,
		"CALLING %s(",
		eCharStar((*(Functor **)(P+1))->f_functor->a_pname),
		Arg1,
		X);
}
#endif /* DEBUG3 */
			if(ActionPending) {
				ActionPending = 0;
				if(SignalPending) {
					SaveForInterrupt;
					processSignals();
					RestoreAfterInterrupt;
				}
				if(W != 0) {
					SuspendedVars = Arg1;
/* No point coming back here later.
 * BUG!  Yes there is!  The debugger gets awfully confused if Self isn't set
 * right.  Occurs if target is a spypoint.
					Self = *(Functor **)(P+1);
					RecordPC(P);
					P = Self->f_code;
					RecordPC(P);
*/
					goto wake;
				}
			}
			Self = *(Functor **)(P+1);
			RecordPC(P);
			P = Self->f_code;
			RecordPC(P);
		}
		wop(cEXECSOT): {
			/* register Object v; */

			CountLI;
#ifdef DEBUG3
if(callflg) {
	displayCall(stdout, "CALLING self recursively with args %s(", "", Arg1, X);
}
#endif /* DEBUG3 */
			if(ActionPending) {
				ActionPending = 0;
				if(SignalPending) {
					SaveForInterrupt;
					processSignals();
					RestoreAfterInterrupt;
				}
				if(W != 0) {
					SuspendedVars = Arg1;
					/*
					 * Different from EXEC because we may bind X[Arg2]
					 * during the suspended goal's execution.
					 */
					goto wake;
				}
			}
			v = X[Arg2]; DeRef(v);
			RecordPC(P);
			P = ((Program *)P)[1 + eSType(v)];
			RecordPC(P);
		}
		wop(cCALL): {
			CountLI;
#ifdef DEBUG3
if(callflg) {
	displayCall(stdout,
		"CALLING %s(",
		eCharStar((*(Functor **)(P+1))->f_functor->a_pname),
		Arg2,
		X);
}
#endif /* DEBUG3 */
			if(ActionPending) {
				ActionPending = 0;
				if(SignalPending) {
					SaveForInterrupt;
					processSignals();
					RestoreAfterInterrupt;
				}
				if(W != 0) {
					SuspendedVars = Arg2;
					/* BUG?  Is it worth making this like EXEC? */
					goto wake;
				}
			}
			if((Word *)E > (Word *)B)
				A = (Word *)E + Arg1;
			CP = P + 2;
			Self = *(Functor **)(P+1);
			RecordPC(P);
			P = Self->f_code;
			RecordPC(P);
		}
		wop(cFRUN):
			SaveForEverything;
#ifdef DEBUG5
	{
		register int i;
		fprintf(stderr, "FRUN %x(", (* (PFInt *) (P + 1)));
		for(i = 0; i < Arg1; i++) {
			fprintf(stderr, "%x%s", X[i], (i < Arg1 - 1 ? ", ": ""));
		}
		fprintf(stderr, ")\n");
	}
#endif
			if((** (PFInt *) (P + 1))(X, (int) Arg1)) {
				RestoreAfterEverything;
				P += niFRUN;
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cAPPLY):
			panic("APPLY not yet implemented");
		wop(cFUNCALL): {
			/* register Object v; */
			register int n;

			n = Arg1;
			if(ActionPending) {			/* Pretty rare, but necessary. */
				ActionPending = 0;
				if(SignalPending) {
					SaveForInterrupt;
					processSignals();
					RestoreAfterInterrupt;
				}
				if(W != 0) {
					SuspendedVars = n + 1;
					goto wake;
				}
			}
			v = X[n]; DeRef(v);
			switch(eType(v)) {
			when tREF:
			case tDEL:
				SaveForBuilding;
				CD = makeDelay(n + 1, P);
				mark(v, CD);
				RestoreAfterBuilding;
				RecordPC(P);
				P = CP;
				trailOverflow(0);
				heapOverflow(0);
			when tICN:
				if(!IsAtom(v)) {
					warning("Uncallable object to FUNCALL");
					goto fail;
				}
				Self = enterFunctor(n, (Atom *)eRef(v));
				RecordPC(P);
				P = Self->f_code;
			when tLST: {
				register Object *l;

				Self = enterFunctor(2 + n, &SymCons);
				RecordPC(P);
				P = Self->f_code;
				for(n--; n >= 0; n--)
					X[n + 2] = X[n];
				l = eRef(v);
				X[0] = l[0];
				X[1] = l[1];
			}
			when tCHR: {
				register char *s;

				Self = enterFunctor(2 + n, &SymCons);
				RecordPC(P);
				P = Self->f_code;
				for(n--; n >= 0; n--)
					X[n + 2] = X[n];
				s = (char *)eRef(v);
				X[0] = MakeSmallInt(s[0]);
				X[1] = StarToString(s + 1);
			}
			when tSTR: {
				register Object *f;
				register int arity;

				f = eRef(v);
				arity = eNArgs((Functor *)(*f));
				Self = enterFunctor(arity + n, eFunctor((Functor *)(*f)));
				RecordPC(P);
				P = Self->f_code;
				for(n--; n >= 0; n--)
					X[n + arity] = X[n];
				for(n = arity; n > 0; n--)
					X[n - 1] = f[n];
			}
			when tBMV:
			case tUCN:
				warning("Uncallable object to FUNCALL");
				goto fail;
			}
			RecordPC(P);
		}
		wop(cCATCH): {
			/* register Object v; */

			/*
			 * Note that CATCH doesn't bother to check ActionPending.
			 * The call to the predicate that contains the CATCH
			 * will do anything that is needed.
			 */

			if((Word *)E > (Word *)B)		/* Always true */
				A = (Word *)E + Arg1;
			CP = P + 2;						/* Padded for EnvSize macro */
			v = X[Arg2]; DeRef(v);
			X[0] = v;
			Self = *(Functor **)(P+1);
			RecordPC(P);
			P = Self->f_code;
			RecordPC(P);
		}
		wop(cTHROW): {
			TrailRecord *TRB;
			Object OldW;

			/*
			 * Note that no effort is made to remove unsafe variables
			 * from the skipped environments.  In the normal course of
			 * events there is no way to access any of them after the
			 * catch.
			 */
			OldW = W;			/* To restore on failure of unify() */
			for( ; E != (Environment *) NULL; ) {
				if(OpCodeOf(CP - 2) == cCATCH) {
					TRB = TR;
					SaveForUnify;
					if(unify(Y[0], X[Arg1])) {
						RestoreAfterUnify;
						RecordPC(P);
						P = CP;
						RecordPC(P);
	   					if((Word *)E > (Word *)B)
		   					A = (Word *) E + EnvSize;
	   					else
		   					A = (Word *) B + ChoiceSize(B);
						goto ReadMode;
					} else {
						W = OldW;
						/* Because TR and MR.mr_tr may be the same thing. */
						TR = failure(MR.mr_tr, TRB);
					}
				}
				CP = (E-1)->e_cp;
				E = (E-1)->e_ce;
			}
			panic("Thrown object not caught");
		}
		wop(cJ):
		cop(cJTRUE):	/* Fall Through */
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
		wop(cJFAIL):
			P += niJFAIL;
		wop(cJVX): {
			/* register Object v; */

			v = X[Arg1];
			DeRef3(v,
				{ RecordPC(P); P = ((Program *)P)[1]; RecordPC(P); },
				{ P += niJVX; }
			);
		}
		wop(cJNVX): {
			/* register Object v; */

			v = X[Arg1];
			DeRef3(v,
				{ P += niJNVX; },
				{ RecordPC(P); P = ((Program *)P)[1]; RecordPC(P); }
			);
		}
		wop(cJC): {
			/* register Object v; */

			v = X[Arg1]; DeRef(v);
			if(IsIConst(v)) {
				if(v == ((Object *)P)[1]) {
					RecordPC(P);
					P = ((Program *)P)[2];
					RecordPC(P);
				} else
					P += niJC;
			} else if(IsUConst(v)) {					/* BUG! */
				if(IsFloat(v)) {
					if(IsFloat(((Object *)P)[1])
							&&	eFloat(v) == eFloat(((Object *)P)[1])) {
						RecordPC(P);
						P = ((Program *)P)[2];
						RecordPC(P);
					} else
						P += niJC;
				} else if(IsInt32(v)) {
					if(IsInt32(((Object *)P)[1])
							&&	eInt32(v) == eInt32(((Object *)P)[1])) {
						RecordPC(P);
						P = ((Program *)P)[2];
						RecordPC(P);
					} else
						P += niJC;
				} else
					panic("Impossible UCN type in JC");
			} else
				P += niJC;
		}
		wop(cJS): {
			/* register Object v; */

			v = X[Arg1]; DeRef(v);
			if(IsStr(v) && *eRef(v) == ((Object *)P)[1]) {
				RecordPC(P);
				P = ((Program *)P)[2];
				RecordPC(P);
			} else
				P += niJS;
		}
		wop(cSOT): {
			/* register Object v; */

			v = X[Arg1]; DeRef(v);
			RecordPC(P);
			P = ((Program *)P)[1 + eSType(v)];
			RecordPC(P);
		}
		wop(cSOC): {
			/* register Object v; */

			RecordPC(P);
			/* Warning: Doesn't check that X[Arg1] is a constant. */
			v = X[Arg1]; DeRef(v);
			if(!IsFloat(v)) {
				if(Arg3 == 0) /* no non-float alternatives */
					P = (((Program **)P)[1])[1 << Arg3];
				else
					P = (((Program **)P)[1])[ExtField(v, Arg2, Arg3)];
			} else {
				register Object *ftab;
				register int i, nfloats;

				ftab = ((Object *)*(P + 1)) + (1 << Arg3) + 2;
				nfloats = *(int *)(ftab - 1);
				for(i = 0; i < nfloats; i++) {
					if(eFloat(ftab[i]) == eFloat(v))
						break;
				}
				if(i == nfloats)
					P = (((Program **)P)[1])[(1 << Arg3)];
				else
					P = ((Program *)(ftab + nfloats))[i];
			}
			RecordPC(P);
		}
		wop(cSOCE): {
			/* register Object v; */

			RecordPC(P);
			/* Warning: Doesn't check that X[Arg1] is a constant. */
			v = X[Arg1]; DeRef(v);
			if(!IsFloat(v)) {
				register int i;
				register Object key;

				i = ExtField(v, Arg2, Arg3);
				key = (*(Object **)(P + 1))[(1 << Arg3) + i];
				if(key == v || key == 0)
					P = (((Program **)P)[1])[i];
				else
					P = (((Program **)P)[1])[(1 << (Arg3 + 1))];
			} else {
				register Object *ftab;
				register int i, nfloats;

				ftab = ((Object *)*(P + 1)) + (1 << (Arg3 + 1)) + 2;
				nfloats = *(int *)(ftab - 1);
				for(i = 0; i < nfloats; i++)
					if(eFloat(ftab[i]) == eFloat(v))
						break;
				if(i == nfloats)
					P = (((Program **)P)[1])[(1 << (Arg3 + 1))];
				else
					P = ((Program *)(ftab + nfloats))[i];
			}
			RecordPC(P);
		}
		wop(cSOS): {
			/* register Object v; */

			/* Warning: Doesn't check that X[Arg1] is a structure. */
			v = X[Arg1]; DeRef(v);
			RecordPC(P);
			P = (((Program **)P)[1])[ExtField(*eRef(v), Arg2, Arg3)];
			RecordPC(P);
		}
		wop(cSOSE): {
			/* register Object v; */
			register int i;
			register Object key;

			/* Warning: Doesn't check that X[Arg1] is a structure. */
			v = X[Arg1]; DeRef(v);
			v = *eRef(v);
			i = ExtField(v, Arg2, Arg3);
			key = (*(Object **)(P + 1))[(1 << Arg3) + i];
			RecordPC(P);
			if(key == v || key == 0)
				P = (((Program **)P)[1])[i];
			else
				P = (((Program **)P)[1])[(1 << (Arg3 + 1))];
			RecordPC(P);
		}
		wop(cFVAR): {
			/* register Object v; */

			X[Arg2] = firstVar(X[Arg1]);
			P += niFVAR;
		}
		wop(cSPYMDEL):
			/*
			 * Note that the Delay records for spyed on Delays
			 * are different sized from ordinary Delays.
			 */
			if(!testFlagOff(flgLOCALDEBUG)) {
				Object OldX0, OldX1;

				Flags[flgDELAYED] = StarToAtom(&SymTrue);
				OldX0 = X[0];
				OldX1 = X[1];
				X[0] = Call;
				X[1] = CallNumber;
				SaveForBuilding;
				CD = makeDelay(2, WakeDebug);
				(CD-1)->d_sleeping = 1;
				RestoreAfterBuilding;
				X[0] = OldX0;
				X[1] = OldX1;
				P += niSPYMDEL;
				trailOverflow(0);
				heapOverflow(0);
				goto ReadMode;
			} /* ELSE */
		cop(cMKDEL):		/* FALL THROUGH! */
			SaveForBuilding;
			CD = makeDelay((int) Arg1, ((Program *)P)[1]);
			RestoreAfterBuilding;
			P += niMKDEL;
			trailOverflow(0);
			heapOverflow(0);
		wop(cMARK):
			SaveForBuilding;
			mark(X[Arg1], CD);
			RestoreAfterBuilding;
			P += niMARK;
			trailOverflow(0);
			heapOverflow(0);
		wop(cDELAY):
			RecordPC(P);
			P = CP;
			RecordPC(P);
		wop(cWAKE): {
			/* register Object v; */

			v = X[0]; DeRef(v);
			CD = (Delay *) eRef(v);    /* BUG.  Type checking? */
			if(CD->d_sleeping) {
				register int i;

				{
					/* Like an EXEC rather than a CALL */
					RecordPC(P);
					P = CD->d_p;
					RecordPC(P);
					OldTR = TR;			/* Save TR for WEVAL/WISEQ */
					trail2(1, &CD->d_sleeping, TR);
					CD->d_sleeping = 0;
					CD++;
					for(i = (CD-1)->d_n - 1; i >= 0; i--) {
						X[i] = ((Object *)CD)[i];
#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "X[%d] = ", i);
	displayTerm(stdout, X[i]); fprintf(stdout, "\n");
}
#endif /* DEBUG */
					}
#ifdef DEBUG3
if(callflg) {
	displayCall(stdout,
		"WAKING something or other with args %s(",
		"",
		(CD-1)->d_n,
		X);
}
#endif /* DEBUG3 */
				}
			} else {
				RecordPC(P);
				P = CP;
				RecordPC(P);
			}
		}
		wop(cNECK):
			if(W != 0) {
				register int i, j;
				register Environment *CE;

#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "W != 0\n");
	displayTerm(stdout, W); fprintf(stdout, "\n");
}
#endif /* DEBUG */

				/* Fake an ALL to make room to store CP and save temporaries */
				stackOverflow(256 + wordsof(Choice) + wordsof(Environment));
				CE = E;
				E = (Environment *) A;
				E->e_cp = CP;
				E->e_ce = CE;
				j = Arg1;
				E++;
				A = (Word *)E + j;
				for(i = j - 1; i >= 0; i--) {
					Y[i] = X[i];
#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "X[%d] = ", i);
	displayTerm(stdout, X[i]); fprintf(stdout, "\n");
}
#endif /* DEBUG */
				}

				X[0] = W;
				W = 0;
				CP = P+1;
				RecordPC(P);
				P = WakeUp;
				RecordPC(P);
			} else
				P += niNECK;
		wop(cWOKEN): {
				/* register Object v; */
				register Object *vp;

				vp = (Object *) (E-1);

				/* DALL */
				RecordPC(P);
				P = (E-1)->e_cp;
				RecordPC(P);
				E = (E-1)->e_ce;
				/*
				 * Find number of registers saved when it was decided
				 * to wake goals.  We know that WOKEN is only used
				 * when the current environment is immediately above
				 * the one in which variables are saved.
				 */
				v = (vp - (Object *) E) - 1;
#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "Restoring %d registers from 0x%x\n", v + 1, (Word) E);
}
				for( ; v >= 0; v--) {
					X[v] = Y[v];
if(traceflg) {
	switch(OpCodeOf(P)) {
		case cCUTX:
		case cCUTY:
		case cSOFTCUT:
			;
		break;
		default:
			fprintf(stdout, "X[%d] = ", v);
			displayTerm(stdout, X[v]); fprintf(stdout, "\n");
	}
}
				}
#else /* DEBUG */
				for( ; v >= 0; v--)
					X[v] = Y[v];
#endif /* DEBUG */
				CP = (E-1)->e_cp;
				E = (E-1)->e_ce;
				if((Word *)E > (Word *)B)
					A = (Word *) E + EnvSize;
				else
					A = (Word *) B + ChoiceSize(B);
			}
		wop(cTYPX): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(((1 << eType(v)) & Arg2) == 0)
				goto fail;
			P += niTYPX;
		}
		wop(cTYPY): {
			/* register Object v; */

			CountLI;
			v = Y[Arg1]; DeRef(v);
			if(((1 << eType(v)) & Arg2) == 0)
				goto fail;
			P += niTYPY;
		}
		wop(cCTYPX): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(!IsConst(v) || ((1 << eUType(v)) & Arg2) == 0)
				goto fail;
			P += niCTYPX;
		}
		wop(cCTYPY): {
			/* register Object v; */

			CountLI;
			v = Y[Arg1]; DeRef(v);
			if(!IsConst(v) || ((1 << eUType(v)) & Arg2) == 0)
				goto fail;
			P += niCTYPY;
		}
		wop(cJTYPX): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(((1 << eType(v)) & Arg2) == 0) {
				RecordPC(P);
				P = ((Program *)P)[1];
				RecordPC(P);
			} else
				P += niJTYPX;
		}
		wop(cJTYPY): {
			/* register Object v; */

			CountLI;
			v = Y[Arg1]; DeRef(v);
			if(((1 << eType(v)) & Arg2) == 0) {
				RecordPC(P);
				P = ((Program *)P)[1];
				RecordPC(P);
			} else
				P += niJTYPY;
		}
		wop(cJCTYPX): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(!IsConst(v) || ((1 << eUType(v)) & Arg2) == 0) {
				RecordPC(P);
				P = ((Program *)P)[1];
				RecordPC(P);
			} else
				P += niJCTYPX;
		}
		wop(cJCTYPY): {
			/* register Object v; */

			CountLI;
			v = Y[Arg1]; DeRef(v);
			if(!IsConst(v) || ((1 << eUType(v)) & Arg2) == 0) {
				RecordPC(P);
				P = ((Program *)P)[1];
				RecordPC(P);
			} else
				P += niJCTYPY;
		}
		wop(cEREF): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			X[Arg2] = MakeSmallInt(eRef(v));
			P += niEREF;
		}
		wop(cMKOBJ): {
			/* register Object v; */
			register Object t, ct;

			CountLI;
			t = X[Arg1]; DeRef(t);
			ct = X[Arg2]; DeRef(ct);
			if(!IsSmallInt(t) || !IsSmallInt(ct))
				panic("Integer expected in MKOBJ");
			v = X[Arg3]; DeRef(v);
			X[Arg4] = MakeObject(t, ct, v);
			P += niMKOBJ;
		}
		wop(cFAIL):
			goto fail;
		wop(cLABELX):
			/*X[Arg1] = StarToBlock(B);*/
			X[Arg1] = MakeSmallInt((Word *)B - stackBase);
			P += niLABELX;
		wop(cLABELY):
			/*Y[Arg1] = StarToBlock(B);*/
			Y[Arg1] = MakeSmallInt((Word *)B - stackBase);
			P += niLABELY;
		wop(cCUTX): {
			/* register Object v; */

			CountLI;
			/*
			 * BUG.
			 * It would be better if CUT knew exactly how many
			 * variables to save.  However, very few goals are
			 * woken here so the extra space taken probably doesn't
			 * matter all that much.
			 *
			 * Because waking goals at places other than calls
			 * potentially complicates things like stack backtrace,
			 * we don't process signals here.
			 *
			 * Ideally, everything would be cleaner if we just
			 * suspended the cut as well and waited for the next call
			 * to wake things, but there are too many other things
			 * that would need to be decompiled too.
			 */
			if(W != 0) {
				ActionPending = SignalPending;
				SuspendedVars = 256;
				goto wake;
			}
			v = X[Arg1]; DeRef(v);
			/*
			if(!IsBlock(v))
				panic("Argument to CUTX not a block");
		   	v = (Object)eRef(v);
			*/
			if(!IsSmallInt(v))
				panic("Argument to CUTX not an index");
		   	v = (Object)(stackBase + eSmallInt(v));
		   	if(B > (Choice *)v) {
		   		B = (Choice *)v;
				HB = (B-1)->c_h;
		   		TR = trimTrail(TR, B, HB);
				/* BUG!
				 * Because we can't easily find the size of the current
				 * environment within the clause that allocated it, we only
				 * pop the stack if a choice point is on top.
				 *
				 * The only ways to find the size of the current environment
				 * involve either traversing the stack of choice points or
				 * computing it at compile-time.  SOFTCUT could probably use
				 * the former without loss of efficiency, but the latter is
				 * the better method.  Unfortunately, it requires still more
				 * variations on the CUT instruction.
				 *
				 * The stack will be properly adjusted if there is a DALL
				 * before the next choice point or environment creation.
				 */
				if((Word *)B > (Word *)E)
					A = (Word *) B + ChoiceSize(B);
			}
			P += niCUTX;
		}
		wop(cCUTY): {
			/* register Object v; */

			CountLI;
			/* BUG.  See CUTX. */
			if(W != 0) {
				ActionPending = SignalPending;
				SuspendedVars = 256;
				goto wake;
			}
			v = Y[Arg1]; DeRef(v);
			/*
			if(!IsBlock(v))
				panic("Argument to CUTY not a block");
		   	v = (Object)eRef(v);
			*/
			if(!IsSmallInt(v))
				panic("Argument to CUTY not an index");
		   	v = (Object)(stackBase + eSmallInt(v));
		   	if(B > (Choice *)v) {
				B = (Choice *)eRef(v);
				HB = (B-1)->c_h;
		   		TR = trimTrail(TR, B, HB);
				/* BUG!  See CUTX. */
				if((Word *)B > (Word *)E)
					A = (Word *) B + ChoiceSize(B);
			}
			P += niCUTY;
		}
		wop(cSOFTCUT): {
			/* register Object v; */

			CountLI;
			/* BUG.  See CUTX. */
			if(W != 0) {
				ActionPending = SignalPending;
				SuspendedVars = 256;
				goto wake;
			}
			v = X[Arg1]; DeRef(v);
			/*
			if(!IsBlock(v))
				panic("Argument to SOFTCUT not a block");
			(((Choice *) eRef(v)) - 1)->c_retry = noChoice;
			*/
			if(!IsSmallInt(v))
				panic("Argument to SOFTCUT not an index");
			(((Choice *) (stackBase + eSmallInt(v))) - 1)->c_retry = noChoice;
			P += niSOFTCUT;
		}
		wop(cONCUT): {
			/* register Object v; */

			trailOverflow(1);
			v = X[Arg1]; DeRef(v);
			/* BUG.  No error checking. */
			TR->tr_address = cFunctDummy + eSmallInt(v);
			v = X[Arg2]; DeRef(v);
			TR->tr_value = v;
			TR++;
			P += niONCUT;
		}
		wop(cERROR):
			fprintf(stdout, "%s", errorMessages[Arg1]);
			switch(Arg2) {
			when 0:
				P += niERROR;
			when 1:
				goto fail;
			when 2:
				panic("");
			when 3:
				fprintf(
					stdout,
					"%s/%d\n",
					eCharStar(Self->f_functor->a_pname),
					Self->f_arity);
				goto fail;

			break;
			default:
				panic("Undefined error action");
			}
		wop(cALL):
			stackOverflow(256 + wordsof(Environment));
			((Environment *) A)->e_ce = E;
			E = (Environment *) A;
			E->e_cp = CP;
			E++;
			A = (Word *)E + Arg1;
			CP = P + 2;				/* Make EnvSize macro valid */
			P += niALL;
		wop(cDALL):
			P += niDALL;
			CP = (E-1)->e_cp;
			E = (E-1)->e_ce;
			if((Word *)E > (Word *)B)
				A = (Word *) E + EnvSize;
			else
				A = (Word *) B + ChoiceSize(B);
		wop(cTE):
			/*
			 * Choice point creation in the body of a clause must wake
			 * calls because W is not restored on backtracking.
			 */
			EnterShallowMode;
			stackOverflow(512 + wordsof(Choice) + wordsof(Environment));

			if(ActionPending) {
				ActionPending = 0;
				if(SignalPending) {
					SaveForInterrupt;
					processSignals();
					RestoreAfterInterrupt;
				}
				if(W != 0) {
					SuspendedVars = Arg3;
					goto wake;
				}
			}

#ifdef SHALLOW
			HB = H;
			H = A + wordsof(Choice);
			(((Choice *)H)-1)->c_b = B;
			(((Choice *)H)-1)->c_tr = TR;
			(((Choice *)H)-1)->c_retry = ((Program *)P)[1];
			A = H + Arg1;
			B = (Choice *) H;
			H = HB;
			P += niTE;
			goto ShallowReadMode;
#else /* SHALLOW */
			HB = H;
			H = A + wordsof(Choice);
			(((Choice *)H)-1)->c_b = B;
			(((Choice *)H)-1)->c_tr = TR;
			(((Choice *)H)-1)->c_retry = ((Program *)P)[1];
			(((Choice *)H)-1)->c_e = E;
			(((Choice *)H)-1)->c_cp = CP;
			{
				/* register Object v; */

				v = Arg1;
				A = H + v;
				wcopy(H, X, v);
			}
			B = (Choice *) H;
			H = ((((Choice *)H)-1)->c_h = HB);
			P += niTE;
#endif /* SHALLOW */
		wop(cRE):
			(B-1)->c_retry = ((Program *)P)[1];
			A = (Word *) B + ChoiceSize(B);
			P += niRE;
		wop(cTRE):
			/* BUG!  Inefficiency.
			 * The placing of the top of stack is conservative.
			 * SOFTCUT may have removed the choice point below
			 * the current one.
			 */
			A = (Word *) --B;
			B = B->c_b;
			HB = (B-1)->c_h;
			P += niTRE;
		wop(cT):
			/* BUG!  (potential)
			 * Choice-point creation in the body of a clause needs to
			 * wake calls.
			 *
			 * Body choice-points are created with the TE instruction,
			 * but if T is ever to be used in the body of a clause,
			 * or predicate preludes to be allowed to bind variables,
			 * then similar code to the TE instruction will be needed
			 * here and the compiler will need to be changed to output
			 * the proper number of registers to be saved while the
			 * woken goals are resumed.
			 */
			EnterShallowMode;
			stackOverflow(512 + wordsof(Choice) + wordsof(Environment));
#ifdef SHALLOW
#ifdef extra_reg0
			((Choice *) A)->c_b = B;
			B = 1 + (Choice *) A;
			(B-1)->c_tr = TR;
			(B-1)->c_retry = P + niT;
			HB = H;
			A = (Word *) B + Arg1;
#else /* extra_reg0 */
			HB = H;
			H = A + wordsof(Choice);
			(((Choice *)H)-1)->c_b = B;
			(((Choice *)H)-1)->c_tr = TR;
			(((Choice *)H)-1)->c_retry = P + niT;
			A = H + Arg1;
			B = (Choice *) H;
			H = HB;
#endif /* extra_reg0 */
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
			goto ShallowReadMode;
#else /* SHALLOW */
#ifdef extra_reg0
			((Choice *) A)->c_b = B;
			B = 1 + (Choice *) A;
			(B-1)->c_tr = TR;
			(B-1)->c_retry = P + niT;
			(B-1)->c_e = E;
			(B-1)->c_cp = CP;
			(B-1)->c_h
				= HB
				= H;
			{
				/* register Object v; */

				v = Arg1;
				A = (Word *) B + v;
				wcopy((Word *)B, X, v);
			}
#else /* extra_reg0 */
			HB = H;
			H = A + wordsof(Choice);
			(((Choice *)H)-1)->c_b = B;
			(((Choice *)H)-1)->c_tr = TR;
			(((Choice *)H)-1)->c_retry = P + niT;
			(((Choice *)H)-1)->c_e = E;
			(((Choice *)H)-1)->c_cp = CP;
			{
				/* register Object v; */

				v = Arg1;
				A = H + v;
				wcopy(H, X, v);
			}
			B = (Choice *) H;
			H = ((((Choice *)H)-1)->c_h = HB);
#endif /* extra_reg0 */
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
#endif /* SHALLOW */
		wop(cR):
			(B-1)->c_retry = P + niR;
			A = (Word *) B + ChoiceSize(B);
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
		wop(cTR):
			/* BUG!  Inefficiency.
			 * The placing of the top of stack is conservative.
			 * SOFTCUT may have removed the choice point below
			 * the current one.
			 */
			A = (Word *) --B;
			B = B->c_b;
			HB = (B-1)->c_h;
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
		wop(cRET):
			CMR = MR.mr_suspended;
			return;
		wop(cABORT):
			CMR = MR.mr_suspended;		/* Not strictly necessary */
			longjmp(re_entry_point, 1);
		wop(cEXIT): {
			/* register Object v; */

			v = X[Arg1]; DeRef(v);
			if(IsSmallInt(v))
				exit(eSmallInt(v));
			else
				exit(1);
		}
		wop(cPRINTF):
			/* BUG!  There is no good reason for printf to fail. */
			SaveForBuilding;		/* No need to restore though. */
			if(p_printf(X[Arg1], X[Arg2], X[Arg3])) {
				P += niPRINTF;
			} else {
				goto fail;
			}
		wop(cPRTNUM):
			SaveForBuilding;			/* Excessive */
			if(p_printNumber(X[Arg1], X[Arg2], X[Arg3], X + Arg4)) {
				RestoreAfterBuilding;
				P += niPRTNUM;
			} else
				goto fail;
		wop(cSPRT): {
			SaveH;
			if(p_termToString(X[Arg1], X[Arg2], X + Arg3)) {
				RestoreH;
				P += niSPRT;
				heapOverflow(0);
			} else
				goto fail;
		}
		wop(cDISPX):
			displayTerm(stdout, X[Arg1]); fprintf(stdout, "\n");
			P += niDISPX;
		wop(cTRACE):
#ifdef DEBUG
			traceflg = regflg = Arg1;
#endif /* DEBUG */
#ifdef DEBUG3
			callflg = Arg1;
#endif /* DEBUG3 */
			P += niTRACE;
		wop(cGET): {
			/* register Object v; */

			CountLI;
			v = p_sget(inputStream, Arg2);
			if(IsNIL(v)) {
				InterruptAction = intCOUNT;
				/*
				 * Process interrupts immediately -- there's a
				 * lot of (repeat, get(X)) loops around!
				 */
				SaveForInterrupt;
				processSignals();
				RestoreAfterInterrupt;
				goto fail;
			}
			InterruptAction = intCOUNT;
			X[Arg1] = v;
			P += niGET;
		}
		wop(cPUT): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(IsSmallInt(v)) {
				(*outputStream->s_put)(eSmallInt(v), outputStream);
				P += niPUT;
			} else {
				warning("Non-integer to PUT");
				goto fail;
			}
		}
		wop(cSGET): {
			register Stream *st;
			/* register Object v; */

			CountLI;
			st = validateStream(X[Arg1], 'r');
			if(st == (Stream *) NULL) {
				warning("Invalid stream to SGET");
				goto fail;
			}
			v = p_sget(st, Arg3);
			if(IsNIL(v)) {
				InterruptAction = intCOUNT;
				/*
				 * Process interrupts immediately -- there's a
				 * lot of (repeat, get(X)) loops around!
				 */
				SaveForInterrupt;
				processSignals();
				RestoreAfterInterrupt;
				goto fail;
			}
			InterruptAction = intCOUNT;
			X[Arg2] = v;
			P += niSGET;
		}
		wop(cSGETTOK): {
			register Stream *st;
			/* register Object v; */

			CountLI;
			st = validateStream(X[Arg1], 'r');
			if(st == (Stream *) NULL) {
				warning("Invalid stream to SGETTOK");
				goto fail;
			}
			if(st->s_iseof == StarToAtom(&SymEOF)) {
				warning("Attempt to read past end of file in SGETTOK");
				if(st == streams + 0) {
					if(testFlagOn(flgFILE))
						warning("Re-opening user_input on terminal");
					(void)freopen("/dev/tty", "r", st->s_fp);
					st->s_instance++;
					st->s_iseof = StarToAtom(&SymFail);
				}
				CMR = MR.mr_suspended;		/* Not strictly necessary */
				longjmp(re_entry_point, 1);		/* Abort */
			}
			v = getToken(
					st, X + Arg2, X + Arg3, (char *) H, (char *) heapMax);
			switch(v) {
			when -1:							/* EOF */
				st->s_iseof = StarToAtom(&SymEOF);
				P += niSGETTOK;
			when -2:							/* Interrupted */
				goto fail;

			break;
			default:
				H += NWORDS(v);
				P += niSGETTOK;
			}
		}
		wop(cSPUT): {
			/* register Object v; */
			register Stream *st;

			CountLI;
			st = validateStream(X[Arg1], 'w');
			if(st == (Stream *) NULL) {
				warning("Invalid stream to SPUT");
				goto fail;
			}
			v = X[Arg2]; DeRef(v);
			if(IsSmallInt(v)) {
				(*st->s_put)(eSmallInt(v), st);
				P += niSPUT;
			} else {
				warning("Non-integer to SPUT");
				goto fail;
			}
		}
		wop(cSPUTL): {
			register Stream *st;

			CountLI;
			st = validateStream(X[Arg1], 'w');
			if(st == (Stream *) NULL) {
				warning("Invalid stream to SPUTL");
				goto fail;
			}
			putlStream(X[Arg2], st);
			P += niSPUTL;
		}
		wop(cTOKEN): {
			/* register Object v; */
			register int len;

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(IsNIL(v))
				/*goto fail*/;
			else if(!IsString(v)) {
				warning("String expected in TOKEN");
				goto fail;
			}
			len = tokenize(
					eCharStar(v),
					X + Arg2,
					X + Arg3,
					X + Arg4,
					(char *) H,
					(char *) heapMax);
			if(len == -1)			/* Unexpected EOF */
				goto fail;
			H += NWORDS(len);
			P += niTOKEN;
		}
		wop(cOPEN): {
			register Stream *st;

			CountLI;
			/* note that openStream never returns a stdio stream */
			st = openStream(X[Arg1], X[Arg2]);
			if(st == (Stream *) NULL) {
				if(testFlagOn(flgFILE)) {
					CMR = MR.mr_suspended;		/* Not strictly necessary */
					longjmp(re_entry_point, 1);
				} else
					goto fail;
			} else {
				register Word *s;

				heapOverflow(3);
				X[Arg3] = StarToStr(H);
				s = eRef(st->s_identifier);
				*H++ = *s++;
				*H++ = *s++;
				*H++ = *s;
				P += niOPEN;
			}
		}
		wop(cCLOSE): {
			register Stream *st;

			CountLI;
			SaveH;
			if(!closeStream(X[Arg1], X + Arg2)) {
				if(testFlagOn(flgFILE)) {
					CMR = MR.mr_suspended;		/* Not strictly necessary */
					longjmp(re_entry_point, 1);
				} else
					goto fail;
			} else {
				RestoreH;
				P += niCLOSE;
			}
		}
		wop(cGETSTR): {
			/* register Object v; */
			register Stream *st;

			v = X[Arg1]; DeRef(v);
			if(!IsSmallInt(v)) {
				warning("Integer expected in GETSTR");
				goto fail;
			}
			v = eSmallInt(v);
			if(v < 0 || v >= nstreams) {
/*
				fprintf(stderr, "v = %d, nstreams@0x%x = %d, flag is 0x%x\n",
					v, &nstreams, nstreams, Flags[flgNSTREAMS]);
*/
				warning("Stream number out of range in GETSTR");
				goto fail;
			}
			st = streams + v;
			st->s_chars = st->s_counts->s_chars;
			st->s_lines = st->s_counts->s_lines;
			st->s_linepos = st->s_counts->s_linepos;
			X[Arg2] = StarToStr(st);
			P += niGETSTR;
		}
		wop(cFLUSH): {
			register Stream *st;

			CountLI;
			st = validateStream(X[Arg1], 'w');
			if(st == (Stream *) NULL) {
				warning("Invalid stream to FLUSH");
				goto fail;
			} else {
				fflush(st->s_fp);
				P += niFLUSH;
			}
		}
		wop(cCLRERR): {
			register Stream *st;

			CountLI;
			st = validateStream(X[Arg1], 'e');
			if(st == (Stream *) NULL) {
				warning("Invalid stream to CLRERR");
				goto fail;
			} else {
				clearerr(st->s_fp);
				st->s_iseof = StarToAtom(&SymFail);
				P += niCLRERR;
			}
		}
		wop(cSETSTR): {
			register Stream *st;

			CountLI;
			st = validateStream(X[Arg2], Arg1 ? 'w' : 'r');
			if(st == (Stream *) NULL) {
				warning("Invalid stream to SETSTR");
				goto fail;
			} else {
				if(Arg1 == 0)
					inputStream = st;
				else
					outputStream = st;
				P += niSETSTR;
			}
		}
		wop(cCURRSTR): {
			/* register Object v; */

			if(Arg1 == 0)
				v = inputStream->s_identifier;
			else
				v = outputStream->s_identifier;
			if(IsStr(v)) {
				register Word *s;

				heapOverflow(3);
				X[Arg2] = StarToStr(H);
				s = eRef(v);
				*H++ = *s++;
				*H++ = *s++;
				*H++ = *s;
			} else
				X[Arg2] = v;
			P += niCURRSTR;
		}
		wop(cFORK):
			if(fork4(X + Arg2, X + Arg3, X + Arg4, (Arg1 ? X + Arg5 : NULL)))
				P += niFORK;
			else
				goto fail;
		wop(cSYSCALL):
			CountLI;
			SaveForUnify;
			if(sysCall(Arg1, X[Arg2], X[Arg3], X[Arg4], X + Arg5)) {
				RestoreAfterUnify;
				P += niSYSCALL;
				heapOverflow(0);
			} else {
				RestoreTR;
				goto fail;
			}
		wop(cLOAD): {
			/* register Object v; */
			register Functor *init, *inf;

			v = X[Arg1]; DeRef(v);
			if(IsString(v)) {
				inf = enterFunctor(0, &SymInitializing);
				inf->f_code = initializing;
				inf->f_codeType = fCOMPILED;
				inf->f_sourceFile = StarToAtom(&SymUser);
				init = enterFunctor(0, &Sym_Init);
				init->f_code = undefinedPredicate;
				init->f_codeType = fCOMPILED;
				init->f_sourceFile = NIL;
				if(!load(eCharStar(v))) {
					inf->f_code = notInitializing;
					warning2("Unable to load %s", eCharStar(v));
					goto fail;
				}
				if(init->f_sourceFile != NIL) {
					SaveForInterrupt;
					callFunctor(init);
					RestoreAfterInterrupt;
					init->f_code = undefinedPredicate;
					init->f_sourceFile = NIL;
				}
				inf->f_code = notInitializing;
				SourceFile = StarToAtom(&SymUser);
				P += niLOAD;
			} else {
				warning("Non-string to LOAD");
				goto fail;
			}
		}
		wop(cILOAD):
			p_iload(X[Arg1], X[Arg2], X[Arg3]);
			P += niILOAD;
		wop(cFLOAD):
			SaveForBuilding;
			if(fload(X[Arg1], X[Arg2], X[Arg3], X + Arg4)) {
				RestoreAfterBuilding;
				P += niFLOAD;
			} else {
				warning("Foreign function load failed");
				goto fail;
			}
		wop(cSPYPT):
			if(testFlagOff(flgLOCALDEBUG))
				P = ((Program *)P)[1];
			else {
				heapOverflow(1 + 256);
				if(Self->f_arity == 0)
					X[0] = StarToAtom(Self->f_functor);
				else {
					register int i;
					Word *OldH;

					OldH = H;
					/* BUG!  The StrHeader is just Self! */
					*H++ = StarToStrHeader(Self->f_arity, Self->f_functor);
					/*
					 * Globalize variables, if only to prevent the heap
					 * pointing to the stack.
					 */
					for(i = 0; i < Self->f_arity; i++) {
						/* register Object v; */

						v = X[i]; DeRefPure(v);
#ifdef NO_LOCAL_VARS
#else /* NO_LOCAL_VARS */
						if(IsRef(v) && (Word *)eRefStar(v) > (Word *) H)
						{	trailS(eRefStar(v), TR);
							*eRefStar(v) = StarToRef((Object *) H + i);
							v = (Object) ((Object *) H + i);
							*(Object *)v = MakeUnboundRef(v);
						} else
#endif /* NO_LOCAL_VARS */
							H[i] = v;
					}
					H += Self->f_arity;
					X[0] = StarToStr(OldH);
				}
				X[1] = Flags[flgCALLNUMBER]++;
				P += niSPYPT;
			}
		wop(cSPY):
			p_spy(X[Arg1], X[Arg2]);
			P += niSPY;
		wop(cNOSPY):
			p_nospy(X[Arg1], X[Arg2]);
			P += niNOSPY;
		wop(cPSTOT):
#ifdef PSTOT
			SaveH;
			if(p_pstot(X[Arg1], X + Arg2)) {
				RestoreH;
				P += niPSTOT;
				heapOverflow(0);
			} else
				goto fail;
#else /* PSTOT */
			panic("PSTOT not implemented in this version");
#endif /* PSTOT */
		wop(cSIMC):
			if(p_simc(Arg1, X + Arg2, X + Arg3, X + Arg4, X + Arg5))
				P += niSIMC;
			else
				goto fail;
		wop(cDSIMC):
			if(p_dsimc(Arg1, X + Arg2, X + Arg3, X + Arg4, X + Arg5, X + Arg6))
				P += niDSIMC;
			else
				goto fail;
		wop(cSQL):
			if(p_sql(Arg1, X + Arg2, X + Arg3, X + Arg4))
				P += niSQL;
			else
				goto fail;

		break;
		case 255:
			/*
			 * Ensure that jump table is full so that we can dispense
			 * with bounds checking if we feel so inclined.
			 */
		default:
			fprintf(stdout,
				"Unknown OP %d at %x\n",
				OpCode,
				(Word)(P));
			panic("");
		}
		continue;

		wake: {
			register Environment *CE;

#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "W != 0\n");
	displayTerm(stdout, W); fprintf(stdout, "\n");
}
#endif /* DEBUG */
		/* Fake an ALL to make room to store CP and save temporaries */
			stackOverflow(256 + wordsof(Choice) + wordsof(Environment));
			CE = E;
			E = (Environment *) A;
			E->e_cp = CP;
			E->e_ce = CE;
			E++;
			A = (Word *)E + SuspendedVars;
			wcopy((Word *)E, X, SuspendedVars);

			X[0] = W;
			W = 0;
			CP = P;
			RecordPC(P);
			P = WakeUp;
			RecordPC(P);
		}
		continue;

		fail: {
#ifdef DEBUG
			register Word *OldH;

			OldH = H;
if(
	traceflg) {
	fprintf(stdout, "Failing\n");
}
#endif /* DEBUG */
			RecordPC(P);
			P = (B-1)->c_retry;
			RecordPC(P);
#ifdef INLINE
			TR = failure(TR, (B-1)->c_tr);
#else /* INLINE */
			{   register TrailRecord *tb;

				tb = (B-1)->c_tr;
				while(TR > tb) {
					TR--;
#ifdef DEBUG
if(traceflg) {
	fprintf(stdout,
		"RESTORING _%x TO 0x%x\n", (Word)TR->tr_address, TR->tr_value);
}
#endif /* DEBUG */
					*TR->tr_address = TR->tr_value;
				}
			}
#endif /* INLINE */
			W = 0;
#ifdef extra_reg0
			E = (B-1)->c_e;
			CP = (B-1)->c_cp;
			H = (B-1)->c_h;
			wcopy(X, (Object *)B, ChoiceSize(B));
#else /* extra_reg0 */
			H = (Word *) B;
			E = (((Choice *) H)-1)->c_e;
			CP = (((Choice *) H)-1)->c_cp;
			wcopy(X, H, ChoiceSize(B));
			H = (((Choice *) H)-1)->c_h;
#endif /* extra_reg0 */

#ifdef DEBUG
			while(OldH > H)
				*--OldH = 0;
#endif /* DEBUG */
		}
	}

/* Write Mode */

#define rop(op) when op: goto CONCAT(L,op)

WriteMode:
	for(;;) {
		register Object v;

#ifdef DEBUG
if(regflg || traceflg) {
	register int i;

if(regflg)
	fprintf(stdout,
	"%5x %5x %5x %5x %5x ",
	(Word*)E, (Word*)B, (Word*)A, TR, (Word*)H);
if(traceflg) {
	fprintf(stdout, "w");
	(void) displayInstruction(stdout, 0, P);
}
for(i = 0; i < spyflg; i++) {
	Word *address, old;

	address = SpyPoints[i].sp_address;
	old = SpyPoints[i].sp_value;
	if(*address != old) {
		fprintf(stdout, "*0x%x changed from 0x%x(%d) to 0x%x(%d)\n",
			address,
			old, old,
			*address, *address);
		SpyPoints[i].sp_value = *address;
	}
}
}
#endif /* DEBUG */
#ifdef MEASURE
WriteModeOps[OpCode]++;
#endif /* MEASURE */
	switch(OpCode) {
	rop(cGVARX);
	rop(cGVARA);
	rop(cGVARY);
	rop(cGLVARY);
	rop(cGVALX);
	rop(cGVALY);
	rop(cGC);
	rop(cGNIL);
	rop(cGS);
	rop(cGSX1);
	rop(cGSX2);
	rop(cGL);
	rop(cGLVX2);
	rop(cPVARX);
	rop(cPVARY);
	rop(cPVALY);
	rop(cPVALY2);
	rop(cPUVAL);
	rop(cPC);
	rop(cPNIL);
	rop(cPS);
	rop(cPL);
	when cUVOID: {
		/* register Object v; */

		v = Arg1;
		P += niUVOID;
		/* Assume that Arg1 > 1 */
		do {
			*H = StarToUnboundRef(H);
			H++;
		} while(--v > 0);
	}
	when cUVOID1:
		*H = StarToUnboundRef(H);
		H++;
		P += niUVOID1;
	when cUVARX:
	case cUVARA:		/* Fall Through */
		X[Arg1] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		P += niUVARX;
	when cUVARY:
		Y[Arg1] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		P += niUVARY;
	when cUVARXVARX:
	case cUVARAVARA:	/* Fall Through */
		X[Arg1] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		X[Arg2] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		P += niUVARXVARX;
	when cUVARYVARY:
		Y[Arg1] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		Y[Arg2] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		P += niUVARYVARY;
	when cUVARXLVX:
	case cUVARALVX: {	/* Fall Through */
		/* register Object v; */

		X[Arg1] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		v = X[Arg2]; using(v);
		unify_value_write(v, 1, X[Arg2], retryTarget(cUVARXLVXw));
		P += niUVARXLVX;
	}
	when cULVXVARX:
	case cULVXVARA: {	/* Fall Through */
		/* register Object v; */

		v = X[Arg1]; using(v);
		unify_value_write(v, 1, X[Arg1], retryTarget(cULVXVARXw));
		X[Arg2] = StarToRef(H);
		*H = StarToUnboundRef(H);
		H++;
		P += niULVXVARX;
	}
	when cULVXLVX: {
		/* register Object v; */

		v = X[Arg1]; using(v);
		unify_value_write(v, 1, X[Arg1], retryTarget(cULVXVARXw1));
		v = X[Arg2]; using(v);
		unify_value_write(v, 1, X[Arg2], retryTarget(cULVXVARXw2));
		P += niULVXLVX;
	}
	when cUVALX:
#ifdef NO_LOCAL_VARS
	case cULVX:
#endif /* NO_LOCAL_VARS */
	{
		/* register Object v; */

		v = X[Arg1]; using(v);
		DeRefPure(v);
		*H++ = v;
		P += niUVALX;
	}
	when cUVALY:
#ifdef NO_LOCAL_VARS
	case cULVY:
#endif /* NO_LOCAL_VARS */
	{
		/* register Object v; */

		v = Y[Arg1]; using(v);
		DeRefPure(v);
		*H++ = v;
		P += niUVALY;
	}
#ifdef NO_LOCAL_VARS
#else /* NO_LOCAL_VARS */
	when cULVX: {
		/* register Object v; */

		v = X[Arg1]; using(v);
		DeRefPure(v);
		if(!IsRef(v) || (Word *)eRefStar(v) < H)    /* Grunge */
			*H++ = v;
		else {
			*H = StarToUnboundRef(H);
			bindS(eRefStar(v), StarToRef(H), TR);
			X[Arg1] = StarToRef(H);
			H++;
			binding(v);
		}
		P += niULVX;
	}
	when cULVY: {
		/* register Object v; */

		v = Y[Arg1]; using(v);
		DeRefPure(v);
		if(!IsRef(v) || (Word *)eRefStar(v) < H)    /* Grunge */
			*H++ = v;
		else {
			*H = StarToUnboundRef(H);
			bindS(eRefStar(v), StarToRef(H), TR);
			H++;
			binding(v);
		}
		P += niULVY;
	}
#endif /* NO_LOCAL_VARS */
	when cUC:
		*H++ = ((Object *)P)[1];
		P += niUC;
	when cUL:
		*H = StarToList(H + 1);
		H++;
		P += niUL;
		heapOverflow(2);
	when cUS:
		*H = StarToStr(H + 1);
		H++;
		*H++ = ((Object *)P)[1];
		P += niUS;
		heapOverflow(1 + MAXARITY);
	rop(cARITH);
	rop(cAPUSHX);
	rop(cAPUSHY);
	rop(cAPUSHI);
	rop(cAPUSHF);
	when cPUSHX:
	case cPUSHY:
	case cPUSHVX:
	case cPUSHVY:
	case cPUSHI:
	case cPUSHF:
	case cPOPX:
	case cPOPY:
	case cPOPVX:
	case cPOPVY:
	case cPOPC:
	case cAFUNC:
	case cAPRED:
	case cJPRED:
		fprintf(stdout, "Inappropriate OP %s at %x in WriteMode\n",
			bytecodes[OpCode].op_name, (Word)(P));
		panic("How did you compile this junk?");
	rop(cWEVAL);
	rop(cOCCURS);
	rop(cNAME);
	rop(cLTOS);
	rop(cUNIV);
	rop(cFUNCTOR);
	rop(cARG);
	rop(cISEQ);
	rop(cWISEQ);
	rop(cCVTTOH);
	rop(cPLUS);
	rop(cDEFINED);
	rop(cARITIES);
	rop(cFLAGS);
	rop(cEXECS);
	rop(cSYMBOL);
	rop(cCOPY);
	rop(cUNCOPY);
	rop(cMAKEBMT);
	rop(cLINKBMT);
	rop(cINST);
	rop(cERASE);
	rop(cPROPS);
	rop(cABOLISH);
	rop(cREPLACN);
	rop(cREPLACV);
	rop(cSETARG);
	rop(cSETARGV);
	rop(cAREF);
	rop(cASET);
	rop(cID);
	rop(cNOTID);
	rop(cIDTEST);
	rop(cCOMPARE);
	rop(cSORT);
	rop(cPRO);
	rop(cDALLPRO);
	rop(cEXEC);
	rop(cDALLEXE);
	rop(cEXECSOT);
	rop(cCALL);
	rop(cFRUN);
	rop(cAPPLY);
	rop(cFUNCALL);
	rop(cCATCH);
	rop(cTHROW);
	rop(cJ);
	when cJTRUE:
		P += niJTRUE;
	when cJFAIL:
		RecordPC(P);
		P = ((Program *)P)[1];
		RecordPC(P);
	rop(cJVX);
	rop(cJNVX);
	rop(cJC);
	rop(cJS);
	rop(cSOT);
	rop(cSOC);
	rop(cSOCE);
	rop(cSOS);
	rop(cSOSE);
	rop(cFVAR);
	rop(cSPYMDEL);
	rop(cMKDEL);
	rop(cMARK);
	rop(cDELAY);
	rop(cWAKE);
	rop(cNECK);
	rop(cWOKEN);
	rop(cTYPX);
	rop(cTYPY);
	rop(cCTYPX);
	rop(cCTYPY);
	rop(cJTYPX);
	rop(cJTYPY);
	rop(cJCTYPX);
	rop(cJCTYPY);
	rop(cEREF);
	rop(cMKOBJ);
	rop(cFAIL);
	rop(cLABELX);
	rop(cLABELY);
	rop(cCUTX);
	rop(cCUTY);
	rop(cSOFTCUT);
	rop(cONCUT);
	rop(cERROR);
/* ALL is duplicated here so that it may appear amongst UNIFY instructions
 * without changing mode.  Note that DALL cannot be in this situation.
 */
	when cALL:
		stackOverflow(256 + wordsof(Environment));
		((Environment *) A)->e_ce = E;
		E = (Environment *) A;
		E->e_cp = CP;
		E++;
		A = (Word *)E + Arg1;
		CP = P + 2;				/* Make EnvSize macro valid */
		P += niALL;
	rop(cDALL);
	rop(cTE);
	rop(cRE);
	rop(cTRE);
	rop(cT);
	rop(cR);
	rop(cTR);
	rop(cRET);
	rop(cABORT);
	rop(cEXIT);
	rop(cPRINTF);
	rop(cPRTNUM);
	rop(cSPRT);
	rop(cDISPX);
	rop(cTRACE);
	rop(cGET);
	rop(cPUT);
	rop(cSGET);
	rop(cSGETTOK);
	rop(cSPUT);
	rop(cSPUTL);
	rop(cTOKEN);
	rop(cOPEN);
	rop(cCLOSE);
	rop(cFLUSH);
	rop(cCLRERR);
	rop(cSETSTR);
	rop(cCURRSTR);
	rop(cFORK);
	rop(cLOAD);
	rop(cILOAD);
	rop(cFLOAD);
	rop(cSPYPT);
	rop(cSPY);
	rop(cNOSPY);
	rop(cPSTOT);
	rop(cSIMC);
 	rop(cDSIMC);
	rop(cSQL);

	break;
	case 255:
		/*
		 * Ensure that jump table is full so that we can dispense
		 * with bounds checking if we feel so inclined.
		 */
	default:
		fprintf(stdout, "Unknown OP %d at %x\n", OpCode, (Word)(P));
		panic("");
	}
	continue;
	}

#ifdef SHALLOW

#undef wop
#define wop(op) when op: CONCAT(LS,op)
#undef wopop
#define wopop(op1, op2) when op1: case op2: CONCAT(LS,op1): CONCAT(LS,op2)
#undef cop
#define cop(op) case op: CONCAT(LS,op)

#define CondExitShallowMode(a) \
	{ if((a) < ChoiceSize(B)) goto exitShallowReadMode; }

/* Shallow Read Mode */
ShallowReadMode:
	for(;;) {
		register Object v;

#ifdef DEBUG
if(regflg || traceflg) {
	register int i;

if(regflg)
	fprintf(stdout,
	"%5x %5x %5x %5x %5x ",
	(Word*)E, (Word*)B, (Word*)A, TR, (Word*)H);
if(traceflg) {
	fprintf(stdout, "R");
	(void) displayInstruction(stdout, 0, P);
}
for(i = 0; i < spyflg; i++) {
	Word *address, old;

	address = SpyPoints[i].sp_address;
	old = SpyPoints[i].sp_value;
	if(*address != old) {
		fprintf(stdout, "*0x%x changed from 0x%x(%d) to 0x%x(%d)\n",
			address,
			old, old,
			*address, *address);
		SpyPoints[i].sp_value = *address;
	}
}
}
#endif /* DEBUG */
#ifdef MEASURE
ShallowReadModeOps[OpCode]++;
#endif /* MEASURE */
		switch(OpCode) {
		wop(cGVARX):
			X[Arg1] = X[Arg2];
			P += niGVARX;
		wop(cGVARA):
			goto exitShallowReadMode;
		wop(cGVARY):
			Y[Arg1] = X[Arg2];
			P += niGVARY;
		wop(cGVALX): {
			SaveForUnify;
			if(unify(X[Arg2], X[Arg1])) {
				RestoreAfterUnify;
				P += niGVALX;
				/* BUG?  heapOverflow(0); */
			} else {
				RestoreTR;
				goto shallowFail;
			}
		}
		wop(cGVALY):
			SaveForUnify;
			if(!unify(X[Arg2], Y[Arg1])) {
				RestoreTR;
				goto shallowFail;
			}
			RestoreAfterUnify;
			P += niGVALY;
			/* BUG?  heapOverflow(0); */
		wop(cGC): {
			/* register Object v; */

			v = X[Arg1];
			P += niGC;
			DeRef3(v,
				{
					if(IsRef(v)) {
						bind(eRefStar(v), ((Object *)P)[-1], TR);
						binding(v);
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, ((Object *)P)[-1], H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						binding(v);
					}
				},
				if(IsIConst(v)) {
					if(((Object *)P)[-1] != v)
						goto shallowFail;
				} else if(IsUConst(v)) {					/* BUG! */
					if(IsFloat(v)) {
						if(		!IsFloat(((Object *)P)[-1])
							||	eFloat(v) != eFloat(((Object *)P)[-1]))
							goto shallowFail;
					} else if(IsInt32(v)) {
						if(		!IsInt32(((Object *)P)[-1])
							||	eInt32(v) != eInt32(((Object *)P)[-1]))
							goto shallowFail;
					} else
						panic("Impossible UCN type in GC");
				} else
					goto shallowFail;
			);
		}
		wop(cGNIL): {
			/* register Object v; */

			v = X[Arg1];
			P += niGNIL;
			DeRef3(v,
				{
					if(IsRef(v)) {
						bind(eRefStar(v), NIL, TR);
						binding(v);
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, NIL, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						binding(v);
					}
				},
				{ if(NIL != v)
					goto shallowFail; }
			);
		}
		wop(cGS): {
			/* register Object v; */

			v = X[Arg1];
		ShallowGetStruct:
			DeRef3(v,
				{
					heapOverflow(1 + MAXARITY + wordsof(DelayHeader));
					if(IsRef(v)) {
						*eRefStar(v) = StarToStr(H);
						trail(eRefStar(v), TR);
						*H++ = ((Object *)P)[1];
						P += niGS;
						goto ShallowWriteMode;
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, StarToStr(H),
							H, A, TR, W, SaveForShift, RestoreAfterShift);
						*H++ = ((Object *)P)[1];
						P += niGS;
						goto ShallowWriteMode;
					}
				},
				{
					if(IsStr(v)) {
						S = (Object *) eRef(v);
						if(*S++ != ((Object *)P)[1])
							goto shallowFail;
						P += niGS;
					} else
						goto shallowFail;
				}
			);
		}
		wop(cGSX1): {
			/* register Object v; */

			CondExitShallowMode(Arg2);
			v = X[Arg1];
			DeRef3(v,
				{
					heapOverflow(2 + wordsof(DelayHeader));
					*H++ = ((Object *)P)[1];
					X[Arg2] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					P += niGSX1;
					if(IsRef(v)) {
						bind(eRefStar(v), StarToStr(H-2), TR);
						goto ShallowWriteMode;
					} else /* if(IsDel(v)) */ {
						register Object p;

						p = StarToStr(H - 2);
						v = (Object) eRef(v);
						bindDel(v, p, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						goto ShallowWriteMode;
					}
				},
				{
					if(IsStr(v)) {
						S = (Object *) eRef(v);
						if(S[0] != ((Object *)P)[1])
							goto shallowFail;
						v = S[1];
						if(IsVar(v))
							v = MakeIndirect(v);
						X[Arg2] = v;
						P += niGSX1;
					} else
						goto shallowFail;
				}
			);
		}
		wop(cGSX2): {
			/* register Object v; */

			CondExitShallowMode(Arg2);
			CondExitShallowMode(Arg3);
			v = X[Arg1];
			DeRef3(v,
				{
					heapOverflow(3 + wordsof(DelayHeader));
					*H++ = ((Object *)P)[1];
					X[Arg2] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					X[Arg3] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					P += niGSX2;
					if(IsRef(v)) {
						bind(eRefStar(v), StarToStr(H-3), TR);
						goto ShallowWriteMode;
					} else /* if(IsDel(v)) */{
						register Object p;

						p = StarToStr(H - 3);
						v = (Object) eRef(v);
						bindDel(v, p, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
						goto ShallowWriteMode;
					}
				},
				{
					if(IsStr(v)) {
						S = (Object *) eRef(v);
						if(S[0] != ((Object *)P)[1])
							goto shallowFail;
						X[Arg2] = S[1];
						X[Arg3] = S[2];
						P += niGSX2;
					} else
						goto shallowFail;
				}
			);
		}
		wop(cGL): {
			/* register Object v; */

			v = X[Arg1];
		ShallowGetList:
			DeRef3(v,
				{
					heapOverflow(2 + wordsof(DelayHeader));
					P += niGL;
					if(IsRef(v)) {
						v = (Object) eRef(v);
						*(Object *)v = StarToList(H);
						trail(v, TR);
						goto ShallowWriteMode;
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, StarToList(H),
							H, A, TR, W, SaveForShift, RestoreAfterShift);
						goto ShallowWriteMode;
					}
				},
				{
					if(IsList(v)) {
						S = (Object *) eRef(v);
						P += niGL;
					} else if(IsString(v)) {
						/* Not worth while having a ShallowStringReadMode. */
						goto exitShallowReadMode;
					} else
						goto shallowFail;
				}
			);
		}
		wop(cGLVX2): {
			/* register Object v; */

			CondExitShallowMode(Arg2);
			CondExitShallowMode(Arg3);
			v = X[Arg1];
			DeRef3(v,
				{
					heapOverflow(2 + wordsof(DelayHeader));
					X[Arg2] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					X[Arg3] = StarToRef(H);
					*H = StarToUnboundRef(H);
					H++;
					P += niGLVX2;
					if(IsRef(v)) {
						bind(eRefStar(v), StarToList(H-2), TR);
					} else /* if(IsDel(v)) */ {
						register Object p;
	
						p = StarToList(H - 2);
						v = (Object) eRef(v);
						bindDel(v, p, H, A, TR, W,
								SaveForShift, RestoreAfterShift);
					}
				},
				{
					if(IsList(v)) {
						S = (Object *) eRef(v);
						X[Arg2] = S[0];
						X[Arg3] = S[1];
						P += niGLVX2;
					} else if(IsString(v)) {
						S = (Object *) eRef(v);
						X[Arg2] = MakeSmallInt(*(char *)S);
						S = (Object *) (sizeof(char) + (Word) S);
						if(*(char *)S == '\0')
							X[Arg3] = NIL;
						else
							X[Arg3] = StarToString(S);
						P += niGLVX2;
					} else
						goto shallowFail;
				}
			);
		}
		wop(cPVARX):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPVARY):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPVALY):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPVALY2):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPUVAL):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPC):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPNIL):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPS):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cPL):		/* Fill out the switch table a bit. */
			goto exitShallowReadMode;
		wop(cUVOID):
			S += Arg1;
			P += niUVOID;
		wop(cUVOID1):
			S += 1;
			P += niUVOID1;
		wop(cUVARX):
			X[Arg1] = *S++;
			P += niUVARX;
		wop(cUVARA):
			goto exitShallowReadMode;
		wop(cUVARY):
			Y[Arg1] = *S++;
			P += niUVARY;
		wop(cUVARAVARA):
			goto exitShallowReadMode;
		wop(cUVARXVARX):
			X[Arg1] = *S++;
			X[Arg2] = *S++;
			P += niUVARXVARX;
		wop(cUVARYVARY):
			Y[Arg1] = *S++;
			Y[Arg2] = *S++;
			P += niUVARYVARY;
		wop(cUVARALVX):
			goto exitShallowReadMode;
		wop(cUVARXLVX): {
			X[Arg1] = *S++;
			SaveForUnify;
			if(unify(*S++, X[Arg2])) {
				RestoreAfterUnify;
				P += niUVARXLVX;
			} else {
				RestoreTR;
				goto shallowFail;
			}
		}
		wop(cULVXVARX): {
			SaveForUnify;
			if(unify(*S++, X[Arg1])) {
				RestoreAfterUnify;
			} else {
				RestoreTR;
				goto shallowFail;
			}
			X[Arg2] = *S++;
			P += niULVXVARX;
		}
		cop(cULVXVARA): {
			SaveForUnify;
			if(unify(*S, X[Arg1])) {
				RestoreAfterUnify;
			} else {
				RestoreTR;
				goto shallowFail;
			}
			goto exitShallowReadMode;
		}
		wop(cULVXLVX): {
			SaveForUnify;
			if(unify(*S++, X[Arg1]) && unify(*S++, X[Arg2])) {
				RestoreAfterUnify;
				P += niULVXLVX;
			} else {
				RestoreTR;
				goto shallowFail;
			}
		}
		wopop(cUVALX,cULVX): {
			SaveForUnify;
			if(unify(*S++, X[Arg1])) {
				RestoreAfterUnify;
				P += niUVALX;
			} else {
				RestoreTR;
				goto shallowFail;
			}
		}
		wopop(cUVALY,cULVY):
			SaveForUnify;
			if(unify(*S++, Y[Arg1])) {
				RestoreAfterUnify;
				P += niUVALY;
			} else {
				RestoreTR;
				goto shallowFail;
			}
		wop(cUC): {
			/* register Object v; */

			v = *S++;
			P += niUC;
			DeRef3(v,
				{
					if(IsRef(v)) {
						bindH(eRefStar(v), ((Object *)P)[-1], TR);
					} else /* if(IsDel(v)) */ {
						v = (Object) eRef(v);
						bindDel(v, ((Object *)P)[-1], H, A, TR, W,
								SaveForShift, RestoreAfterShift);
					}
					binding(v);
				},
				{
					if(IsIConst(v)) {
						if(v != ((Object *)P)[-1])
							goto shallowFail;
					} else if(IsUConst(v)) {					/* BUG! */
						if(IsFloat(v)) {
							if(		!IsFloat(((Object *)P)[-1])
								||	eFloat(v) != eFloat(((Object *)P)[-1]))
								goto shallowFail;
						} else if(IsInt32(v)) {
							if(		!IsInt32(((Object *)P)[-1])
								||	eInt32(v) != eInt32(((Object *)P)[-1]))
								goto shallowFail;
						} else
							panic("Impossible UCN type in UC");
					} else
						goto shallowFail;
				}
			);
		}
		wop(cUL):
			v = *S;
			goto ShallowGetList;
		wop(cUS):
			v = *S;
			goto ShallowGetStruct;
		wop(cID):
			if(!identical(X[Arg1], X[Arg2]))
				goto shallowFail;
			P += niID;
		wop(cNOTID):
			if(identical(X[Arg1], X[Arg2]))
				goto shallowFail;
			P += niNOTID;
		wop(cOCCURS):
			CountLI;
			if(p_occurs(X[Arg1], X[Arg2]))
				P += niOCCURS;
			else
				goto shallowFail;
		wop(cJ):
		cop(cJTRUE):	/* Fall Through */
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
		wop(cJFAIL):
			P += niJFAIL;
		wop(cTYPX): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(((1 << eType(v)) & Arg2) == 0)
				goto shallowFail;
			P += niTYPX;
		}
		wop(cTYPY): {
			/* register Object v; */

			CountLI;
			v = Y[Arg1]; DeRef(v);
			if(((1 << eType(v)) & Arg2) == 0)
				goto shallowFail;
			P += niTYPY;
		}
		wop(cCTYPX): {
			/* register Object v; */

			CountLI;
			v = X[Arg1]; DeRef(v);
			if(!IsConst(v) || ((1 << eUType(v)) & Arg2) == 0)
				goto shallowFail;
			P += niCTYPX;
		}
		wop(cCTYPY): {
			/* register Object v; */

			CountLI;
			v = Y[Arg1]; DeRef(v);
			if(!IsConst(v) || ((1 << eUType(v)) & Arg2) == 0)
				goto shallowFail;
			P += niCTYPY;
		}
		wop(cFAIL):
			goto shallowFail;
		wop(cLABELX):
			CondExitShallowMode(Arg1);
			/*X[Arg1] = StarToBlock(B);*/
			X[Arg1] = MakeSmallInt((Word *)B - stackBase);
			P += niLABELX;
		wop(cLABELY):
			/*Y[Arg1] = StarToBlock(B);*/
			Y[Arg1] = MakeSmallInt((Word *)B - stackBase);
			P += niLABELY;
		wop(cCUTX): {
			/* register Object v; */

			CountLI;
			if(W != 0)
				goto exitShallowReadMode; /* Can't wake goals in shallow mode */
			v = X[Arg1]; DeRef(v);
			/*
			if(!IsBlock(v))
				panic("Argument to CUTX not a block");
			P += niCUTX;
		   	v = (Object)eRef(v);
			*/
			if(!IsSmallInt(v))
				panic("Argument to CUTX not an index");
			P += niCUTX;
		   	v = (Object)(stackBase + eSmallInt(v));
		   	if(B > (Choice *)v) {
				/* No choice point other than the top one can be shallow. */
				if((Word *)E > (Word *)B) {
					/* BUG!
					 * CP is technically wrong now, but she'll be right!
					 */
					(E-1)->e_cp = CP;
				}
		   		B = (Choice *)v;
				HB = (B-1)->c_h;
		   		TR = trimTrail(TR, B, HB);
				if((Word *)B > (Word *)E)
					A = (Word *) B + ChoiceSize(B);
				goto ReadMode;
			}
		/*
		 * Except for this possibility, we could just jump
		 * direct to ReadMode CUTX.
		 */
		}
		wop(cCUTY): {
			/* register Object v; */

			CountLI;
			/* BUG.  See CUTX. */
			if(W != 0)
				goto exitShallowReadMode; /* Can't wake goals in shallow mode */
			v = Y[Arg1]; DeRef(v);
			/*
			if(!IsBlock(v))
				panic("Argument to CUTY not a block");
			P += niCUTY;
		   	v = (Object)eRef(v);
			*/
			if(!IsSmallInt(v))
				panic("Argument to CUTY not an index");
			P += niCUTY;
		   	v = (Object)(stackBase + eSmallInt(v));
		   	if(B > (Choice *)v) {
				/* No choice point other than the top one can be shallow. */
				if((Word *)E > (Word *)B) {
					/* BUG!
					 * CP is technically wrong now, but she'll be right!
					 */
					(E-1)->e_cp = CP;
				}
				B = (Choice *)v;
				HB = (B-1)->c_h;
		   		TR = trimTrail(TR, B, HB);
				/* BUG!  See CUTX. */
				if((Word *)B > (Word *)E)
					A = (Word *) B + ChoiceSize(B);
				goto ReadMode;
			} /* See CUTX. */
		}
		wop(cALL):
			/* Stack overflow check subsumed by shallow c.p. creation. */
			((Environment *) A)->e_ce = E;
			E = (Environment *) A;
			/* E->e_cp = CP;	BUG */
			E++;
			A = (Word *)E + Arg1;
			/* CP = P + 2;				Make EnvSize macro valid */
			P += niALL;
		wop(cTE):
			goto exitShallowReadMode;
		wop(cRE):
			RetryShallowMode;
			(B-1)->c_retry = ((Program *)P)[1];
			A = (Word *) B + ChoiceSize(B);
			P += niRE;
		wop(cTRE):
			/* BUG!  Inefficiency.
			 * The placing of the top of stack is conservative.
			 * SOFTCUT may have removed the choice point below
			 * the current one.
			 */
			TrustShallowMode;
			A = (Word *) --B;
			B = B->c_b;
			HB = (B-1)->c_h;
			P += niTRE;
			goto ReadMode;
		wop(cT):
			goto exitShallowReadMode;
		wop(cR):
			RetryShallowMode;
			(B-1)->c_retry = P + niR;
			A = (Word *) B + ChoiceSize(B);
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
		wop(cTR):
			/* BUG!  Inefficiency.
			 * The placing of the top of stack is conservative.
			 * SOFTCUT may have removed the choice point below
			 * the current one.
			 */
			TrustShallowMode;
			A = (Word *) --B;
			B = B->c_b;
			HB = (B-1)->c_h;
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
			goto ReadMode;

		break;			/* Fill out the switch table. */
		case cARITH:
		case cAPUSHX:
		case cAPUSHY:
		case cAPUSHI:
		case cAPUSHF:
		case cPUSHX:
		case cPUSHY:
		case cPUSHVX:
		case cPUSHVY:
		case cPUSHI:
		case cPUSHF:
		case cPOPX:
		case cPOPY:
		case cPOPVX:
		case cPOPVY:
		case cPOPC:
		case cAFUNC:
		case cAPRED:
		case cJPRED:
		case cDALL:
		case cWEVAL:
		case cNAME:
		case cLTOS:
		case cUNIV:
		case cFUNCTOR:
		case cARG:
		case cISEQ:
		case cWISEQ:
		case cCVTTOH:
		case cPLUS:
		case cDEFINED:
		case cARITIES:
		case cFLAGS:
		case cEXECS:
		case cSYMBOL:
		case cCOPY:
		case cUNCOPY:
		case cMAKEBMT:
		case cLINKBMT:
		case cINST:
		case cERASE:
		case cPROPS:
		case cABOLISH:
		case cREPLACN:
		case cREPLACV:
		case cSETARG:
		case cSETARGV:
		case cAREF:
		case cASET:
		case cIDTEST:
		case cCOMPARE:
		case cSORT:
		case cPRO:
		case cDALLPRO:
		case cEXEC:
		case cDALLEXE:
		case cEXECSOT:
		case cCALL:
		case cFRUN:
		case cAPPLY:
		case cFUNCALL:
		case cCATCH:
		case cTHROW:
		case cJVX:
		case cJNVX:
		case cJC:
		case cJS:
		case cSOT:
		case cSOC:
		case cSOCE:
		case cSOS:
		case cSOSE:
		case cFVAR:
		case cSPYMDEL:
		case cMKDEL:
		case cMARK:
		case cDELAY:
		case cWAKE:
		case cNECK:
		case cWOKEN:
		case cJTYPX:
		case cJTYPY:
		case cJCTYPX:
		case cJCTYPY:
		case cEREF:
		case cMKOBJ:
		case cSOFTCUT:
		case cONCUT:
		case cERROR:
		case cRET:
		case cABORT:
		case cEXIT:
		case cPRINTF:
		case cPRTNUM:
		case cSPRT:
		case cDISPX:
		case cTRACE:
		case cGET:
		case cPUT:
		case cSGET:
		case cSGETTOK:
		case cSPUT:
		case cSPUTL:
		case cTOKEN:
		case cOPEN:
		case cCLOSE:
		case cFLUSH:
		case cCLRERR:
		case cSETSTR:
		case cCURRSTR:
		case cFORK:
		case cLOAD:
		case cILOAD:
		case cFLOAD:
		case cSPYPT:
		case cSPY:
		case cNOSPY:
		case cPSTOT:
/*
Probably enough dummies, and any more give SunOS CC's parser indigestion!
		case cSIMC:
		case cDSIMC:
		case cSQL:
*/
		case 255:
			/*
			 * Ensure that jump table is full so that we can dispense
			 * with bounds checking if we feel so inclined.
			 */
		default:
			goto exitShallowReadMode;
		}
		continue;

		exitShallowReadMode: {
#ifdef MEASURE
			ShallowReadModeOps[OpCode]--;
#endif /* MEASURE */
			ExitShallowMode;
			if((Word *) E > (Word *) B) {
				(B-1)->c_e = (E-1)->e_ce;
				(B-1)->c_cp
					= (E-1)->e_cp
					= CP;
				/* BUG!  CP is technically wrong now, but she'll be right! */
			} else {
				(B-1)->c_e = E;
				(B-1)->c_cp = CP;
			}
			(B-1)->c_h = HB;
			wcopy((Object *)B, X, ChoiceSize(B));
			goto ReadMode;
		}

		shallowFail: {
#ifdef DEBUG
if(traceflg) {
	fprintf(stdout, "Failing\n");
}
#endif /* DEBUG */
#ifdef INLINE
			TR = failure(TR, (B-1)->c_tr);
#else /* INLINE */
			{   register TrailRecord *tb;

				tb = (B-1)->c_tr;
				while(TR > tb) {
					TR--;
#ifdef DEBUG
if(nprocs > 1 || traceflg) {
	fprintf(stdout,
		"RESTORING _%x TO 0x%x\n", (Word)TR->tr_address, TR->tr_value);
}
#endif /* DEBUG */
					*TR->tr_address = TR->tr_value;
				}
			}
#endif /* INLINE */
			if((Word *) E > (Word *) B) {
				/* CP = (E-1)->e_cp; */
				E = (E-1)->e_ce;
			}
			H = HB;
			W = 0;
		}
	}

/* Shallow Write Mode */

#undef rop
#define rop(op) when op: goto CONCAT(LS,op)

#undef CondExitShallowMode
#define CondExitShallowMode(a) \
	{ if((a) < ChoiceSize(B)) goto exitShallowWriteMode; }

ShallowWriteMode:
		for(;;) {
			register Object v;

#ifdef DEBUG
if(regflg || traceflg) {
	register int i;

if(regflg)
	fprintf(stdout,
	"%5x %5x %5x %5x %5x ",
	(Word*)E, (Word*)B, (Word*)A, TR, (Word*)H);
if(traceflg) {
	fprintf(stdout, "W");
	(void) displayInstruction(stdout, 0, P);
}
for(i = 0; i < spyflg; i++) {
	Word *address, old;

	address = SpyPoints[i].sp_address;
	old = SpyPoints[i].sp_value;
	if(*address != old) {
		fprintf(stdout, "*0x%x changed from 0x%x(%d) to 0x%x(%d)\n",
			address,
			old, old,
			*address, *address);
		SpyPoints[i].sp_value = *address;
	}
}
}
#endif /* DEBUG */
#ifdef MEASURE
ShallowWriteModeOps[OpCode]++;
#endif /* MEASURE */
		switch(OpCode) {
		rop(cGVARX);
		rop(cGVARA);
		rop(cGVARY);
		rop(cGVALX);
		rop(cGVALY);
		rop(cGC);
		rop(cGNIL);
		rop(cGS);
		rop(cGSX1);
		rop(cGSX2);
		rop(cGL);
		rop(cGLVX2);
		rop(cPVARX);
		rop(cPVARY);
		rop(cPVALY);
		rop(cPVALY2);
		rop(cPUVAL);
		rop(cPC);
		rop(cPNIL);
		rop(cPS);
		rop(cPL);
		when cUVOID: {
			/* register Object v; */

			v = Arg1;
			P += niUVOID;
			/* Assume that Arg1 > 0 */
			do {
				*H = StarToUnboundRef(H);
				H++;
			} while(--v > 0);
		}
		when cUVOID1:
			*H = StarToUnboundRef(H);
			H++;
			P += niUVOID1;
		when cUVARX:
			X[Arg1] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			P += niUVARX;
		when cUVARA:
			goto exitShallowWriteMode;
		when cUVARY:
			Y[Arg1] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			P += niUVARY;
		when cUVARXVARX:
			X[Arg1] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			X[Arg2] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			P += niUVARXVARX;
		when cUVARAVARA:
			goto exitShallowWriteMode;
		when cUVARYVARY:
			Y[Arg1] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			Y[Arg2] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			P += niUVARYVARY;
		when cUVARXLVX: {
			/* register Object v; */

			X[Arg1] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			v = X[Arg2]; using(v);
			unify_value_write(v, 0, X[Arg2], retryTarget(cUVARXLVXws));
			P += niUVARXLVX;
		}
		when cUVARALVX:
			goto exitShallowWriteMode;
		when cULVXVARX: {
			/* register Object v; */

			v = X[Arg1]; using(v);
			unify_value_write(v, 0, X[Arg1], retryTarget(cULVXVARXws));
			X[Arg2] = StarToRef(H);
			*H = StarToUnboundRef(H);
			H++;
			P += niULVXVARX;
		}
		when cULVXVARA:
			goto exitShallowWriteMode;
		when cULVXLVX: {
			/* register Object v; */

			v = X[Arg1]; using(v);
			unify_value_write(v, 0, X[Arg1], retryTarget(cULVXLVXws1));
			v = X[Arg2]; using(v);
			unify_value_write(v, 0, X[Arg2], retryTarget(cULVXLVXws2));
			P += niULVXLVX;
		}
		when cUVALX:
#ifdef NO_LOCAL_VARS
		case cULVX:
#endif /* NO_LOCAL_VARS */
		{
			/* register Object v; */

			v = X[Arg1]; using(v);
			DeRefPure(v);
			*H++ = v;
			P += niUVALX;
		}
		when cUVALY:
#ifdef NO_LOCAL_VARS
		case cULVY:
#endif /* NO_LOCAL_VARS */
		{
			/* register Object v; */

			v = Y[Arg1]; using(v);
			DeRefPure(v);
			*H++ = v;
			P += niUVALY;
		}
#ifdef NO_LOCAL_VARS
#else /* NO_LOCAL_VARS */
		when cULVX: {
			/* register Object v; */

			v = X[Arg1]; using(v);
			DeRefPure(v);
			if(!IsRef(v) || (Word *)eRefStar(v) < H)    /* Grunge */
				*H++ = v;
			else {
				*H = StarToUnboundRef(H);
				bindS(eRefStar(v), StarToRef(H), TR);
				H++;
				binding(v);
			}
			P += niULVX;
		}
		when cULVY: {
			/* register Object v; */

			v = Y[Arg1]; using(v);
			DeRefPure(v);
			if(!IsRef(v) || (Word *)eRefStar(v) < H)    /* Grunge */
				*H++ = v;
			else {
				*H = StarToUnboundRef(H);
				bindS(eRefStar(v), StarToRef(H), TR);
				H++;
				binding(v);
			}
			P += niULVY;
		}
#endif /* NO_LOCAL_VARS */
		when cUC:
			*H++ = ((Object *)P)[1];
			P += niUC;
		when cUL:
			*H = StarToList(H + 1);
			H++;
			P += niUL;
			heapOverflow(2);
		when cUS:
			*H = StarToStr(H + 1);
			H++;
			*H++ = ((Object *)P)[1];
			P += niUS;
			heapOverflow(1 + MAXARITY);
		rop(cID);
		rop(cNOTID);
		rop(cOCCURS);
		rop(cJ);
		when cJTRUE:
			P += niJTRUE;
		when cJFAIL:
			RecordPC(P);
			P = ((Program *)P)[1];
			RecordPC(P);
		rop(cTYPX);
		rop(cTYPY);
		rop(cCTYPX);
		rop(cCTYPY);
		rop(cFAIL);
		rop(cLABELX);
		rop(cLABELY);
		rop(cCUTX);
		rop(cCUTY);
/* ALL is duplicated here so that it may appear amongst UNIFY instructions
 * without changing mode.  Note that DALL cannot be in this situation.
 */
		when cALL:
			/* Stack overflow check subsumed by shallow c.p. creation. */
			((Environment *) A)->e_ce = E;
			E = (Environment *) A;
			/* E->e_cp = CP;	BUG */
			E++;
			A = (Word *)E + Arg1;
			/* CP = P + 2;				Make EnvSize macro valid */
			P += niALL;
		rop(cTE);
		rop(cRE);
		rop(cTRE);
		rop(cT);
		rop(cR);
		rop(cTR);

		break;
		case cARITH:
		case cAPUSHX:
		case cAPUSHY:
		case cAPUSHI:
		case cAPUSHF:
		case cPUSHX:
		case cPUSHY:
		case cPUSHVX:
		case cPUSHVY:
		case cPUSHI:
		case cPUSHF:
		case cPOPX:
		case cPOPY:
		case cPOPVX:
		case cPOPVY:
		case cPOPC:
		case cAFUNC:
		case cAPRED:
		case cJPRED:
		case cDALL:
		case cWEVAL:
		case cNAME:
		case cLTOS:
		case cUNIV:
		case cFUNCTOR:
		case cARG:
		case cISEQ:
		case cWISEQ:
		case cCVTTOH:
		case cPLUS:
		case cDEFINED:
		case cARITIES:
		case cFLAGS:
		case cEXECS:
		case cSYMBOL:
		case cCOPY:
		case cUNCOPY:
		case cMAKEBMT:
		case cLINKBMT:
		case cINST:
		case cERASE:
		case cPROPS:
		case cABOLISH:
		case cREPLACN:
		case cREPLACV:
		case cSETARG:
		case cSETARGV:
		case cAREF:
		case cASET:
		case cIDTEST:
		case cCOMPARE:
		case cSORT:
		case cPRO:
		case cDALLPRO:
		case cEXEC:
		case cDALLEXE:
		case cEXECSOT:
		case cCALL:
		case cFRUN:
		case cAPPLY:
		case cFUNCALL:
		case cCATCH:
		case cTHROW:
		case cJVX:
		case cJNVX:
		case cJC:
		case cJS:
		case cSOT:
		case cSOC:
		case cSOCE:
		case cSOS:
		case cSOSE:
		case cFVAR:
		case cSPYMDEL:
		case cMKDEL:
		case cMARK:
		case cDELAY:
		case cWAKE:
		case cNECK:
		case cWOKEN:
		case cJTYPX:
		case cJTYPY:
		case cJCTYPX:
		case cJCTYPY:
		case cEREF:
		case cMKOBJ:
		case cSOFTCUT:
		case cONCUT:
		case cERROR:
		case cRET:
		case cABORT:
		case cEXIT:
		case cPRINTF:
		case cPRTNUM:
		case cSPRT:
		case cDISPX:
		case cTRACE:
		case cGET:
		case cPUT:
		case cSGET:
		case cSGETTOK:
		case cSPUT:
		case cSPUTL:
		case cTOKEN:
		case cOPEN:
		case cCLOSE:
		case cFLUSH:
		case cCLRERR:
		case cSETSTR:
		case cCURRSTR:
		case cFORK:
		case cLOAD:
		case cILOAD:
		case cFLOAD:
		case cSPYPT:
		case cSPY:
		case cNOSPY:
		case cPSTOT:
/*
Probably enough dummies, and any more give SunOS CC's parser indigestion!
		case cSIMC:
		case cDSIMC:
		case cSQL:
*/
		case 255:
		/*
		 * Ensure that jump table is full so that we can dispense
		 * with bounds checking if we feel so inclined.
		 */
		default:
			goto exitShallowWriteMode;
		}
		continue;

		exitShallowWriteMode: {
#ifdef MEASURE
			ShallowWriteModeOps[OpCode]--;
#endif /* MEASURE */
			ExitShallowMode;
			if((Word *) E > (Word *) B) {
				(B-1)->c_e = (E-1)->e_ce;
				(B-1)->c_cp
					= (E-1)->e_cp
					= CP;
				/* BUG!  CP is technically wrong now, but she'll be right! */
			} else {
				(B-1)->c_e = E;
				(B-1)->c_cp = CP;
			}
			(B-1)->c_h = HB;
			wcopy((Object *)B, X, ChoiceSize(B));
			goto WriteMode;
		}
		}

#endif /* SHALLOW */
}
