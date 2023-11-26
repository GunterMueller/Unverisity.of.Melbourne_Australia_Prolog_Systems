/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#define max(x, y) (x > y ? x : y)
#define min(x, y) (x < y ? x : y)

#define IOBUFSIZE max(BUFSIZ, 8 * 1024)

/*
 * Cover up one of the inaddequacies of System V.
 *
 * Could be done better.
 */
#ifdef BSD4
#define INTRCHK(x) x
#else /* BSD4 */
#define INTRCHK(x) {do {errno = 0; x; } while(errno == EINTR);}
#endif /* BSD4 */

#define PutC(c, st) INTRCHK(putc((c), (st)->s_fp))

#ifdef PROTOVERFLOW
#	define overflowCheck(name, top, inc, bound, max)
#	define overflowSignal(name, top, inc, bound, max)
#else
#	ifdef HASH_CONCAT
#		define overflowCheck(name, top, inc, bound, max) { \
			if((Word *) ((top) + (inc)) >= (Word *) (bound)) { \
				if((Word *) ((top) + (inc)) >= (Word *) (max)) \
					panic("" # name " Overflow"); \
				else { \
					ActionPending = 1; \
					SignalPending = 1; \
				} \
			} \
		}
#	else
#		define overflowCheck(name, top, inc, bound, max) { \
			if((Word *) ((top) + (inc)) >= (Word *) (bound)) { \
				if((Word *) ((top) + (inc)) >= (Word *) (max)) \
					panic("name Overflow"); \
				else { \
					ActionPending = 1; \
					SignalPending = 1; \
				} \
			} \
		}
#	endif

#	define overflowSignal(name, top, inc, bound) { \
		if((Word *) ((top) + (inc)) >= (Word *) (bound)) { \
			ActionPending = 1; \
			SignalPending = 1; \
		} \
	}
#endif

#define heapOverflowCheck(top, inc) \
	overflowCheck(Heap, top, inc, heapTop, heapMax)
#define heapOverflowSignal(inc, pH) \
	overflowSignal(Heap, pH, inc, heapTop)

#define stackOverflowCheck(top, inc) \
	overflowCheck(Stack, top, inc, stackTop, stackMax)
#define stackOverflowSignal(inc, pS) \
	overflowSignal(Stack, pS, inc, stackTop)

#define trailOverflowCheck(top, inc) \
	overflowCheck(Trail, top, inc, trailTop, trailMax)
#define trailOverflowSignal(inc, pT) \
	overflowSignal(Trail, pT, inc, trailTop)

/* Routines that guarantee timely signal processing redefine these. */
#define TRAILOVERFLOWSIGNAL 0
#define HEAPOVERFLOWSIGNAL 0

#ifdef INLINE
/* BUG!  trailEntry1() relies on StarToUnboundRef(v) == v. */
extern TrailRecord *trailEntry1(), *trailEntry2();
#else /* INLINE */
#define trailEntry1(tr, v) ( \
		(tr)->tr_address = (Word *)StarToUnboundRef(v), \
		(tr)->tr_value = (Word)(v), \
		(tr) + 1 )
#define trailEntry2(tr, v, xx) ( \
		(tr)->tr_address = (Word *)(v), \
		(tr)->tr_value = (Word)(xx), \
		(tr) + 1 )
#endif /* INLINE */

/* Base macro for trailing. */
#define trailMacroMacro(comp, enter, tr) \
    if(comp) { \
		if(TRAILOVERFLOWSIGNAL) { \
			trailOverflowSignal(0, tr); \
		} else { \
			trailOverflowCheck(tr, 1); \
		} \
		(tr) = enter; }

/* Base macro for trailing tREFs. */
#define trailMacro(comp, v, tr) \
	trailMacroMacro(comp, trailEntry1(tr, v), tr)

/* Base macro for trailing things other than tREFs. */
#define trail2Macro(comp, xx, v, tr) \
	trailMacroMacro(comp, trailEntry2(tr, v, xx), tr)

#ifdef NO_LOCAL_VARS
#define trail(v, tr) trailH(v, tr)
#else /* NO_LOCAL_VARS */
#define trail(v, tr) \
	trailMacro( \
		((Word*)(v) < (Word*)B && (Word*)(v) >= (Word*)H) \
			|| (Word*)(v) < (Word*)HB, \
		v, tr)
#endif /* NO_LOCAL_VARS */

/* v is known to be on the heap */
#define trailH(v, tr) \
	trailMacro((Word*)(v) < (Word*)HB, v, tr)

/* v is known to be on the stack */
#define trailS(v, tr) \
	trailMacro((Word*)(v) < (Word*)B, v, tr)

/* Always trail */
#define trailAlways(v, tr) trailMacro(1, v, tr)

#ifdef NO_LOCAL_VARS
#define trail2(xx, v, tr) trailH2(xx, v, tr)
#else /* NO_LOCAL_VARS */
#define trail2(xx, v, tr) \
	trail2Macro( \
		((Word*)(v) < (Word*)B && (Word*)(v) >= (Word*)H) \
			|| (Word*)(v) < (Word*)HB, \
		xx, v, tr)
#endif /* NO_LOCAL_VARS */

/* v is known to be on the heap */
#define trailH2(xx, v, tr) \
	trail2Macro((Word*)(v) < (Word*)HB, xx, v, tr)

/* v is known to be on the stack */
#define trailS2(xx, v, tr) \
	trail2Macro((Word*)(v) < (Word*)B, xx, v, tr)

/* Always trail */
#define trailAlways2(xx, v, tr) trail2Macro(1, xx, v, tr)

#ifdef DEBUG
#define using(v) {if(traceflg) \
	{ displayTerm(stdout, (v)); printf("\n"); }}
#define binding(v) {if(traceflg)\
	{ printf("_%x <- ", eRefStar(v));\
	displayTerm(stdout, *(Object*)eRefStar(v)); printf("\n"); }}
#else /* DEBUG */
#define using(v)
#define binding(v)
#endif /* DEBUG */

/* Note that tDELs are always on the heap. */
#define bindDel(p, val, H, A, TR, W, save, restore) { \
	if((W) == 0) { \
		ActionPending = 1; \
		(W) = ((DelayHeader *)(p))->h_marks; \
	} else { \
		if(HEAPOVERFLOWSIGNAL) { \
			heapOverflowSignal(0, H); \
		} else { \
			heapOverflowCheck(H, wordsof(DelayHeader)); \
		} \
		*(H) = ((DelayHeader *)(p))->h_marks; \
		*((H)+1) = (Word) (W); \
		(W) = StarToList(H); \
		(H) += wordsof(DelayHeader); \
	} \
	bindH2((p), (val), (TR)); }

#define bind(p, val, TR) { \
	trail((p), (TR)); \
	*(Object *)(p) = (Object)(val); }

#define bindH(p, val, TR) { \
	trailH((p), (TR)); \
	*(Object *)(p) = (Object)(val); }

#define bindS(p, val, TR) { \
	trailS((p), (TR)); \
	*(Object *)(p) = (Object)(val); }

#define bind2(p, val, TR) { \
	trail2(*(Word *)(p), (p), (TR)); \
	*(Object *)(p) = (Object)(val); }

#define bindH2(p, val, TR) { \
	trailH2(*(Word *)(p), (p), (TR)); \
	*(Object *)(p) = (Object)(val); }

#define bindS2(p, val, TR) { \
	trailS2(*(Word *)(p), (p), (TR)); \
	*(Object *)(p) = (Object)(val); }

#define forcedBind(p, val, TR) { \
	trailAlways2(*(Word *)(p), (p), TR); \
	*(Object *)(p) = (Object)(val); }

#define forcedBind2(p, val, TR) { \
	trailAlways2(*(Word *)(p), (p), TR); \
	*(Object *)(p) = (Object)(val); }

#ifdef INLINE
extern void wcopy();
#define DeRef(v) (v = deRef(v))
#define DeRefPure(v) (v = deRef(v))
#else /* INLINE */
#define wcopy(x, y, n) { \
	register int wcopy__i; \
	for(wcopy__i = (n) - 1; wcopy__i >= 0; wcopy__i--) \
		((Word *)(x))[wcopy__i] = ((Word *)(y))[wcopy__i]; }
#ifdef DEBUG2
#define DeRef(v) (v = deRef(v))
#define DeRefPure(v) (v = deRef(v))
#else /* DEBUG2 */
#define DeRef(v) {while(IsIndirect(v)) v = *(Object *) eRef(v);}
#define DeRefPure(v) DeRef(v)
#endif /* DEBUG2 */
#endif /* INLINE */
#ifdef DEBUG2
#define DeRef3(v, ifvar, ifnvar) \
	{ DeRef(v); if(IsVar(v)) {ifvar} else {ifnvar} }
#else /* DEBUG2 */
#define DeRef3(v, ifvar, ifnvar) { \
	do { \
		if(IsVar(v)) { \
			if(IsVarUnbound(v)) { \
				ifvar; \
				break; \
			} else \
				v = *(Object *) eRef(v); \
		} else { \
			ifnvar; \
			break; \
		} \
	} while(1); }
#endif /* DEBUG2 */
