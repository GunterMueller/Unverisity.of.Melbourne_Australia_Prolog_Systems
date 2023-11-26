/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"
#include <signal.h>
#include <setjmp.h>

/*
 * All these are initialized to zero by cc.
 */
int ActionPending;				/* There are interrupts or woken goals */
Functor *SignalActions[NSIG];
int SignalPending, SignalsPending[NSIG];
int BreakCounter;

/*
 * Count, abort or return on interrupt.
 */
int InterruptAction;
jmp_buf interrupt_return_point;
extern jmp_buf re_entry_point;

/*
dumpMachine(m)
register Machine *m;
{
	fprintf(stderr, "P = %8x", m->mr_p);
	fprintf(stderr, " CP = %8x", m->mr_cp);
	fprintf(stderr, "  E = %8x", m->mr_e);
	fprintf(stderr, "  B = %8x", m->mr_b);
	fprintf(stderr, "  A = %8x\n", m->mr_a);
	fprintf(stderr, "H = %8x", m->mr_h);
	fprintf(stderr, " HB = %8x", m->mr_hb);
	fprintf(stderr, " TR = %8x", m->mr_tr);
	fprintf(stderr, "  X = %8x", m->mr_x);
	fprintf(stderr, "  W = %8x\n", m->mr_w);
	fprintf(stderr, " heapBase = %8x  heapTop = %8x  heapMax = %8x\n",
		heapBase, heapTop, heapMax);
	fprintf(stderr, "stackBase = %8x stackTop = %8x stackMax = %8x\n",
		stackBase, stackTop, stackMax);
	fprintf(stderr, "trailBase = %8x trailTop = %8x trailMax = %8x\n",
		trailBase, trailTop, trailMax);
#ifdef PROTOVERFLOW
	fprintf(stderr, "RedZones = %d\n", RedZoneStatus);
#endif
}
*/

/*
 * Apply a Functor to a vector of Objects.
 * Warning: the vector must be at least 256 Objects long.
 */
void
applyFunctor(f, X)
register Functor *f;
Object X[];
{
	register Choice *B;
	register Machine *cmr = CMR;
	Machine MR;

	stackOverflowCheck(wordsof(Choice), cmr->mr_a);

	MR = *cmr;

	B = (Choice *) MR.mr_a;
	B->c_e = MR.mr_e;
	B->c_b = MR.mr_b;
	B->c_cp = ret;
	B->c_retry = ret;
	B->c_tr = MR.mr_tr;
	B->c_h = MR.mr_h;
	B++;

	MR.mr_p = f->f_code;
	MR.mr_b = B;
	MR.mr_a = (Word *) B;

	MR.mr_x = X;
	MR.mr_cp = ret;
	MR.mr_suspended = cmr;

	CMR = &MR;

	interpret(f);

	CMR = cmr;

	/*
	 * Make sure that everything that uses recursive execs resets
	 * ActionPending because otherwise some delays might be lost.
	 * Could equally be achieved by putting ActionPending in the
	 * Machine, but one would then need to go to as much trouble to
	 * make sure that interrupts were processed despite recursion
	 * entry or exit.
	 */
	if(MR.mr_w != 0)
		ActionPending = 1;
}

/*
 * Call a Functor.
 */
void
callFunctor(f)
register Functor *f;
{
	Object X[256];

	applyFunctor(f, X);
}

static void
invokeHandler(f, sig, n)
register Functor *f;
int sig, n;
{
	Object X[256];

	BreakCounter++;

	X[0] = MakeSmallInt(BreakCounter);
	X[1] = MakeSmallInt(sig);
	X[2] = MakeSmallInt(n);

	applyFunctor(f, X);

	clearerr(stdin);	/* The usual way to exit from a break is <EOF>. */

	BreakCounter--;
}

/*
 * Receive a signal and count it.
 * Unprotect the red-zone buffers at the ends of the stacks if appropriate.
 * Possibly abort or return as well.
 * The last two of these are generally only enabled during input.
 *
 * BUG!  There are race conditions everywhere in this code!
 */

void
receiveSignal(sig)
register int sig;
{
	(void) signal(sig, receiveSignal);

	ActionPending = 1;
	SignalPending++;

#ifdef PROTOVERFLOW

/*
dumpMachine(CMR);
fprintf(stderr, "signal %d received\n", sig);
*/
	if(sig == SIGSEGV) {
		if(clearRedBufferZones() == -1) {
			perror("");
			panic("Can't unprotect expanded memory");
		}
		return;
	}
#endif /* PROTOVERFLOW */

	SignalsPending[sig]++;

	switch(InterruptAction) {
	when intCOUNT:
		return;
	when intABORT:
		longjmp(re_entry_point, 1);
	when intRETURN:
		longjmp(interrupt_return_point, 1);
	}
}

void
processSignals()
{
	register int sig;
	int SaveSig[NSIG];

	checkOverflowsAndShift();

	/*
	 * BUG!  Race to copy received signal vector before more signals arrive.
	 *
	 * Also, is it worth while copying the SignalAction vector too?
	 */
	SignalPending = 0;
	for(sig = 1; sig < NSIG; sig++) {
		SaveSig[sig] = SignalsPending[sig];
		SignalsPending[sig] = 0;
	}

	for(sig = 1; sig < NSIG; sig++)
		if(SaveSig[sig])
			invokeHandler(SignalActions[sig], sig, SaveSig[sig]);
}
