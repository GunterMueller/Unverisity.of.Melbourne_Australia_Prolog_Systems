/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

#include "mu.h"
#ifdef PROTOVERFLOW
#	include <sys/mman.h>
#endif

#ifdef DEBUG
static void
checkTDataArena(void)
{
	register Memory *mp;

	for(mp = tDataArenas; mp != NULL; mp = mp->mem_next) {
		Object *a;
		for(a = mp->mem_base; a + 2 < mp->mem_top; a += a[0]) {
			register Object *b;
			unsigned int ts;

			b = (Object *) a[1];
			if(b == NULL)
				continue;
			ts = NWORDS(sizeOfTerm(*b));
			if(ts != a[0] - 2) {
				printf(
				"Warning: Term size discrepancy.  Size is %d -- should be %d\n",
					ts, a[0] - 2);
				displayTerm(stdout, *b); printf("\n");
			}
		}
	}
}
#endif

static Memory *
allocMemory(arenas, n, grow, compact)
Memory **arenas;
unsigned int n;
int grow, compact;
{
	register Memory *m, *mp;
	int np;

#ifdef DEBUG
	checkTDataArena();
#endif /* DEBUG */

	/*
	 * Is there enough in the first arena?
	 */
	m = *arenas;
	if(m->mem_max - m->mem_top >= n)
		return m;

#ifdef DEBUG
printf("Looking for %d Words in arena 0x%x\n", n, arenas);
#endif /* DEBUG */

	/*
	 * Look for an arena to compact.
	 */
	mp = (Memory *) NULL;
	np = n;
	if(compact)
		for(m = *arenas; m != (Memory *) NULL; m = m->mem_next) {
			register int k;

			k = (m->mem_max - m->mem_top) + m->mem_freed;
#ifdef DEBUG
printf("Arena 0x%x of size %d Words has %.2f%% free and %.2f%% freed\n",
	m, m->mem_size, 100.0 * m->mem_freed / m->mem_size,
	100.0 * k / m->mem_size);
#endif /* DEBUG */
			if(k >= np && k > 0.25 * m->mem_size) {
				mp = m;
				np = k;
			}
		}

	if(mp == (Memory *) NULL) {
		/*
		 * Need to get more space.
		 */
		if(n > grow)
			grow = n;
#ifdef DEBUG
printf("Allocating %d Words more memory in arena list @ 0x%x.\n", grow, arenas);
#endif /* DEBUG */
		mp = (Memory *) malloc(sizeof(Memory) + sizeof(Word) * grow);
		if(mp == (Memory *) NULL)
			panic("Unable to malloc more memory");
		mp->mem_base = (Word *)mp + wordsof(Memory);
		mp->mem_top = mp->mem_base;
		mp->mem_max = mp->mem_base + grow;
		mp->mem_size = grow;
		mp->mem_freed = 0;
		mp->mem_next = *arenas;
		programSize += grow;
		*arenas = mp;
		return mp;
	} else {
		register Memory *newmp;
		register Word *a;
		/*
		 * Compact the arena found and move it to the front of the
		 * arena list.
		 */
		newmp = (Memory *) malloc(sizeof(Memory) + sizeof(Word) * mp->mem_size);
		if(newmp == (Memory *) NULL)
			panic("Unable to malloc more memory");
		newmp->mem_base = (Word *)newmp + wordsof(Memory);
		newmp->mem_top = newmp->mem_base;
		newmp->mem_max = newmp->mem_base + mp->mem_size;
		newmp->mem_size = mp->mem_size;
		newmp->mem_freed = 0;
		newmp->mem_next = *arenas;
		*arenas = newmp;

		/* Unlink the old arena. */
		for(m = *arenas; m != (Memory *) NULL; m = m->mem_next)
			if(m->mem_next == mp)
				break;
		m->mem_next = mp->mem_next;

		/*
		 * Copy allocated objects into the new area,
		 * updating the BMT's that own them.
		 */
		for(a = mp->mem_base; a + 2 < mp->mem_top; a += a[0]) {
			register Object *b;
			int nvars;

			b = (Object *) a[1];
			if(b == NULL)
				continue;
#ifdef DEBUG
{
	unsigned int ts = NWORDS(sizeOfTerm(*b));
if(ts != a[0] - 2) {
	printf(
		"Warning: Term size discrepancy.  Size is %d -- should be %d\n",
		ts, a[0] - 2);
	displayTerm(stdout, *b); printf("\n");
}
}
#endif
			*b = copy(iTDATA, b, *b, &nvars);
			if(nvars != 0)
				panic("Variables found during compaction in allocMemory()");
		}
#ifdef DEBUG
	printf("Copying complete.\n");
#endif

		free((char *) mp);			/* Free the old arena. */
		return newmp;
	}
}

static Memory *
findArena(m, a)
register Memory *m;
register Word *a;
{
	do {
		if(m->mem_base <= a && a < m->mem_max)
			return m;
		m = m->mem_next;
	} while(m != (Memory *) NULL);
	return NULL;
}

char *
storeString(s)
register char *s;
{
	return strcpy((char *) dalloc((unsigned) strlen(s) + 1), s);
}

char *
talloc(n)
register unsigned n;
{
	register Memory *m;
	register char *a;

	n = NWORDS(n);
	m = allocMemory(&codeArenas, n, eSmallInt(Flags[flgCODEGROW]), 0);
	a = (char *) m->mem_top;
	m->mem_top += n;
	return a;
}

void
tfree(m)
char *m;
{
}

char *
dalloc(n)
unsigned n;
{
	register Memory *m;
	register char *a;
	register unsigned nwords;

	nwords = NWORDS(n);
	m = allocMemory(&dataArenas, nwords, eSmallInt(Flags[flgDATAGROW]), 0);
	a = (char *) m->mem_top;
	m->mem_top += nwords;
	return a;
}

void
dfree(m)
char *m;
{
}

char *
tdalloc(n, owner)
register unsigned n;
Object *owner;
{
	register Memory *m;
	register Word *top;

	n = NWORDS(n);
	if(n == 0)
		return NULL;
	n += 2;
	m = allocMemory(&tDataArenas, n, eSmallInt(Flags[flgTDATAGROW]), 1);
	top = m->mem_top;
	m->mem_top += n;
	top[0] = n;
	top[1] = (Word) owner;
	return (char *) (top + 2);
}

void
tdfree(a)
Word *a;
{
	register Memory *m;

	a -= 2;
	m = findArena(tDataArenas, a);
	if(m == (Memory *) NULL || a[1] == NULL)
		panic("Freeing unallocated memory in tdfree()");
	a[1] = NULL;
	m->mem_freed += a[0];
}

void
tdfreeObject(x)
register Object x;
{
	DeRef(x);
	switch(eType(x)) {
	when tLST:
	case tCHR:
	case tSTR:
		tdfree(eRef(x));
	when tUCN:
		if(IsFloat(x) || IsInt32(x))
			tdfree(eRef(x));
		else
			panic("Impossible UCN type in tdfreeObject()");
	}
}

#ifdef PROTOVERFLOW

/* Note manual pages say PROT_EXECUTE! */
#define prot_all PROT_READ|PROT_WRITE|PROT_EXEC

int RedZoneStatus = rzCLEAR;

static int
prot(base, top, pr)
register caddr_t base, top;
int pr;
{
/* fprintf(stderr, "prot(0x%x, 0x%x, 0x%x)\n", base, top, pr); */
	return mprotect(base, (top - base) & ~(PageSize - 1), pr);
}

int
setRedZones()
{
	if(RedZoneStatus == rzSET)
		panic("Trying to set red-zones twice");
	RedZoneStatus = rzSET;

	if(prot((caddr_t)heapTop, (caddr_t)heapMax, PROT_NONE))
		return -1;
	if(prot((caddr_t)stackTop, (caddr_t)stackMax, PROT_NONE))
		return -1;
	if(prot((caddr_t)trailTop, (caddr_t)trailMax, PROT_NONE))
		return -1;
	return 0;
}

int
clearRedBufferZones()
{
	if(RedZoneStatus != rzSET)
		panic("Trying to clear red-buffer-zones when not set");
	RedZoneStatus = rzCHECK;

	if(prot((caddr_t)heapTop, (caddr_t)heapMax - PageSize,
			prot_all))
		return -1;
	if(prot((caddr_t)stackTop, (caddr_t)stackMax - PageSize,
			prot_all))
		return -1;
	if(prot((caddr_t)trailTop, (caddr_t)trailMax - PageSize,
			prot_all))
		return -1;
	return 0;
}

int
clearRedZones()
{
	if(RedZoneStatus == rzCLEAR)
		panic("Trying to clear red-zones when not set");
	RedZoneStatus = rzCLEAR;

	if(prot((caddr_t)heapTop, (caddr_t)heapMax, prot_all))
		return -1;
	if(prot((caddr_t)stackTop, (caddr_t)stackMax, prot_all))
		return -1;
	if(prot((caddr_t)trailTop, (caddr_t)trailMax, prot_all))
		return -1;
	return 0;
}
#endif /* PROTOVERFLOW */

/*
 * Adjust the contents of the trail or stack to account for their
 * new positions after shifting.
 */
static void
stackAdjust(base, top, trailDisp, stackDisp)
register Object *base, *top;
register Word trailDisp, stackDisp;		/* Stack displacements in bytes */
{
	register Object x;

	for( ; base < top; base++) {
		x = *base;
		if(stackBase <= (Object *) x && (Object *) x < stackMax)
			*base = x + stackDisp;
		else if((IsVar(x) || IsBlock(x))
				&& stackBase <= eRef(x) && eRef(x) < stackMax)
			*base = x + stackDisp;
		else if(trailBase <= (TrailRecord *) x && (TrailRecord *) x < trailMax)
			*base = x + trailDisp;
	}
}

/*
 * Rearrange the stacks.
 */
void
shiftStacks(incH, incS, incT)
Word incH, incS, incT;
{
	Word *pH, *pS;
	TrailRecord *pT;
	Word heapSize, trailSize, stackSize;
	Word newSize, newHeapSize, newTrailSize, newStackSize;
	Word heapUsed, trailUsed, stackUsed;
	Word *newMemory;
	Word *newHeapBase, *newHeapMax;
	TrailRecord *newTrailBase, *newTrailMax;
	Word *newStackBase, *newStackMax;
	Word trailDisp, stackDisp;		/* Stack displacements in bytes */
	Machine *m;

	pH = CMR->mr_h;
	pS = CMR->mr_a;
	pT = CMR->mr_tr;
#ifdef DEBUG
	fprintf(stderr, "Growing stacks by:\n");
	fprintf(stderr, "\tHeap: %d\n", incH);
	fprintf(stderr, "\tStack: %d\n", incS);
	fprintf(stderr, "\tTrail: %d\n", incT);
#endif /* DEBUG */

#ifdef PROTOVERFLOW
	if(clearRedZones() == -1) {
		perror("");
		panic("Can't unprotect expanded memory");
	}
#endif /* PROTOVERFLOW */

	heapSize = heapMax - heapBase;
	heapUsed = pH - heapBase;
	stackSize = stackMax - stackBase;
	stackUsed = pS - stackBase;
	trailSize = (trailMax - trailBase) * wordsof(TrailRecord);
	trailUsed = (pT - trailBase) * wordsof(TrailRecord);

	for(newHeapSize = heapSize; newHeapSize <= heapUsed + incH; )
		newHeapSize *= 2;
	for(newStackSize = stackSize; newStackSize <= stackUsed + incS; )
		newStackSize *= 2;
	for(newTrailSize = trailSize; newTrailSize <= trailUsed + incT; )
		newTrailSize *= 2;

#ifdef PROTOVERFLOW
	newSize
		= PageSize / sizeof(Word) + newHeapSize + newStackSize + newTrailSize;
#else /* PROTOVERFLOW */
	newSize = newHeapSize + newStackSize + newTrailSize;
#endif /* PROTOVERFLOW */

#ifdef DEBUG
	fprintf(stderr, "New sizes are:\n");
	fprintf(stderr, "\tHeap: %d\n", newHeapSize);
	fprintf(stderr, "\tStack: %d\n", newStackSize);
	fprintf(stderr, "\tTrail: %d\n", newTrailSize);
#endif /* DEBUG */

	newMemory = (Word *) realloc(memory, newSize * sizeof(Word));
#ifdef DEBUG
printf("Memory was at 0x%x, realloced to 0x%x\n", memory, newMemory);
#endif /* DEBUG */
	if(!LegalAddress(newMemory + newSize))
		panic("Exceeding addressable memory.");
	if(newMemory != memory)
		panic("Growing stacks has required shifting the heap.");

	newHeapBase = heapBase;
	newHeapMax = newHeapBase + newHeapSize;
	newStackBase = newHeapMax;
	newStackMax = newStackBase + newStackSize;
	newTrailBase = (TrailRecord *) newStackMax;		/* Alignment? */
	newTrailMax = (TrailRecord *) (newStackMax + newTrailSize);

	/*
	 * Move the stacks in the right order otherwise we might clobber
	 * one with the other.
	 */
	if(newTrailBase > trailBase) {
		bcopy((char*)trailBase, (char*)newTrailBase, trailUsed * sizeof(Word));
	}
	if(newStackBase != stackBase)
		bcopy((char*)stackBase, (char*)newStackBase, stackUsed * sizeof(Word));
	if(newTrailBase < trailBase) {
		bcopy((char*)trailBase, (char*)newTrailBase, trailUsed * sizeof(Word));
	}

	stackDisp = (char *)newStackBase - (char *)stackBase;
	trailDisp = (char *)newTrailBase - (char *)trailBase;

	stackAdjust(newStackBase, newStackBase + stackUsed, trailDisp, stackDisp);
	stackAdjust(
		(Word *) newTrailBase,
		(Word *) newTrailBase + trailUsed,
		trailDisp, stackDisp);

	for(m = CMR; m != (Machine *) NULL; m = m->mr_suspended) {
		stackAdjust(m->mr_x, m->mr_x + 256, trailDisp, stackDisp);
		m->mr_a = (Word *) ((char *) m->mr_a + stackDisp);	/* Ugly, no? */
		m->mr_b = (Choice *) ((char *) m->mr_b + stackDisp);
		m->mr_e = (Environment *) ((char *) m->mr_e + stackDisp);
		m->mr_tr = (TrailRecord *) ((char *) m->mr_tr + trailDisp);
	}

	memory = newMemory;		/* Why bother? */

	heapBase = newHeapBase;
	heapMax = newHeapMax;
	heapTop = heapMax - HeapBreathingSpace;
	stackBase = newStackBase;
	stackMax = newStackMax;
	stackTop = stackMax - StackBreathingSpace;
	trailBase = newTrailBase;
	trailMax = newTrailMax;
	trailTop = (TrailRecord *) ((Word *)trailMax - TrailBreathingSpace);

#ifdef PROTOVERFLOW
	if(setRedZones() == -1) {
		perror("");
		panic("Can't protect expanded memory from overflow");
	}
#endif /* PROTOVERFLOW */
}

void
checkOverflowsAndShift()
{
	Word incH, incS, incT;

	incH = incS = incT = 0;
	if(CMR->mr_h /*+ HeapBreathingSpace*/ >= heapTop)
		incH = HeapBreathingSpace;
	if(CMR->mr_a /*+ StackBreathingSpace*/ >= stackTop)
		incS = StackBreathingSpace;
	if((Word *)(CMR->mr_tr) /*+ TrailBreathingSpace*/ >= (Word *)trailTop)
		incT = TrailBreathingSpace;
#ifdef PROTOVERFLOW
	if(RedZoneStatus == rzCHECK)
#else /* PROTOVERFLOW */
	if(incH || incS || incT)
#endif /* PROTOVERFLOW */
		shiftStacks(incH, incS, incT);
}
