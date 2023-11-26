/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

#include "mu.h"
#include "defs.h"
#include "nltypes.h"

#define	atom_name(W)		(strings + (symtab[W].sym_name))
#define	functor_name(f)		atom_name(heap[f])
#define	functor_arity(f)	(heap[f+1])
#define	mod_name(W)		(strings + (symtab[W].sym_module))

extern	Word	*heap;
extern	SymTab	*symtab;
extern	char	*strings;

/*
 * displayWord:
 *	Dump contents of word from code, data or heap segments
 *
 * Returns number of words dumped--usually 1.
 */
int
displayWord(seg, w, typ)
Word *seg, *w;
char typ;
{
	register Word	wrd = *w;

	printf("%08x\t%08x\t%02x\t", bytesin(seg,w,Word), wrd, typ);
	switch (typ)
	{
	when nOPCODE:
		printf("%-8s %d,%d,%d\n",
			bytecodes[eOpCode(wrd)].op_name,
			eArg1(wrd), eArg2(wrd), eArg3(wrd));
	when nREGS:
		printf("%-8s %d,%d,%d,%d\n", "...",
			eOpCode(wrd), eArg1(wrd), eArg2(wrd), eArg3(wrd));
	when nSTRING:
#ifdef LITTLEENDIAN
		printf("\"%c%c%c%c\"\n",wrd,wrd>>8,wrd>>16,wrd>>24);
#else /* LITTLEENDIAN */
		printf("\"%c%c%c%c\"\n",wrd>>24,wrd>>16,wrd>>8,wrd);
#endif /* LITTLEENDIAN */
	when nNUMBER:
		printf("$%d\n", eCValue(wrd));
	when nFLOAT:
		printf("#(FLT,%x)\n",wrd);
	when nINT32:
		printf("#(I32,%x)\n",wrd);
	when nBIT32:
		printf("$$%d\n", wrd);
	when nFVAL: {
		register int i;

		printf("$%g\n", *(Real *)w);
		for(i = wordsof(Real); i > 1; i--) {
			w++;
			printf("%08x\t%08x\n", bytesin(seg, w, Word), *w);
		}
		return(wordsof(Real));
	}
	when nINT32VAL:
		printf("$%d\n", wrd);
	when nATOM:
		printf("$%s\n", atom_name(wrd));
	when nSTRUCT:
		printf("$%s/%d\n", functor_name(wrd), functor_arity(wrd));
	when nLABEL:
		printf("@%x\n",wrd);
	when nATLAB:
		printf("&%s\n",atom_name(wrd));
	when nPROCLAB:
		printf("&%s/%d\n", atom_name(heap[wrd]), heap[(wrd)+1]);
	when nFUNCSYM:
		printf("func:%s\n", atom_name(wrd));
	when nFUNCARITY:
		printf("args:%d\n", wrd);
	when nFUNCCODE:
		if (wrd == lUNUSED)
			printf("No Code\n");
		else
			printf("code@%d\n", wrd);
	when nFUNCNEXT:
		printf("()->:%d\n", wrd);
	when nHASHTAB:
		printf("hash:%d\n", wrd);
	when nHASHDEFAULT:
		printf("#default\n");
	when nHASHNEXT:
		printf("##->:%d\n", wrd);
	when nTAGREF:
		printf("#(REF,%x)\n",wrd);
	when nTAGDEL:
		printf("#(DEL,%x)\n",wrd);
	when nTAGLST:
		printf("#(LST,%x)\n",wrd);
	when nTAGCHR:
		printf("#(CHR,%x)\n",wrd);
	when nTAGSTR:
		printf("#(STR,%x)\n",wrd);
	when nTAGUREF:
		printf("#(UREF,%x)\n",wrd);
	when nTAGUDEL:
		printf("#(UDEL,%x)\n",wrd);
	otherwise:
		printf("???\n");
	}
	return(1);
}
