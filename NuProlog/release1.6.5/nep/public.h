/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987, 1988 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

typedef int Word;
typedef Word Object;
typedef double Real;

extern Object NUNIL;

int NUIsVar();
int NUIsBound();
int NUIsStructure();
int NUIsFunctor();
int NUIsList();
int NUIsString();
int NUIsAtom();
int NUIsNIL();
int NUIsBlock();
int NUIsPointer();
int NUIsInt();
int NUIsFloat();
int NUIsNumber();
Object NUDeReference();
Word *NUeRef();
Word NUeInt();
Real NUeFloat();
char *NUeString();
Word NUeArity();
Object *NUeArgs();
Object NUeFunctor();
char *NUePrintName();
Object NUeModule();
char *NUStringFromAtomNumber();
Object NUMakeInt();
Object NUMakeFloat();
Object NUMakeAtom();
Object NUMakeBlock();
Object NUMakePointer();
Object NUStringToAtom();
Word *NUAllocateOnHeap();
int NUIsOnHeap();
int NUIsOnStack();
void NUTrailValue();
