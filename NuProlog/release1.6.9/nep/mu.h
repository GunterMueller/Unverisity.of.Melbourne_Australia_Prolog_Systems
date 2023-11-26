/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */


#if defined(PROTOVERFLOW) && (!defined(sun) || !defined(sparc))
Panic: Can only use page protection for stack overflow on Sun4s.
#endif

#include <sys/types.h>
#if defined(BSD4) || MACHINE_SGI
#	include <sys/param.h>
#else
#	define MAXPATHLEN 1024
#endif
#include <stdio.h>
#include <errno.h>
#ifdef MACHINE_SUN4_SOLARIS
#	include <unistd.h>
#endif
#include "machdep.h"
#include "bytecodes.h"
#include "malloc.h"
#include "types.h"
#include "syms.h"
#include "macros.h"

/* #define NUVERSION 10527 */		/* 1.5.27 */ 
#include "version.h"

#define when break; case

#define Talloc(size) talloc((unsigned)(size))
#define Dalloc(size) dalloc((unsigned)(size))
#define TDalloc(size, owner) tdalloc((unsigned)(size), owner)
extern int endfree;
extern void free();
extern char *malloc(), *balloc(), *calloc(), *realloc();
char *strcpy(), *talloc(), *dalloc(), *tdalloc();
void shiftStacks(), checkOverflowsAndShift();
extern int HeapBreathingSpace, StackBreathingSpace, TrailBreathingSpace;
#ifdef PROTOVERFLOW
	extern int RedZoneStatus;
	int setRedZones(), clearRedBufferZones(), clearRedZones();
#endif
#ifdef BSD4
	char *index(), *rindex();
	caddr_t sbrk();
#else
	char *strchr(), *strrchr();
#	ifndef MACHINE_SUN4_SOLARIS
		char *sbrk();
#	endif
#endif
int brk(), malloc_brk();
int access(), chdir();
#ifndef PROTO_UMASK
	extern mode_t umask(mode_t);
#endif
#ifndef PROTO_CHMOD
	extern int chmod(const char *, mode_t);
#endif
#ifndef MACHINE_SUN4_SOLARIS
	int getegid(), getgid(), getpid(), getppid();
	int getuid(), geteuid();
	int sleep();
	int fork();
#endif
int kill(), link(), unlink();
int system();
char *getlogin(), *getwd();
long atol();
double atof();

int p_arg(), p_functor(), p_univ(), p_name(), p_occurs();
void p_replacn(), p_replacn_var();
int p_setarg(), p_setarg_var();
int p_aref(), p_aset();
Object p_predicateArities();
Object pushString(), pushAssoc(), pushCons(), pushList2();
void panic(), warning(), warning2(), arithError(), arithErrorN();
void displayTerm();
void writeTerm(), writeStruct(), writeList();
void interpret();
TrailRecord *failure();
void applyFunctor(), callFunctor();
int load(), fload();
int runv();
void cputime();
PFInt addressOfFunction();
void receiveSignal();
void processSignals();
Object deRef(), unify(), identical(), firstVar(), eval();
void bindVariable();
void mark();
Delay *makeDelay();
TrailRecord *trimTrail();
Atom *enterAtom();
SymbolTable *newSymbolTable();
Functor *lookupFunctor(), *enterFunctor();
char *storeString();
unsigned char *listToString();
Object *listToObjects();
void initMalloc(), initIO();
int checkMalloc();
Stream *openStream();
int closeStream();
int getToken();
Object p_sget();
void putlStream();
Stream *validateStream();
Object copy(), uncopy();
int p_makeBMT(), p_linkBMT(), p_instance(), p_erase(), p_abolish();
Object tail();
Word sizeOfTerm(), sizeOfEmbeddedTerm();
void copyVariablesToTopOfHeap();

double round();
void f_add(), f_sub(), f_mult(), f_div(), f_intdiv(), f_mod();
void f_bitand(), f_bitor(), f_bitxor(), f_lsh(), f_rsh(), f_logand();
void f_logor(), f_lognot(), f_minus(), f_bitcomp(), f_sin(), f_cos();
void f_tan(), f_asin(), f_acos(), f_atan(), f_atan2(), f_exp();
void f_log(), f_log10(), f_power(), f_sqrt(), f_integer(), f_float();
void f_round(), f_lt(), f_le(), f_gt(), f_ge(), f_eq(), f_ne();
void f_eval(), f_sub_r(), f_div_r(), f_intdiv_r(), f_mod_r();
void f_lsh_r(), f_rsh_r(), f_atan2_r(), f_power_r(), f_add1(), f_sub1();
void f_byte(), f_ubyte(), f_half(), f_uhalf(), f_word(), f_address();
void f_single(), f_double();
int p_and(), p_or(), p_lt(), p_le(), p_gt(), p_ge(), p_eq(), p_ne();

int p_simc(), p_dsimc(), p_sql();
int p_simc_query(), p_simc_next(), p_simc_assert();
int p_simc_delete(), p_simc_end(), p_simc_abort();
int p_dsimc_open(), p_dsimc_free(), p_dsimc_cv();
int p_dsimc_sfbquery()/*, p_dsimc_end(), p_dsimc_abort()*/;
int p_sql_query(), p_sql_next(), p_sql_end();
int p_sql_modify(), p_sql_abort();

extern Memory *codeArenas, *dataArenas, *tDataArenas;
extern Word programSize;
extern Object freeList;
extern Object Flags[];
extern SymbolTable *stab;	/* Temporary, but long lived, kludge */
extern struct qelem adjhead, buckets[NBUCKETS];
extern int ActionPending;

extern Machine *CMR;		/* Current Machine */
extern TrailRecord *trailBase, *trailTop, *trailMax;
extern Word *memory, *memoryMax;
extern Word *heapBase, *heapTop, *heapMax;
extern Word *stackBase, *stackTop, *stackMax;
extern caddr_t lowWater, highWater;
extern char **environment;
extern char *errorMessages[];
extern char charTypes[], isPrint[];
extern SysCall SystemCalls[];
extern Instruction WIsEq[], undefinedPredicate[];
extern Instruction ret[], initializing[], notInitializing[];
extern Stream *streams;
extern int nstreams;
extern Stream *inputStream, *outputStream;
extern int InterruptAction;
extern int ArithError;
extern Object DelayArith;
extern Arith ArithTemp[];
extern Arith *ArithStack, *ArithStackMax;
extern Functor *SignalActions[];
extern int SignalPending, SignalsPending[];
extern int BreakCounter;
extern char currentDirectory[], nepologFileName[];
#define NPATHS 6
extern char *PATHS[NPATHS];
#define PATHheader PATHS[0]
#define PATHbin PATHS[1]
#define PATHwake PATHS[2]
#define PATHnep PATHS[3]
#define PATHnubin PATHS[4]
#define PATHnulib PATHS[5]

extern Atom firstSystemAtom, lastSystemAtom;
extern PFInt cFunctTable[];
extern Object cFunctDummy[], *lastCFunctDummy;

extern PFVoid ArithFuncTable[];
extern Atom *ArithFuncAtoms[];
extern char ArithFuncArity[];
extern PFInt ArithPredTable[];
extern Atom *ArithPredAtoms[];
extern char ArithPredArity[];

extern Object SourceFile;

extern Program WakeUp, PureWakeUp;
extern Program WakeDebug;

extern Instruction WEvaluate[];

extern int coreflg;

extern Structure StrHeader6_BMT, StrHeader2_Ref, StrHeader2Stream;
extern Structure StrHeader2Cons, StrHeader2Equal, StrHeader1_VAR;
extern Structure StrHeader1Write_Var, StrHeader1Parentheses;
extern Structure StrHeaderAtomHeader, StrHeader3_Prop;

#ifdef DEBUG
extern int spyflg, regflg, traceflg;
extern SpyStruct SpyPoints[];
extern Program PastPCs[];
extern Choice *PastBs[];
extern int NextPCSlot;
#endif /* DEBUG */

#ifdef DEBUG3
extern int callflg;
#endif /* DEBUG3 */
