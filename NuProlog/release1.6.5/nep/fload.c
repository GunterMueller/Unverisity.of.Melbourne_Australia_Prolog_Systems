/*
 * Please note that this code is the property of the University of Melbourne
 * and is Copyright 1985, 1990 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

/*
 * The authorship of some parts of the code here is currently unknown.
 * I'm trying to track them down.
 * It appears to be public domain, though, and was used in MU-Prolog.
 */

#include "mu.h"

#ifdef FLOAD
/*
 * SO FAR, only works on Berkeley-type Unix systems.
 * Don't bother to define FLOAD on other systems,
 * or you'll simply get a bunch of syntax errors.
 */

#include <sys/stat.h>
#include <a.out.h>

#ifdef MACHINE_SGI
#	define COFF
#	define N_UNDF 0		/* BUG! */
#	define X_OK 01		/* BUG! */
#endif

#ifdef MACHINE_DEC
#	define COFF
#	define X_OK 01		/* BUG!  Should include <unistd.h> instead. */
#endif

#ifdef MACHINE_ALPHA
#	define COFF
#endif

#ifdef COFF
#	include <filehdr.h>
#	include <syms.h>
#	include <ldfcn.h>
#else
#	include <sys/file.h>
#endif

typedef struct nlist LoaderSymbol;

#define IS(name, op) \
	int name(x) register Object x; \
	{ DeRef(x); return(op(x)); }

IS(NUIsVar, IsVar)
IS(NUIsBound, IsBound)
IS(NUIsStructure, IsStr)
IS(NUIsList, IsList)
IS(NUIsString, IsString)
IS(NUIsAtom, IsAtom)
IS(NUIsNIL, IsNIL)
IS(NUIsBlock, IsBlock)
IS(NUIsInt, IsInt)
IS(NUIsFloat, IsFloat)
IS(NUIsNumber, IsNumber)

int
NUIsFunctor(x, s, a)
register Object x;
register char *s;
register int a;
{
	register Structure f;

	DeRef(x);
	if(IsStr(x)) {
		f = *(Structure *)eRef(x);
		return(		strcmp(eCharStar(eFunctor(f)->a_pname), s) == 0
				&&	eNArgs(f) == a);
	} else if(IsList(x))
		return(strcmp(".", s) == 0 && 2 == a);
	else
		return(0);
}

Object
NUDeReference(x)
register Object x;
{
	DeRef(x);
	return(x);
}

#define EXTRACT(name, op, type) \
	type name(x) register Object x; \
	{ DeRef(x); return(op(x)); }

EXTRACT(NUeRef, eRef, Word *)
EXTRACT(NUeInt, eInt, Word)
EXTRACT(NUeFloat, eFloat, double)
EXTRACT(NUeString, eCharStar, char *)

double
NUeNumber(x)
register Object x;
{
	DeRef(x);
	if(IsFloat(x))
		return(eFloat(x));
	else if(IsSmallInt(x))
		return((double) eSmallInt(x));
	else if(IsInt32(x))
		return((double) eInt32(x));
	else 
		return(0.0);
}
									
Word
NUeArity(x)
register Object x;
{
	DeRef(x);
	if(IsStr(x))
		return(eNArgs(*(Structure *)eRef(x)));
	else if(IsList(x))
		return(2);
	else
		return(0);
}

Object *
NUeArgs(x)
register Object x;
{
	DeRef(x);
	if(IsStr(x))
		return(eRef(x) + 1);
	else if(IsList(x))
		return(eRef(x));
	else
		return((Object *) NULL);
}

Object
NUeFunctor(x)
register Object x;
{
	DeRef(x);
	if(IsStr(x))
		return(StarToAtom(eFunctor(*(Structure *)eRef(x))));
	else if(IsList(x))
		return(StarToAtom(&SymCons));
	else
		return(MakeSmallInt(0));
}

char *
NUePrintName(x)
register Object x;
{
	DeRef(x);
	if(IsAtom(x))
		return(eCharStar(((Atom *)eRef(x))->a_pname));
	else
		return((char *) NULL);
}

Object
NUeModule(x)
register Object x;
{
	DeRef(x);
	if(IsAtom(x))
		return(((Atom *)eRef(x))->a_module);
	else
		return(NIL);
}

char *
NUStringFromAtomNumber(x)
register Word x;
{
	return(eCharStar(((Atom *)x)->a_pname));
}

#define MAKE(name, op, type) \
	Object name(x) register type x; \
	{ return(op(x)); }

MAKE(NUMakeFloat, pushFloat, Real)
MAKE(NUMakeAtom, MakeAtom, Word *)
MAKE(NUMakeBlock, MakeBlock, Word *)
MAKE(NUMakePointer, MakeBlock, Word *)

Object
NUMakeInt(x)
register Word x;
{
	return(MakeInt(x, CMR->mr_h));
}

Object
NUStringToAtom(s)
register char *s;
{
	return(StarToAtom(enterAtom(s, stab, (Atom *) NULL)));
}

Word *
NUAllocateOnHeap(n)
register Word n;
{
	register Word *mem;

	mem = CMR->mr_h;
	if(mem + n >= heapMax)
		return((Word *) NULL);
	else {
		CMR->mr_h = mem + n;
		return(mem);
	}
}

int
NUIsOnHeap(p)
register Word *p;
{
	return(heapBase <= p && p < heapMax);
}

int
NUIsOnStack(p)
register Word *p;
{
	return(stackBase <= p && p < stackMax);
}

void
NUTrailValue(p)
Word *p;
{
	register TrailRecord *tr;

	tr = CMR->mr_tr;
	if(tr + 1 >= trailMax) {
		fprintf(stderr, "Panic: Trail Overflow in NUtrailValue()\n");
		exit(1);
	}
	if(p < (Word *) CMR->mr_b && p > CMR->mr_h || p < CMR->mr_hb) {
		CMR->mr_tr = tr + 1;
		tr->tr_address = p;
		tr->tr_value = *p;
	}
}

/************************************************************************
*																		*
*								MU-PROLOG								*
*								=========								*
*																		*
* (C) Copyright 1985 Lee Naish, Melbourne University					*
*																		*
*	Written by Lee Naish, Department of Computer Science,				*
*	Melbourne University, Parkville 3052, Australia.					*
*																		*
*	No liability for errors or omissions in this system is				*
*	expressed, implied, impressed or explied.							*
*																		*
************************************************************************/

#define ROUND PageSize

#ifdef pyramid		/* according to the documentation 'pyramid' is */
#	define pyr		/* defined on pyramids but, here at least, 'pyr' is */
#endif
#ifdef pyr		/* Pyramid 90X */
#	include <sys/mman.h>
#endif

#ifdef MACHINE_ENCORE_BSD_EMUL
#	define LD "/bsd/bin/ld"
#endif
#ifdef MACHINE_SGI
#	define LD "/usr/bin/ld"
#endif
#ifndef LD
#	define LD "/bin/ld"
#endif

/*
	Run time loading of functions.
	- Basic technique.
		The loader ld(1) -A option specifies incremental loading.
		The output of the loader will be a file which can be read
		into an already executing program. The symbol table of the
		already executing program must be specifed.

		The currently executing file is the MU-Prolog binary (BIN)
		If it is found, the loader is invoked with
		ld -N		Do not make text portion readable or sharable
				(deleted this option - non-portable)
			-x 	no local symbols in symbol table
			-A <executing file>
				used to construct new symbol table
			-T <text end>
				specifies the text segment origin.
			-e <name>
				name of an entry point
			-o <filename>
				output file name
			<files>
				object files to be used.
			<library>
				libraries to be searched, etc.
			-lc
				default library
	Support functions:
	LoaderSymbol {
		char *ls_name;	(pointer to entry point name)
		PFInt ls_function; (call address)
		};
	Set_program_name()
		Should be called first to check the program text file name.
		This is used in the loading process.
	PFInt *
	loadFile(files, libraries, functiosn, nentries)
	char **files, **libraries, **functions;
	int nentries;

		"Files" is a string containing the object files and libraries
		to be given to the loader.

		The "entries" points to a list of symbol_references structures.
		Each structure has the name of a function, and a function
		address field which is set to the name of the function.
			LoaderSymbol entries[] = {
				{ "_ugly" }, { "_really" }, { (char *)0 } };
			NOTE: this is acceptable C, missing field initializers
			are supposed to be acceptable.

	IMPLEMENTATION:
		The functions use the stdio package, and write error
	messages on STDOUT.  If an error is detected, a 0 is returned.
		The sbrk() system call is used to allocate memory.  This
	should have no effect on a properly designed memory allocation package,
	such as malloc(3).
		The external entry point names must be specified in the
	form that the loader needs.  For example, the C name "ugly"
	would have to be specified as "_ugly", and the F77 name "ugly"
	as "_ugly_".
*/

static char Symbol_name[MAXPATHLEN + 1];	/* name of the new object file */
char *strcpy(), *mktemp();
extern char *sys_errlist[];
static PFInt *loadFile();
extern int errno;	/* system errors */
extern caddr_t sbrk();	/* memory allocation */

static int
Ok_to_exec(str)
char *str;
{
	struct stat stbuff;

	return(
		stat(str, &stbuff) == 0		/* stat it */
		&& (stbuff.st_mode&S_IFMT) == S_IFREG	/* check for file */
		&& access(str,X_OK) == 0	/* and can be executed */
		);
}

/*
	Set_program_name()
		Checks the file name for execute perms.
*/

static int
Set_program_name()
{
	int i;

	if((i = Ok_to_exec(nepologFileName)) == 0)
		fprintf(stderr, "can't exec %s\n", nepologFileName);
	return(i);
}

static int
objectsToChars(obs, chars)
register Object *obs;
register char **chars;
{
	register int i;

	for(i = 0; obs[i] != 0; i++) {
		register Object f;

		f = obs[i];
		if(IsAtom(f))
			chars[i] = eCharStar(((Atom *) eRef(f))->a_pname);
		else if(IsString(f))
			chars[i] = (char *) eRef(f);
		else {
			warning("String or atom expected in objectsToChars()");
			return(-1);
		}
	}
	chars[i] = (char *) NULL;
	return(i);
}

int
fload(filesT, librariesT, functionsT, addressesT)
Object filesT, librariesT, functionsT, *addressesT;
{
	Object *filesV, *librariesV, *functionsV;
	char **files = (char **) NULL;
	char **libraries = (char **) NULL;
	char **functions = (char **) NULL;
	PFInt *addresses = (PFInt *) NULL;
	register int i;
	int nentries;
	int retcode = 1;

	if(sizeof(Object *) != sizeof(char *))
		panic("Size assumptions in fload() wrong");

	filesV = listToObjects(filesT, (Object *) NULL, 0);
	files = (char **) filesV;					/* Re-using space */
	if(filesV == (Object *) NULL || objectsToChars(filesV, files) < 0) {
		warning("List of file names expected in fload()");
		retcode = 0;
		goto cleanup;
	}

	librariesV = listToObjects(librariesT, (Object *) NULL, 0);
	libraries = (char **) librariesV;			/* Re-using space */
	if(		librariesV == (Object *) NULL
			|| objectsToChars(librariesV, libraries) < 0) {
		warning("List of library names expected in fload()");
		retcode = 0;
		goto cleanup;
	}

	functionsV = listToObjects(functionsT, (Object *) NULL, 0);
	if(functionsV == (Object *) NULL) {
		warning("List of function names expected in fload()");
		retcode = 0;
		goto cleanup;
	}

	functions = (char **) functionsV;			/* Re-using space */
	i = objectsToChars(functionsV, functions);
	if(i < 0) {
		retcode = 0;
		goto cleanup;
	}
	nentries = i;

	if(!Set_program_name()) {
		warning("Unable to access NU-Prolog binary.");
		retcode = 0;
		goto cleanup;
	}

	addresses = loadFile(files, libraries, functions, nentries);
	if(addresses == (PFInt *)NULL) {
		warning("Unable to fload()");
		retcode = 0;
		goto cleanup;
	}

	{
		register Object X;

		for(X = NIL, i = nentries - 1; i >= 0; i--)
			X = pushCons(StarToBlock(addresses[i]), X);
		*addressesT = X;
	}

cleanup:
	if(files != (char **) NULL)
		free(files);
	if(libraries != (char **) NULL)
		free(libraries);
	if(functions != (char **) NULL)
		free(functions);
	if(addresses != (PFInt *) NULL)
		free(addresses);
	return(retcode);
}

/*
	loadFile(files, libraries, functions, nentries)
	1. generate a command for the loader.
		NOTE: this uses the execv function.
	2. execute the loader command.
	3. get the entry points and return an array of them
*/

#ifdef MACHINE_ENCORE

/* Courtesy of Encore Computer Corporation and SICS */

struct gsym {
	struct syment sment;	/* symbol table entry */
	char *smscnptr;			/* input section ptr */
	long smmyacid;			/* SDP id of this entry */
	long smchainid;			/* SDP id ptr to collision chain */
	long smauxid;			/* SDP id to aux entry list */
	long smnewval;			/* symbol value after relocation*/
							/* This field is also used by commspec() 
							   to hold a pointer to the AIDFNSCN 
							   actitem for the section where the 
							   common symbol will be
							   allocated. This use is finished before 
							   relocation and is isolated in commspec()*/
	long smoutndx;			/* index of symbol in output sym tab*/
	long smnewvirt;			/* "new" virtual value or offset */
	short smnamelength;
	short smrefs;			/* references for checking if none */
	char smlocflg;			/* flag indicating this entry is to be
							   written in the "local" part of table
							   (for example, functions) */
	char smundmsg;			/* flag for undefined symbols to
							   indicate that error msg was printed
							   (see plocsyms in output.c) */
							/* also used for unreferenced msg */
	char sm_seen;			/* flag indicating that we've seen this
							   isn't "undefined" as far as "scope hiding" is
							   concerned. */
	char smupdated;			/* 0 until smnewval updated (relocated) */
	enum scope sm_how;		/* Should symbol be hidden or not? */
	struct syment *smentp;	/* .o file symbol entry, clean */
	};

extern struct gsym *findsym();

/* end courtesy :-) */

static PFInt *
loadFile(files, libraries0, functions, nentries)
char **files, **libraries0, **functions;
int nentries;
{
	static int initialized = 0;
	register PFInt *addresses = (PFInt *) NULL;
	register int i;
	int nlibraries = 0;
	char **libraries = (char **) NULL;
	PFInt *retcode = (PFInt *) NULL;
	struct stat sb;
#ifdef MACHINE_ENCORE_BSD_EMUL
	static char *libpaths[] = {"/bsd/lib/lib", "/bsd/usr/lib/lib"};
#else /* MACHINE_ENCORE_BSD_EMUL */
	static char *libpaths[] = {"/lib/lib", "/usr/lib/lib"};
#endif /* MACHINE_ENCORE_BSD_EMUL */

	if(!initialized) {
		initialized = 1;
		if(!initsyms(nepologFileName)) {
			warning("Unable to initialize dynload() in loadFile()");
			goto cleanup;
		}
	}

	for(i = 0; libraries0[i] != (char *) NULL; i++)
		;
	nlibraries = i;
	libraries = (char **) malloc((nlibraries + 1) * sizeof(char *));
	if(libraries == (char **) NULL) {
		warning("Unable to malloc() memory in loadFile()");
		goto cleanup;
	}
	for(i = 0; i < nlibraries; i++) {
		register char *l0, *l;

		l0 = libraries0[i];
		if(l0[0] == '-' && l0[1] == 'l') {
			register int j;

			for(j = 0; j < sizeof(libpaths) / sizeof(char *); j++) {
				l = malloc(strlen(libpaths[j]) + strlen(l0) + 2);
				if(l == (char *) NULL) {
					warning("Unable to malloc() memory in loadFile()");
					nlibraries = i + 1;
					goto cleanup;
				}
				sprintf(l, "%s%s.a", libpaths[j], l0 + 2);
				if(stat(l, &sb) == 0)
					break;
				free(l);
				l = (char *) NULL;
			}
			if(l == (char *) NULL) {
				warning("Can't find library in loadFile().  Use full path.");
				nlibraries = i + 1;
				goto cleanup;
			}
			libraries[i] = l;
		} else
			libraries[i] = l0;
	}
	libraries[i] = (char *) NULL;

	for(i = 1; files[i] != (char *) NULL; i++) {
		char *loadadr;

		if(dynload(files[i], &loadadr, 0L, libraries) <= 0) {
			warning("Dynload() failed in loadFile()");
			goto cleanup;
		}
	}
	{
		char *loadadr;

		if(dynload(files[0], &loadadr, 0L, libraries) <= 0) {
			warning("Dynload() failed in loadFile()");
			goto cleanup;
		}
	}

	addresses = (PFInt *)malloc(sizeof(PFInt *) * (nentries + 1));
	if(addresses == (PFInt *) NULL) {
		warning("Unable to malloc() memory in loadFile()");
		goto cleanup;
	}
	for(i = 0; i < nentries; i++) {
		register struct gsym *s;

		s = findsym(functions[i]);
		if(s == (struct gsym *) NULL) {
			fprintf(stderr, functions[i]);
			warning("Symbol undefined in loadFile()");
			goto cleanup;
		}
		addresses[i] = (PFInt) (s->smnewval);
	}
	retcode = addresses;
	goto done;

cleanup:
	if(addresses != (PFInt *) NULL)
		free(addresses);
done:
	if(libraries != (char **) NULL) {
		for(i = 0; i < nlibraries; i++)
			if(libraries[i] != libraries0[i])
				free(libraries[i]);
		free(libraries);
	}
	return(retcode);
}

#else /* MACHINE_ENCORE */

#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))

#ifdef COFF
#	define N_NAME(x) (x).n_name
#else
#	define N_NAME(x) (x).n_un.n_name
#endif

static PFInt *
loadFile(files, libraries, functions, nentries)
char **files, **libraries, **functions;
int nentries;
{
	LoaderSymbol *entries;
	PFInt *addresses;
	static char *file_proto="/tmp/llXXXXXX";
	int readsize;			/* total amount of object to read */
	int	totalsize;			/* total memory needed */
	caddr_t base;			/* current end of memory */
	caddr_t originalBase;	/* beginning of area given by balloc() */
	caddr_t newBase;		/* beginning of area given by realloc() */
	int fcb;				/* used to read the object file */
#ifdef COFF
	LDFILE *ldptr;
	struct filehdr fileHeader;
	struct aouthdr header;
	FILE *fp;
#else
	struct exec header;		/* a.out header */
#endif
	int nfiles, nlibraries;
	char **args, **argsp;	/* Loader arguments */
	int nargs;
	char baseAsHex[20];		/* buffer for one of the loader arguments */
	register int i;

#ifdef DEBUG5
		printf("calling loadFile\n");
#endif
	entries = (LoaderSymbol *) malloc((nentries + 1) * sizeof(LoaderSymbol));
	if(entries == (LoaderSymbol *) NULL) {
		warning("Unable to malloc memory for function table in fload()");
		return((PFInt *) NULL);
	}
	N_NAME(entries[nentries]) = "";
	for(i = 0; i < nentries; i++)
		N_NAME(entries[i]) = functions[i];

	(void) strcpy(file_proto, "/tmp/llXXXXXX");	/* gets clobbered */
	/* create a temp file to hold output from loader */
	(void) strcpy(Symbol_name, mktemp(file_proto));

	/* force end of memory to be aligned to a ROUND byte boundary */
	/* note: this restriction is applied by the loader */
	originalBase = (caddr_t) balloc(ROUND);
#ifdef DEBUG5
		printf("initial balloc(%d) returns 0x%x\n", ROUND, (int)originalBase);
#endif
	base = (caddr_t) round(((int)originalBase), ROUND);
#ifdef DEBUG5
		printf("base %x, base %x\n", originalBase, base);
#endif

	/* find the loader name */
	if(!Ok_to_exec(LD)) {
		warning("cannot find loader");
		free(entries);
		return((PFInt *) NULL);
	}

	for(i = 0; files[i] != (char *) NULL; i++)
		;
	nfiles = i;
	for(i = 0; libraries[i] != (char *) NULL; i++)
		;
	nlibraries = i;
	nargs = nentries * 2 + nfiles + nlibraries + 1;

	/* different loaders need different options (YUK!) */
	/* It's a matter of trial and error - you may even need */
	/* to go as far as reading the manual */
	args = (char **) malloc(sizeof(char *) * (nargs + 100));
	if(args == (char **) NULL) {
		warning("Can't malloc arg vector in loadFile()");
		free(entries);
		return((PFInt *) NULL);
	}

	argsp = args;
	*argsp++ = LD;
#if (pyr || vax || sun) && !interdata
	*argsp++ = "-N";
#endif
	*argsp++ = "-x";
	*argsp++ = "-A";
	*argsp++ = nepologFileName;
	*argsp++ = "-T";
	sprintf(baseAsHex, "%x", base);
	*argsp++ = baseAsHex;
	*argsp++ = "-o";
	*argsp++ = Symbol_name;
#ifdef DEBUG5
		printf("Symbol_name %s\n", Symbol_name);
#endif

	/* now add the entry points */

	if(N_NAME(entries[0]) == (char *) NULL) {
		warning("missing entry name");
		free(entries);
		return((PFInt *) NULL);
	}
	*argsp++ = "-e";
	*argsp++ = N_NAME(entries[0]);

	/* set up the rest */
	for(i = 1; i < nentries; i++) {
		*argsp++ = "-u";
		*argsp++ = N_NAME(entries[i]);
	}

	/* now add the object files and user-specified libraries */
	for(i = 0; i < nfiles; i++)
		*argsp++ = files[i];
	for(i = 0; i < nlibraries; i++)
		*argsp++ = libraries[i];

	/* now add the defaults */
	*argsp++ = "-lc";
	*argsp++ = (char *) NULL;

#ifdef DEBUG5
		printf("loader command: ");
		for(i = 0; args[i] != (char *) NULL; i++)
			printf(" %s", args[i]);
		printf("\n");
#endif

	if(runv(LD, args) != 0) {
		(void)unlink(Symbol_name);
		warning("load of objects and entries failed");
		free(entries);
		return((PFInt *) NULL);
	}

#ifdef DEBUG5
	printf("load was successful\n");
#endif /* DEBUG5 */

	/* now try and read the information from the symbol table */

#ifdef COFF

    ldptr = ldopen(Symbol_name, NULL);
    if(ldreadst(ldptr, -1) == FAILURE) {
        fprintf(stderr, "cannot read symbol table of temp file %s: %s\n",
            Symbol_name, sys_errlist[errno]);
        free(entries);
        return((PFInt *) NULL);
    }
#ifdef DEBUG5
		printf("output file opened successfully\n");
#endif /* DEBUG5 */

	/* read the a.out header and find out how much room to allocate */

	fp = fopen(Symbol_name, "r");
	if(fp == NULL) {
        fprintf(stderr, "cannot read temp file %s: %s\n",
            Symbol_name, sys_errlist[errno]);
        free(entries);
        return((PFInt *) NULL);
	}
    fread(&fileHeader, sizeof(fileHeader), 1, fp);
    fread(&header, sizeof(header), 1, fp);

	/* calculate  sizes */
    readsize = header.tsize + header.dsize;
    totalsize = readsize + header.bsize;
    /* BUG!  Do we need to round up? */
	/* Probably good idea for systems which have explicit execute permission. */
    totalsize = round(totalsize, ROUND);    /* round up a bit */
#ifdef DEBUG5
        printf("read header: tsize %d, dsize %d, bsize %d, entry 0x%x\n",
            header.tsize, header.dsize, header.bsize, header.entry);
        printf("readsize %d, totalsize %d\n", readsize, totalsize);
#endif

#else /* COFF */

	if((fcb = open(Symbol_name, O_RDONLY)) < 0) {
		fprintf(stderr, "cannot open temp file %s: %s\n",
			Symbol_name, sys_errlist[errno]);
		free(entries);
		return((PFInt *) NULL);
	}
#ifdef DEBUG5
		printf("output file opened successfully\n");
#endif /* DEBUG5 */

	/* read the a.out header and find out how much room to allocate */

	if(sizeof(header) != read(fcb, (char *)&header, sizeof(header))) {
		fprintf(stderr, "cannot read header from temp file %s\n",
			Symbol_name);
		free(entries);
		return((PFInt *) NULL);
	}

	/* calculate  sizes */
	readsize = round(header.a_text, 4) + round(header.a_data, 4);
	totalsize = readsize + header.a_bss;
	totalsize = round(totalsize, ROUND);	/* round up a bit */
#ifdef DEBUG5
		printf("read header: a_text %d, a_data %d, a_entry 0x%x\n",
			header.a_text, header.a_data, header.a_entry);
		printf("readsize %d, totalsize %d\n", readsize, totalsize);
#endif /* DEBUG5 */

#endif /* COFF */

	/* Increase size of balloc()'d segment to store the new code. */
	newBase = realloc(originalBase, totalsize + (base - originalBase));
#ifdef DEBUG5
		printf("realloc(0x%x, 0x%x) returns 0x%x\n",
			originalBase, totalsize + (base - originalBase), newBase);
#endif /* DEBUG5 */
	if(newBase != originalBase) {
		warning("fload: realloc() moved memory");
		free(entries);
		return((PFInt *) NULL);
	}

	/* now read in the functions */
	
#ifdef COFF
	if(fseek(fp, (long)N_TXTOFF(fileHeader, header), SEEK_SET)) {
		fprintf(stderr, "cannot seek to %d in %s: %s\n",
			N_TXTOFF(fileHeader, header),
			Symbol_name,
			sys_errlist[errno] );
		free(entries);
		return((PFInt *) NULL);
	}
#ifdef DEBUG5
		printf("Reading %d bytes from %d in %s\n",
			readsize,
			N_TXTOFF(fileHeader, header),
			Symbol_name);
#endif
	if(readsize != fread((char *)base, 1, readsize, fp)) {
		fprintf(stderr, "cannot read %s: %s\n", Symbol_name,
			sys_errlist[errno] );
		free(entries);
		return((PFInt *) NULL);
	}
	/* clear bss */
	bzero(readsize + (char *)base, totalsize - readsize);
#else /* COFF */
	if(readsize != read(fcb, (char *)base, readsize)) {
		fprintf(stderr, "cannot read %s: %s\n", Symbol_name,
			sys_errlist[errno] );
		free(entries);
		return((PFInt *) NULL);
	}
#endif /* COFF */
			/* on Pyramid, we have to make the new functions */
			/* executable */
#ifdef pyr
	mprotect(base, totalsize, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif /* pyr */

	/* set the first entry up to the header value */

	if(nlist(Symbol_name, entries) == -1) {
		warning("failed to find all symbols");
		warning("nlist() failed.");
		free(entries);
		return((PFInt *) NULL);
	}

	(void)close(fcb);
	(void)unlink(Symbol_name);
	addresses = (PFInt *)malloc(sizeof(PFInt *) * (nentries + 1));
	if(addresses == (PFInt *) NULL) {
		warning("Unable to malloc() memory in loadFile()");
		free(entries);
		return((PFInt *) NULL);
	}
	for(i = 0; i < nentries; i++) {
		if(entries[i].n_type == N_UNDF) {
			fprintf(stderr, N_NAME(entries[i]));
			warning("Symbol undefined in loadFile()");
			free(addresses);
			free(entries);
			return((PFInt *) NULL);
		}
		addresses[i] = (PFInt) (entries[i].n_value);
	}
	free(entries);
	return(addresses);
}

#ifdef JUNK
savedload(fd, bp)
	int fd;
{
	char *start;
	long *bp;

	bp = &savebuf.l[S_DLOAD];
	while(*bp) {
		start = (char*)*bp++;
#ifdef DEBUG5
		printf("writing from %x to %x\n", (int)start, (int)*bp);
#endif /* DEBUG5 */
		write(fd, start, (char*)*bp++ - start);
	}
}

restdload(fd)
	int fd;
{
	caddr_t end;		/* current end of memory */
	int diff, n, totalsize;
	char *start;
	long *bp;

	bp = &savebuf.l[S_DLOAD];
	if(!*bp)
		return(1);
	if(*bp < (int)sbrk(0)) {
		fprintf(stderr, "Extra memory allocated already\n");
		return(0);
	}
	while(*bp) {
		start = (char*)*bp;
		if(brk(start) != 0) {
			fprintf(stderr, "brk failure - 15 car pileup\n");
			return(0);
		}
		totalsize = *(bp+1)-*bp;
#ifdef DEBUG5
		printf("start=0x%x, tot=0x%x\n", (int)start, (int)totalsize);
#endif /* DEBUG5 */
		/* allocate more memory, using sbrk */

		end = sbrk( totalsize );
		if( (int)end <= 0 ) {
			fprintf(stderr, "sbrk failed to allocate: %s\n",
				sys_errlist[errno] );
			return( 0 );
		}
		read(fd, start, totalsize);
			/* on Pyramid, we have to make the new functions */
			/* executable */
#ifdef pyr
		mprotect(end, totalsize, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif /* pyr */
		bp += 2;
	}
	return(1);
}
#endif /* JUNK */

#endif /* MACHINE_ENCORE */

#else /* FLOAD */

int
fload(files, functions, addresses)
Object files, functions, *addresses;
{
	panic("fload: foreign file loading not available in this version");
}

#endif /* FLOAD */
