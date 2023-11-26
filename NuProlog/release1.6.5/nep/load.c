/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

#include "mu.h"
#include <ctype.h>
#include <sys/file.h>
#include <sys/stat.h>
#ifdef BSD4
#include <strings.h>
#else
#include <string.h>
#include <fcntl.h>
#endif
#include "defs.h"
#include "nltypes.h"

#define	atom_name(W)		(strings + (symtab[(W)].sym_name))
#define	mod_name(W)		(strings + (symtab[(W)].sym_module))
#define	proc_name(W)		(atom_name(heap[(W)]))
#define	proc_arity(W)		(heap[(W)+1])

static	int	obj_file;	/* object code file descriptor */

static	Header	module;		/* header information for module */

static	char	*bytes;		/* block of memory for segments */

Object	SourceFile;			/* Atomic name of the file being loaded. */
							/* It is used by iload to set dynamic preds' */
							/* source file properly. */
							/* Set to 'user' by the top-level. */

extern	Word	*top;		/* top of code+data segment */

extern	Word	*code;		/* start of code segment for module */
extern	Word	*data;		/* start of data segment for module */
extern	Word	*heap;		/* start of junk heap segment for module */
extern	char	*types;		/* start of code type-info segment for module */
extern	char	*dtypes;	/* start of data type-info segment for module */
extern	char	*htypes;	/* start of heap type-info segment for module */
extern	SymTab	*symtab;	/* start of symbol table segment for module */
extern	char	*strings;	/* start of string table segment for module */

/*
 * load:
 *	Grab a *.no format object module and load it into Nepolog
 */
int
load(obj_name)
char *obj_name;
{
	reg	char	*t;
	reg	Word	*w;
		Word	*nextw;
	reg	Word	*lastop;
		Atom	*a;
		Functor *f;
		PFInt	fn;
		Word	*list;
		Word	deflt;
		int	arity;
		char	*atom;
		char	SourceFileName[MAXPATHLEN];
	static	char	mainRedefinedAlready[2] = {'\0', '\0'};
		Word	*makeHashTable();
		Word	*tab_to_list();
		int	outOfDate();
	extern	int	loadflg, sysflg;
	extern	Functor *enterFunctor();
	extern	int	displayWord();

	if ((obj_file = open(obj_name,O_RDONLY,0644)) < 0) {
		return(0);
	}
	if (outOfDate(obj_file,"na"))
		fprintf(stderr, "Warning: %s out of date.\n", obj_name);
	if(obj_name[0] == '/')
		strcpy(SourceFileName, obj_name);
	else
		sprintf(SourceFileName, "%s/%s", currentDirectory, obj_name);
#ifdef sck_stab
	SourceFile =
		StarToAtom(enterAtom(NON_DB_ATOM, SourceFileName, stab, (Atom *) NULL));
#else
	SourceFile = StarToAtom(enterAtom(SourceFileName, stab, (Atom *) NULL));
#endif
	if(sysflg)
		addprop(&Sym_SystemSourceFiles, StarToAtom(&Sym_File), SourceFile);
	else
		addprop(&Sym_SourceFiles, StarToAtom(&Sym_File), SourceFile);

	if(	   read(obj_file, &module, sizeof(Header)) != sizeof(Header)
		|| module.nl_magic != nMAGIC
		|| !alloc_work_areas())
	{
		close(obj_file);
		return(0);
	}

	if(module.nl_version != NUVERSION) {
		fprintf(stderr,
			"Warning: %s is a version %d.%d.%d object file.  Not loaded.\n",
			obj_name,
			module.nl_version / 10000,
			(module.nl_version % 10000) / 100,
			module.nl_version % 100);
		close(obj_file);
		return(0);
	}

	if (loadflg) {
		printf("%-8s %8s %8s %8s %8s %8s %8s %8s %8s\n",
			"Header:", "code", "data", "heap",
			"types", "dtypes", "htypes", "symtab", "strings");
		printf("%08x %08x %08x %08x %08x %08x %08x %08x %08x\n",
			module.nl_magic, module.nl_code,
			module.nl_data, module.nl_heap,
			module.nl_types, module.nl_dtypes, module.nl_htypes,
			module.nl_symtab, module.nl_strings);
	}

	/*
	 * Scan heap, allocating entries for functors
	 */
	if (loadflg)
		printf("\nProcess functors:\n");
	for (nextw = w = heap, t = htypes; w < (Word*)types; w++,t++) {
		if (loadflg && w >= nextw)
			nextw = w + displayWord(heap, w, *t);
		if (*t == nFUNCSYM) {
			atom = atom_name(*w++); t++;
			arity = *w++; t++;
#ifdef sck_stab
			a = enterAtom(NON_DB_ATOM, atom, stab, (Atom *)NULL);
#else
			a = enterAtom(atom, stab, (Atom *)NULL);
#endif
			f = enterFunctor(arity, a);
			if(*w != lUNUSED) {
				if(f->f_sourceFile != NIL && testFlagOn(flgREDEFWARN))
					if(arity < 2 && strcmp(atom, "main") != 0
							&& mainRedefinedAlready[arity]) {
						mainRedefinedAlready[arity] = 1;
						fprintf(stderr,
							"Warning: main/%d multiply defined\n", arity);
					} else
						fprintf(stderr,
							"Warning: %s/%d redefined\n", atom, arity);
				f->f_code = (Program)(code + *w);
				f->f_codeType = fCOMPILED;
				f->f_sourceFile = SourceFile;
			}
		}
	}

	/*
	 * Rescan heap, filling entries for consts and labels in hash tables
	 */
	if (loadflg)
		printf("\nProcess hash tables:\n");
	for (nextw = w = heap, t = htypes; w < (Word*)types; w++,t++) {
		if (loadflg && w >= nextw)
			nextw = w + displayWord(heap, w, *t);
		switch (*t) {
		when nATOM:
#ifdef sck_stab
			*w = MakeAtom(enterAtom(NON_DB_ATOM, atom_name(*w), stab, NULL));
#else
			*w = MakeAtom(enterAtom(atom_name(*w), stab, NULL));
#endif
		when nSTRUCT:
			arity = proc_arity(*w);
			atom = proc_name(*w);
#ifdef sck_stab
			*w = MakeStrHeader(arity, enterAtom(NON_DB_ATOM, atom, stab, NULL));
#else
			*w = MakeStrHeader(arity, enterAtom(atom, stab, NULL));
#endif
		when nLABEL:
			*w = (Word)code + *w;
		when nFLOAT:
			*w = MakeFloat((Word)data +*w);
		when nINT32:
			*w = MakeInt32((Word)data +*w);
		}
	}

	/*
	 * Scan code+data, converting load-time values to run-time values
	 */
	if (loadflg) printf("\nProcess Code + Data:\n");
	for (nextw = w = code, t = types; w < top; w++,t++) {
		if (loadflg && w >= nextw)
			nextw = w + displayWord(code, w, *t);
		switch (*t) {
		when nOPCODE:
		case nREGS:
			lastop = w;
		when nSTRING:
		case nNUMBER:
		case nFVAL:
		case nINT32VAL:
		case nBIT32:
			/* ignore absolute values */
		when nATOM:
#ifdef sck_stab
			*w = MakeAtom(enterAtom(NON_DB_ATOM, atom_name(*w), stab, NULL));
#else
			*w = MakeAtom(enterAtom(atom_name(*w), stab, NULL));
#endif
		when nSTRUCT:
			arity = proc_arity(*w);
			atom = proc_name(*w);
#ifdef sck_stab
			*w = MakeStrHeader(arity, enterAtom(NON_DB_ATOM, atom, stab, NULL));
#else
			*w = MakeStrHeader(arity, enterAtom(atom, stab, NULL));
#endif
		when nLABEL:
			*w = (Word)code + *w;
		when nFLOAT:
			*w = MakeFloat((Word)data + *w);
		when nINT32:
			*w = MakeInt32((Word)data + *w);
		when nATLAB:
#ifdef sck_stab
			*w = (Word) enterAtom(NON_DB_ATOM, atom_name(*w), stab, NULL);
#else
			*w = (Word) enterAtom(atom_name(*w), stab, NULL);
#endif
		when nPROCLAB:
			arity = proc_arity(*w);
			atom = proc_name(*w);
#ifdef sck_stab
			f = enterFunctor(arity, enterAtom(NON_DB_ATOM, atom, stab, NULL));
#else
			f = enterFunctor(arity, enterAtom(atom, stab, NULL));
#endif
			*w = (Word)f;
		when nFUNCREF:
			fn = addressOfFunction(enterAtom(atom_name(*w), stab, NULL));
			*w = (Word)fn;
		when nHASHTAB:
			list = tab_to_list(heap, *w);
			deflt = *(list+1);
			list = (Word *)(*(list+2));
			*w = (Word)makeHashTable(list, lastop, deflt);
		when nTAGREF:
			*w = MakeRef((Word)code + *w);
		when nTAGLST:
			*w = MakeList((Word)code + *w);
		when nTAGCHR:
			*w = MakeString((Word)code + *w);
		when nTAGSTR:
			*w = MakeStr((Word)code + *w);
		when nTAGUREF:
			*w = MakeUnboundRef((Word)code + *w);
		when nTAGUDEL:
			*w = MakeUnboundDel((Word)code + *w);
		otherwise:
			panic("load: unrecognized code type");
		}
	}
	top = --w; /* for Nepolog testing */

	/*
	 * Finish up
	 */
	close(obj_file);
	free(heap);

	if (loadflg) printf("Load Done\n");

	return(1);
}

/*
 * alloc_work_areas:
 *	Allocate space for segments of object file & fill with data
 *	Assumes "talloc" always returns word-aligned addresses
 */
int
alloc_work_areas()
{
	int	perm_size;
	int	temp_size;

	perm_size = module.nl_code +
			module.nl_data;
	temp_size = module.nl_heap +
			module.nl_types +
			module.nl_dtypes +
			module.nl_htypes +
			module.nl_symtab +
			module.nl_strings ;

	/*
	 * Allocate space for code+data areas
	 * load() returns a pointer to these
	 */
	if ((bytes = (char *)talloc(perm_size)) == NULL)
		panic("load: out of memory");
	code = (Word *)(bytes);
	data = (Word *)(bytes += module.nl_code);
	top  = (Word *)(bytes += module.nl_data);
	if(read(obj_file, code, perm_size) != perm_size)
		return(0);

	/*
	 * Allocate space for temporary areas
	 */
	if ((bytes = (char *)malloc(temp_size)) == NULL)
		panic("load: out of memory");
	heap = (Word *)(bytes);
	types = (char *)(bytes += module.nl_heap);
	dtypes = (char *)(bytes += module.nl_types);
	htypes = (char *)(bytes += module.nl_dtypes);
	symtab = (SymTab *)(bytes += module.nl_htypes);
	strings = (char *)(bytes += module.nl_symtab);
	if(read(obj_file, heap, temp_size) != temp_size)
		return(0);
	return(1);
}


/*
 * Bits and pieces for building hash tables
 */

#define	hash_const(H)	(*(H+0))
#define	hash_label(H)	(*(H+1))
#define	hash_next(H)	(*(H+2))

typedef	struct htab {
	int	shift;
	int	type;
	int	nbits;
	int	size;
	int	*used;
	Word	**which;
} HashTab;

static int max_hash_depth;

/*
 * makeHashTable:
 *	Build a hash table from info in heap - return pointer to it
 */
Word *
makeHashTable(list, op, deflt)
Word *list;
Word *op;
Word *deflt;
{
	HashTab	*htab;
	Bool	check_dups();
	HashTab	*make_htab();
	extern int hashflg, hashdflg;

#ifdef DEBUG
	if (hashflg)
		fprintf(stderr, "default: %x\n", deflt);
#endif /* DEBUG */
	if (check_dups(list))
		panic("load: duplicate values in hash table");

	max_hash_depth = 0;
	if ((htab = make_htab(list, op, deflt, 0)) == (HashTab *)NULL)
		panic("load: can't make hash table");

#ifdef DEBUG
	if (hashdflg)
		printf("Max hash table depth: %d\n", max_hash_depth);
	if (hashflg)
		print_table(htab,0);
#endif /* DEBUG */

	*op = MakeIns3(eOpCode(*op), eArg1(*op), htab->shift, htab->nbits);

	list = (Word *)htab->which;
	free(htab->used);
	free(htab);
	return(list);
}


/*
 * tab_to_list:
 *	Convert from array of Words beginning at &area[offset]
 *	into a real linked list (i.e. make offsets into real pointers)
 */
Word *
tab_to_list(area,offset)
Word area[];
int offset;
{
	int	i, next;
	Word	*entry;

	for (i = offset; i != lUNUSED; i = next)
	{
		entry = &area[i];
		next = (Word)hash_next(entry);
		if (next == lUNUSED)
			hash_next(entry) = (Word)WordNULL;
		else
			hash_next(entry) = (Word)(&area[next]);
	}
	return(&area[offset]);
}

/*
 * check_dups:
 *	Check a list of values for duplicates (O(n^2) ... who cares)
 */
Bool
check_dups(list)
Word *list;
{
	Word	*tail;

	/*
	 * Scan list - checking for duplicates
	 * with each entry beyond the current one
	 */
	for ( ; list != WordNULL; list = (Word *)hash_next(list))
	{
		tail = (Word *)hash_next(list);
		for ( ; tail != WordNULL; tail = (Word *)hash_next(tail))
			if (hash_const(list) == hash_const(tail))
				return(TRUE);
	}
	return(FALSE);			
}

/*
 * make_htab:
 *	Derive a hash table from a value/label list
 *	"list" is a list of value/address pairs in the heap
 *	"op" is a pointer to the opc-word for the SOC/SOS
 *	  instruction used as a template for building the 
 *	  instructions to reference sub-hash-tables.
 */
HashTab *
make_htab(list, op, deflt, lev)
Word *list;
Word *op;
Word *deflt;
int lev;
{
	HashTab	*htab;
	Word	*l, *next, *ftab;
	int	nwords, nfloats, nvals, i, hash;
	int	nbits, size, mask;
	int	shift, bestshift;
	int	ncolls, bestcolls;
	int	nused, bestused;
	int	else_type;
	extern	int hashflg, hashdflg;

	else_type = (eOpCode(*op) == cSOCE || eOpCode(*op) == cSOSE);
#ifdef DEBUG
	if (hashflg) {
		printf("---MakeHtab\n");
		print_list(list);
	}
	if (hashdflg && lev > max_hash_depth)
		max_hash_depth = lev;
#endif /* DEBUG */

	/*
	 * Find how many value/label pairs
	 * Check whether there are any FLOATS
	 */
	nvals = 0;
	nfloats = 0;
	for (l = list; l != WordNULL; l = (Word *)hash_next(l))
	{
		Word c = hash_const(l);

		if (IsFloat(c))
			nfloats++;
		else if (IsInt32(c))
			panic("32-bit ints in hash table NYI");
		else
			nvals++;
	}
#if 0
fprintf(stderr, "nfloats:%d nvals:%d\n",nfloats,nvals);
#endif

	/*
	 * Determine smallest hash table size
	 */
	nbits = 0;
	size = 1;
	mask = 0;
	while (size < nvals) {
		nbits++;
		size <<= 1;
		mask <<= 1;
		mask |= 1;
	}

	/*
	 * Allocate and initialise hash table objects.
	 * The actual table (htab->which) may be one larger
	 *  than "size" indicates, since we store a FAIL opcode
	 *  at the end of the table for unused entries to point to
	 *  when we don't have a default label.
	 */
	htab = (HashTab *)malloc(sizeof(HashTab));
	if (htab == (HashTab *)NULL)
		return((HashTab *)NULL);
	htab->used = (int *)malloc(size*sizeof(int));
	if (htab->used == (int *)NULL)
		return((HashTab *)NULL);

#if 0
fprintf(stderr,"Size: %d\n",size);
#endif
	nwords = size + 2; /* "+2" for default label & nfloats counter*/
	if (else_type) 
		nwords += size;
	if (nfloats > 0)
		nwords += (2*nfloats);

	htab->which = (Word **)talloc((nwords)*sizeof(Word));
	if (htab->which == (Word **)NULL)
		return((HashTab *)NULL);

	if (deflt == (Word *)lUNUSED) {
#ifdef DEBUG
		if (hashflg)
			fprintf(stderr,"No default, using fail\n");
#endif /* DEBUG */
		deflt = (Word *)MakeIns0(cFAIL);
	}
	if (else_type) {
		htab->which[2*size] = deflt;
		htab->which[2*size+1] = (Word *)nfloats;
	}
	else {
		htab->which[size] = deflt;
		htab->which[size+1] = (Word *)nfloats;
	}

	/*
	 * Process floats into sorted (binary-search) table
	 */
	if (nfloats > 0)
	{
		ftab = (Word *)(&(htab->which[else_type ? 2*size+2 : size+2]));
		i = 0;
		for (l = list; l != WordNULL; l = (Word *)hash_next(l))
		{
			if (IsFloat(hash_const(l))) {
#if 0
fprintf(stderr,"put %08x @ %08x for i=%d\n", hash_const(l), &(ftab[i]), i);
#endif
				ftab[i] = hash_const(l);
				ftab[nfloats + i] = hash_label(l);
				i++;
			}
		}
	}

	/*
	 * If we only have floats, nvals will be zero
	 */
	if (nvals > 0)
	{
		/*
		 * Try all possible shifts for hash
		 */
		bestcolls = size;
		bestused = 0;
		for (shift = 0; shift < WORDSIZE; shift++)
		{
			for (i = 0; i < size; i++)
				htab->used[i] = 0;

			nused = 0;
			ncolls = 0;
			for (l = list; l != WordNULL; l = (Word *)hash_next(l))
			{
				if (!IsFloat(hash_const(l))) {
					hash = (hash_const(l) >> shift) & mask;
					htab->used[hash]++;
					if (htab->used[hash] == 1)
						nused++;
					if (htab->used[hash] == 2)
						ncolls++;
				}
			}
			if (nused > bestused) {
				bestused = nused;
				bestshift = shift;
			}

			if (ncolls == 0)
				break;
		}

		/*
		 * Set up the hash table,
		 * handle collisions with linked lists
		 */
		shift = bestshift;
		for (i = 0; i < size; i++) {
			htab->used[i] = 0;
			htab->which[i] = WordNULL;
		}

		for (l = list; l != WordNULL; l = next)
		{
			if (IsFloat(hash_const(l)))
				next = (Word *)hash_next(l);
			else {
				hash = (hash_const(l) >> shift) & mask;
#ifdef DEBUG
				if (hashflg)
					printf("%08x hashes to %d\n", hash_const(l), hash);
#endif /* DEBUG */
				htab->used[hash]++;
				next = (Word *)hash_next(l);
				hash_next(l) = (Word)(htab->which[hash]);
				htab->which[hash] = l;
				if (else_type)
					htab->which[size+hash] = (Word *)(hash_const(l));
#ifdef DEBUG
				if (hashflg)
					printf("which[%d]=%08x@%x\n",hash,hash_const(l),l);
#endif /* DEBUG */
			}
		}

		/*
		 * Rescan hash table,
		 * convert empty entries to FAIL ops,
		 * convert from pointers to heap nodes to actual labels,
		 * turn lists into new hash tables via recursive make_htab
		 */
#ifdef DEBUG
		if (hashflg)
			fprintf(stderr,"Final default: %x\n",deflt);
#endif /* DEBUG */
		for (i = 0; i < size; i++) {
			if (htab->used[i] == 0)
				htab->which[i] = deflt;
			else if (htab->used[i] == 1)
				htab->which[i] = (Word *)hash_label(htab->which[i]);
			else if (htab->used[i] > 1)
			{
				Word	*ocode;
				HashTab	*sub_table;

				sub_table = make_htab(htab->which[i],op,deflt,lev+1);
				ocode = (Word *)talloc(2*sizeof(Word));
				if (ocode == WordNULL)
					panic("load: can't make hash instruction");

				*ocode = MakeIns3(eOpCode(*op), eArg1(*op),
						sub_table->shift, sub_table->nbits);
				*(ocode+1) = (Word)sub_table->which;
				htab->which[i] = ocode;
				if (else_type)
					htab->which[size+i] = (Word *) 0;
				free(sub_table->used);
				free(sub_table);
			}
		}
	}

#if 0
fprintf(stderr, "Nwords:%d\n", nwords);
for (i = 0; i < nwords; i++)
	fprintf(stderr,"%d,%08x: %08x\n", i, &(htab->which[i]), htab->which[i]);
#endif
	/*
	 * Return information about hash table
	 */
	htab->size = size;
	htab->type = else_type;
	htab->shift = shift;
	htab->nbits = nbits;
	return(htab);
}

/*
 * print_list:
 *	Print out list of hash table entries
 */
print_list(list)
Word *list;
{
	for ( ; list != WordNULL; list = (Word *)hash_next(list))
		printf("    const:%08x label:L@%06x next:%x\n",
			hash_const(list), hash_label(list), hash_next(list));
}

/*
 * print_table:
 *	Print out hash table
 */
print_table(htab,level)
HashTab *htab;
int level;
{
	int	i,j, size, nfloats;
	Word	*entry, *ftab;

	size = htab->size;
	printf("Table @ %08x\nsize:%d shift:%d nbits:%d\n",
		htab, size, htab->shift, htab->nbits);
#if 1
for (i = 0, ftab = (Word *)htab->which; i < 20; i++)
fprintf(stderr, "tab[%2d]: %08x @ %08x\n", i, ftab[i], &(ftab[i]));
#endif
	for (i = 0; i < size; i++)
	{
		for (j = 0; j < level; j++)
			printf("\t");
		printf("%2d: ",i);
		entry = htab->which[i];
		if (htab->used[i] == 0)
			printf("unused\n");
		else if (htab->used[i] == 1)
			printf("const:%08x label:L@%06x next:%x\n",
			hash_const(entry), hash_label(entry), hash_next(entry));
		else {
#if 0
			/*
			 * We don't have a "HashTab" structure for
			 * subtables .. they are "installed" on creation
			 */
			entry = (Word *)(*entry + 1);
			print_table(entry, level+1);
#else
			printf("subtab@%x\n",entry);
#endif
		}
	}
	ftab = (Word *)(&(htab->which[htab->type ? 2*size+2 : size+2]));
	printf("\ndefault: %08x  nfloats: %d\n", *(ftab-2), *(ftab-1), ftab);
	nfloats = (int) *(ftab - 1);
	for (i = 0; i < nfloats; i++)
		printf("float %08x @ %08x\n", ftab[i], &(ftab[i]));
}

/*
 * outOfDate:
 *	Check whether a save or object file is out of date wrt 
 *	the interpreter "nep" or the assembler "na"
 *	Returns 1 if there are any problems or out of date, else 0
 */
int
outOfDate(fd, prog)
int fd;
char *prog;
{
	char	progname[MAXPATHLEN];
	struct	stat nupstat, objstat;

	strcpy(progname, PATHnubin);
	strcat(progname, prog);
	if (stat(progname, &nupstat) < 0) {
		fprintf(stderr,
			"Can't stat %s ... (next line may be junk)\n", progname);
		return(1);
	}
	if (fstat(fd, &objstat) < 0) {
		fprintf(stderr,
			"Can't stat object/save file ... (next line may be junk)\n");
		return(1);
	}

	if (objstat.st_mtime < nupstat.st_mtime)
		return(1);
	else
		return(0);
}
