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
#ifdef BSD4
#include <strings.h>
#else
#include <string.h>
#include <fcntl.h>
#endif
#include "defs.h"
#include "nltypes.h"

#ifdef DEBUG
#define	dbug(msg,obj)	{if (hashflg) \
			{ fprintf(stderr,"%s:",msg);\
			  displayTerm(stderr,(obj)); fprintf(stderr,"\n"); }}
#else /* DEBUG */
#define dbug(msg,obj)
#endif /* DEBUG */

#define	SUCCEED		1
#define	FAILURE		0

#define	BYTESperWORD	sizeof(Word)

#define	listHead(x)	((Object)(eRef(x)))
#define	listTail(x)	((Object)*((x)+1))

typedef struct lbl {
	int lbl_offset;
	Word *lbl_use;
}
Label;

typedef struct dlist {
	Word *d_use;
	struct dlist *d_next;
}
DataRef;

typedef struct htlist {
	int ht_offset;
	int ht_lastop;
	struct	htlist *ht_next;
}
HtabRef;

typedef struct htab {
	Word htab_const;
	Word htab_label;
	struct	htab *htab_next;
}
HashTabEntry;

typedef struct santa {
	Word *cl_code;
	int cl_cwords;
	Word *cl_data;
	int cl_dwords;
	struct	santa *cl_next;
}
Clause;

#define	functorName(x)	eCharStar(((Atom *)eFunctor(*x))->a_pname)
#define	atomName(x)	eCharStar(((Atom *)eRef(x))->a_pname)

static void initPred(), endPred(), loadPred(), loadClause();
static void codeCheck(), dataCheck(), constCheck(), tableCheck();
static void codeLoad(), dataLoad();
static Word constLoad();
static void initLabels(), addLabel(), bindLabels(), bindDataRefs();
static void buildHashTables();
static Word *initTableList();
static void freeTableList();

static	Word	*code;		/* start of code segment for predicate */
static	Word	*data;		/* start of data segment for predicate */

static	int	nCodeWords;	/* size of code for predicate */
static	int	nDataWords;	/* size of data for predicate */

static	int	codeOffset;	/* current offset within code space */
static	int	dataOffset;	/* current offset within data space */

static	Word	*xcode;		/* current Word in code segment */
/* JWS static	Word	*xdata; */		/* current Word in data segment */

static	int	lastop;		/* offset where last instruction began */

/* JWS static	Word *top; */		/* top of code+data segment */

static	Atom *predicate = (Atom *)NULL; /* Atom describing predicate functor */
static	char *pred_name = "BUG"; /* string giving name of predicate functor */
static	int	pred_arity = -1;/* arity of predicate */
static	Clause	*clauses;	/* linked list of code segments for clauses */

static	HtabRef *hashtabs;	/* linked list of hash table uses */

static	DataRef *dataobjs;	/* list of relocatable data refs */

static	Label	*labels;	/* Table for local program labels */
static	int	MAXLABELS = 999;/* Maximum # labels ... parameterise? */

extern	int	hashflg;	/* flag for printing debugging info */
						/* name comes from hash-table debugging */
						/* now it does more general iload debugging */


static void
freeing(what, m)
char *what;
char *m;
{
	/*fprintf(stderr, "Freeing %s at 0x%x\n", what, m);*/
	free(m);
}

static char *
mallocing(n)
unsigned n;
{
	register char *m;

	m = malloc(n);
	/*fprintf(stderr, "Malloced %d bytes at 0x%x\n", n, m);*/
	return(m);
}

static Object predicate_terminator[4];

/*
 * iload:
 *	Load a LIST of NU-Prolog byte-codes representing one clause
 *	If LIST is [], link all clauses together to form Predicate
 */
p_iload(pred, arity, code_list)
reg Object pred, arity, code_list;
{
	Atom *p_atom;
	char *p_name;
	int p_arity;

	/*
	 * Check pred/arity arguments
	 */
	DeRef(pred);
	if (!IsAtom(pred))
		panic("iload: first arg not an atom");
	p_atom = (Atom *)eRef(pred);
	p_name = atomName(pred);

	DeRef(arity);
	if (!IsSmallInt(arity))
		panic("iload: second arg not an integer");
	p_arity = eSmallInt(arity);

	/*
	 * Try to start new pred before old one loaded fully
	 * Throw away old one, and begin new one right away
	 */
	if (predicate != (Atom *)NULL && 
		(p_atom != predicate || p_arity != pred_arity)) {
		fprintf(stderr, "iload: incomplete definition of %s/%d\n",
			pred_name, pred_arity);
		endPred();
	}

	/*
	 * Start definition of new predicate
	 */
	if (predicate == (Atom *)NULL)
		initPred(p_atom, p_name, p_arity);

	/*
	 * If third arg is [], then we do final linking of clauses in pred
	 * Else we use arg to create a code segment for a new clause
	 */
	DeRef(code_list);
	if (IsList(code_list))
		loadClause(code_list);
	else if (IsNIL(code_list)) {
		predicate_terminator[0] = StarToList(predicate_terminator + 2);
		predicate_terminator[1] = NIL;
		predicate_terminator[2] = MakeSmallInt(cLAST);
		predicate_terminator[3] = NIL;
		loadClause(StarToList(predicate_terminator));
		loadPred(clauses);
		endPred();
	} else
		panic("iload: third arg not a list");

	return(SUCCEED);
}

/*
 * initPred:
 *	Perform some initialisation at start of new Predicate definition
 */
static void
initPred(p_atom, p_name, p_arity)
Atom *p_atom;
char * p_name;
int p_arity;
{
	dataobjs = (DataRef *)NULL;
	hashtabs = (HtabRef *)NULL;
	clauses = (Clause *)NULL;
	initLabels();

	predicate = p_atom;
	pred_name = p_name;
	pred_arity = p_arity;

	nCodeWords = nDataWords = 0;
	codeOffset = dataOffset = 0;
}

/*
 * endPred:
 *	Clean up after the end of a Predicate definition
 */
static void
endPred()
{
	reg Clause *c, *cnext;
	reg HtabRef *h, *hnext;
	reg DataRef *d, *dnext;

	/*
	 * freeDataRef(dataobjs);
	 */
	d = dataobjs;
	while( d != (DataRef *)NULL) {
		dnext = d->d_next;
		freeing("DataRef", (char *)d);
		d = dnext;
	}

	/*
	 * freeHtabRef(htlist);
	 */
	h = hashtabs;
	while (h != (HtabRef *)NULL) {
		hnext = h->ht_next;
		freeing("HTabRef", (char *)h);
		h = hnext;
	}

	/*
	 * freeClauses(clauses);
	 */
	c = clauses;
	while (c != (Clause *)NULL) {
		cnext = c->cl_next;
		freeing("Clause Code", (char *)c->cl_code);
		freeing("Clause", (char *)c);
		c = cnext;
	}

	/*
	 * freeLabels();
	 */
	freeing("Labels", (char *)labels);

	predicate = (Atom *)NULL;
	pred_name = "BUG";
	pred_arity = -1;
}

/*
 * loadPred:
 *	Combine code segments for individual clauses together
 *	in their final resting place as a brand new Predicate
 */
static void
loadPred(clauses)
reg Clause *clauses;
{
	reg Word *clw, *cle;
	reg Word *xcode, *xdata;
	reg Clause *cl;
	Functor *f;
	int nbytes;

	/*
	 * Create space for predicate code
	 */
#ifdef DEBUG
	if (hashflg) {
		fprintf(stderr,"code:%d@%x data:%d@%x\n",nCodeWords,code,nDataWords,data);
		fflush(stderr);
	}
#endif /* DEBUG */
	nbytes = (nCodeWords+nDataWords) * sizeof(Word);
	if ((code = (Word *)talloc(nbytes)) == (Word *)NULL)
		panic("iload: can't allocate code/data space");
	xcode = code;
	xdata = data = code + nCodeWords;
/* JWS	top = data + nDataWords; */

	/*
	 * bind the new code to the functor/arity structure
	 */
#ifdef DEBUG
	if (hashflg)
		fprintf(stderr,"Bind %s/%d to code@%x\n",pred_name,pred_arity,code);
#endif /* DEBUG */
	f = enterFunctor(pred_arity, predicate);
	f->f_codeType = fCOMPILED;
	if (f->f_sourceFile != NIL && testFlagOn(flgREDEFWARN))
		fprintf(stderr, "Warning: %s/%d redefined\n",
				pred_name, pred_arity);
	f->f_code = (Program)code;
	f->f_sourceFile = SourceFile;

	bindDataRefs(dataobjs, data);

	bindLabels(labels, code);

	/*
	 * Traverse list of code segments,
	 * transferring to main Predicate code segment
	 */
	for (cl = clauses; cl != (Clause *)NULL; cl = cl->cl_next) {
		clw = cl->cl_code;
		cle = cl->cl_code + cl->cl_cwords;
		while (clw < cle)
			*xcode++ = *clw++;
		clw = cl->cl_data;
		cle = cl->cl_data + cl->cl_dwords;
		while (clw < cle)
			*xdata++ = *clw++;
	}

	buildHashTables(hashtabs, code);

	if(xcode != code + nCodeWords + nDataWords) {		/* JWS */
		printf("xcode = 0x%x != 0x%x\n", xcode, code + nCodeWords + nDataWords);
		fflush(stdout);
		panic("Iload Error");
	}
}

/*
 * loadClause:
 *	Place byte-codes into code segment for a single clause
 */
static void
loadClause(code_list)
reg Object code_list;
{
	reg Object c;
	reg Clause *cl;
	int nbytes;
	int cCodeWords;
	int cDataWords;
	Clause *newclause;

	/*
	 * Remember code/data word count at start of clause,
	 * so we can work out how much code/data there is
	 */
	cCodeWords = nCodeWords;
	cDataWords = nDataWords;

	/*
	 * First pass:
	 * traverse byte-code list, determining sizes,
	 *	resolving external addresses, etc. etc.
	 *	BUT not labels internal to predicate
	 */
/*dbug("code",code_list);*/
	for (c = deRef(code_list); !IsNIL(c); ) {
		reg Word car;

		c = (Object)eRef(c);
		car = ((Object *) c)[0]; DeRef(car);
		c = ((Object *) c)[1]; DeRef(c);
dbug("head",car);
/*dbug("tail",c);*/
		switch (eType(car))
		{
		when tLST:
			codeCheck(car); /* check+count bytecode/regs */
		when tSTR:
			dataCheck(car); /* check+count data */
		otherwise:
			panic("iload: bad object in code list");
		}
	}

	/*
	 * Create temp space for clause code
	 */
	cCodeWords = nCodeWords - cCodeWords;
	cDataWords = nDataWords - cDataWords;
	if(nDataWords != 0)
		panic("nDataWords != 0");
	nbytes = (cCodeWords+cDataWords) * sizeof(Word);
	if ((code = (Word *)mallocing(nbytes)) == (Word *)NULL)
		panic("iload: can't allocate code/data space for clause");
	xcode = code;
/*	JWS xdata = data = code + cCodeWords; */

	/*
	 * Second pass:
	 * traverse byte-code list, installing code & data
	 */
	for (c = deRef(code_list); !IsNIL(c); ) {
		reg Word car;

#if 0
		Word *xx = xcode;
#endif
		c = (Object)eRef(c);
		car = ((Object *) c)[0]; DeRef(car);
		c = ((Object *) c)[1]; DeRef(c);
dbug("head",car);
/*dbug("tail",c);*/
		switch (eType(car)) {
		when tLST:
			codeLoad(car); /* load bytecode/regs */
#if 0
			fprintf(stderr,"  %x code:%x\n", xx, *xx);
#endif
		when tSTR:
			dataLoad(car); /* load address */
#if 0
			fprintf(stderr,"  %x data:%x\n", xx, *xx);
#endif
		otherwise:
			/* no need to check on this pass */;
		}
	}

	if(xcode != code + cCodeWords + cDataWords) {		/* JWS */
		printf("xcode = 0x%x != 0x%x\n", xcode, code + cCodeWords + cDataWords);
		fflush(stdout);
		panic("Iload Error");
	}

	/*
	 * If we get this far, install code for this clause
	 * onto list of code segments for clauses of the Predicate
	 */
	if ((newclause = (Clause*)mallocing(sizeof(Clause))) == (Clause *)NULL)
		panic("iload: can't allocate clause space");
	newclause->cl_code = code;
	newclause->cl_cwords = cCodeWords;
	newclause->cl_data = data;
	newclause->cl_dwords = cDataWords;
	newclause->cl_next = (Clause *)NULL;
	if (clauses == (Clause *)NULL)
		clauses = newclause;
	else {
		cl = clauses;
		while (cl->cl_next != (Clause *)NULL)
			cl = cl->cl_next;
		cl->cl_next = newclause;
	}
}

/*
 * codeCheck
 *	Work out sizes of code segments
 */
static void
codeCheck(c)
reg Object c;
{
	reg int nreg;

	nreg = 0;
	for (c = deRef(c); !IsNIL(c); )
	{
		reg Word car;

		c = (Object) eRef(c);
		car = ((Object *) c)[0]; DeRef(car);
		c = ((Object *) c)[1]; DeRef(c);
		if (!IsSmallInt(car) || eSmallInt(car) < 0 || eSmallInt(car) > 255)
			panic("iload: invalid bytecode/register");
		nreg++;
		if (nreg == BYTESperWORD) {
			nreg = 0;
			nCodeWords++;
		}
	}
	if (nreg != 0)
		nCodeWords++;
}

/*
 * dataCheck
 *	Work out sizes of data objects and addresses of labels
 */
static void
dataCheck(c)
reg Object c;
{
	reg Word *cp;
	reg Atom *func;
	reg int args;
	reg Word *arg, *arg2;

	cp = eRef(c);
	func = (Atom *)eFunctor(*cp);
	args = eNArgs(*cp);
	if (func == &SymColon) /* program label */
	{
dbug("label",c);
		if (args != 1)
			panic("iload: invalid label");
		arg = cp+1;
		if (!IsSmallInt(*arg))
			panic("iload: invalid label");
		addLabel(eSmallInt(*arg),nCodeWords);
	} else if (func == &SymDollar) /* constant or functor */
	{
dbug("const",c);
		constCheck(c);
		nCodeWords++;
	} else if (func == &SymAmpersand) /* label or predicate reference */
	{
dbug("ref",c);
		switch(args) {
		when 1:
			arg = cp+1;
			if (!IsSmallInt(*arg))
				panic("iload: invalid label reference");
			nCodeWords++;
		when 2:
			arg = cp+1;
			arg2 = cp+2;
			if (!IsAtom(*arg))
				panic("iload: invalid predicate");
			if (!IsSmallInt(*arg2))
				panic("iload: invalid predicate arity");
			nCodeWords++;
		otherwise:
			panic("iload: invalid label/predicate reference");
		}
	}
	else if (func == &SymCall) /* call to "C" function */
	{
dbug("cfun",c);
		if (args != 1)
			panic("iload: invalid C function reference");
		arg = cp+1;
		if (!IsBlock(deRef(*arg)))
			panic("iload: block expected for C function");
		nCodeWords++;
	}
	else if (func == &SymHash) /* tagged object */
	{
dbug("obj",c);
		arg = cp+1;
		if (!IsList(*arg) && !IsStr(*arg) && !IsString(*arg))
			panic("iload: invalid tagged object");
		nCodeWords++;
/* JWS		nDataWords += sizeOfTerm(cp); */
	}
	else if (func == &SymTable) /* hash table */
	{
dbug("#tab",c);
		if (args != 1)
			panic("iload: invalid hash table");
		arg = cp+1;
		if (!IsList(*arg))
			panic("iload: invalid hash table");
		tableCheck((Object)*arg);
		nCodeWords++;
	}
	else
		panic("iload: bad address field");
}

/*
 * constCheck:
 *	check whether a term looks like '$'(<const>) or '$'(<atom>,<arity>)
 */
static void
constCheck(c)
reg Object c;
{
	reg Word *cp, *arg, *arg2;
	reg Atom *func;
	reg int args;

	cp = eRef(c);
	func = (Atom *)eFunctor(*cp);
	args = eNArgs(*cp);
	if (func != &SymDollar)
		panic("iload: invalid constant functor");
	switch (args) {
	when 1:
		arg = cp+1;
		if (!IsConst(*arg))
			panic("iload: invalid constant");
	when 2:
		arg = cp+1;
		arg2 = cp+2;
		if (!IsAtom(*arg))
			panic("iload: invalid functor in constant");
		if (!IsSmallInt(*arg2))
			panic("iload: invalid functor arity in constant");
	otherwise:
		panic("iload: invalid const/functor");
	}
}

/*
 * tableCheck:
 *	check validity of hash table
 */
static void
tableCheck(tab)
reg Object tab;
{
	reg Object t;
	reg Word *tp;
	reg Word *cp;
	reg Atom *func;
	reg Word *arg, *arg2;

	for (t = tab; !IsNIL(t); t = listTail(tp))
	{
		/*
		 * Each element in list should look like:
		 *  ':'('$'(<const>), <number>) OR
		 *  ':'('$'(<const>,<arity>), <number>)
		 */
		DeRef(t);
		t = listHead(t);
dbug("#elem",t);
		tp = eRef(t);
		if (!IsStr(*tp))
			panic("iload: invalid hash table entry");
		cp = eRef(*tp);
		func = (Atom *)eFunctor(*cp);
		if (func != &SymColon)
			panic("iload: invalid functor in hash table entry");
		if (eNArgs(*cp) != 2)
			panic("iload: invalid args in hash table entry");
		arg = cp+1; /*DeRef(*arg);*/
		arg2 = cp+2; /*DeRef(*arg2);*/
		constCheck(*arg);
		if (!IsSmallInt(*arg2))
			panic("iload: invalid label in hash table entry");
	}
}

/*
 * codeLoad:
 *	load bytcode/regs into word
 *	error checking was done in the first pass
 */
static void
codeLoad(c)
reg Object c;
{
	reg Object r;
	reg Word *rp;
	reg Word *rr;
	reg int v, nreg;
	Word regs[BYTESperWORD];

	lastop = codeOffset;
	rr = regs;
	nreg = 0;
	for (r = c; !IsNIL(r); r = listTail(rp))
	{
		DeRef(r);
		r = listHead(r);
		rp = eRef(r);
		v = eInt(*rp);
		if (v < 0 || v > 255)
			panic("iload: invalid bytecode/register");
		*rr++ = v;
		nreg++;
		if (nreg == BYTESperWORD) {
			*xcode++ = MakeIns3(regs[0],regs[1],regs[2],regs[3]);
			codeOffset++;
			rr = regs;
			nreg = 0;
		}
	}
	if (nreg != 0)
		codeOffset++;
	switch (nreg) {
	when 0: /* ignore */;
	when 1: *xcode++ = MakeIns0(regs[0]);
	when 2: *xcode++ = MakeIns1(regs[0],regs[1]);
	when 3: *xcode++ = MakeIns2(regs[0],regs[1],regs[2]);
	/* need more cases if BYTESperWORD bigger */
	}
}

/*
 * dataLoad:
 *	load data/address into word
 */
static void
dataLoad(c)
reg Object c;
{
	reg Word *cp;
	reg Atom *func;
	reg Word *addr;
	reg int args;
	reg Word *arg, *arg2;

	cp = eRef(c);
	func = (Atom *)eFunctor(*cp);
	args = eNArgs(*cp);
	if (func == &SymColon) /* program label */
	{
		/* ignore on this pass */
	}
	else if (func == &SymDollar) /* constant or functor */
	{
		*xcode++ = constLoad(cp);
		codeOffset++;
	}
	else if (func == &SymAmpersand) /* label or predicate reference */
	{
		switch (args) {
		when 1:
			/* insert reference to program label */
			arg = cp+1;
			addr = xcode;
			*xcode++ = (Word)(labels[eInt(*arg)].lbl_use);
			labels[eInt(*arg)].lbl_use = addr;
			codeOffset++;
		when 2:
			/* make predicate reference in code space */
			arg = cp+1;
			arg2 = cp+2;
			*xcode++
				= (Word)enterFunctor(eInt(*arg2), (Atom *) eRef(*arg));
			codeOffset++;
		}
	}
	else if (func == &SymCall) /* call to "C" function */
	{
		arg = cp+1;
		*xcode++ = (Word)(eRef(deRef(*arg)));
		codeOffset++;
	}
	else if (func == &SymHash) /* tagged object */
	{
		int dummy;

		/*
		 * make copy of object in data area 
		 */
		arg = cp+1;
		switch (eType(deRef(*arg))) {
		when tSTR:
		case tCHR:
		case tLST:
			*xcode++ = (Word) copy(iDATA, (Object *) NULL, *arg, &dummy);
		otherwise:
			/* should not happen !*/
			panic("iload: bad tag type");
		}
		codeOffset++;
		if(dummy != 0)
			panic("Variables in supposedly ground term to iload");
	}
	else if (func == &SymTable) /* hash table */
	{
		/*
		 * build a hash table and
		 * put reference to hash table in codeword
		 */
		arg = cp+1;
		addHashTab(codeOffset, lastop);
		*xcode++ = (Word)arg;
		codeOffset++;
	}
	else
		panic("iload: bad address field");
}

/*
 * constLoad:
 *	form load value for a constant or struct header
 */
static Word
constLoad(cp)
reg Word *cp;
{
	reg Word *arg, *arg2;

	switch (eNArgs(*cp)) {
	when 1:
		/* copy constant into code space */
		arg = cp+1;
		return((Word)*arg);
	when 2:
		/* make struct functor reference in code space */
		arg = cp+1;
		arg2 = cp+2;
		return((Word)MakeStrHeader(eInt(*arg2), *arg));
	default:
		panic("constLoad()");
	}
}

/*
 * initLabels:
 *	create and clear Label Table for new predicate
 */
static void
initLabels()
{
	reg Label *l, *endlabels;

	if ((labels = (Label *)mallocing(MAXLABELS*sizeof(Label))) == (Label *)NULL)
		panic("iload: can't make local labels table");
	endlabels = &labels[MAXLABELS];
	for (l = labels; l < endlabels; l++) {
		l->lbl_offset = -1;
		l->lbl_use = (Word *)NULL;
	}
}

/*
 * addLabel:
 *	insert label plus its relative address in Label Table
 */
static void
addLabel(l,a)
reg int l;
reg int a;
{
	if (l > MAXLABELS)
		panic("iload: toooo many labels (max:999)");
	if (labels[l].lbl_offset != -1)
		panic("iload: duplicate label");
	
	labels[l].lbl_offset = a;
}

/*
 * bindLabels:
 *	Convert labels from relocatable to absolute
 */
static void
bindLabels(labs, code)
Label *labs;
Word *code;
{
	reg Word *w, *wnext;
	reg Label *l, *endlabels;
	static char err[128];

	endlabels = &labels[MAXLABELS];
	for (l = labs; l < endlabels; l++) {
		if (l->lbl_offset == -1) {
			if (l->lbl_use != (Word *)NULL) {
				sprintf(err,"iload: undefined label in %s/%d",
						pred_name, pred_arity);
				panic(err);
			}
		}
		else {
			if (l->lbl_use == (Word *)NULL)
				fprintf(stderr, "Warning: unused label in %s/%d\n",
						pred_name, pred_arity);
			else {
				w = l->lbl_use;
				while (w != (Word *)NULL) {
					wnext = (Word *)*w;
					*w = (Word)(l->lbl_offset + code);
					w = wnext;
				}
			}
		}
	}
}

/*
 * addDataRef:
 *	Add entry to list of relocatable data object refs
 */
addDataRef(ptr)
Word *ptr;
{
	reg DataRef *newref;

	if ((newref = (DataRef *)mallocing(sizeof(DataRef))) == (DataRef *)NULL)
		panic("iload: no space for data ref list");
	newref->d_use = ptr;
	newref->d_next = dataobjs;
	dataobjs = newref;
}

/*
 * bindDataRefs:
 *	Make relocatable refs to objects absolute
 */
static void
bindDataRefs(objs, data)
DataRef *objs;
Word *data;
{
	reg DataRef *d;
	reg Word *addr;

	for (d = objs; d != (DataRef *)NULL; d = d->d_next) {
		addr = d->d_use;
		*addr = (Word)(data + *addr);
	}
}

/*
 * buildHashTables:
 *	Traverse hash table list, building and installing tables
 */
static void
buildHashTables(tabs, code)
HtabRef *tabs;
Word *code;
{
	reg HtabRef *h;
	reg Word *addr, *tab, *op;
	Word *initTableList();
	extern Word makeHashTable();

	for (h = tabs; h != (HtabRef *)NULL; h = h->ht_next) {
		addr = code + h->ht_offset;
		op = code + h->ht_lastop;
		tab = initTableList((Object)*addr);
		*addr = (Word)makeHashTable(tab, op, (Word *)lUNUSED);
		freeTableList(tab);
	}
}

/*
 * addHashTab:
 *	Add a new entry in list of hash tables to be built later
 */
addHashTab(offset, lastop)
int offset, lastop;
{
	reg HtabRef *newhash;

	if ((newhash = (HtabRef *)mallocing(sizeof(HtabRef))) == (HtabRef *)NULL)
		panic("iload: no space for hash tab list");
	newhash->ht_offset = offset;
	newhash->ht_lastop = lastop;
	newhash->ht_next = hashtabs;
	hashtabs = newhash;
}

/*
 * initTableList:
 *	Convert from NU-Prolog term into a linked list
 *	which looks like part of the heap from a `.no' file
 */
static Word *
initTableList(term)
reg Object term;
{
	reg Object t;
	reg Word *tp;
	reg Word *cp;
	reg Word *arg, *arg2;
	reg HashTabEntry *htab_list, *new;

	htab_list = (HashTabEntry *)NULL;
	for (t = term; !IsNIL(t); t = listTail(tp))
	{
		/*
		 * Each element in list should look like:
		 *  ':'('$'(<const>), <number>) OR
		 *  ':'('$'(<const>,<arity>), <number>)
		 */
		DeRef(t);
		t = listHead(t);
		tp = eRef(t); 
		cp = eRef(*tp); /* skip ':' */
		arg = cp+1;
		arg2 = cp+2;
		if ((new = (HashTabEntry *)mallocing(sizeof(HashTabEntry)))
							== (HashTabEntry *)NULL)
			panic("iload: out of memory for hash table");
		new->htab_const = constLoad(eRef(*arg));
		new->htab_label = labels[eInt(*arg2)].lbl_offset;
		new->htab_next = htab_list;
		htab_list = new;
	}
	return((Word *)htab_list);
}

/*
 * freeTableList:
 *	Clean up linked list used for hash table
 */
static void
freeTableList(list)
Word *list;
{
	/* BUG!  Didn't John ever do this? */
}
