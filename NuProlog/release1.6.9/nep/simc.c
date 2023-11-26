/*
 * simc.c  -  functions to access external database
 *	      using superimposed codeword indexing scheme
 *
 * Copyright (C) 1986, 1987, The University of Melbourne
 *
 * Author: John Shepherd
 */

#include "mu.h"
#include "db.h"
#include <ctype.h>

/* debugging code */
#if 0
#define	panic(msg)	exit(fprintf(stderr,"Panic: %s\n",msg))
#define	dbug(msg,obj)	{ fprintf(stderr,"%s:",msg);\
			  displayTerm(stderr,(obj)); fprintf(stderr,"\n"); }
#else
#define dbug(x,y)
#endif


/* Call the various dsimc functions from Prolog. */
int
p_dsimc(f, X1, X2, X3, X4, X5)
int f;
Object *X1, *X2, *X3, *X4, *X5;
{
	switch(f) {
	when 0:
		return(p_dsimc_open(*X1, *X2, *X3, X4));
	when 1:
		return(p_dsimc_free(*X1));
	when 2:
		return(p_dsimc_cv(*X1, *X2, X3));
	when 3:
		return(p_dsimc_sfbquery(*X1, *X2, *X3, *X4, X5));
	when 4:
		return(p_dsimc_sfbquery(*X1, *X2, NULL, *X3, X4));
	/* Necessary?  BUG!
	when 5:
		return(p_dsimc_end(*X1));
	when 6:
		return(p_dsimc_abort(*X1));
	*/
	break;
	default:
		panic("Illegal dsimc service requested.");
		return(0);
	}
}

/*
 * p_dsimc_open:
 *	Open a predicate in a database and allocate some buffers
 */
p_dsimc_open(db, pred, arity, Buffers)
register Object db, pred, arity, *Buffers;
{
	Word *buf;
	char *db_name, *rel_name;
	int rel_arity;
	char msg[100];

	DeRef(db);
	if (!IsAtom(db))
		panic("simc_open: first arg not an atom");
	db_name = AtomToString(db);

	DeRef(pred);
	if (!IsAtom(pred))
		panic("simc_open: second arg not an atom");
	rel_name = AtomToString(pred);

	DeRef(arity);
	if (!IsSmallInt(arity))
		panic("simc_open: third arg not an integer");
	rel_arity = eSmallInt(arity);

	if ((buf = (Word *)pool_open(db_name, rel_name, rel_arity)) == (Word *)0) {
		sprintf(msg, "can't open relation %s in database %s\n",
				rel_name, db_name);
		panic(msg);
	}
	*Buffers = MakeBlock(buf);
	return(SUCCEED);
}

/*
 * p_dsimc_free:
 *	Free up buffers used by an SFB query
 */
p_dsimc_free(buffers)
register Object buffers;
{
	/*
	 * Eventually, this may re-organise things in the memory
	 * management system so that the buffers in the pool can
	 * be easily re-used
	 * For the present, we just let nature take its course;
	 * they will be re-used if needed, regardless of SFB
	 */
	return(SUCCEED);
}

/*
 * p_dsimc_cv:
 *	Return choice vectors of relation
 *	for the Prolog part of SFB to use
 */
p_dsimc_cv(buf, qry, CVs)
register Object buf, qry, *CVs;
{
	char *query;
	static char template[BUFSIZ];

	DeRef(buf);
	if (!IsBlock(buf))
		panic("dsimc_cv: invalid buffers\n");

dbug("qry",qry);
	DeRef(qry);
	if (!IsString(qry))
		panic("dsimc_cv: invalid query string");
	query = eCharStar(qry);
#if 0
fprintf(stderr,"*qry:%s:\n",query);
#endif

	/* BUG!  DeRef(*CVs); */

	/*
	 * Grab template from relation structure
	 * and convert into form useful for Prolog
	 */
	rel_template(eRef(buf), query, template);

	*CVs = MakeString(template);
dbug("cv",*CVs);
	return(SUCCEED);
}

/* Call the various simc functions from Prolog. */
int
p_simc(f, X1, X2, X3, X4)
int f;
Object *X1, *X2, *X3, *X4;
{
	switch(f) {
	when 0:
		return(p_simc_hash(*X1, *X2, *X3, X4));
	when 1:
		return(p_simc_query(*X1, *X2, *X3, X4));
	when 2:
		return(p_simc_next(*X1, X2));
	when 3:
		return(p_simc_end(*X1));
	when 4:
		return(p_simc_assert(*X1, *X2));
	when 5:
		return(p_simc_delete(*X1));
	when 6:
		return(p_simc_abort(*X1));
	break;
	default:
		panic("Illegal simc service requested.");
		return(0);
	}
}

/*
 * p_simc_query:
 *	Initiate query from external SIMC database
 */
p_simc_query(db, qry, op, Qdesc)
register Object db, qry, op, *Qdesc;
{
	unsigned int f, opn;
	char *dbname, *query;

dbug("db",db);
	DeRef(db);
	if (!IsAtom(db))
		panic("simc_query: first arg not an atom");
	dbname = AtomToString(db);
#if 0
fprintf(stderr,"*db:%s:\n",dbname);
#endif

dbug("qry",qry);
	DeRef(qry);
	if (!IsString(qry))
		panic("simc_query: second arg not a string");
	query = eCharStar(qry);
#if 0
fprintf(stderr,"*qry:%s:\n",query);
#endif

dbug("op",op);
	DeRef(op);
	if (!IsSmallInt(op))
		panic("simc_query: third arg not an integer");
	opn = eSmallInt(op);
#if 0
fprintf(stderr,"*op:%s:\n",opn);
#endif

	/* BUG!  DeRef(*Qdesc); */
	/* no need to check var args */

	if ((f = trans_open(dbname, query, opn, 0)) == 0)
		panic("can't open database/relation");
	*Qdesc = MakeBlock(f);
	return(SUCCEED);
}

/*
 * p_simc_hash:
 *	Return cluster value for a term
 */
p_simc_hash(db, qry, op, Cluster)
register Object db, qry, op, *Cluster;
{
	unsigned int f, opn;
	char *dbname, *query;
	int hash_value();

dbug("db",db);
	DeRef(db);
	if (!IsAtom(db))
		panic("simc_hash: first arg not an atom");
	dbname = AtomToString(db);
#if 0
fprintf(stderr,"*db:%s:\n",dbname);
#endif

dbug("qry",qry);
	DeRef(qry);
	if (!IsString(qry))
		panic("simc_hash: second arg not a string");
	query = eCharStar(qry);
#if 0
fprintf(stderr,"*qry:%s:\n",query);
#endif

dbug("op",op);
	DeRef(op);
	if (!IsSmallInt(op))
		panic("simc_hash: third arg not an integer");
	opn = eSmallInt(op);
#if 0
fprintf(stderr,"*op:%s:\n",opn);
#endif

	/* BUG!  DeRef(*Cluster); */
	/* no need to check var args */

	if ((f = trans_open(dbname, query, opn, 0)) == 0)
		panic("can't open database/relation");
	*Cluster = MakeSmallInt(hash_value(f));
	trans_close((Object *) f);
	return(SUCCEED);
}

/*
 * p_dsimc_sfbquery:
 *	Initiate query from external SIMC database
 */
p_dsimc_sfbquery(buf, qry, vec, op, Qdesc)
register Object buf, qry, vec, op, *Qdesc;
{
	unsigned int f;
	int opn;
	char *query, *sfbvec;
	char buffer[41];

dbug("db",buf);
	DeRef(buf);
	if (!IsBlock(buf))
		panic("dsimc_sfbquery: first arg not a buffer pool");

dbug("qry",qry);
	DeRef(qry);
	if (!IsString(qry))
		panic("dsimc_sfbquery: second arg not a string");
	query = eCharStar(qry);

	if (vec == (Object)NULL)
		sfbvec = NULL;
	else {
dbug("vec",vec);
		DeRef(vec);
		if (IsString(vec))
			sfbvec = eCharStar(vec);
		else if (IsList(vec)) {
			listToString(vec, buffer, buffer+40, 1);
			sfbvec = buffer;
		}
		else
			panic("dsimc_sfbquery: third arg not a string or []");
	}
#if 0
fprintf(stderr,"sfbvec:%s\n",buffer);
#endif

dbug("op",op);
	DeRef(op);
	if (!IsSmallInt(op))
		panic("dsimc_sfbquery: third arg not an integer");
	opn = eSmallInt(op);
#if 0
fprintf(stderr,"*op:%s:\n",opn);
#endif

	/* BUG!  DeRef(*Qdesc); */
	/* no need to check var args */

	if ((f = trans_sfbquery(eRef(buf), query, opn, sfbvec)) == 0)
		panic("dsimc_sfbquery: can't commence transaction");
	*Qdesc = MakeBlock(f);
	return(SUCCEED);
}

/*
 * p_simc_next:
 *	Fetch next record during SIMC database query
 */
p_simc_next(trans, Next)
register Object trans, *Next;
{
	char *answer;
	char *trans_fetch();

	DeRef(trans);
	if (!IsBlock(trans))
		panic("simc_next: bad transaction descriptor");

	/* BUG!  DeRef(*Next); */
	/* no need to check var args */

	if ((answer = trans_fetch(eRef(trans))) == NULL)
#if 0
		answer = "?-db_end.";
#else
		answer = "dbEnd";
#endif
	*Next = MakeString(answer);
	return(SUCCEED);
}

/* 
 * p_simc_assert:
 *	Insert a new Prolog term into an external SIMC database
 */
p_simc_assert(db,term)
register Object db, term;
{
	char *dbname, *termstring;

	DeRef(db);
	if (!IsAtom(db))
		panic("simc_assert: database arg is not an atom");
	dbname = AtomToString(db);

	DeRef(term);
	if (!IsString(term))
		panic("simc_assert: second arg is not a string");
	termstring = eCharStar(term);

#if 0
fprintf(stderr,"assert(%s,%s)\n",dbname,termstring);
#endif
	if (trans_assert(dbname, termstring, opASSERT, 0) != 1)
		panic("simc_assert: can't insert term into database");
	return(SUCCEED);
}

/*
 * p_simc_delete:
 *	Delete the "current" record in a SIMC database
 *	Used by the retract preidcates
 */
p_simc_delete(trans)
register Object trans;
{
	DeRef(trans);
	if (!IsBlock(trans))
		panic("simc_delete: bad transaction descriptor");
	rec_delete(eRef(trans));
	return(SUCCEED);
}

/*
 * p_simc_end:
 *	Clean up after a SIMC database transaction
 */
p_simc_end(trans)
register Object trans;
{
	DeRef(trans);
	if (!IsBlock(trans))
		panic("simc_end: bad transaction descriptor");
	trans_close(eRef(trans));
	return(SUCCEED);
}

/*
 * p_simc_abort:
 *	Clean up in the middle of a SIMC database transaction
 */
p_simc_abort(trans)
register Object trans;
{
#if 0
fprintf(stderr,"simc_abort(%x)\n",trans);
#endif
	DeRef(trans);
	if (!IsBlock(trans))
		panic("simc_abort: bad transaction descriptor");
	trans_close(eRef(trans));
	return(SUCCEED);
}

#ifdef PSTOT
/*
 * tokeniser stuf for $pstot
 */
#define	MAXTOK	8192
#define	MAXARGS	64
#define	tokCON	0
#define	tokNUM	1
#define	tokLPAR	2
#define	tokRPAR	3
#define	tokCOMM	4
#define	tokEND	5
#define	tokVAR	6 /* VARIABLES & '_' */

#define H (CMR->mr_h)

#define	check_var(c)	((('A' <= (c)) && ((c) <= 'Z')) || ((c) == '_'))

static char *stok;
static char *tokstr;
static int  toktyp;

typedef	struct
{
	char	name[MAXTOK];	/* print name of variable */
	Word	*ptr;		/* pointer to heap */
} VAR_ARRAY;


/*
 * p_pstot:
 *	Prefix-format String TO Term
 *	Converts string representation of term in prefix
 *	format into NU-Prolog term structure on the heap
 *	Much faster than using sread/2 ... we hope
 */
p_pstot(str, Term)
register Object str, *Term;
{
	Object parsePrefixString();
	VAR_ARRAY vars[MAXARGS];/* KEEP ALL THE VARIABLES HERE */
	char tokenBuffer[MAXTOK];
	int 	  numvar=0; 	/* # VARIABLE SEEN SO FAR */

	tokstr = tokenBuffer;

	DeRef(str);
	if (!IsString(str)) {
		warning("simc_next: bad string input for pstot");
		return(0);
	}

	stok = eCharStar(str);
	*Term = parsePrefixString(&vars[0], &numvar);
	return(1);
}

/*
 * parsePrefixString:
 *	The workhorse of pstot
 *	Takes a C string as input
 *	Returns a NU-Prolog structure Object
 */
static
Object
parsePrefixString(vars, numvar)
VAR_ARRAY *vars;
int *numvar;
{
	register int i, tt, nargs;
	Word *base;
	Object var_ptr;
	int value;
	Atom *funct;
	/* Atom **a; */
	Object args[MAXARGS];
	Object make_space();

#if 0
fprintf(stderr,"parsing:%s\n", stok);
#endif

	token();
#if 0
fprintf(stderr, "token:%s\n", tokstr);
#endif
	switch ((tt = toktyp)) {
	case tokCON:
		funct = enterAtom(tokstr, stab, (Atom *)NULL);
		/* EEEEEEEEEEEEEEEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHGGGGGGGGGGGGGGGGGGHHHHHHHHHHHHHHH!!!!!!!!!!!! a = lookupAtom(tokstr, stab);
		if (*a == (Atom *) NULL)
		*/
		if(funct == (Atom *) NULL)
			printf("Something wrong ! Cannot find %s\n",tokstr);	
		else 
			/*printf("Found %s\n",eCharStar((*a)->a_pname));*/
		/*print_st(stab);*/
		break;
	case tokNUM:
		value = atoi(tokstr);
		break;
	case tokVAR:
		/* printf("\nIT IS A VAR !%s\n"," "); */
		var_ptr = make_space(vars, numvar);
		break;
	}

	token();
	if (toktyp != tokLPAR)
		/* simple const - no args */
		switch (tt) {
		case tokCON: return(MakeAtom(funct));
		case tokNUM: return(MakeSmallInt(value));
		case tokVAR: return(var_ptr); /* TAGGED IN make_space */
		}
	else {
		/* functor plus args */
		nargs = 0;
		do
			args[nargs++] = parsePrefixString(vars, numvar);
		while (toktyp == tokCOMM);
		token();

		/* build term on heap */
		base = H;
		*H++ = (Word)MakeStrHeader(nargs, funct);
		for (i = 0; i < nargs; i++) {
			*H++ = args[i];
			if(H >= heapMax)
				panic("Heap Overflow in $pstot");
		}

		/* return ref to term on heap */
#if 0
fprintf(stderr,"made term ");
displayTerm(stderr, MakeStr(base));
fprintf(stderr," @%08x\n",base);
#endif
		return(MakeStr(base));
	}
}

print_st(tab)
SymbolTable *tab;
{ 	Atom **ptr;
	int	size;
	printf("#ENTRIES = %d\n",tab->st_entries);

	/*printf("NAME = %d\n",tab->st_name);
	printf("SIZE = %d\n",tab->st_size);
	printf("LOAD = %d\n",tab->st_load);
	*/

	printf("Printing atoms.....\n");
	ptr = tab->st_table;
	size = 1 << tab->st_size;
	while (ptr < (tab->st_table + size) ) {
		if (*ptr != (Atom *) NULL)
			printf("NAME = %s\n", eCharStar((*ptr)->a_pname) );
		ptr++;
		}

	/* printf("MODULE = %d\n", (**ptr).a_module);
	printf("HEADER = %d\n", (**ptr).a_header);
	printf("FUNCTOR = %d\n", (**ptr).a_functors);
	printf("PROP = %d\n", (**ptr).a_prop);
	printf("PREC = %d\n", (**ptr).a_prec);
	*/
}

/*
 * token:
 *	Yet another simple tokeniser ...
 *	how many are there lurking in the system now?
 */
static
token()
{
	register char *t_str;

	t_str = tokstr;
	toktyp = 0;

	switch (*stok) {
	/* BUG!  Can't hack embedded quotes or \ escapes. */
	case '\'' :  /* quoted atom */
		stok++;
		while (*stok != '\'')
			*t_str++ = *stok++;
		stok++;
		*t_str = '\0';
		toktyp = tokCON;
		break;
	case '\"' :  /* string */
		stok++;
		while (*stok != '\"')
			*t_str++ = *stok++;
		stok++;
		*t_str = '\0';
		toktyp = tokCON; /* for the time being ... */
		break;
	case '(' : /* lparen */
		stok++;
		if (*stok == ' ') stok++; /* GET RID OF SPACE ! */
		toktyp = tokLPAR;
		break;
	case ')' :  /* rparen */
		stok++;
		toktyp = tokRPAR;
		break;
	case ',' :  /* comma */
		stok++;
		if (*stok == ' ') stok++; /* GET RID OF SPACE ! */
		toktyp = tokCOMM;
		break;
	case '\0' : 
	case EOF : 
		toktyp = tokEND;
		break;
	default : /* atom or variable*/
		while (index("(),'", *stok) == NULL)
			if (*stok == '\0' || *stok == EOF)
				break;
			else
				*t_str++ = *stok++;
		*t_str = '\0';
		if (!isdigit(tokstr[0])) {
			if (check_var(tokstr[0]))
				toktyp = tokVAR;
			else			
				toktyp = tokCON;
		}
		else {
			toktyp = tokNUM;
			for (t_str = tokstr; *t_str != '\0'; t_str++)
				if (!isdigit(*t_str)) {
					toktyp = tokCON;
					break;
				}
		}
	} /* switch */
}

Object
make_space(vars, numvar)
VAR_ARRAY *vars;
int *numvar;
{
	int i = 0;
	VAR_ARRAY *p;

	for(p = vars; (strcmp(tokstr, p->name) != 0) && ( i < *numvar); i++ , p++)
		/* skip through var table */;

	if ( i == MAXARGS )
		panic("var_array FULL");
	else if ( i == *numvar ) { /* VAR ENCOUNTERED FOR THE 1st TIME */
		/* printf("FIRST TIME ! numvar = %d\n",*numvar); */
/* BUG!  Should be strcpy(). */
		strncpy( (*(vars + i)).name, tokstr, MAXTOK);
		(*numvar) ++;
		(*(vars + i)).ptr = H; /* ALLOCATE VAR ON HEAP */
		*H = MakeUnboundRef( H );
		return( *H++ );
	}
	else { /* POINTS TO THE SAME PLACE ON HEAP */
		return(MakeRef( (*(vars + i)).ptr )); 
	}
} /* make_space */

#endif /* PSTOT */
