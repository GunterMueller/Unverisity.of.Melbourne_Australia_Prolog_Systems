/*
 * parse.c  -  deductive database package (term parsing operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

/*
 * Parser constants
 */

#define	WEIGHTS		0
#define	REC_BITS	1
#define	SEG_BITS	2

#define	NOSIGN		(ONES >> 1)

#define	RAND_DEG	4


/*
 * Parser variables
 */

Word	cluster;	/* bit string to "suggest" best segment */
Word	clustars;	/* bit string to note holes in cluster for DSIMC */

static	Char	*nextch;	/* current char for parser */
static	short	maskpos;	/* position of element in mask descriptor */
static	short	nodenum;	/* position of element in skeleton */
static	Tokn	tok;		/* type of last token */
static	Char	*tok_str;	/* chars which comprise a token */
static	Int	tok_val;	/* value of a numerical token */

static	Int	*seg_ptr;	/* pointer to list of seg bits */
static	Int	*rec_ptr;	/* pointer to list of rec bits */

static	Int	seg_ctr;	/* counter for list of seg bits */
static	Int	rec_ctr;	/* counter for list of rec bits */

static	Int	mass;		/* total of all weights in Elem tree */
static	Int	rremainder;	/* how many rec bits left after normalising */
static	Int	sremainder;	/* how many seg bits left after normalising */

static	Int	level;		/* level of tree for parsing functions */

static char *
allocateStringBuffer(n)
int n;
{
	register char *s;

	s = (char *) malloc(n + 1);
	if(s == (char *) NULL) {
		fprintf(stderr,
			"Unable to allocate string buffer in key_parse()");
		exit(1);
	}
	return(s);
}

/*
 * elem_make:
 *	Generate an Elem structure for a term with N args
 */
Elem *
elem_make(nargs, mskpos)
Int nargs;
Int mskpos;
{
	r_Elem	*el;
	r_Int	elem_bytes;

	elem_bytes = sizeof(Elem) + (nargs - 1) * sizeof(Elem *);

#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"elem_make:");
#endif /* DBUG */
	if ((el = (Elem *)malloc((unsigned)elem_bytes)) == ElemNULL) {
		error("elem_out_of_memory");
		return(ElemNULL);
	}

	el->e_name = StrNULL;
	el->e_name = 0;
	el->e_isvar = FALSE;
	el->e_cluster = 0;
	el->e_rnbits = 0;
	el->e_snbits = 0;
	el->e_rmaskpos = mskpos;
	el->e_smaskpos = mskpos;
	el->e_nargs = nargs;
	el->e_maxargs = nargs;
	el->e_recbits = NULL;
	el->e_segbits = NULL;

	return(el);
}

/*
 * elem_copy:
 *	Generate an copy of an Elem structure for a term with N args
 */
Elem *
elem_copy(original)
Elem *original;
{
	r_Elem	*el;
	r_Int	i, elem_bytes;

#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"elem_copy:");
#endif /* DBUG */
	/*
	 * Allocate same amount of space as for original Elem
	 */
	elem_bytes = sizeof(Elem) + (original->e_nargs - 1) * sizeof(Elem *);

	if ((el = (Elem *)malloc((unsigned)elem_bytes)) == ElemNULL) {
		error("elem_out_of_memory");
		return(ElemNULL);
	}

	el->e_rnbits = original->e_rnbits;
	el->e_snbits = original->e_snbits;
	el->e_nodenum = original->e_nodenum;
	el->e_rmaskpos = original->e_rmaskpos;
	el->e_smaskpos = original->e_smaskpos;
	el->e_maxargs = original->e_maxargs;
	el->e_name = original->e_name;
	el->e_isvar = original->e_isvar;
	el->e_cluster = original->e_cluster;
	el->e_recbits = original->e_recbits;
	el->e_segbits = original->e_segbits;
	el->e_nargs = original->e_nargs;
	for (i = 0; i < original->e_nargs; i++)
		el->e_args[i] = ElemNULL;
#ifdef DBUG
	debug(PARSE_DBUG) skel_print("new Elem:",el);
#endif /* DBUG */
	return(el);
}

/*
 * skel_free:
 *	Free up storage allocated for an Elem tree
 */
void
skel_free(tree)
Elem *tree;
{
	r_Int	i;

	if (tree == ElemNULL)
		return;
	for (i = 0; i < tree->e_maxargs; i++) {
		skel_free(tree->e_args[i]);
		tree->e_args[i] = ElemNULL;
	}
	cfree(tree->e_name,Char);
	cfree(tree,Elem);
}

/*
 * simc_hash:
 *	Hash an element of a record into a codeword
 */
static Int
simc_hash(node,seed,info,clust)
Elem *node;
Int seed;
Reln *info;
Word *clust;
{
	r_Char	*key;
	r_Int	i, j, knt;
	r_Word	xyzzy;
	r_Int	range;
	r_Int	bits_set;
	r_Int	bit_count;
	r_Word	*fptr, *rptr;
	Word	state[RAND_DEG];

	/*
	 * Generate seed for random numbers using key
	 */
	xyzzy = 0;
	key = node->e_name;
	if (isdigit(*key))
		xyzzy = atoi(key);
	else {
		for (i = 0; *key != '\0'; i++, key++) {
			/* get rid of any sign extension */
			j = *key & 0377;
			/* knt is a sort of sliding window ... */
			knt = (i + ((i & 3) << 3)) & 037;
			/* rotate the stripped *key by knt */
			xyzzy ^= (j << (knt) | j >> (32 - knt));
		}
	}

	/*
	 * Initialise random number generator
	 */
	xyzzy &= NOSIGN;
#ifdef DBUG
	debug(TREE_DBUG)
		fprintf(stderr,"token:%s: seed:%x: xyzzy:%x:\n",
				node->e_name,seed,xyzzy);
#endif /* DBUG */
	state[0] = xyzzy;
	for (i = 1; i < RAND_DEG; i++)
		state[i] = 1103515245*state[i-1] + 12345;
	fptr = &state[RAND_DEG-1];
	rptr = &state[0];
	for (i = 0; i < RAND_DEG; i++) {
		*fptr += *rptr;
		xyzzy = (*fptr >> 1) & NOSIGN;
		if(++fptr >= &state[RAND_DEG]) {
			fptr = state;
			++rptr;
		}
		else  {
			if(++rptr >= &state[RAND_DEG]) rptr = state;
		}
	}

	*clust = xyzzy;
	i = node->e_nargs + 1;
	j = node->e_nodenum & 037;
	xyzzy = (xyzzy << (j) | xyzzy >> (32-j));
	xyzzy ^= (seed << (i) | seed >> (32-i));
	state[0] = xyzzy;
	fptr = &state[RAND_DEG-1];
	rptr = &state[0];

	/*
	 * Generate bits in seg codeword
	 */
	range = info->rd_scwordsz;
	bits_set = node->e_snbits;
	node->e_segbits = seg_ptr;
#ifdef DBUG
	debug(TREE_DBUG)
		fprintf(stderr,"token:%s: xyzzy:%x:\n",node->e_name,xyzzy);
#endif /* DBUG */
	for (bit_count = 0; bit_count < bits_set; bit_count++)
	{
		*fptr += *rptr;
		xyzzy = (*fptr >> 1) & NOSIGN;
		if(++fptr >= &state[RAND_DEG]) {
			fptr = state;
			++rptr;
		}
		else  {
			if(++rptr >= &state[RAND_DEG]) rptr = state;
		}
		*seg_ptr++ = (xyzzy >> 10) % range;
		seg_ctr++;
	}

	/*
	 * Generate bits in rec codeword
	 */
	range = info->rd_rcwordsz;
	bits_set = node->e_rnbits;
	node->e_recbits = rec_ptr;
	for (bit_count = 0; bit_count < bits_set; bit_count++)
	{
		*fptr += *rptr;
		xyzzy = (*fptr >> 1) & NOSIGN;
		if(++fptr >= &state[RAND_DEG]) {
			fptr = state;
			++rptr;
		}
		else  {
			if(++rptr >= &state[RAND_DEG]) rptr = state;
		}
		*rec_ptr++ = (xyzzy >> 10) % range;
		rec_ctr++;
	}

	return(xyzzy);
}

/*
 * do_match:
 *	Perform comparison of parse tree and term string
 */
Bool
do_match(node)
Elem *node;
{
	return(TRUE);
}

/*
 * match:
 *	Test whether two facts match
 *	Compare the parse tree of "fact1" to the text string of "fact2"
 */
Bool
match(fact1, fact2)
Elem *fact1;
String fact2;
{
	Bool	retcode;

	/*
	 * Initialise parser stuff
	 */
	nextch = fact2;
	tok_str = allocateStringBuffer(strlen(nextch) + 1024);

	/*
	 * Perform comparison
	 */
	retcode = do_match(fact1);
	free(tok_str);
	return(retcode);
}

/*
 * rebuildToVec:
 *	Reconstruct term, inserting choice vectors for variables
 */
Word
rebuildToVec(buf, qtree, ktree, nbits, dumping)
Char **buf;
Elem *qtree, *ktree;
Int nbits, dumping;
{
	r_Int	i;
	r_Char	*t;
	Char	*b;
	Word	vec, w;
	Elem	**qargs, **kargs;

	if (!dumping) {
		vec = ktree->e_cluster;
		if (ktree->e_maxargs > 0)
			for (i = 0; i < ktree->e_maxargs; i++)
				vec |= rebuildToVec(&b, ElemNULL, kargs[i], nbits, 0);
		return(vec);
	}
	b = *buf;
	qargs = &(qtree->e_args[0]);
	kargs = &(ktree->e_args[0]);

	if (qtree->e_isvar) {
		vec = ktree->e_cluster;
		for (i = 0; i < qtree->e_maxargs; i++)
			vec |= rebuildToVec(&b, ElemNULL, kargs[i], nbits, 0);
		if (dumping) {
			*b++ = '(';
			*b++ = '[';
			for (i = 0; i < nbits; i++) {
				if ((vec >> (30-i)) & 1)
					*b++ = '_';
				else {
					*b++ = '0';
					*b++ = '\'';
					*b++ = '*';
				}
				if (i < nbits-1)
					*b++ = ',';
			}
			*b++ = ']';
			*b++ = ',';
			*b++ = '[';
			for (; i < 31; i++) {
				if ((vec >> (30-i)) & 1)
					*b++ = '_';
				else {
					*b++ = '0';
					*b++ = '\'';
					*b++ = '*';
				}
				if (i < 30)
					*b++ = ',';
			}
			*b++ = ']';
			*b++ = ')';
		}
	}
	else {
		for (t = qtree->e_name; *t != ChNULL; )
			*b++ = *t++;
		if (qtree->e_nargs > 0) {
			*b++ = '(';
			for (i = 0; i < qtree->e_nargs; i++) {
				w = rebuildToVec(&b, qargs[i], kargs[i], nbits, 1);
				if (i < qtree->e_nargs-1)
					*b++ = ',';
			}
			*b++ = ')';
		}
	}
	*buf = b;
	return(vec);
}

/*
 * token:
 *	Tokeniser (lexical analysis) for parser
 *	Very simple, since we don't want to pre-empt any decisions
 *	about quoting, etc. made by host language ... however, years
 *	of heartbreak have made it drift towards Prolog standards
 */
void
token()
{
	String	t_str;

	while (isspace(*nextch)) nextch++;
	
	t_str = tok_str;
	tok_val = 0;

	if (isdigit(*nextch)) /* number */
	{
		while (isdigit(*nextch)) {
			*t_str++ = *nextch;
			tok_val = tok_val*10 + *nextch++ - '0';
		}
		/*
		 * Note that you can't read a float this way -- you're likely
		 * to get a different least sig. bit than atof().
		 */
		if (*nextch == '.') {
			*t_str++ = *nextch++;
			while (isdigit(*nextch)) {
				*t_str++ = *nextch;
				tok_val = tok_val*10 + *nextch++ - '0';
			}
		}
		*t_str = '\0';
		tok = TokNUMBER;
	} else if (*nextch == '\'') /* quoted atom */
	{
		*t_str++ = *nextch++;
		while (*nextch != '\'')
			*t_str++ = *nextch++;
		*t_str++ = *nextch++;
		*t_str = '\0';
		tok = TokSTRING;
	} else if (*nextch == '\"') /* string */
	{
		*t_str++ = *nextch++;
		while (*nextch != '\"')
			*t_str++ = *nextch++;
		*t_str++ = *nextch++;
		*t_str = '\0';
		tok = TokSTRING;
	} else if (*nextch == '(') /* lparen */
	{
		nextch++;
		tok = TokLPAREN;
	} else if (*nextch == ')') /* rparen */
	{
		nextch++;
		tok = TokRPAREN;
	} else if (*nextch == ',') /* comma */
	{
		nextch++;
		tok = TokCOMMA;
	} else if (*nextch == '\0' || *nextch == EOF)
	{
		tok = TokNULL;
	} else /* atom */
	{
		while (index("(),'",*nextch) == StrNULL)
			if (*nextch == '\0' || *nextch == EOF)
				break;
			else
				*t_str++ = *nextch++;
		*t_str = '\0';
		tok = TokATOM;
	}
}

/*
 * do_cluster_fill:
 *	Traverse unused branch of template, collecting cluster masks
 *	Used when query tree has variables and is smaller than template
 */
Word
do_cluster_fill(skel)
Elem *skel;
{
	r_Int	i;
	r_Word	mask;

	mask = skel->e_cluster;
	for (i = 0; i < skel->e_maxargs; i++)
		mask |= do_cluster_fill(skel->e_args[i]);
	return(mask);
}

/*
 * do_key_fill:
 *	Fill out branch of query tree according to template
 *	tree when key is too small
 */
Elem *
do_key_fill(skel,filltok)
Elem *skel;
String filltok;
{
	r_Int	i;
	Elem	*node;

	/*
	 * Make new node in parse tree by copying from template tree
	 */
	node = elem_copy(skel);
#ifdef DBUG
	debug(MALLOC_DBUG) fprintf(stderr,"node_name_f:");
#endif /* DBUG */
	node->e_name = mkstr(strlen(filltok));
	strcpy(node->e_name, filltok);
	node->e_nargs = 0;

	if (ground(filltok)) {
		/*
		 * Constant - widen/deepen key tree to fill out template
		 */
		for (i = 0; i < skel->e_maxargs; i++)
			node->e_args[i] = do_key_fill(skel->e_args[i], filltok);
	}
	else {
		/*
		 * Variable - scan template tree to collect cluster masks
		 */
		node->e_cluster = do_cluster_fill(skel);
		node->e_isvar = TRUE;
	}
	return(node);
}

/*
 * do_key_parse:
 *	Parse a key value into a tree of Elems, restricted by a skeleton
 */
Elem *
do_key_parse(skel)
Elem *skel;
{
	r_Int	i, nargs;
	r_Elem	*tree, *node;

	/*
	 * Read next token (which should be a piece of data
	 * Then look ahead one token to see whether functor or just atom
	 */
	token();
	if (!isdatum(tok))
		return(ElemNULL);
	token();

	if (tok == TokCOMMA || tok == TokRPAREN) { /* atom or var */
		/*
		 * Consume one token ... input string is "bigger" than template
		 */
		if (skel == ElemNULL)
			return(ElemNULL);

		return(do_key_fill(skel,tok_str));
	}
	else if (tok == TokLPAREN) { /* Functor + args */	

		if (skel != ElemNULL) {
			node = elem_copy(skel);
#ifdef DBUG
			debug(MALLOC_DBUG) fprintf(stderr,"node_name_c:");
#endif /* DBUG */
			node->e_name = mkstr(strlen(tok_str));
			strcpy(node->e_name, tok_str);
		}
		nargs = 0;
		for( ; ; ) {
			if (skel == ElemNULL || nargs >= skel->e_maxargs)
				tree = do_key_parse(ElemNULL);
			else {
				tree = do_key_parse(skel->e_args[nargs]);
				node->e_args[nargs] = tree;
				if (tree == ElemNULL)
					break;
			}
			nargs++;
			if (tok == TokCOMMA)
				continue;
			else if (tok == TokRPAREN) {
				/*
				 * Widen, if insufficient args for functor
				 */
				if (skel != ElemNULL)
					for (i = nargs; i < skel->e_maxargs; i++)
						node->e_args[i] = do_key_fill(skel->e_args[i], tok_str);
				token();
				break;
			}
			else
				return(ElemNULL); /* syntax error */
		}
		if (skel == ElemNULL)
			return(ElemNULL);
		else {
			node->e_nargs = nargs;
#ifdef DBUG
			debug(PARSE_DBUG) skel_print("partial key tree:",node);
#endif /* DBUG */
			return(node);
		}
	}
	else
		return(ElemNULL); /* syntax error */
}

/*
 * queryToVec:
 *	Convert a query into a term containing choice vectors
 */
queryToVec(rel, query, buf)
Reln *rel;
String query;
Char **buf;
{
	Int	nbits;
	Word	w;
	Elem	*qtree;

	/*
	 * Parse the term
	 */
	nextch = query;
	tok_str = allocateStringBuffer(strlen(nextch) + 1024);
	maskpos = 0;
	nodenum = 0;
	qtree = do_key_parse(skeleton(rel));
	if (qtree == ElemNULL) {
		free(tok_str);
		return(FALSE);
	}

	/*
	 * Reconstruct the term
	 */
	for (w = Nls(rel,0), nbits = 0; w > 0; w >>= 1, nbits++)
		/* compute nbits */;
	w = rebuildToVec(buf, qtree, skeleton(rel), nbits-1, 1);
	free(tok_str);
	return(TRUE);
}

/*
 * choice_bits
 *	Given a hash and a choice vector, put the top N bits of
 *	the hash into the N bits of the choice vector
 */
Word
choice_bits(hash, cvec)
Word hash;
Word cvec;
{
	r_Int	i,j;
	Word	cbits = 0;

	for (i = j = 30; i >= 0; i--)
		if (((cvec >> i) & 1) == 1) {
			cbits |= ((hash >> (j-i)) & (1 << i));
			j--;
		}
	return(cbits);
}

/*
 * do_key_hash
 *	Traverse key parse tree, generating (bit lists for) descriptors
 */
void
do_key_hash(node,seed,rel)
Elem *node;
Int seed;
Reln *rel;
{
	r_Int	i;
	Word	clust;

	if (node->e_isvar) {
		if (isassert(rel->rd_operation)) {
			*seg_ptr++ = node->e_smaskpos;
			seg_ctr++;
			*rec_ptr++ = node->e_rmaskpos;
			rec_ctr++;
		}
		clustars |= node->e_cluster;
	}
	else {
		seed = simc_hash(node, seed, rel, &clust);

#if 0
fprintf(stderr,"choice_bits for %s (%08x,%08x) = %08x\n",
	node->e_name, clust, node->e_cluster, choice_bits(clust,node->e_cluster));
#endif
		cluster |= choice_bits(clust,node->e_cluster);

		for (i = 0; i < node->e_maxargs; i++)
			do_key_hash(node->e_args[i], seed, rel);
	}
}

/*
 * cmp:
 *	Compare two Ints, for use in sorting seg bits
 */
Int
cmp(x,y)
Int *x, *y;
{
	return(*x - *y);
}

/*
 * key_parse:
 *	Parse a key value into a tree of Elems, according to a skeleton
 */
Bool
key_parse(tr, key)
Trans *tr;
String key;
{
	r_Reln	*rel = tr->relation;
#ifdef TODO
	Why is it ignoring??? funny inputs
	e.g. people(a,b,c,d,e) (for 4 arg functor)
		people(a,b,c,d)junk (correct args but trailing junk)
#endif /* TODO */

	/*
	 * Initialise parser & descriptor variables
	 */
	nextch = key;
	tok_str = allocateStringBuffer(strlen(nextch) + 1024);
	maskpos = 0;
	nodenum = 0;
	seg_ptr = tr->seg_list;
	rec_ptr = tr->rec_list;
	seg_ctr = rec_ctr = 0;
	cluster = 0;
	clustars = 0;

	/*
	 * Do the parsing
	 */
	tr->query = do_key_parse(skeleton(rel));
	if (tr->query == ElemNULL) {
		free(tok_str);
		return(FALSE);
	}

	/*
	 * Set up descriptors
	 */
	do_key_hash(tr->query, 0, rel);
#ifdef DBUG
	debug(TREE_DBUG) skel_print("query tree:",tr->query);
#endif /* DBUG */
	if (isassert(rel->rd_operation))
		qsort((char *)tr->seg_list, seg_ctr, sizeof(Int), cmp);
	else {
		if (Us(rel) > seg_ctr)
			tr->seg_ratio = 1.0;
		else
			tr->seg_ratio = (float)Us(rel) / (float)seg_ctr;
		if (Ur(rel) > rec_ctr)
			tr->rec_ratio = 1.0;
		else
			tr->rec_ratio = (float)Ur(rel) / (float)rec_ctr;
	}
	tr->nsegbits = seg_ctr;
	tr->nrecbits = rec_ctr;

	free(tok_str);

	return(TRUE);
}

/*
 * do_skel_parse:
 *	Turn a template into a tree of Elems
 */
Elem *
do_skel_parse()
{
	Int	weight;
	Int	clust;
	Char	maskflag;
	r_Elem	*node;
	r_Elem	*tree;
	r_Int	i, nargs;
	short	my_maskpos;
	short	my_node_number;
	Elem	*args[MAXKEYS];

	token();
	if (tok != TokATOM)
		return(ElemNULL); /* syntax error */
	if (sscanf(tok_str,"%c:%d:%x",&maskflag,&weight,&clust) != 3)
		return(ElemNULL); /* syntax error */
	
	/*
	 * Never set top bit for any cluster/hash values
	 */
	clust &= NOSIGN;

	my_node_number = nodenum++;
	if (maskflag == 'm')
		my_maskpos = maskpos++;
	else
		my_maskpos = (short)-1;
	token();
	if (tok == TokCOMMA || tok == TokRPAREN || tok == TokNULL) {
		/*
		 * Simple element
		 */
		node = elem_make(0,my_maskpos);
		node->e_nodenum = my_node_number;
		node->e_weight = weight;
		node->e_cluster = clust;
		nextch--; /* backup for tokeniser */
		return(node);
	}
	else if (tok == TokLPAREN) {
		/*
		 * Functor + args
		 */	
		nargs = 0;
		for( ; ; ) {
			if ((tree = do_skel_parse()) == ElemNULL)
				break;
			if (nargs >= MAXKEYS)
				fatal("term_too_complex");
			args[nargs++] = tree;
			token();
			if (tok == TokCOMMA)
				continue;
			else if (tok == TokRPAREN)
				break;
			else
				return(ElemNULL); /* syntax error */
		}
		node = elem_make(nargs,my_maskpos);
		node->e_nodenum = my_node_number;
		node->e_weight = weight;
		node->e_cluster = clust;
		for (i = 0; i < nargs; i++)
			node->e_args[i] = args[i];
		return(node);
	}
	else
		return(ElemNULL);
}

/*
 * skel_parse:
 *	Turn a template into a tree of Elems
 */
Bool
skel_parse(info,templat)
Reln *info;
String templat;
{
	/*
	 * Initialise parsing variables
	 */
	nextch = templat;
	tok_str = allocateStringBuffer(strlen(nextch) + 1024);
	maskpos = 0; /* to ignore Relation */
	nodenum = 0;

	/*
	 * Do the parsing
	 */
	if ((info->rd_skel = do_skel_parse()) == ElemNULL) {
		free(tok_str);
		return(FALSE);
	}
	info->rd_masksz = maskpos;

	free(tok_str);
	return(TRUE);
}

/*
 * skel_weigh:
 *	Compute total of weights in an Elem tree
 */
Int
skel_weigh(skel,type)
Elem *skel;
Int type;
{
	Int	i, total_weight;

	switch (type)
	{
	case WEIGHTS:
		total_weight = skel->e_weight; break;
	case REC_BITS:
		total_weight = skel->e_rnbits; break;
	case SEG_BITS:
		total_weight = skel->e_snbits; break;
	}
	for (i = 0; i < skel->e_nargs; i++)
		total_weight += skel_weigh(skel->e_args[i],type);
	return(total_weight);
}

/*
 * do_skel_normalise:
 *	Normalise weights to numbers-of-bits in tree of Elems
 */
void
do_skel_normalise(skel,rbits,sbits,rcwbits,scwbits)
Elem *skel;
Int rbits, sbits;
Int rcwbits, scwbits;
{
	r_Int	i, weight;

	weight = skel->e_weight;
	skel->e_rnbits = (weight * rbits) / mass;
	skel->e_snbits = (weight * sbits) / mass;
	if (skel->e_rmaskpos >= 0)
		skel->e_rmaskpos += rcwbits;
	if (skel->e_smaskpos >= 0)
		skel->e_smaskpos += scwbits;
	for (i = 0; i < skel->e_nargs; i++)
		do_skel_normalise(skel->e_args[i],rbits,sbits,rcwbits,scwbits);
}

/*
 * do_rskel_distribute:
 *	Distribute remaining numbers-of-rec-bits in tree of Elems
 */
void
do_rskel_distribute(skel)
Elem *skel;
{
	Int	i;

	if (rremainder <= 0)
		return;
	if (level > 0) {
		skel->e_rnbits++;
		rremainder--;
	}
	level++;
	for (i = 0; i < skel->e_nargs; i++)
		if (rremainder <= 0)
			return;
		else
			do_rskel_distribute(skel->e_args[i]);
	level--;
}

/*
 * do_sskel_distribute:
 *	Distribute remaining numbers-of-seg-bits in tree of Elems
 */
void
do_sskel_distribute(skel)
Elem *skel;
{
	Int	i;

	if (sremainder <= 0)
		return;
	if (level > 0) {
		skel->e_snbits++;
		sremainder--;
	}
	level++;
	for (i = 0; i < skel->e_nargs; i++)
		if (sremainder <= 0)
			return;
		else
			do_sskel_distribute(skel->e_args[i]);
	level--;
}


/*
 * skel_normalise:
 *	Normalise weights to numbers-of-bits in tree of Elems
 *	Convert relative mask positions to absolute positions
 *	Re-create tree with correctly allocated nodes
 */
Bool
skel_normalise(info)
Reln *info;
{
	r_Elem	*skel;
	r_Int	rbits, sbits;
	r_Int	rcwbits, scwbits;

	/*
	 * From information on Reln, determine important constants
	 */
	skel = info->rd_skel;
	rbits = skel->e_nargs * info->rd_rnbits;
	sbits = skel->e_nargs * info->rd_snbits;
	rcwbits = info->rd_rcwordsz;
	scwbits = info->rd_scwordsz;

	/*
	 * Determine total of "weights" in Elem tree
	 */
	mass = skel_weigh(skel,WEIGHTS);

	/*
	 * Traverse tree, converting "weights" to "bits"
	 *  and setting up mask positions to account for codeword length.
	 */
	do_skel_normalise(skel,rbits,sbits,rcwbits,scwbits);
	info->rd_skel = skel;

	/*
	 * Re-traverse, distributing remaining bits
	 */
	rremainder = rbits - skel_weigh(skel,REC_BITS);
	sremainder = sbits - skel_weigh(skel,SEG_BITS);

	if (rremainder > 0) {
		level = 0;
		do_rskel_distribute(skel);
	}
	if (sremainder > 0) {
		level = 0;
		do_sskel_distribute(skel);
	}
	return(TRUE);
}

#ifdef TODO
/*
 * quotify:
 *	Put quotes around atoms when retrieving records from database
 *	This is (sort of) a converse of token() above
 */
String
quotify(str)
Char *str;
{
	r_Char	*s, *r;
	r_Bool	quotable, quoted;
	static	Char	retbuf[BUFSIZ];

	quotable = TRUE;
	r = retbuf;
	for (s = str; *s != ChNULL; s++)
	{
		if (quotable) {
			if (!isdigit(*s) && *s != '\"') {
				*r++ = '\'';
				quoted = TRUE;
			}
			quotable = FALSE;
		}
		else {
			switch(*s) {
			}
		}
		*r++ = *s;
	}
}
#endif

#ifdef DBUG
/*
 * do_skel_print:
 *	Print Elems in a term tree, showing structure by indentation
 */
void
do_skel_print(node)
Elem *node;
{
	Int	i;

	for (i = 0; i < level; i++)
		putc('\t',stderr);
	fprintf(stderr,":%s:%d:%d:%d:%d:%d:%d:%d:%d:%x:\n",
			node->e_name==NULL?"NULL":node->e_name,
			(int)node->e_nodenum,
			node->e_rnbits, node->e_snbits,
			(int)node->e_rmaskpos, node->e_smaskpos,
			node->e_maxargs, node->e_nargs,
			node->e_isvar, node->e_cluster);

	if (node->e_recbits != NULL) {
		for (i = 0; i < level; i++)
			putc('\t',stderr);
		fprintf(stderr,"rec:");
		for (i = 0; i < node->e_rnbits; i++)
			if (node->e_recbits[i] == -1)
				break;
			else
				fprintf(stderr," %4d",node->e_recbits[i]);
		putc('\n',stderr);
	}

	if (node->e_segbits != NULL) {
		for (i = 0; i < level; i++)
			putc('\t',stderr);
		fprintf(stderr,"seg:");
		for (i = 0; i < node->e_snbits; i++)
			if (node->e_segbits[i] == -1)
				break;
			else
				fprintf(stderr," %4d",node->e_segbits[i]);
		putc('\n',stderr);
	}

	level++;
	for (i = 0; i < node->e_maxargs; i++)
		if (node->e_args[i] != ElemNULL)
			do_skel_print(node->e_args[i]);
	level--;
}

/*
 * skel_print:
 *	Print Elems in a term tree, showing structure by indentation
 */
void
skel_print(label,node)
String label;
Elem *node;
{
	if (node == ElemNULL) {
		fprintf(stderr,"ElemNULL\n");
		return;
	}
	level = 0;
	fprintf(stderr, "%s\n", label);
	do_skel_print(node);
}
#endif /* DBUG */
