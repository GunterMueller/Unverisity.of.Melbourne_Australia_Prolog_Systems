/*
 * parse.h  -  deductive database package (term parsing definitions)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#ifndef	PARSE_H
#define	PARSE_H

/*
 * Tokn:
 *	Types of lexical elements recognised by tokeniser
 */
typedef	enum
{
	TokATOM, TokNUMBER, TokSTRING,
	TokLPAREN, TokRPAREN, TokCOMMA,
	TokNULL, TokERR
} Tokn;

#define	r_Tokn		register Tokn


/*
 * Elem:
 *	Component of a key value (e.g. for "f(a,X)" = {"f","a","X"} )
 */
typedef struct Elem {
	short	e_rnbits;	/* # bits to set in record desc */
	short	e_snbits;	/* # bits to set in segment desc */
	short	e_nodenum;	/* position of node in skeleton */
	short	e_rmaskpos;	/* which bit for mask in record desc */
	short	e_smaskpos;	/* which bit for mask in segment desc */
	short	e_maxargs;	/* maximum # arguments (0 if const or var) */
	String	e_name;		/* ptr to char string for this element */
	Int	e_isvar;	/* If element is a variable in a query,
				/* this indicates no bits set descriptor */
	Int	e_cluster;	/* mask telling which bits to use for clustering*/
	Int	*e_recbits;	/* bits which are set in record desc */
	Int	*e_segbits;	/* bits which are set in segment desc */
	Int	e_nargs;	/* actual # arguments (0 if const or var) */
	struct	Elem *e_args[1];/* dummy array containing pointers to
				   other Elems which describe arguments */
/*	Int	e_recbits[];	** list of bits set in record desc	
				** The e_recbits field above points here */
/*	Int	e_segbits[];	** list of bits set in segment desc
				** The e_segbits field above points here */
} Elem;

#define	r_Elem		register Elem
#define	ElemNULL	(Elem *)NULL

#define	e_weight	e_rnbits


extern	Word	cluster;	/* best segment number */
extern	Word	clustars;	/* gaps in seg number - for DSIMC */
extern	Int	depth;		/* term depth, for debugging output */

/*
 * Parsing operations
 */

/*
 * elem_make:
 *	create a new Elem for an atom with N args
 */
extern	Elem	*elem_make(/* relation, n_args, mask_pos */);

/*
 * skel_free:
 *	Free up storage allocated to an Elem tree
 */
extern	void	skel_free(/* tree */);

/* 
 * ground:
 *	Test whether a key value is bound or variable
 */
#define	ground(key)	((isupper(*(key)) || (*(key)) == '_') ? FALSE : TRUE)

/*
 * match:
 *	Test whether two facts "match"
 */
extern	Bool	match(/* fact1, fact2 */);

/*
 * key_parse:
 *	Parse a key value to produce an Elem tree
 */
extern	Bool	key_parse(/* relation, key_string */);

/*
 * skel_parse:
 *	Parse a key value to produce an Elem tree
 */
extern	Bool	skel_parse(/* relation_info, template_string */);

/*
 * skel_normalise:
 *	Normalise weights to numbers-of-bits in tree of Elems
 */
extern	Bool	skel_normalise(/* relation_info */);

/*
 * skel_print:
 *	Print out a tree of Elems (debugging)
 */
extern	void	skel_print(/* elem_tree */);

/*
 * token:
 *	Tokeniser (lexical analysis) for parser
 */
extern	void	token();

/*
 * isletter:
 *	Predicate to test whether char is a letter from Roman alphabet
 */
#define	isletter(ch)	(isupper(ch) || islower(ch))

/*
 * isdatum:
 *	Predicate to test whether a token is a piece of information
 *	or a punctuation symbol (such as "(", ",", ")")
 */
#define	isdatum(tok)	((tok) == TokNUMBER || \
			 (tok) == TokATOM || \
			 (tok) == TokSTRING)

#endif /*	PARSE_H */
