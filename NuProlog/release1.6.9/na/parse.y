/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * parse.y  -  Nepolog assembler (parser)
 */

%{

/*
 *  Interface to lexical analyser, global defs, etc.
 */

#include "na.h"

#define	tNONE	(~0)		/* value for NO tag (see types.h) */

#define	MAXREGS	30		/* max register args per instruction */

int	regc;			/* counter for register args */
Word	regv[MAXREGS];		/* place to accumulate register args */

Word	xopcode;		/* index of opcode word in CODE segment */
				/* so that we can go back and add reg args */

/* Immediately preceding instruction.  Some instruction pairs are
 * collapsed by the assembler.
 */
Word	lastopcode = cLAST;
Word	lastregv[MAXREGS];

Bool	in_table = FALSE;	/* flag for parsing hash tables */
Word	tab_prev;		/* index of previous table entry in heap */
Word	tab_start;		/* index of first table entry in heap */
Bool	need_default = FALSE;	/* flag to warn of hash tab default */
Word	tab_default;		/* default label in hash table */

/*
 * Macros
 */

#define	label_used(a)		((a) != lUNUSED)
#define	label_defined(a)	(((a) & lUNDEFINED) == 0)
#define	label_addr(a)		((a) & lADDRESS)
#define	label_forward(a)	((a) | lUNDEFINED)

#define	add_reg(REG)	((regc < MAXREGS) ? \
				(regv[regc++] = (REG)) : \
				yyerror("too many register args"))

#define YYMAXDEPTH 300

%}

%start	module

%union	{
	char	*lval_string;
	int	lval_int;
	unsigned int	lval_bits;
	double	*lval_float;
}

%token	<lval_string>
	ATOM
	STRING

%token	<lval_float>
	FLOAT

%token	<lval_int>
	NUMBER

%token	<lval_bits>
	XNUMBER

%token	<lval_int>
	OPCODE

%token	popWORD
	popPRED
	popSTRING
	popMODULE
	popDIRTY
	popCLAUSE

%token	ARITY
	COMMA
	CONST
	LABEL
	LPAREN
	RPAREN
	REF
	TAG

%token	END
	EOLN
	INVALID

%%

module
:  /* empty */
|  instruction_list
{
	end_pred();
}
;

instruction_list
:  labelled_instruction
|  instruction_list labelled_instruction
;

labelled_instruction
:  label_list instruction EOLN
|  instruction EOLN
|  label_list EOLN
|  EOLN
;

label_list
:  label clear_op
|  label_list label
;

label
:  NUMBER LABEL
{
	register Word	loc, next;
	register Word	here = bytesin(code,xcode,Word);
	_Label	*lab;

	lab = find_label($1);

#if 0
	/* HOW CAN WE DETECT MULTIPLY DEFINED LABELS? */
	if (label_used(lab->lab_code))
		err("label %d defined twice in module %s", $1, module_name);
#endif

	/*
	 * Fill in forward references to label in code
	 */
	loc = lab->lab_code;
	while (label_used(loc)) {
		loc = label_addr(loc);
		next = code[loc];
		code[loc] = here;
		loc = next;
	}
	lab->lab_code = label_addr(here);

	/*
	 * Fill in forward references to label in data
	 */
	loc = lab->lab_data;
	while (label_used(loc)) {
		loc = label_addr(loc);
		next = data[loc];
		data[loc] = here;
		loc = next;
	}
	lab->lab_data = label_addr(here);

	/*
	 * Fill in forward references to label in heap
	 */
	loc = lab->lab_heap;
	while (label_used(loc)) {
		loc = label_addr(loc);
		next = heap[loc];
		heap[loc] = here;
		loc = next;
	}
	lab->lab_heap = label_addr(here);
};

instruction
:  clear_op pseudo_op
|  operation
|  clear_op INVALID
|  clear_op error
;

clear_op
:
{
	lastopcode = cLAST;
}

pseudo_op
:  popWORD arg
{
	/*
	 * There is nothing to do here because the
	 * "arg" rule has already added the argument
	 */
}
|  popPRED ATOM COMMA NUMBER
{
	end_pred();
	(void) add_functor($2,$4,xcode-code);
}
|  popSTRING STRING
{
	register int	i = 0;
	register int	done = 0;
	register char	*s = $2;
	register char	*c;
		 Word	chars = 0;

	c = (char *)&chars;
	for (s = $2; !done; s++) {
		if (i == 4) {
			add_code(nSTRING, chars);
			i = chars = 0;
			c = (char *)&chars;
		}
		*c++ = *s;
		i++;
		if (*s == ChNULL)
			done++;
	}
	if (i > 0) {
		while (i++ < 4)
			*c++ = '\0';
		add_code(nSTRING, chars);
	}
}
|  popMODULE ATOM
{
	register char *s = $2;

	module_name = xstrings;
	while (*s != ChNULL)
		*xstrings++ = *s++;
	*xstrings++ = ChNULL;
}
|  popDIRTY
|  popDIRTY NUMBER
|  popCLAUSE
;

operation
:  OPCODE start_arg_list arg_list
{
	register int opc;

	opc = $1;
	if(mergeflg) {
		switch(lastopcode) {
		when cPVALY:
#ifdef NO_LOCAL_VARS
		case cPUVAL:
#endif /* NO_LOCAL_VARS */
			if(lastregv[1] + 1 != regv[1])
				break;
			if(opc != cPVALY
#ifdef NO_LOCAL_VARS
				&& opc != cPUVAL
#endif /* NO_LOCAL_VARS */
			)
				break;
			regv[2] = lastregv[1];
			regv[1] = regv[0];
			regv[0] = lastregv[0];
			regc = 3;
			opc = cPVALY2;
			xcode -= 1;
			xtypes -= 1;
			xopcode = (xcode - 1) - code;
#ifdef SHALLOW
		when cUVARA:
			if(opc == cUVARX || opc == cUVARA)
				opc = cUVARAVARA;
			else if(opc == cUVALX || opc == cULVX)
				opc = cUVARALVX;
			else
				break;
			regv[1] = regv[0];
			regv[0] = lastregv[0];
			regc = 2;
			xcode -= 1;
			xtypes -= 1;
			xopcode = (xcode - 1) - code;
		when cUVARX:
			if(opc == cUVARX)
				opc = cUVARXVARX;
			else if(opc == cUVARA)
				opc = cUVARAVARA;
			else if(opc == cUVALX || opc == cULVX)
				opc = cUVARXLVX;
			else
				break;
#else /* SHALLOW */
		when cUVARX:
		case cUVARA:
			if(opc == cUVARX || opc == cUVARA)
				opc = cUVARXVARX;
			else if(opc == cUVALX || opc == cULVX)
				opc = cUVARXLVX;
			else
				break;
#endif /* SHALLOW */
			regv[1] = regv[0];
			regv[0] = lastregv[0];
			regc = 2;
			xcode -= 1;
			xtypes -= 1;
			xopcode = (xcode - 1) - code;
		when cUVARY:
			if(opc != cUVARY)
				break;
			regv[1] = regv[0];
			regv[0] = lastregv[0];
			regc = 2;
			opc = cUVARYVARY;
			xcode -= 1;
			xtypes -= 1;
			xopcode = (xcode - 1) - code;
		when cUVALX:
		case cULVX:
			if(opc == cUVALX || opc == cULVX)
				opc = cULVXLVX;
			else if(opc == cUVARX)
				opc = cULVXVARX;
			else if(opc == cUVARA)
#ifdef SHALLOW
				opc = cULVXVARA;
#else /* SHALLOW */
				opc = cULVXVARX;
#endif /* SHALLOW */
			else
				break;
			regv[1] = regv[0];
			regv[0] = lastregv[0];
			regc = 2;
			xcode -= 1;
			xtypes -= 1;
			xopcode = (xcode - 1) - code;
		}
		lastopcode = opc;
		{
			register int i;

			for(i = regc - 1; i >= 0; i--)
				lastregv[i] = regv[i];
		}
		switch(opc) {
		when cUVOID:
			if(regv[0] != 1)
				break;
			opc = cUVOID1;
			regc = 0;
		}
	}
/* JUNK
printf("%8d %s(%d,%d,%d) %d\n",
	xopcode,
	bytecodes[opc].op_name, regv[0], regv[1], regv[2], regc);
*/

	switch (regc)
	{
	when 0: code[xopcode] = MakeIns0(opc);
	when 1: code[xopcode] = MakeIns1(opc, regv[0]);
	when 2: code[xopcode] = MakeIns2(opc, regv[0], regv[1]);
	when 3: code[xopcode] = MakeIns3(opc, regv[0], regv[1], regv[2]);
	otherwise:
		{
			int i, nwords = (regc-4)/4+1;
			Word wrd, xdest, xsrc;
			Word xtdest, xtsrc;

			/*
			 * Shunt all Word args up to make room for extra regs
			 */
			xsrc = (xcode - code) - 1;
			for (i = 0; i < nwords; i++)
				add_code(0,0);
			xdest = (xcode - code) - 1;
			xtsrc = xsrc;
			xtdest = xdest;
			while (xsrc > xopcode) {
				code[xdest--] = code[xsrc--];
				types[xtdest--] = types[xtsrc--];
			}
			
			/*
			 * Add opcode + reg args
			 */
			code[xopcode++]
				= MakeIns3(opc, regv[0], regv[1], regv[2]);
			xtdest = xopcode;
			for (i = 3; (i+3) < regc; i += 4) {
				code[xopcode++]
					= MakeIns3(regv[i], regv[i+1], regv[i+2], regv[i+3]);
				types[xtdest++] = nREGS;
			}
			switch (regc % 4) {
			when 0:
				code[xopcode++] = MakeIns0(regv[i]);
				types[xtdest++] = nREGS;
			when 1:
				code[xopcode++] = MakeIns1(regv[i], regv[i+1]);
				types[xtdest++] = nREGS;
			when 2:
				code[xopcode++] = MakeIns2(regv[i], regv[i+1], regv[i+2]);
				types[xtdest++] = nREGS;
			when 3: /* never happens - handled by loop above */
				break;
			}
		}
	}
	/*
	 * Check whether we have a 3 argument SOC/SOS
	 * Move default label from code area to the front
	 * of the hash table in the heap
	 */
	if (opc == cSOC || opc == cSOS || opc == cSOCE || opc == cSOSE) {
		if (tab_default != -1) {
			register Word where;
			Label	*lab;

			lab = find_label(tab_default);
			where = lab->lab_heap;
			if (label_defined(where))
				heap[tab_start+1] = where;
			else if (!label_used(where)) {
				lab->lab_heap = label_forward(tab_start+1);
				heap[tab_start+1] = where;
			}
			else {
				lab->lab_heap = label_forward(tab_start+1);
				heap[tab_start+1] = label_addr(where);
			}
		}
	}
}
;

start_arg_list
:  /* Reset for arg list - leave spot for opcode */
{
	regc = 0;
	xopcode = xcode - code;
	add_code(nOPCODE, 0);
	need_default = FALSE;
	tab_default = -1;
};

arg_list
:  /* empty */
|  arg
|  arg_list COMMA arg
;

arg
:  reg
|  const
|  ref
|  tag
|  table
;

reg
:  NUMBER
{
	add_reg($1);
};

const
:  atom
|  number
|  bit32
|  struct
;

atom
:  CONST ATOM
{
	if (in_table)
		add_heap(nATOM, add_atom($2));
	else
		add_code(nATOM, add_atom($2));
};

number
:  CONST NUMBER
{
	if(SmallInt($2)) {
		if (in_table)
			add_heap(nNUMBER, MakeSmallInt($2));
		else
			add_code(nNUMBER, MakeSmallInt($2));
	} else {
		if (in_table)
			add_heap(nINT32, $2);
		else
			add_code(nINT32, $2);
	}
}
|  CONST FLOAT
{
	if (in_table)
		add_heap(nFLOAT, $2);		/* BUG!  Why does this work? */
	else
		add_code(nFLOAT, $2);		/* BUG!  Why does this work? */
};

bit32
:  CONST CONST NUMBER
{
	if (in_table)
		yyerror("immediate integer constant in table");
	else
		add_code(nBIT32, $3);
};

struct
:  CONST ATOM ARITY NUMBER
{	
	if (in_table)
		add_heap(nSTRUCT, add_functor($2,$4,lUNUSED));
	else
		add_code(nSTRUCT, add_functor($2,$4,lUNUSED));
};

ref
:  atom_ref
|  number_ref
|  proc_ref
|  func_ref
;

atom_ref
:  REF ATOM
{
	if (in_table)
		add_heap(nATLAB, add_atom($2));
	else
		add_code(nATLAB, add_atom($2));
};

number_ref
:  REF NUMBER
{
	register Word	where;
#ifdef OLD_LABELS
	register LabTab	*lab = &labels[$2];
#else
	_Label	*lab;

	lab = find_label($2);
#endif /* OLD_LABELS */

	if (need_default)
		tab_default = $2;
	else if (in_table) {
		where = lab->lab_heap;
		if (label_defined(where))
			add_heap(nLABEL, where);
		else if (!label_used(where)) {
			lab->lab_heap = label_forward(xheap-heap);
			add_heap(nLABEL, where);
		}
		else {
			lab->lab_heap = label_forward(xheap-heap);
			add_heap(nLABEL, label_addr(where));
		}
	}
	else {
		where = lab->lab_code;
		if (label_defined(where))
			add_code(nLABEL, where);
		else if (!label_used(where)) {
			lab->lab_code = label_forward(xcode-code);
			add_code(nLABEL, where);
		}
		else {
			lab->lab_code = label_forward(xcode-code);
			add_code(nLABEL, label_addr(where));
		}
	}
};

proc_ref
:  REF ATOM ARITY NUMBER
{
	if (in_table)
		add_heap(nPROCLAB, add_functor($2,$4,lUNUSED));
	else
		add_code(nPROCLAB, add_functor($2,$4,lUNUSED));
};

func_ref
:  REF REF ATOM
{
	if (in_table)
		add_heap(nFUNCREF, add_atom($3));
	else
		add_code(nFUNCREF, add_atom($3));
};

tag
:  TAG LPAREN ATOM COMMA REF NUMBER RPAREN
{
	register char	tagtype;
	register Word	where;
#ifdef OLD_LABELS
	register LabTab	*lab = &labels[$6];
#else
	_Label	*lab;

	lab = find_label($6);
#endif /* OLD_LABELS */

	if (strcmp($3, "REF") == 0)
		tagtype = nTAGREF;
	else if (strcmp($3, "DEL") == 0)
		tagtype = nTAGDEL;
	else if (strcmp($3, "LST") == 0)
		tagtype = nTAGLST;
	else if (strcmp($3, "CHR") == 0)
		tagtype = nTAGCHR;
	else if (strcmp($3, "STR") == 0)
		tagtype = nTAGSTR;
	else if (strcmp($3, "UREF") == 0)
		tagtype = nTAGUREF;
	else if (strcmp($3, "UDEL") == 0)
		tagtype = nTAGUDEL;
	else
		yyerror("unknown tag");

	where = lab->lab_code;
	if (!label_defined(where))
		lab->lab_code = xcode-code;
	add_code(tagtype, where);
}
|  TAG LPAREN ATOM COMMA XNUMBER RPAREN
{
	if (strcmp($3, "BIT") != 0)
		yyerror("incompatible tag/bitstring");
	else
		add_code(nNUMBER, $5);
};

table
:  REF LPAREN start_table tab_list RPAREN
{
	add_code(nHASHTAB, tab_start);
	in_table = FALSE;
	need_default = TRUE;
};

start_table
:  /* empty */
{
	/*
	 * First entry in hash table is a dummy which
	 * will contain the address of the default label
	 */
	in_table = TRUE;
	tab_start = xheap - heap;
	add_heap(nHASHDEFAULT, 0);
	add_heap(nLABEL, lUNUSED);
	tab_prev = xheap - heap;
	add_heap(nHASHNEXT, lUNUSED);
};

tab_list
:  /* empty */
|  tab_entry
|  tab_list COMMA tab_entry
;

tab_entry
:  const LABEL number_ref
{
	add_heap(nHASHNEXT, lUNUSED);
	if (tab_prev == lUNUSED)
		tab_start = (xheap-heap) - 3;
	else
		heap[tab_prev] = (xheap-heap) - 3;
	tab_prev = (xheap-heap) - 1;
};

%%

/*
 * add_code:
 *	Add a Word into the code area
 */
add_code(typ, val)
char typ;
Word val;
{
	if ((xcode - code)*sizeof(Word) >= CODE_SIZE)
		grow_work_area(code, xcode, CODE_SIZE);
	if (xtypes - types >= TYPES_SIZE)
		grow_work_area(types, xtypes, TYPES_SIZE);
#if 0
fprintf(stderr,"add code %08x @ %08x, type %d @ %08x\n",val,xcode,typ,xtypes);
#endif
	*xcode++ = val;
	*xtypes++ = typ;
}

/*
 * add_data:
 *	Add a Word-length object into data area
 */
add_data(typ, val)
char typ;
Word val;
{
	if ((xdata - data)*sizeof(Word) >= DATA_SIZE)
		grow_work_area(data, xdata, DATA_SIZE);
	if (xdtypes - dtypes >= DTYPES_SIZE)
		grow_work_area(dtypes, xdtypes, DTYPES_SIZE);
#if 0
fprintf(stderr,"add data %08x @ %08x, type %d @ %08x\n",val,xdata,typ,xdtypes);
#endif
	*xdata++ = val;
	*xdtypes++ = typ;
}

/*
 * add_heap:
 *	Add a Word-length object into heap area
 */
add_heap(typ,val)
char typ;
Word val;
{
	if ((xheap - heap)*sizeof(Word) >= HEAP_SIZE)
		grow_work_area(heap, xheap, HEAP_SIZE);
	if (xhtypes - htypes >= HTYPES_SIZE)
		grow_work_area(htypes, xhtypes, HTYPES_SIZE);
#if 0
fprintf(stderr,"add heap %08x @ %08x, type %d @ %08x\n",val,xheap,typ,xhtypes);
#endif
	*xheap++ = val;
	*xhtypes++ = typ;
}

/*
 * add_functor:
 *	Find entry for an atom/arity in junk heap;
 *	Return location of entry; add if not there already
 */
int
add_functor(atom,arity,loc)
char *atom;
int arity;
Word loc;
{
	register Word	sym, fn, new;

	sym = add_atom(atom);
	fn = symtab[sym].sym_functors;
#if 0
fprintf(stderr,"add functor %s/%d @ %08x\n",atom,arity,&heap[fn]);
#endif

	/*
	 * Search for instance of this functor 
	 */
	while (fn != lUNUSED) {
		if (heap[fn+1] != arity)
			fn = heap[fn+3];
		else {
			if (loc != lUNUSED)
				heap[fn+2] = loc;
			return(fn);
		}
	}

	/*
	 * Add new functor instance
	 */
	new = xheap-heap;
	add_heap(nFUNCSYM, sym);
	add_heap(nFUNCARITY, arity);
	add_heap(nFUNCCODE, loc);
	add_heap(nFUNCNEXT, symtab[sym].sym_functors);
	symtab[sym].sym_functors = new;
	return(new);
}

/*
 * yyinit:
 *	Initialise parser/lexical_analyser
 */
yyinit()
{
	register int	i;

#ifdef LINE_BUFFER
	xxendline = TRUE;
#endif /* LINE_BUFFER */
	yyin = src_file;
	yylineno = 0;
	yyerrs = 0;
	nextchar();
}

/*
 *  yywrap:
 *	Clean up after the parser
 */
yywrap()
{
	return(1);
}

/*
 *  yyerror:
 *	Print a message for syntax errors
 */
yyerror(s)
String s;
{
	if (islower(s[0]))
		s[0] = toupper(s[0]);
	lerr(s);
	yyerrs++;
}

/*
 * lerr:
 *	Print error message for a particular line
 *	Knows about lexical analysis/parser variables
 */
lerr(format,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
char *format;
int a0,a1,a2,a3,a4,a5,a6,a7,a8,a9;
{
	int	i;
	char	*l, *b, buf[BUFSIZ];

#ifdef LINE_BUFFER
	/*
	 * Echo line + pointer to error location
	 */
	fputs(xxline,stderr);
	b = buf; l = xxline;
	for (i = 1; i < xxnchars; i++, b++, l++)
		*b = *l=='\t'?'\t':' ';
	*b++ = '^'; *b++ = '\n'; *b = '\0';
#endif /* LINE_BUFFER */

	/*
	 * Print error message in form usable by error(1)
	 */
	fputs(buf,stderr);
	fprintf(stderr, "\"%s\", line %d: ", src_name, yylineno);
	fprintf(stderr, format, a0,a1,a2,a3,a4,a5,a6,a7,a8,a9);
	putc('\n',stderr);
}

/*
 * end_pred:
 *	Clean up bits and pieces at the end of a predicate
 */
end_pred()
{
#if 0
fprintf(stderr,"end_pred\n");
#endif
	scan_code_for_floats();
	scan_heap_for_floats();
	clear_labels();
	code_start = xcode - code;
	types_start = xtypes - types;
	heap_start = xheap - heap;
	htypes_start = xhtypes - htypes;
}

scan_code_for_floats()
{
	register int	i;
	register Word	*xc, *top;
	register char	*xt;

	/*
	 * Complete processing of preceding predicate
	 */
	/* scan code segment for floats */
	top = xcode;
	xc = code + code_start;
	xt = types + types_start;
	for (; xc < top; xc++, xt++)
	{
		if (*xt == nFLOAT) {
			int	dwords, foff;
			double	*val, *dval;

			val = (double *)*xc;
			foff = (xdata - data);
			*xc = foff * sizeof(Word);	
			dwords = sizeof(double)/sizeof(Word);
#if 0
fprintf(stderr, "float %0.5f @ %08x needs %d words\n", *val, val, dwords);
#endif
			for (i = 0; i < dwords; i++)
				add_data(nFVAL, 0);
			dval = (double *)(data + foff);
			*dval = *val;
		} else if (*xt == nINT32) {
			register Word val;

			val = *xc;
			*xc = (xdata - data) * sizeof(Word);	
			add_data(nINT32VAL, val);
		}
	}
}

scan_heap_for_floats()
{
	register int	i;
	register Word	*xc, *top;
	register char	*xt;

	/* scan heap for floats */
	top = xheap;
	xc = heap + heap_start;
	xt = htypes + htypes_start;
	for (; xc < top; xc++, xt++)
	{
		if (*xt == nFLOAT) {
			int	dwords, foff;
			double	*val, *dval;

			val = (double *)*xc;
			foff = (xdata - data);
			*xc = foff * sizeof(Word);	
			dwords = sizeof(double)/sizeof(Word);
#if 0
fprintf(stderr, "float %0.5f @ %08x needs %d words\n", *val, val, dwords);
#endif
			for (i = 0; i < dwords; i++)
				add_data(nFVAL, 0);
			dval = (double *)(data + foff);
			*dval = *val;
		} else if (*xt == nINT32) {
			register Word val;

			val = *xc;
			*xc = (xdata - data) * sizeof(Word);	
			add_data(nINT32VAL, val);
		}
	}
}

#ifdef OLD_LABELS
clear_labels()
{
	register int	i;
	register LabTab	*l = labels;

	/*
	 * Clean out label table ready for next predicate
	 */
	for (i = 0; i < NLABELS; i++,l++) {
		if ((label_used(l->lab_code) && !label_defined(l->lab_code)) ||
		    (label_used(l->lab_data) && !label_defined(l->lab_data)) ||
		    (label_used(l->lab_heap) && !label_defined(l->lab_heap))  )
			err("label %d undefined in module %s", i, module_name);
		l->lab_code = l->lab_data = l->lab_heap = lUNUSED;
	}
}
#endif /* OLD_LABELS */
