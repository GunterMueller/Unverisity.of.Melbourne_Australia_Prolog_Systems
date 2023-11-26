/*
 * lex.c  -  Nepolog Assembler (lexical analyser)
 *
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

#include <ctype.h>
#include <signal.h>
#include <math.h>
#include "na.h"
#include "parse.h"

/*
 * Functions used by lexical analysis
 */
extern	char	*malloc();
extern	char	*save_string();
extern	double	*save_float();

/*
 * Debugging things
 */
#if 0
#if 1
#define	Return(X)	{fprintf(stderr,"typ:%d str:'%s'\n", X, tokstr); return(X);}
#else
#define	Return(X)	{ \
			if ((X) < chSEMI) \
				{fprintf(stderr,"typ:%d str:'%s'\n", X, tokstr); return(X);} \
			else \
				{fprintf(stderr,"typ:%d\n", X); return(X);} \
			}
#endif
#else
#define Return(X)	return(X);
#endif

/*
 * Lex/parse interface
 */
#define	int_val(I)	yylval.lval_int = (I)
#define	float_val(F)	yylval.lval_float = save_float(F)
#define	string_val(S)	yylval.lval_string = save_string(S)

/*
 * Character classes
 */
#define	chBADCH		(1 << 0)
#define	chSPACE		(1 << 1)
#define	chALPHA		(1 << 2)
#define	chDIGIT		(1 << 3)
#define	chSEMI		(1 << 4)
#define	chCOLON		(1 << 5)
#define chDQUOTE	(1 << 6)
#define	chLQUOTE	(1 << 7)
#define	chRQUOTE	(1 << 8)
#define	chLPAREN	(1 << 9)
#define	chRPAREN	(1 << 10)
#define	chLBRACK	(1 << 11)
#define	chRBRACK	(1 << 12)
#define	chCOMMA		(1 << 13)
#define	chPOINT		(1 << 14)
#define	chDOLLAR	(1 << 15)
#define	chPCENT		(1 << 16)
#define	chSLASH		(1 << 17)
#define	chSTAR		(1 << 18)
#define	chAMPER		(1 << 19)
#define	chSUBTR		(1 << 20)
#define	chHASH		(1 << 21)
#define	chEOLN		(1 << 29)
#define	chEOF		(1 << 30)

/*
 * Global lexical analysis data structures
 */
char	yyinfile[1024];		/* name of current input file */
FILE	*yyin;			/* current input stream */
int	yylineno;		/* input stream line number */

char	xbuf[200];		/* initial token string buffer */
char	*tokstr = xbuf;		/* text string of current token */
char	*newstr;
int	tokval;			/* value of number tokens */
int	toktyp;			/* type of current input token */
int	toksiz;			/* size of current token */
int	tokmax = 200;		/* size of token buffer */

/*
 * Private lexical analysis data structures
 */
int	chnext;			/* next character on input stream */
Word	chtype;			/* char type of "chnext" */

Word	chtypes[129] =		/* table of char types */
{
/* | 00 nul | 01 soh | 02 stx | 03 etx | 04 eot | 05 enq | 06 ack | 07 bel | */
     chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE,
/* | 08 bs  | 09 ht  | 0a nl  | 0b vt  | 0c np  | 0d cr  | 0e so  | 0f si  | */
     chSPACE, chSPACE,  chEOLN, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE,
/* | 10 dle | 11 dc1 | 12 dc2 | 13 dc3 | 14 dc4 | 15 nak | 16 syn | 17 etb | */
     chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE,
/* | 18 can | 19 em  | 1a sub | 1b esc | 1c fs  | 1d gs  | 1e rs  | 1f us  | */
     chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE, chSPACE,
/* | 20 sp  | 21  !  | 22  "  | 23  #  | 24  $  | 25  %  | 26  &  | 27  '  | */
     chSPACE, chBADCH,chDQUOTE,  chHASH,chDOLLAR, chPCENT, chAMPER,chRQUOTE,
/* | 28  (  | 29  )  | 2a  *  | 2b  +  | 2c  ,  | 2d  -  | 2e  .  | 2f  /  | */
    chLPAREN,chRPAREN,  chSTAR, chBADCH, chCOMMA, chSUBTR, chPOINT, chSLASH,
/* | 30  0  | 31  1  | 32  2  | 33  3  | 34  4  | 35  5  | 36  6  | 37  7  | */
     chDIGIT, chDIGIT, chDIGIT, chDIGIT, chDIGIT, chDIGIT, chDIGIT, chDIGIT,
/* | 38  8  | 39  9  | 3a  :  | 3b  ;  | 3c  <  | 3d  =  | 3e  >  | 3f  ?  | */
     chDIGIT, chDIGIT, chCOLON,  chSEMI, chBADCH, chBADCH, chBADCH, chBADCH,
/* | 40  @  | 41  A  | 42  B  | 43  C  | 44  D  | 45  E  | 46  F  | 47  G  | */
     chBADCH, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA,
/* | 48  H  | 49  I  | 4a  J  | 4b  K  | 4c  L  | 4d  M  | 4e  N  | 4f  O  | */
     chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA,
/* | 50  P  | 51  Q  | 52  R  | 53  S  | 54  T  | 55  U  | 56  V  | 57  W  | */
     chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA,
/* | 58  X  | 59  Y  | 5a  Z  | 5b  [  | 5c  \  | 5d  ]  | 5e  ^  | 5f  _  | */
     chALPHA, chALPHA, chALPHA,chLBRACK, chBADCH,chRBRACK, chBADCH, chALPHA,
/* | 60  `  | 61  a  | 62  b  | 63  c  | 64  d  | 65  e  | 66  f  | 67  g  | */
    chLQUOTE, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA,
/* | 68  h  | 69  i  | 6a  j  | 6b  k  | 6c  l  | 6d  m  | 6e  n  | 6f  o  | */
     chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA,
/* | 70  p  | 71  q  | 72  r  | 73  s  | 74  t  | 75  u  | 76  v  | 77  w  | */
     chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA, chALPHA,
/* | 78  x  | 79  y  | 7a  z  | 7b  {  | 7c  |  | 7d  }  | 7e  ~  | 7f del | */
     chALPHA, chALPHA, chALPHA, chBADCH, chBADCH, chBADCH, chBADCH, chBADCH,
/* | 80 EOF |							  */
      chEOF,
};

typedef struct {
	char *op_chars;
	int op_code;
} opentry;

/* Opcodes are generated automatically. */
/* DO NOT modify the following line! */
/* Start Opcodes */

static opentry opcodes[] =
{
	{"ABOLISH", cABOLISH},
	{"ABORT", cABORT},
	{"AFUNC", cAFUNC},
	{"ALL", cALL},
	{"APPLY", cAPPLY},
	{"APRED", cAPRED},
	{"APUSHF", cAPUSHF},
	{"APUSHI", cAPUSHI},
	{"APUSHX", cAPUSHX},
	{"APUSHY", cAPUSHY},
	{"AREF", cAREF},
	{"ARG", cARG},
	{"ARITH", cARITH},
	{"ARITIES", cARITIES},
	{"ASET", cASET},
	{"CALL", cCALL},
	{"CATCH", cCATCH},
	{"CLOSE", cCLOSE},
	{"CLRERR", cCLRERR},
	{"COMPARE", cCOMPARE},
	{"COPY", cCOPY},
	{"CTYPX", cCTYPX},
	{"CTYPY", cCTYPY},
	{"CURRSTR", cCURRSTR},
	{"CUTX", cCUTX},
	{"CUTY", cCUTY},
	{"CVTTOH", cCVTTOH},
	{"DALL", cDALL},
	{"DALLEXE", cDALLEXE},
	{"DALLPRO", cDALLPRO},
	{"DEFINED", cDEFINED},
	{"DELAY", cDELAY},
	{"DISPX", cDISPX},
	{"DSIMC", cDSIMC},
	{"ERASE", cERASE},
	{"EREF", cEREF},
	{"ERROR", cERROR},
	{"EXEC", cEXEC},
	{"EXECS", cEXECS},
	{"EXECSOT", cEXECSOT},
	{"EXIT", cEXIT},
	{"FAIL", cFAIL},
	{"FLAGS", cFLAGS},
	{"FLOAD", cFLOAD},
	{"FLUSH", cFLUSH},
	{"FORK", cFORK},
	{"FRUN", cFRUN},
	{"FUNCALL", cFUNCALL},
	{"FUNCTOR", cFUNCTOR},
	{"FVAR", cFVAR},
	{"GC", cGC},
	{"GET", cGET},
	{"GETSTR", cGETSTR},
	{"GL", cGL},
	{"GLVARY", cGLVARY},
	{"GLVX2", cGLVX2},
	{"GNIL", cGNIL},
	{"GS", cGS},
	{"GSX1", cGSX1},
	{"GSX2", cGSX2},
	{"GVALX", cGVALX},
	{"GVALY", cGVALY},
	{"GVARA", cGVARA},
	{"GVARX", cGVARX},
	{"GVARY", cGVARY},
	{"ID", cID},
	{"IDTEST", cIDTEST},
	{"ILOAD", cILOAD},
	{"INST", cINST},
	{"ISEQ", cISEQ},
	{"J", cJ},
	{"JC", cJC},
	{"JCTYPX", cJCTYPX},
	{"JCTYPY", cJCTYPY},
	{"JFAIL", cJFAIL},
	{"JNVX", cJNVX},
	{"JPRED", cJPRED},
	{"JS", cJS},
	{"JTRUE", cJTRUE},
	{"JTYPX", cJTYPX},
	{"JTYPY", cJTYPY},
	{"JVX", cJVX},
	{"LABELX", cLABELX},
	{"LABELY", cLABELY},
	{"LAST", cLAST},
	{"LINKBMT", cLINKBMT},
	{"LOAD", cLOAD},
	{"LTOS", cLTOS},
	{"MAKEBMT", cMAKEBMT},
	{"MARK", cMARK},
	{"MKDEL", cMKDEL},
	{"MKOBJ", cMKOBJ},
	{"NAME", cNAME},
	{"NECK", cNECK},
	{"NOSPY", cNOSPY},
	{"NOTID", cNOTID},
	{"OCCURS", cOCCURS},
	{"ONCUT", cONCUT},
	{"OPEN", cOPEN},
	{"PC", cPC},
	{"PL", cPL},
	{"PLUS", cPLUS},
	{"PNIL", cPNIL},
	{"POPC", cPOPC},
	{"POPVX", cPOPVX},
	{"POPVY", cPOPVY},
	{"POPX", cPOPX},
	{"POPY", cPOPY},
	{"PRINTF", cPRINTF},
	{"PRO", cPRO},
	{"PROPS", cPROPS},
	{"PRTNUM", cPRTNUM},
	{"PS", cPS},
	{"PSTOT", cPSTOT},
	{"PUSHF", cPUSHF},
	{"PUSHI", cPUSHI},
	{"PUSHVX", cPUSHVX},
	{"PUSHVY", cPUSHVY},
	{"PUSHX", cPUSHX},
	{"PUSHY", cPUSHY},
	{"PUT", cPUT},
	{"PUVAL", cPUVAL},
	{"PVALY", cPVALY},
	{"PVALY2", cPVALY2},
	{"PVARX", cPVARX},
	{"PVARY", cPVARY},
	{"R", cR},
	{"RE", cRE},
	{"REPLACN", cREPLACN},
	{"REPLACV", cREPLACV},
	{"RET", cRET},
	{"SETARG", cSETARG},
	{"SETARGV", cSETARGV},
	{"SETSTR", cSETSTR},
	{"SGET", cSGET},
	{"SGETTOK", cSGETTOK},
	{"SIMC", cSIMC},
	{"SOC", cSOC},
	{"SOCE", cSOCE},
	{"SOFTCUT", cSOFTCUT},
	{"SORT", cSORT},
	{"SOS", cSOS},
	{"SOSE", cSOSE},
	{"SOT", cSOT},
	{"SPRT", cSPRT},
	{"SPUT", cSPUT},
	{"SPUTL", cSPUTL},
	{"SPY", cSPY},
	{"SPYMDEL", cSPYMDEL},
	{"SPYPT", cSPYPT},
	{"SQL", cSQL},
	{"SYMBOL", cSYMBOL},
	{"SYSCALL", cSYSCALL},
	{"T", cT},
	{"TE", cTE},
	{"THROW", cTHROW},
	{"TOKEN", cTOKEN},
	{"TR", cTR},
	{"TRACE", cTRACE},
	{"TRE", cTRE},
	{"TYPX", cTYPX},
	{"TYPY", cTYPY},
	{"UC", cUC},
	{"UL", cUL},
	{"ULVX", cULVX},
	{"ULVXLVX", cULVXLVX},
	{"ULVXVARA", cULVXVARA},
	{"ULVXVARX", cULVXVARX},
	{"ULVY", cULVY},
	{"UNCOPY", cUNCOPY},
	{"UNIV", cUNIV},
	{"US", cUS},
	{"UVALX", cUVALX},
	{"UVALY", cUVALY},
	{"UVARA", cUVARA},
	{"UVARALVX", cUVARALVX},
	{"UVARAVARA", cUVARAVARA},
	{"UVARX", cUVARX},
	{"UVARXLVX", cUVARXLVX},
	{"UVARXVARX", cUVARXVARX},
	{"UVARY", cUVARY},
	{"UVARYVARY", cUVARYVARY},
	{"UVOID", cUVOID},
	{"UVOID1", cUVOID1},
	{"WAKE", cWAKE},
	{"WEVAL", cWEVAL},
	{"WISEQ", cWISEQ},
	{"WOKEN", cWOKEN},
	{"abolish", cABOLISH},
	{"abort", cABORT},
	{"afunc", cAFUNC},
	{"all", cALL},
	{"apply", cAPPLY},
	{"apred", cAPRED},
	{"apushf", cAPUSHF},
	{"apushi", cAPUSHI},
	{"apushx", cAPUSHX},
	{"apushy", cAPUSHY},
	{"aref", cAREF},
	{"arg", cARG},
	{"arith", cARITH},
	{"arities", cARITIES},
	{"aset", cASET},
	{"call", cCALL},
	{"catch", cCATCH},
	{"close", cCLOSE},
	{"clrerr", cCLRERR},
	{"compare", cCOMPARE},
	{"copy", cCOPY},
	{"ctypx", cCTYPX},
	{"ctypy", cCTYPY},
	{"currstr", cCURRSTR},
	{"cutx", cCUTX},
	{"cuty", cCUTY},
	{"cvttoh", cCVTTOH},
	{"dall", cDALL},
	{"dallexe", cDALLEXE},
	{"dallpro", cDALLPRO},
	{"defined", cDEFINED},
	{"delay", cDELAY},
	{"dispx", cDISPX},
	{"dsimc", cDSIMC},
	{"erase", cERASE},
	{"eref", cEREF},
	{"error", cERROR},
	{"exec", cEXEC},
	{"execs", cEXECS},
	{"execsot", cEXECSOT},
	{"exit", cEXIT},
	{"fail", cFAIL},
	{"flags", cFLAGS},
	{"fload", cFLOAD},
	{"flush", cFLUSH},
	{"fork", cFORK},
	{"frun", cFRUN},
	{"funcall", cFUNCALL},
	{"functor", cFUNCTOR},
	{"fvar", cFVAR},
	{"gc", cGC},
	{"get", cGET},
	{"getstr", cGETSTR},
	{"gl", cGL},
	{"glvary", cGLVARY},
	{"glvx2", cGLVX2},
	{"gnil", cGNIL},
	{"gs", cGS},
	{"gsx1", cGSX1},
	{"gsx2", cGSX2},
	{"gvalx", cGVALX},
	{"gvaly", cGVALY},
	{"gvara", cGVARA},
	{"gvarx", cGVARX},
	{"gvary", cGVARY},
	{"id", cID},
	{"idtest", cIDTEST},
	{"iload", cILOAD},
	{"inst", cINST},
	{"iseq", cISEQ},
	{"j", cJ},
	{"jc", cJC},
	{"jctypx", cJCTYPX},
	{"jctypy", cJCTYPY},
	{"jfail", cJFAIL},
	{"jnvx", cJNVX},
	{"jpred", cJPRED},
	{"js", cJS},
	{"jtrue", cJTRUE},
	{"jtypx", cJTYPX},
	{"jtypy", cJTYPY},
	{"jvx", cJVX},
	{"labelx", cLABELX},
	{"labely", cLABELY},
	{"last", cLAST},
	{"linkbmt", cLINKBMT},
	{"load", cLOAD},
	{"ltos", cLTOS},
	{"makebmt", cMAKEBMT},
	{"mark", cMARK},
	{"mkdel", cMKDEL},
	{"mkobj", cMKOBJ},
	{"name", cNAME},
	{"neck", cNECK},
	{"nospy", cNOSPY},
	{"notid", cNOTID},
	{"occurs", cOCCURS},
	{"oncut", cONCUT},
	{"open", cOPEN},
	{"pc", cPC},
	{"pl", cPL},
	{"plus", cPLUS},
	{"pnil", cPNIL},
	{"popc", cPOPC},
	{"popvx", cPOPVX},
	{"popvy", cPOPVY},
	{"popx", cPOPX},
	{"popy", cPOPY},
	{"printf", cPRINTF},
	{"pro", cPRO},
	{"props", cPROPS},
	{"prtnum", cPRTNUM},
	{"ps", cPS},
	{"pstot", cPSTOT},
	{"pushf", cPUSHF},
	{"pushi", cPUSHI},
	{"pushvx", cPUSHVX},
	{"pushvy", cPUSHVY},
	{"pushx", cPUSHX},
	{"pushy", cPUSHY},
	{"put", cPUT},
	{"puval", cPUVAL},
	{"pvaly", cPVALY},
	{"pvaly2", cPVALY2},
	{"pvarx", cPVARX},
	{"pvary", cPVARY},
	{"r", cR},
	{"re", cRE},
	{"replacn", cREPLACN},
	{"replacv", cREPLACV},
	{"ret", cRET},
	{"setarg", cSETARG},
	{"setargv", cSETARGV},
	{"setstr", cSETSTR},
	{"sget", cSGET},
	{"sgettok", cSGETTOK},
	{"simc", cSIMC},
	{"soc", cSOC},
	{"soce", cSOCE},
	{"softcut", cSOFTCUT},
	{"sort", cSORT},
	{"sos", cSOS},
	{"sose", cSOSE},
	{"sot", cSOT},
	{"sprt", cSPRT},
	{"sput", cSPUT},
	{"sputl", cSPUTL},
	{"spy", cSPY},
	{"spymdel", cSPYMDEL},
	{"spypt", cSPYPT},
	{"sql", cSQL},
	{"symbol", cSYMBOL},
	{"syscall", cSYSCALL},
	{"t", cT},
	{"te", cTE},
	{"throw", cTHROW},
	{"token", cTOKEN},
	{"tr", cTR},
	{"trace", cTRACE},
	{"tre", cTRE},
	{"typx", cTYPX},
	{"typy", cTYPY},
	{"uc", cUC},
	{"ul", cUL},
	{"ulvx", cULVX},
	{"ulvxlvx", cULVXLVX},
	{"ulvxvara", cULVXVARA},
	{"ulvxvarx", cULVXVARX},
	{"ulvy", cULVY},
	{"uncopy", cUNCOPY},
	{"univ", cUNIV},
	{"us", cUS},
	{"uvalx", cUVALX},
	{"uvaly", cUVALY},
	{"uvara", cUVARA},
	{"uvaralvx", cUVARALVX},
	{"uvaravara", cUVARAVARA},
	{"uvarx", cUVARX},
	{"uvarxlvx", cUVARXLVX},
	{"uvarxvarx", cUVARXVARX},
	{"uvary", cUVARY},
	{"uvaryvary", cUVARYVARY},
	{"uvoid", cUVOID},
	{"uvoid1", cUVOID1},
	{"wake", cWAKE},
	{"weval", cWEVAL},
	{"wiseq", cWISEQ},
	{"woken", cWOKEN},
};

static int nopcodes = sizeof(opcodes) / sizeof(opentry);

/* End Opcodes */
/* DO NOT modify the preceding line! */

static opentry pseudop[] =
{
	{"word", popWORD},
	{"pred", popPRED},
	{"string", popSTRING},
	{"module", popMODULE},
	{"dirty", popDIRTY},
	{"clause", popCLAUSE},
	{"WORD", popWORD},
	{"PRED", popPRED},
	{"STRING", popSTRING},
	{"MODULE", popMODULE},
	{"DIRTY", popDIRTY},
	{"CLAUSE", popCLAUSE}
};

/*
 * yylex:
 *	does lexical analysis of input stream "yyin"
 *	returns an int which is a token type
 *	sets some global variable for use by parser
 *	 tokstr: string containing text of input token
 *	 toktyp: copy of the value returned by lex()
 */
int
yylex()
{
	_char	*c;
	int	isfloat, prevtype;

startTok:
	switch (chtype)
	{
	when chBADCH:
		do
			nextchar();
		while (chtype == chBADCH);

		if (chnext == EOF)
			Return(EOF)
		else
			goto startTok;

	when chSPACE:
		do
			nextchar();
		while (chtype == chSPACE);

		if (chnext == EOF)
			Return(EOF)
		else
			goto startTok;

	when chEOLN:
		nextchar();
		Return(EOLN)

	when chPCENT:
		do
			nextchar();
		while (chtype != chEOLN);
		nextchar();
		Return(EOLN)

	when chSLASH:
		nextchar();
		if (chtype != chSTAR)
			Return(ARITY)
		else {
			/* process comment */
			int prevtype = chSTAR;

			for (;;) {
				nextchar();
				if (chtype == chSLASH && prevtype == chSTAR)
					break;
				if (chtype == chEOF)
					yyerror("unterminated comment");
				prevtype = chtype;
			}
			nextchar();
			goto startTok;
		}

	when chPOINT: {
		_int i;
		register opentry *k;

		nextchar();
		c = tokstr;
		*c = chnext;
		i = 0;
		do
		{
			if (i == tokmax) {
				expandTokBuf();
				c = tokstr+i;
			}
			*++c = nextchar();
			i++;

		} while (chtype == chALPHA || chtype == chDIGIT);
		*c = '\0';
		toksiz = i;
		for (	k = pseudop, i = 0;
				i < sizeof(pseudop) / sizeof(opentry);
				k++, i++)
			if (k->op_chars[0] == tokstr[0] && streq(k->op_chars, tokstr))
				Return(k->op_code)
		yyerror("invalid pseudo-op");
	}

	when chDIGIT:
	case chSUBTR: {
		_int i;

		c = tokstr;
		*c = chnext;
		isfloat = FALSE;
		i = 0;
		do
		{
			if (i == tokmax) {
				expandTokBuf();
				c = tokstr+i;
			}
			*++c = nextchar();
			if (chtype == chSUBTR || chtype == chPOINT ||
				chnext == 'e' || chnext == 'E') {
				chtype = chDIGIT;
				isfloat = TRUE;
			}
			i++;

		} while (chtype == chDIGIT);
		*c = '\0';
		toksiz = i;
		if (isfloat) {
			float_val(atof(tokstr));
			Return(FLOAT)
		}
		else {
			int_val(atoi(tokstr));
			Return(NUMBER)
		}
	}

		
	when chALPHA: {
		_int	i, hi, lo, mid;

		c = tokstr;
		*c = chnext;
		i = 0;
		do
		{
			if (i == tokmax) {
				expandTokBuf();
				c = tokstr+i;
			}
			*++c = nextchar();
			i++;

		} while (chtype == chALPHA || chtype == chDIGIT);
		*c = '\0';
		toksiz = i;
		/* binary search of opcode table */
		lo = 0; hi = nopcodes-1;
		while (lo <= hi)
		{
			mid = (hi + lo) >> 1;
			i = strcmp(tokstr, opcodes[mid].op_chars);
			if (i < 0) 
				hi = mid - 1;
			else if (i > 0)
				lo = mid + 1;
			else {
				int_val(opcodes[mid].op_code);
				Return(OPCODE)
			}
		}
		string_val(tokstr);
		Return(ATOM)
	}

	when chLBRACK:
		nextchar();
		if (chtype != chRBRACK)
			yyerror("missing right bracket");
		string_val("[]");
		Return(ATOM)

	when chRQUOTE: {
		_int i;

		nextchar();	/* skip quote */
		c = tokstr;
		i = 0;
		for (;;) {
			if (chnext == EOF)
				break;
			else if (chtype == chRQUOTE) {
				_int k;

				k = chnext;
				nextchar();
				if (chtype != chRQUOTE)
					break;
				if (i == tokmax) {
					expandTokBuf();
					c = tokstr+i;
				}
				i++;
				*c++ = k;
			}
			if (i == tokmax) {
				expandTokBuf();
				c = tokstr+i;
			}
			i++;
			*c++ = chnext;
			nextchar();
		}
		*c = '\0';
		if (chnext == EOF)
			fatal("Non-terminated atom string \"%s\"\n", tokstr);
		undouble_quotes('\'', tokstr);
		string_val(tokstr);
		Return(ATOM)
	}

	when chDQUOTE: {
		_int i;

		nextchar();	/* skip double quote */
		c = tokstr;
		i = 0;
		for (;;) {
			if (chnext == EOF)
				break;
			else if (chtype == chDQUOTE) {
				_int k;

				k = chnext;
				nextchar();
				if (chtype != chDQUOTE)
					break;
				if (i == tokmax) {
					expandTokBuf();
					c = tokstr+i;
				}
				i++;
				*c++ = k;
			}
			if (i == tokmax) {
				expandTokBuf();
				c = tokstr+i;
			}
			i++;
			*c++ = chnext;
			nextchar();
		}
		*c = '\0';
		if (chnext == EOF)
			fatal("Non-terminated string \"%s\"\n", tokstr);
		undouble_quotes('"', tokstr);
		string_val(tokstr);
		Return(STRING)
	}

	when chCOMMA:
		nextchar();
		Return(COMMA)
	when chDOLLAR:
		nextchar();
		Return(CONST)
	when chAMPER:
		nextchar();
		Return(REF)
	when chHASH:
		nextchar();
		Return(TAG)
	when chCOLON:
		nextchar();
		Return(LABEL)
	when chLPAREN:
		nextchar();
		Return(LPAREN)
	when chRPAREN:
		nextchar();
		Return(RPAREN)
	when chEOF:
		Return(EOF);
	otherwise:
		do
			nextchar();
		while (chtype == chBADCH);

		if (chnext == EOF)
			Return(EOF)
		else
			goto startTok;
	}

	do
	{
		nextchar();

	} while (chtype == chBADCH);

	if (chnext == EOF)
		Return(EOF)
	else
		goto startTok;
}


/*
 * nextchar:
 *	Read next character from input stream
 */
nextchar()
{
	chnext = getc(yyin);
	if (chnext == EOF)
		chnext = 128;
	else if (chnext == '\n')
		yylineno++;

#ifdef XXDBUG
	fprintf(stderr,"nextc:'%c'\n",chnext);
	/* putc(chnext, stderr); */
#endif /* XXDBUG */

	chtype = chtypes[chnext];
	return (chnext);
}


/*
 * expandTokBuf:
 *	Enlarge the token string buffer for excessive tokens
 */
expandTokBuf()
{
	_char	*newbuf;

	if ((newbuf = malloc(tokmax+200)) == StrNULL)
		fatal("out of token buffer space");
	
	strncpy(newbuf, tokstr, tokmax);
	if (tokstr != xbuf)
		free(tokstr);

	tokstr = newbuf;
	tokmax += 200;
}


/*
 * isblank:
 *	Test whether a string is full of nothing but space(s)
 */
Bool
isblank(line)
char *line;
{
	char *c;

	for (c = line; *c != '\0'; c++)
		if (!isspace(*c))
			return(FALSE);
	return(TRUE);
}


/*
 * save_string:
 *	Make a copy of a new string in the string table
 */
char *
save_string(str)
char *str;
{
	register char *s, *ss, *xs;

#ifdef SLOW
	/*
	 * Check that it already exists
	 * (there *must* be faster ways
	 * than this ... e.g. hash table)
	 *
	 * Even more so now that profile
	 * studies have shown that 50% of
	 * the time is spent in here, and
	 * we don't even call it that much!
	 */
	s = str;
	for (xs = strings; xs < xstrings; xs++) {
		if (*xs == *s) {
			for (ss = xs; *s != ChNULL; s++,xs++)
				if (*xs != *s)
					break;
			if (*xs == *s)
				return(ss);
			else
				s = str;
		}
		while (*xs != ChNULL)
			xs++; /* skip to end of string */
	}
#endif /* SLOW */

	/*
	 * Check that there's room in the string table
	 */
	if ((xstrings - strings + strlen(str) + 1) >= STRINGS_SIZE)
		grow_work_area(strings, xstrings, STRINGS_SIZE);

	/*
	 * Add to table if not already there
	 */
	s = str;
	xs = xstrings;
	while (*s != ChNULL)
		*xs++ = *s++;
	*xs++ = ChNULL;

	ss = xstrings;
	xstrings = xs;
	return(ss);
}


/*
 * undouble_quotes:
 *	Scan a string and fix occurences of '' and ""
 */
undouble_quotes(qchar, str)
char qchar, *str;
{
	register char *s, *t, c;

	for(s = t = str; *s != '\0'; ) {
		c = *s++;
		if(c == qchar && *s == c) {
			*t++ = c;
			s++;
		} else if(c == '\\') {
			c = *s++;
			switch(c) {
			when 'b': *t++ = 8;
			when 't': *t++ = 9;
			when 'n': *t++ = 10;
			when 'v': *t++ = 11;
			when 'f': *t++ = 12;
			when 'r': *t++ = 13;
			when 'e': *t++ = 27;
			when 's': *t++ = 32;
			when 'd': *t++ = 127;

			when '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7': {
				register int i, n;

				i = c - '0';
				for(n = 1; n < 3; n++) {
					c = *s++;
					if('0' <= c && c < '8')
						i = i * 8 + c - '0';
					else
						s--;
				}
				*t++ = i;
			}
			when '^':
				c = *s++;
				*t++ = c & 0x1f;

			break;
			default:
				*t++ = c;
			}
		} else
			*t++ = c;
	}
	*t = '\0';
}


/*
 * save_float:
 *	Grab copy of value of floating point number
 */
double *
save_float(val)
double val;
{
	double *dbl;

	if ((dbl = (double *)malloc(sizeof(double))) == (double *)NULL)
		fatal("No space for making new floating point number");

#if 0
fprintf(stderr, "save %0.5f @ %08x\n", val, dbl);
#endif
	*dbl = val;
	return(dbl);
}
