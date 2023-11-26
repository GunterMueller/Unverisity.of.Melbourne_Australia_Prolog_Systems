/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * main.c  -  Nepolog assembler
 */

#include "na.h"
#ifndef BSD4
#include <fcntl.h>
#endif

#define	WRITING			(O_WRONLY|O_CREAT|O_TRUNC)

char	*my_name;		/* name by which "na" invoked */

char	*src_name;		/* name of source file for current module */
File	src_file;		/* source code file pointer */

char	obj_name[BUFSIZ];	/* name of object file for current module */
int	obj_file;		/* object code file descriptor */

int	yyerrs = 0;		/* how many parsing errors so far */
int	naerrs;			/* how many assembler errors so far */

int	mergeflg = 1;		/* merge some instruction pairs */

Header	module_head;		/* header information for module */
char	*module_name;		/* name of module being processed */

/*
 * main:
 *	Process command line args, invoke assemble() for each module
 */
main(argc,argv)
int argc;
char **argv;
{
	my_name = argv[0];

	init_work_areas();

	if (argc == 1) {
		src_name = "stdin";
		assemble();
	} else {
		while (--argc > 0) {
			src_name = *++argv;
			if(strcmp(src_name, "-m") == 0)
				mergeflg = 1;
			else if(strcmp(src_name, "-n") == 0)
				mergeflg = 0;
			else
				assemble();
		}
	}
	if(naerrs)
		exit(12);
	else
		exit(0);
}

/*
 * assemble:
 *	Assemble source file whose name is given by "src_name"
 */
assemble()
{
	register char	*c;

	if (streq(src_name, "stdin")) {
		src_file = stdin;
		strcpy(obj_name, "xyzzy.no");
	} else {
		strcpy(obj_name, src_name);

		if ((c = rindex(obj_name,'.')) == 0 || !streq(c,".ns")) {
			err("invalid source file: %s", src_name);
			return;
		}
		*++c = 'n'; *++c = 'o';

		if ((src_file = fopen(src_name,"r")) == NULL) {
			err("can't open source file: %s", src_name);
			return;
		}
	}
	if ((obj_file = open(obj_name,WRITING,0666)) < 0) {
		err("can't open object file: %s", obj_name);
		return;
	}

	asm_init();
	yyinit();

	yyparse(src_file);

	if (yyerrs || !code_gen())
		unlink(obj_name);

	fclose(src_file);
	close(obj_file);
}

/*
 * asm_init:
 *	Initialise globals for assembler
 */
asm_init()
{
	xcode = code;
	xdata = data;
	xheap = heap;
	xtypes = types;
	xdtypes = dtypes;
	xhtypes = htypes;
	xsymtab = symtab;
	xstrings = strings;
	module_name = strings;
	strcpy(module_name,src_name);
	xstrings = strings+strlen(src_name)+1;

	code_start = 0;
	types_start = 0;
	heap_start = 0;
	htypes_start = 0;

	init_symbols();
	init_labels();
}

/*
 * code_gen:
 *	Generate "code" by dumping contents of work areas to "objfile"
 */
code_gen()
{
	int	i;
	char	*z;
	extern int errno, sys_nerr;
	extern char *sys_errlist[];

#ifdef BORING
	/*
	 * Zero out the bytes included by aligning
	 */
	i = xtypes - types;
	z = xtypes;
	while ((i++ % 4) != 0)
		*z++ = '\0';

	i = xdtypes - dtypes;
	z = xdtypes;
	while ((i++ % 4) != 0)
		*z++ = '\0';

	i = xhtypes - htypes;
	z = xhtypes;
	while ((i++ % 4) != 0)
		*z++ = '\0';
#endif /* BORING */

	module_head.nl_magic = nMAGIC;
	module_head.nl_version = NUVERSION;
	module_head.nl_code = bytesin(code, xcode, Word);
	module_head.nl_data = bytesin(data, xdata, Word);
	module_head.nl_heap = bytesin(heap, xheap, Word);
	module_head.nl_types = aligned(bytesin(types, xtypes, char));
	module_head.nl_dtypes = aligned(bytesin(dtypes, xdtypes, char));
	module_head.nl_htypes = aligned(bytesin(htypes, xhtypes, char));
	module_head.nl_symtab = bytesin(symtab, xsymtab, SymTab);
	module_head.nl_strings = bytesin(strings, xstrings, char);

	if(	   write(obj_file, &module_head, sizeof(Header)) == -1
		|| write(obj_file, code, module_head.nl_code) == -1
		|| write(obj_file, data, module_head.nl_data) == -1
		|| write(obj_file, heap, module_head.nl_heap) == -1
		|| write(obj_file, types, module_head.nl_types) == -1
		|| write(obj_file, dtypes, module_head.nl_dtypes) == -1
		|| write(obj_file, htypes, module_head.nl_htypes) == -1
		|| write(obj_file, symtab, module_head.nl_symtab) == -1
		|| write(obj_file, strings, module_head.nl_strings) == -1
	) {
		if(errno < sys_nerr)
			err("%s", sys_errlist[errno]);
		else
			perror(my_name);
		return(0);
	}
	return(1);
}
