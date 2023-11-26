/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * dis.c  -  NU-Prolog dis-assembler
 */

#include "na.h"
#ifndef BSD4
#include <fcntl.h>
#endif

char	*my_name;		/* name by which program invoked */

char	obj_name[BUFSIZ];
int	obj_file;		/* object code file descriptor */

int	naerrs;			/* how many disassembler errors so far */

Header	module_head;		/* header information for module */

char	*bytes;			/* block of memory for segments */

Word	*code;			/* start of code segment for module */
Word	*data;			/* start of data segment for module */
Word	*heap;			/* start of junk heap segment for module */
char	*types;			/* start of code type-info segment for module */
char	*dtypes;		/* start of data type-info segment for module */
char	*htypes;		/* start of heap type-info segment for module */
SymTab	*symtab;		/* start of symbol table segment for module */
char	*strings;		/* start of string table segment for module */


/*
 * main:
 *	Process command line args, invoke assemble() for each module
 */
main(argc,argv)
int argc;
char **argv;
{
	my_name = argv[0];

	if (argc == 1)
		fatal("Usage: %s obj_file ...", my_name);
	
	while (--argc > 0) {
		strcpy(obj_name,*++argv);
/* fprintf(stderr,"name:%s:\n",obj_name); */
		disassemble();
	}
}

/*
 * disassemble:
 *	Disassemble object file whose name is given by "obj_name"
 */
disassemble()
{
	register int	i;
	register char	*t;
	register Word	*w;
	register Word	fn;
	register SymTab	*s;
	char name_buf[80];
	extern int displayWord();

	if ((obj_file = open(obj_name,O_RDONLY,0666)) < 0) {
		err("can't open object file: %s", obj_name);
		return;
	}

	read(obj_file, &module_head, sizeof(Header));
	if (module_head.nl_magic != nMAGIC) {
		err("%s: not a NU-Prolog object file",obj_name);
		return;
	}

	alloc_work_areas();

	printf("%-8s %8s %8s %8s %8s %8s %8s %8s %8s\n",
			"Header:", "code", "data", "heap",
			"types", "dtypes", "htypes", "symtab", "strings");
	printf("%08x %08x %08x %08x %08x %08x %08x %08x %08x\n",
			module_head.nl_magic,
			module_head.nl_code,
			module_head.nl_data,
			module_head.nl_heap,
			module_head.nl_types,
			module_head.nl_dtypes,
			module_head.nl_htypes,
			module_head.nl_symtab,
			module_head.nl_strings);
	printf("%8o %8d %8d %8d %8d %8d %8d %8d %8d\n",
			module_head.nl_magic,
			module_head.nl_code,
			module_head.nl_data,
			module_head.nl_heap,
			module_head.nl_types,
			module_head.nl_dtypes,
			module_head.nl_htypes,
			module_head.nl_symtab,
			module_head.nl_strings);

	printf("\nCode:\n");
	for (w = code, t = types; w < data; t++)
		w += displayWord(code, w, *t);

	printf("\nData:\n");
	for (w = data, t = dtypes; w < heap; t++)
		w += displayWord(data, w, *t);

	printf("\nHeap:\n");
	for (w = heap, t = htypes; w < (Word*)types; t++)
		w += displayWord(heap, w, *t);

	printf("\nSymbol Table:\n");
	for (s = symtab; s < (SymTab*)strings; s++) {
#if 0
		if (strings[s->sym_module] == ChNULL)
#else
		if (s->sym_module <= 0)
#endif
			printf("%08x\t%-20s",
				bytesin(symtab,s,SymTab), strings+s->sym_name);
		else {
			sprintf(name_buf, "%s.%s",
				strings+s->sym_module, strings+s->sym_name);
			printf("%08x\t%-20s",
				bytesin(symtab,s,SymTab), name_buf);
		}
		fn = s->sym_functors;
		while (fn != lUNUSED) {
			printf("\t/%d", heap[fn+1]);
			fn = heap[fn+3];
		}
		putchar('\n');
	}

	free(bytes);
	close(obj_file);
}

/*
 * alloc_work_areas:
 *	Allocate space for segments of object file & fill with data
 */
alloc_work_areas()
{
	int	total_size;
	char	*base;

	total_size = module_head.nl_code +
			module_head.nl_data +
			module_head.nl_heap +
			module_head.nl_types +
			module_head.nl_dtypes +
			module_head.nl_htypes +
			module_head.nl_symtab +
			module_head.nl_strings ;

	/*
	 * Assumes "malloc" always returns word-aligned address
	 */
	if ((bytes = (char *)malloc(total_size)) == NULL)
		fatal("can't allocate work areas: %d bytes", total_size);

	base	= bytes;
	code	= (Word *)(base);
	data	= (Word *)(base += module_head.nl_code);
	heap	= (Word *)(base += module_head.nl_data);
	types	= (char *)(base += module_head.nl_heap);
	dtypes	= (char *)(base += module_head.nl_types);
	htypes	= (char *)(base += module_head.nl_dtypes);
	symtab	= (SymTab *)(base += module_head.nl_htypes);
	strings	= (char *)(base += module_head.nl_symtab);

	if(read(obj_file, code, module_head.nl_code) != module_head.nl_code)
		fatal("Can't read code.");
	if(read(obj_file, data, module_head.nl_data) != module_head.nl_data)
		fatal("Can't read data.");
	if(read(obj_file, heap, module_head.nl_heap) != module_head.nl_heap)
		fatal("Can't read heap.");
	if(read(obj_file, types, module_head.nl_types) != module_head.nl_types)
		fatal("Can't read types.");
	if(read(obj_file, dtypes, module_head.nl_dtypes) != module_head.nl_dtypes)
		fatal("Can't read dtypes.");
	if(read(obj_file, htypes, module_head.nl_htypes) != module_head.nl_htypes)
		fatal("Can't read htypes.");
	if(read(obj_file, symtab, module_head.nl_symtab) != module_head.nl_symtab)
		fatal("Can't read symtab.");
	if(read(obj_file, strings, module_head.nl_strings) != module_head.nl_strings)
		fatal("Can't read strings.");
}
