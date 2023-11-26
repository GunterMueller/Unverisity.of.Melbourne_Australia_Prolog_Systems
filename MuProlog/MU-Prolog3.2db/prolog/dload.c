/************************************************************************
*									*
*				MU-PROLOG				*
*				=========				*
*									*
* (C) Copyright 1985 Lee Naish, Melbourne University			*
*									*
*	Written by Lee Naish, Department of Computer Science,		*
*	Melbourne University, Parkville 3052, Australia.		*
*									*
*	No liability for errors or omissions in this system is		*
*	expressed, implied, impressed or explied.			*
*									*
************************************************************************/

#ifdef DLOAD
/*
 * SO FAR, only works on Berkeley-type Unix systems.
 * Don't bother to define DLOAD on other systems,
 * or you'll simply get a bunch of syntax errors.
 */

#include "local.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/file.h>
#include <stdio.h>
#include <a.out.h>

#ifdef PAGSIZ
#define ROUND PAGSIZ
#else
#define ROUND 1024
#endif

	/* defined in the sys/param.h */
#undef DELAY
#undef CLSIZE

#include "types.h"
#include "pred.h"
#include "dict.h"

#ifdef pyramid		/* according to the documentation 'pyramid' is */
#define pyr		/* defined on pyramids but, here at least, 'pyr' is */
#endif
#ifdef pyr		/* Pyramid 90X */
#include <sys/mman.h>
#endif

struct symbol_references{
	char *name;
	int (*function)();
	};

#define MAXREFS 20
#define MAXOBJS 10
static char *objects[MAXOBJS];
static struct symbol_references refs[MAXREFS];

/*	dynamic load of functions in object files. eg. */
/*	$dload(['file.o'...], '-llib1 -llib2', ['_entry'...], [pred(_,_)...]) */
p_dload(t, l)
	Ptr t;
	levtype l;
{
	Ptr file, entry, pred, p, fn;
	levtype lfile, libr, lentry, lpred, l1;
	char **obj;
	struct symbol_references *ref;
	int nobjs, nentries, npreds;

	findbind(targ(1, t), l, &file, &lfile);
	findbind(targ(2, t), l, &libr, &l1);
	findbind(targ(3, t), l, &entry, &lentry);
	findbind(targ(4, t), l, &pred, &lpred);
	if(!IsAtom(libr)) {
		plerror(EECONST);
		return(ERROR);
	}
	nobjs = 0;
	obj = objects;
	while (tdict(file) == Ddot) {		/* scan list of obj files */
		findbind(targ(1,file), lfile, &p, &l1);
		if(!IsAtom(p)) {
			plerror(EEFUNC);
			return(ERROR);
		}
		*obj++ = dname(tdict(p));
		if(++nobjs >= MAXOBJS) {
			plerror(EELIST);	/* change this ## */
			return(ERROR);
		}
		findbind(targ(2, file), lfile, &file, &lfile);
	}
	if (tdict(file) != Dnil) {
		plerror(EELIST);
		return(ERROR);
	}
	*obj = (char *) 0;
	nentries = 0;		/* check overflow and #entries = #preds */
	ref = refs;
	while (tdict(entry) == Ddot) {
		findbind(targ(1,entry), lentry, &p, &l1);
		if(!IsAtom(p)) {
			plerror(EEFUNC);
			return(ERROR);
		}
		ref->function = (int (*)())0;
		(ref++)->name = dname(tdict(p));
		if(++nentries >= MAXREFS) {
			plerror(EELIST);	/* change this ## */
			return(ERROR);
		}
		findbind(targ(2, entry), lentry, &entry, &lentry);
	}
	if (tdict(entry) != Dnil) {
		plerror(EELIST);
		return(ERROR);
	}
	(ref)->name = (char *) 0;
	npreds = 0;
	while (tdict(pred) == Ddot) {
		findbind(targ(1, pred), lpred, &p, &l1);
		if(!IsFunc(p)) {
			plerror(EEFUNC);
			return(ERROR);
		}
		if(dprotect(tdict(p))) {
			plerror(EPROT);
			return(ERROR);
		}
		if(++npreds > nentries) {
			plerror(EELIST);	/* change this ## */
			return(ERROR);
		}
		findbind(targ(2, pred), lpred, &pred, &lpred);
	}
	if (tdict(pred) != Dnil || npreds != nentries) { /* change this ## */
		plerror(EELIST);
		return(ERROR);
	}
	if (nobjs == 0)
		if(nentries != 0)
			return(FAIL);
		else
			return(SUCCEED);
	if( Set_program_name( BIN ) ){
		fprintf( stderr, "cannot find program file %s\n", BIN );
		plerror(EDLOAD);
		return(ERROR);
	}
	if( Load_file( objects, refs, dname(tdict(libr)) ) ){
		plerror(EDLOAD);
		return(ERROR);
	}
	findbind(targ(4, t), l, &pred, &lpred);
	ref = refs;
	while (tdict(pred) == Ddot) {
		findbind(targ(1, pred), lpred, &p, &l1);
		if(fn = (Ptr)(ref++)->function)
			fassert(tdict(p), fn, -1, 0);
		else
			fprintf(stderr, "entry point not found: %s\n",
								(ref-1)->name);
		findbind(targ(2, pred), lpred, &pred, &lpred);
	}
	return(SUCCEED);
}

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
	struct symbol_references{
		char *name;	(pointer to entry point name)
		int (*function)(); (call address)
		};
	Set_program_name( str )
		Must be called first to set up the program text file name.
		This is used in the loading process.
	Load_file( objects, entries, library )
		char **objects;
		struct symbol_references *symbols;
		char *library;

		The "objects" points to a list of object file names terminated
		with a null (char *)0.  For example, a static definition 
			char *objects[] = {"lib.a", "usr_lib.b", 0 };
		Note: currently, a maximum of 10 object files can be
			specified.  This limit is set by the compile time
			variable MAXOBJS.

		The "symbols" points to a list of symbol_references structures.
		Each structure has the name of a function, and a function
		address field which is set to the name of the function.
			struct symbol_references symbols[] = {
				{ _ugly }, { _really }, { (char *)0 } };
			NOTE: this is acceptable C, missing field initializers
			are supposed to be acceptable.

		The "library" should be null (char *)0, or a string containing
		loader library information.  For example:
			entries = "-lmath -ltermlib" 
		Note that this can also be used to specify a list of object
		fields, set up "undefined symbol information" to force
		loading from libraries ("-u _ugly mylib.a -lmath").

	IMPLEMENTATION:
		The functions use the stdio package, and write error
	messages on STDOUT.  If an error is detected, a 1 is returned.
	The PATHS environment variable is used to determine the executing
	function.  If it is not set, then a default set of paths is used.
		The sbrk() system call is used to allocate memory.  This
	should have no effect on a properly designed memory allocation package,
	such as malloc(3).
		The external entry point names must be specified in the
	form that the loader needs.  For example, the C name "ugly"
	would have to be specified as "_ugly", and the F77 name "ugly"
	as "_ugly_".
*/

char Program_name[ MAXPATHLEN ];	/* saves the name of the program */
char Symbol_name[ MAXPATHLEN ];		/* name of the new object file */
extern char 
	*Strend(),	/* append to end of string */
	*strcpy(), *sys_errlist[], *mktemp();
extern int errno;	/* system errors */
extern caddr_t sbrk();	/* memory allocation */

/*
	Set_program_name( str )
		Checks the file name for execute perms,
		and then saves the name.
*/

Set_program_name( str )
	char *str;
    {
	int i;

	(void)strcpy( Program_name, str );
	if( (i = Ok_to_exec( Program_name )) == 0 ){
		fprintf( stderr, "cant exec %s\n", Program_name);
	}
	return( (i == 0) );
    }

/*

		Ok_to_exec( str )
			check perms for execution.
*/

Ok_to_exec( str )
	char *str;
    {
	struct stat stbuff;

	return(
		0 == stat(str, &stbuff)		/* stat it */
		&& (stbuff.st_mode&S_IFMT) == S_IFREG	/* check for file */
		&& 0 == access(str,X_OK)	/* and can be executed */
		);
    }

/*
	Load_file( objects, entries, library )
		NOTE: the number of object files is limited to 10
		in this implementation.
	1. generate a command for the loader.
		NOTE: this uses the execv function.
	2. execute the loader command.
	3. get the entry points
*/

#define round(x,s) ((((x)-1) & ~((s)-1)) + (s))

int
Load_file( objects, entries, library )
	char **objects;
	struct symbol_references *entries;
	char *library;
    {
	char *file;		/* temp variable */
	char cmd_str[2*MAXPATHLEN + 2048],
		*cmd, *cmdend;	/* command string for loader */
	static char *file_proto="/tmp/llXXXXXX";	/* tmp name */
	int readsize,		/* total amount of object to read */
		totalsize,	/* total memory needed */
		n, diff;	/* working variables */
	caddr_t end;		/* current end of memory */
	int fcb;		/* used to read the object file */
	struct exec header;	/* a.out header */
	struct symbol_references *entry;	/* single pointer */
	long *savep;		/* ptr to buf for saving info */

#	ifdef DEBUG
		printf( "calling Load_file\n");
#	endif DEBUG
	for(savep = dloadsbuf; *savep ; savep += 2)
		if(savep >= dloadsbuf+(MAXDLOAD-1)*2) {
			fprintf(stderr, "Too many dynamic loads\n");
			return 1;
		}
	strcpy(file_proto, "/tmp/llXXXXXX");	/* gets clobbered */
	/* create a temp file to hold output from loader */
	(void) strcpy( Symbol_name, mktemp(file_proto) );

	/* force end of memory to be aligned to a ROUND byte boundary */
	/* note: this restriction is applied by the loader */
	end = sbrk( 0 );	/* find the currend end */
#ifdef DEBUG
	printf("initial sbrk(0) returns 0x%x\n", (int)end);
#endif
	n = (int)end;
	diff = round(n,ROUND)-n;	/* get the difference */
#	ifdef DEBUG
		printf( "end %x, diff %x\n", end, diff );
#	endif DEBUG
	if( diff != 0 ){
		/* use sbrk to round up */
		end = sbrk( diff );
		if( (int)end <= 0 ){
			fprintf( stderr,
				"sbrk failed: %s\n", sys_errlist[errno] );
			return( 1 );
		}
		end = sbrk( 0 );
	}

#	ifdef DEBUG
		printf( "sbrk sets end to be 0x%x\n", end );
#	endif DEBUG
	/* save info */
	*savep++ = (long) end;

	/* make up a command */
	cmd = cmd_str;
	cmdend = cmd + sizeof( cmd_str );

	/* find the loader name */
	file = LD;
	if( file == 0 ){
		fprintf( stderr, "cannot find loader\n" );
		return( 1 );
	}

	/* set up the first part of the command string */
	/* different loaders need different options (YUK!) */
	/* Its a matter of trial and error - you may even need */
	/* to go as far as reading the manual */
#if (pyr | vax ) & !interdata
	sprintf( cmd, "%s -N -x -A %s -T %x -o %s ",
#else
	sprintf( cmd, "%s -x -A %s -T %x -o %s ",
#endif
		file,		/* loader */
		Program_name,	/* program text file */
		end, 		/* end of memory */
		Symbol_name	/* executable file */
		);
#	ifdef DEBUG
		printf("Symbol_name %s\n", Symbol_name);
		printf( "command: %s\n", cmd );
#	endif DEBUG

	/* set cmd to end of string */
	cmd = cmd + strlen( cmd );

	/* now add the entry points */

	entry = entries;
	if( entry->name == 0 ){
		fprintf( stderr,"missing entry name" );
		return( 1 );
	}
	cmd = Strend( cmd, "-e", cmdend );
	if( cmd == 0 ){
		fprintf( stderr, "too many entries\n" );
	}
	cmd = Strend( cmd, entry->name, cmdend );
	if( cmd == 0 ){
		fprintf( stderr, "too many entries\n" );
	}
	++entry;
	/* set up the rest */
	for( ; entry->name; ++entry ){
		cmd = Strend( cmd, "-u", cmdend );
		if( cmd == 0 ){
			fprintf( stderr, "too many entries\n" );
		}
		cmd = Strend( cmd, entry->name, cmdend );
		if( cmd == 0 ){
			fprintf( stderr, "too many entries\n" );
		}
	}

	/* now add the object files */
	for( ; *objects; ++objects ){
		cmd = Strend( cmd, *objects, cmdend );
		if( cmd == 0 ){
			fprintf( stderr, "too many objects\n" );
		}
	}

	/* now add the library */
	if( library && *library ){
		cmd = Strend( cmd, library, cmdend );
		if( cmd == 0 ){
			fprintf( stderr, "library too long\n" );
		}
	}
	/* now add the defaults */
	cmd = Strend( cmd, "-lc", cmdend );
	if( cmd == 0 ){
		fprintf( stderr, "total loader command too long\n" );
	}
#	ifdef DEBUG
		printf( "loader command %s\n", cmd_str );
#	endif DEBUG
	if( (n = system( cmd_str )) != 0 ){
		(void)unlink( Symbol_name );
		fprintf( stderr, "load of objects and entries failed\n");
		return( 1 );
	}

#	ifdef DEBUG
		printf( "load was successful\n" );
#	endif DEBUG

	/* now try and read the information from the symbol table */

	if( (fcb = open( Symbol_name, O_RDONLY)) < 0){
		fprintf( stderr, "cannot open temp file %s: %s\n",
			Symbol_name, sys_errlist[errno] );
		return( 1 );
	}
#	ifdef DEBUG
		printf( "output file opened successfully\n" );
#	endif DEBUG

	/* read the a.out header and find out how much room to allocate */

	if(sizeof(header)!=read(fcb,(char *)&header,sizeof( header ))){
		fprintf( stderr, "cannot read header from temp file %s\n",
			Symbol_name );
		return( 1 );
	}


	/* calculate  sizes */
	readsize = round(header.a_text, 4) + round(header.a_data, 4);
	totalsize = readsize + header.a_bss;
	totalsize = round( totalsize, ROUND );	/* round up a bit */
#	ifdef DEBUG
		printf( "read header: a_text %d, a_data %d, a_entry 0x%x\n",
			header.a_text, header.a_data, header.a_entry);
		printf( "readsize %d, totalsize %d\n", readsize, totalsize );
#	endif DEBUG

	/* allocate more memory, using sbrk */

	end = sbrk( totalsize );
#	ifdef DEBUG
		printf( "sbrk(0x%x) returns 0x%x\n", totalsize, end );
#	endif DEBUG
	if( (int)end <= 0 ){
		fprintf(stderr, "sbrk failed to allocate: %s\n",
			sys_errlist[errno] );
		return( 1 );
	}
	/* save new end */
	*savep++ = (long) sbrk(0);

	/* now read in the functions */
	
	if(readsize != read(fcb,(char *)end,readsize)){
		fprintf( stderr, "cannot read %s: %s\n", Symbol_name,
			sys_errlist[errno] );
		return( 1 );
	}
	(void)unlink( Symbol_name );
			/* on Pyramid, we have to make the new functions */
			/* executable */
#ifdef pyr
	mprotect(end, totalsize, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif

	/* set the first entry up to the header value */
	entry = entries;
	entry->function = (int (*)())header.a_entry;
	++entry;

	if( entry->name ){
		if( Find_symbol_names( entry, fcb, header ) ){
			fprintf( stderr, "failed to find all symbols\n" );
			return( 1 );
		}
	}
	(void)close(fcb);
	return( 0 );
    }

/*
	Append a string and a space, check for overflow
*/

char *
Strend( str, append, last )
	char *str, *append, *last;
    {
#ifdef DEBUG
printf("Strend %s\n", append);
#endif
	while( str < last && *append ){
		*str++ = *append++;
	}
	if( *append ){
		return( (char *)0 );
	}
	if( str+1 < last ){
		*str++ = ' ';
		*str = 0;
	} else {
		return( (char *)0 );
	}
	return( str );
    }

/*
	Look through the symbol table and find entries
*/
#define NAMES 100	/* number of entries to read at a time */

Find_symbol_names( entries, fcb, header )
	struct symbol_references *entries;	/* list of entries */
	int fcb;			/* symbol file fcb */
	struct exec header;		/* symbol file header */
    {
	int symbols, strings;		/* offsets to symbols and strings */
	int c, i, n;			/* temp variable */
	int next_symbol;		/* next symbol */
	struct nlist name[NAMES], *nl;		/* entry for a symbol */
	char *str_val;			/* string */
	int str_offset;			/* offset into string tables */
	int str_start, str_end;		/* start and end location of str */
	char str_buff[MAXPATHLEN];	/* should be nice and big */
	char *s;			/* temp variable */

	symbols = N_SYMOFF(header);	/* offset to symbols */
	strings = N_STROFF(header);	/* offset to strings */

	str_start = str_end = 0;
#	ifdef DEBUG
		printf( "symbols %d, strings %d\n", symbols, strings );
#	endif DEBUG
	for( next_symbol = symbols; next_symbol < strings; ){
		/* read in the necessary entries */
		if( lseek( fcb, next_symbol, 0 ) == -1 ){
			fprintf( stderr,"lseek on symbol file failed: %s\n",
				sys_errlist[errno] );
			return( 1 );
		}
		if( sizeof(name) != (n = read(fcb,(char *)name,sizeof(name)))){
			if( n <= 0 ){
				fprintf( stderr,
					"read on symbol file failed: %s\n",
					sys_errlist[errno] );
			}
		}
		n = n / sizeof( struct nlist );
#		ifdef DEBUG
			printf( "read %d symbols\n",n );
#		endif DEBUG
		for( i=0; i < n && next_symbol < strings; ++i ){
			next_symbol += sizeof( struct nlist );
			nl = name+i;
#			ifdef DEBUG
			/* print the information */
			printf(
			"n_un.n_strx %d, n_type %d, n_desc %d, n_value %d\n",
				nl->n_un.n_strx, nl->n_type, nl->n_desc,
				nl->n_value);
#			endif DEBUG
			if( (nl->n_type & N_EXT) ){
#				ifdef DEBUG
					printf( "external symbol\n" );
#				endif DEBUG
				/* seek to the string location */
			readnext:
				str_offset = strings + nl->n_un.n_strx;
#				ifdef DEBUG
				printf( "string offset %d\n", str_offset);
#				endif DEBUG

				/* get string in buffer */
				/* NOTE: this is the first time I have
					ever found a use for a
					"goto";  I suppose I could
					have made the following
					a function.
				*/

				while( str_offset < str_start ||
					str_offset > str_end ){
					/* seek to position and read */
	if( lseek(fcb, str_offset, 0 ) == -1){
		fprintf( stderr,
		"lseek on symbol file failed: %s\n",
			sys_errlist[errno] );
		return( 1 );
	}
	if( (c = read(fcb, str_buff, sizeof( str_buff ) )) <= 0 ){
		fprintf( stderr, "read on symbol file failed: %s\n",
			sys_errlist[errno] );
		return( 1 );
	}
	str_start = str_offset;
	str_end = str_start + c;
				}
				str_val = s =
					str_buff + (str_offset - str_start);
				while( *s && str_offset < str_end ){
					++s; ++str_offset;
				}
				if( *s ){
					str_offset = strings + nl->n_un.n_strx;
					if( str_offset != str_start ){
						str_end = 0;	/* force read */
						goto readnext;
					} else {
						fprintf( stderr,
						"symbol too long\n" );
						return( 1 );
					}
				}
#				ifdef DEBUG
				printf( "str_val %s\n", str_val );
#				endif DEBUG
				if( Check_entry_for_value( str_val,
					(unsigned)nl->n_value,entries) == 0 ){
					return(0);
				}
			}
		}
	}
	return( 0 );
    }

/*
	Check_entry_for_value( str, val, entries )
	char *str: string name
	unsigned val: value for it
	struct symbol_references *entries: list of entries

	1. for each un-set entry, check to see if string matches.
	2. if it does, then you set the function value.
*/

int
Check_entry_for_value( str, val, entries )
	char *str;
	unsigned val;
	struct symbol_references *entries;
    {
	struct symbol_references *entry;
	int found;

	found = 0;
	for( entry = entries; entry->name; ++entry ){
		if( entry->function == 0 ){
			found = 1;
			if( strcmp( str, entry->name ) == 0 ){
				entry->function = (int (*)())val;
#				ifdef DEBUG
				printf( "update %s with %d\n",
					entry->name, entry->function );
#				endif DEBUG
				return( found );
			}
		}
	}
	return( found );
     }


savedload(fd, bp)
	int fd;
{
	char *start;
	long *bp;

	bp = &savebuf.l[S_DLOAD];
	while(*bp) {
		start = (char*)*bp++;
#ifdef DEBUG
		printf("writing from %x to %x\n", (int)start, (int)*bp);
#endif
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
#ifdef DEBUG
		printf("start=0x%x, tot=0x%x\n", (int)start, (int)totalsize);
#endif
		/* allocate more memory, using sbrk */

		end = sbrk( totalsize );
		if( (int)end <= 0 ){
			fprintf(stderr, "sbrk failed to allocate: %s\n",
				sys_errlist[errno] );
			return( 0 );
		}
		read(fd, start, totalsize);
			/* on Pyramid, we have to make the new functions */
			/* executable */
#ifdef pyr
		mprotect(end, totalsize, PROT_READ|PROT_WRITE|PROT_EXEC);
#endif
		bp += 2;
	}
	return(1);
}
#endif
