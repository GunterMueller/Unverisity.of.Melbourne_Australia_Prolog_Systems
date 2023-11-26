/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: John Shepherd
 */

/*
 * util.c  -  Nepolog assembler (utilities)
 */

#include <stdio.h>
#include "na.h"

extern	char	*my_name;	/* name of invoking program */

/*
 * fatal:
 *	Print an error message and exit
 */
fatal(format,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
char *format;
int a0,a1,a2,a3,a4,a5,a6,a7,a8,a9;
{
	err(format,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9);
	exit(13);
}

/*
 * err:
 *	Print a general error message
 */
err(format,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
char *format;
int a0,a1,a2,a3,a4,a5,a6,a7,a8,a9;
{
	naerrs++;
	fprintf(stderr, "%s: ", my_name);
	fprintf(stderr, format, a0,a1,a2,a3,a4,a5,a6,a7,a8,a9);
	putc('\n',stderr);
}
