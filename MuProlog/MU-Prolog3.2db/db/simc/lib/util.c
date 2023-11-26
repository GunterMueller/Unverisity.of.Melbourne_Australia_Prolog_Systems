/*
 * util.c  -  deductive database package (utility operations)
 *
 * Copyright (c) 1985,1986,1987, The University of Melbourne
 */

#include "simc.h"

#ifdef DBUG
Word	debug_flag = 0;
#endif DBUG

/*
 * str_copy:
 *	Make a copy of a string in a "permanent" memory buffer
 *	(in cases where the string lives on a stack and may
 *	 disappear sometime in the future)
 */
String
str_copy(str)
String str;
{
	r_Int nchars;
	r_Char *buf;

	nchars = strlen(str) + 1;
	if ((buf = (String)malloc(nchars)) == NULL)
		return(NULL);
	strcpy(buf, str);
	return(buf);
}

/*
 * error:
 *	Print error message
 */
void
error(msg)
String msg;
{
#if 0
	printf("?-db_error(%s).\n", msg);
#else
	puts(msg);
#endif
	fflush(stdout);
}


/*
 * fatal:
 *	Print error message, then die
 */
void
fatal(msg)
String msg;
{
	error(msg);
	exit(1);
}

#ifdef elxsi
/*
 * cputime:
 *	Measure cpu time in milliseconds since start of process
 */
long
cputime()
{
	asm("	read.cpu.timer	.r0");
	asm("	div.64	.r0,.r0,=40000");
}
#endif elxsi
