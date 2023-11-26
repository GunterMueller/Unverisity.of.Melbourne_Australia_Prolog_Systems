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

/*
data.c
functions for dictionary and memory management
*/

#include "types.h"

#define NULL ((Int)0)
#define diffaddr(x,y)  ((((Int)(x))-((Int)(y)))/sizeof(x))  /* DS: used instead of x-y which is an 'int' if x and y are pointers */

/*
The dictionary is set up as a simple linear hash table.
*/

Int *dict[DICTLEN];
int dentries;		/* number of used positions in table */
static char *sbot, *stop;	/* bottom, top of current block for strings */

Int mat_goalstk[GSTKLEN/1000][1000]; /* DS: long arrays are not allowed on Altos,  */
Int *goalstk = &mat_goalstk[0][0],   /* DS:  so I use a matrix instead! */
	*gtop, *hbot, *rsetend;
Ptr freelist[MAXFREE];

Int nul[] = {NULL, NULL};
Ptr null = &nul[0];

struct sbuffer savebuf;

static
hash(n,i)			/* hash(id) = sum of chars*23 % DICTLEN */
	int n;
	ident i;
{
	int s;
	char *j;

	j = i + IDLEN;
	s = n;
	while(*i && i < j)
		s += *i++;
	return(abs(s * 23) % DICTLEN);
}

static
posn(n,i)			/* posn(id) = index of id in hash table */
	int n;
	ident i;
{
	int j;

	j = hash(n,i);
	while(!dfree(j) && (strcmp(dname(j), i) || dnargs(j) != n
				|| dhide(j) != DVISIBLE))
		j = (j + 1) % DICTLEN;
	return(j);
}

find(n,i)			/* if identifier not in table then insert it */
				/* return it's index */
	int n;
	ident i;
{
	int p;

	p = posn(n,i);
	if(dfree(p))
		if(++dentries < DICTLEN) {
			if(n == 0)	/* a variable */
				dict[p] = newmem(DVNTRYSZ);
			else {
				dict[p] = newmem(DENTRYSZ);
				d_wait(p) = NULL;
				d_proc(p) = (Int) null;
#ifdef CLINDEX
				d_clindex(p) = NULL;	/* (jws) */
#endif
				d_prprec(p) =
				d_inprec(p) =
				d_psprec(p) = -1;
				d_nlinks(p) = 1;
			}
			d_prot(p) = 0;
			scopy((char**)&d_name(p), i);
			d_nargs(p) = n;
		} else {
			error("dictionary exploded\n");
			dumpdict();
		}
	return(p);
}

scopy(n, i)			/* copy string - grab more space if needed */
	char **n, *i;
{
	char *c;

	if((Int*) i > hbot && (Int*) i < goalstk + GSTKLEN) {
		*n = i;			/* string is already in table */
		return(1);
	}
	c = i;
	*n = sbot;
	while(sbot < stop && (*(sbot++) = *(c++)))
		;
	if(*n == sbot || *(sbot-1)) {
		if(gtop + 150 > hbot) {
			error("no room for more names\n");
			return(0);
		}
		stop = (char *) hbot;
		hbot = hbot - 100;
		sbot = (char *) hbot;
		return(scopy(n,i));
	} else
		return(1);
}

dumpdict() {			/* print contents of dictionary */
	int i;

	for(i = 0; i < DICTLEN; i++)
		if(!dfree(i))
			printf("%d\t%d<%s>\n", i, dnargs(i), dname(i));
}


Ptr
newmem(size)			/* return ptr to a block of size ints */
	register int size;
{
	Ptr t;

	size--;
	if(size < MAXFREE && freelist[size]) {
		t = freelist[size];
		freelist[size] = (Ptr) *freelist[size];
		return(t);
	} else {
		if(gtop+51+size > hbot) {
			error("heap eaten\n");
			return(0);
		}
		hbot = hbot - size - 1;
		return(hbot);
	}
}

dispmem(size, p)		/* put block on freelist */
	register int size;
	Ptr p;
{
	size--;
	if(size < MAXFREE) {
		*p = (Int) freelist[size];
		freelist[size] = p;
	}
	/* else should put on a free list of big blocks */
}

displist(size, t)			/* put list of blocks on free list */
	register int size;
	Ptr t;
{
	register Ptr t1;

	size--;
	if(size < MAXFREE) {
		for(t1 = t; lnext(t1); t1 = lnext(t1))
			;
		l_next(t1) = (Int) freelist[size];
		freelist[size] = t;
	}
	/* else should put on a free list of big blocks */
}


dispterm(t)			/* put term on free list(s) */
	register Ptr t;
{
	register Ptr t1;
	register int n;

	if(t != (Ptr)NULL && IsComp(t)) {
		t1 = &targ(1, t);
		n = tnargs(t);
		while(n-- > 0)
			dispterm((Ptr)*(t1++));
		dispmem(tnargs(t)+1, t);
	}
}

#ifdef Int_is_int

#define readlong(f,p,l) read(f,p,l)
#define writelong(f,p,l) write(f,p,l)

#else

#define MAXRW  20000
/* DS: readlong and writelong are added to make it possible to */
/* DS:   read/write large files also on a machine with 16 bit ints */
readlong(fd,p,l)
        int fd; char *p; Int l;
	{
	for( ; l>MAXRW; l -= MAXRW)
		{
		read(fd, p, MAXRW);
		p += MAXRW;
		}
	read(fd,p,(int)l);
	}

writelong(fd,p,l)
        int fd; char *p; Int l;
	{
	for( ; l>MAXRW; l -= MAXRW)
		{
		write(fd, p, MAXRW);
		p += MAXRW;
		}
	write(fd,p,(int)l);
	}
#endif

savedata(fd)
	int fd;
{
	int i;
	long *bp;

	bp = &savebuf.l[S_DATA];
	*bp++ = (long) goalstk;
	*bp++ = (long) rsetend;
	*bp++ = (long) gtop;
	*bp++ = (long) hbot;
	*bp++ = (long) stop;
	*bp++ = (long) sbot;
	for(i = 0; i < MAXFREE; i++)
		*bp++ = (long) freelist[i];
	*bp = (long) dentries;
	writelong(fd, (char*)&savebuf, sizeof(savebuf));
	writelong(fd, (char*)goalstk, diffaddr(rsetend,goalstk) * sizeof(Int));
	writelong(fd, (char*)hbot, diffaddr(goalstk+GSTKLEN,hbot) * sizeof(Int));
	write(fd, (char*)dict, sizeof(dict));
}

restdata(fd)
	int fd;
{
	int i;
	long *bp;

	bp = &savebuf.l[S_DATA];
	if(*bp++ != (long) goalstk) {
		printf("Program altered\n");
		return(0);
	}
	rsetend = (Int*) *bp++;
	gtop = (Int*) *bp++;
	hbot = (Int*) *bp++;
	stop = (char*) *bp++;
	sbot = (char*) *bp++;
	for(i = 0; i < MAXFREE; i++)
		freelist[i] = (Ptr) *bp++;
	dentries = *bp;

	readlong(fd, (char*)goalstk, diffaddr(rsetend,goalstk) * sizeof(Int));
	readlong(fd, (char*)hbot, diffaddr(goalstk+GSTKLEN,hbot) * sizeof(Int));
	read(fd, (char*)dict, sizeof(dict));
	return(1);
}
