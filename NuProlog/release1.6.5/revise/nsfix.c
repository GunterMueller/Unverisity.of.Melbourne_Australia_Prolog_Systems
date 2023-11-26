/*
 * nsfix:
 *	Merges compiled predicates in a new combined .ns file
 *	into the .ns file where they originally occurred
 *	Modified by Lee so it only works with a single original
 *	file, and adds any new predicates to the .ns as well
 *	as modified old predicates.  $init is not modified.
 *	Code could be cleaned up some more - there are relics
 *	of the past.
 */

#include <stdio.h>
#include <ctype.h>
#ifndef BSD4
#include <fcntl.h>
#endif
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>

#define	TRUE	1
#define	FALSE	0

#define	STRLEN	100
#define	PATHLEN	200
#define	MAXBLKS	300	/* could be less */

#define	or	else if

#define	writeblk(F,B)	write((F), (B).base, (B).top - (B).base + 1)

char	mark[7] = "\t.pred";
int	marklen = 6;
char	initmark[17] = "\t.pred\t'$init',0";
int	initmarklen = 16;

char	filename[STRLEN];

struct	Block {
	char	*base;
	int	len;
	char	*top;
}
	blocks[MAXBLKS] = { {0,0,0}, };
int	nblocks;

char	*compbuf;

main(argc, argv)
int argc;
char **argv;
{
	int i;

	argc--; argv++; /* skip prog name */

	scan(*argv);
	argc--; argv++;
/*
	for ( ; argc > 0; argc--, argv++)
*/
		merge(*argv);

	free(compbuf);
}

/*
 * scan:
 *	Grab text of composite .ns file
 *	Scan it to find where predicates begin
 */
scan(compfile)
char *compfile;
{
	char	*gettext();

	if ((compbuf = gettext(compfile)) == NULL)
		return;
	
	if ((nblocks = getblocks(compbuf, blocks)) == 0)
		return;
}

/*
 * getblocks:
 *	Scan text of file and fill in a table of block location/length
 */
int
getblocks(buf, table)
char *buf;
struct Block table[];
{
	register char	*c;
	register char	mark0;
	register int	b, l;
	register int	first_block = 1;

	b = 0;
	mark0 = mark[0];
	for (c = buf; *c != '\0'; c++) {
		if (*c == '\n') {
			if (*(c+1) == mark0
			    && b < MAXBLKS-1
			    && strncmp(c+1, mark, marklen) == 0) {
				if (!first_block) {
					table[b].top = c;
					if (strncmp(table[b].base,
						initmark, initmarklen) != 0)
						b++;
				}
				table[b].base = c+1;
				first_block = 0;
				for (l = 0, c++; *c != '\n'; l++, c++)
					;
				table[b].len = l;
				c--;
			}
		}
	}
	if (!first_block) {
		table[b].top = c-1;
		if (strncmp(table[b].base, initmark, initmarklen) != 0)
			b++;
	}

	return(b);
}

/*
 * gettext:
 *	Grabs the entire text of a file into a buffer in memory
 *	Inserts '\n' at the beginning and terminates with \n\0
 */
char *
gettext(file)
char *file;
{
	struct stat st;
	int	fd;
	char	*buf, *malloc();

	if (stat(file, &st) < 0) {
		fprintf(stderr, "Can't find \"%s\"\n", file);
		return(NULL);
	}
	if ((fd = open(file, O_RDONLY, 0)) < 0) {
		fprintf(stderr, "Can't open \"%s\"\n", file);
		return(NULL);
	}
	if ((fd = open(file, O_RDONLY, 0)) < 0) {
		fprintf(stderr, "Can't read \"%s\"\n", file);
		close(fd);
		return(NULL);
	}
	if((buf = malloc(st.st_size+3)) == NULL) {
		fprintf(stderr, "No of memory for \"%s\"\n", file);
		close(fd);
		return(NULL);
	}
	buf[0] = '\n';
	read(fd, &buf[1], st.st_size);
	buf[st.st_size+1] = '\n';
	buf[st.st_size+2] = '\0';
	close(fd);
	return(buf);
}

/*
 * updated:
 *	Check table to see whether this block has been updated
 */
struct Block *
updated(block)
char *block;
{
	int b;

	for (b = 0; b < nblocks; b++) {
		if (blocks[b].len > 0 &&
			strncmp(blocks[b].base, block, blocks[b].len) == 0) {
			blocks[b].len = -1;
			return(&blocks[b]);
		}
	}
	return((struct Block *)NULL);
}

/*
 * merge:
 *	Scan a known .ns file, looking for things to merge into it
 */
merge(nsfile)
char *nsfile;
{
	register char	*c;
	register char	mark0;
	register int	b, l, nlines;
	struct	Block *new;
	char	*top, *base;
	int	nsf;
	char	*nsbuf;
	char	*gettext();

	if ((nsbuf = gettext(nsfile)) == NULL)
		return;

	if ((nsf = open(nsfile, (O_WRONLY|O_TRUNC), 0644)) < 0) {
		fprintf(stderr, "Can't recreate %s\n", nsfile);
		return;
	}
		/* copy modified blocks */
	for (b = 0; b < nblocks; b++) {
		writeblk(nsf,blocks[b]);
	}
	
		/* copy non-modified blocks */
	mark0 = mark[0];
	nlines = -1;
	b = 0;
	base = nsbuf;
	for (c = nsbuf; *c != '\0'; c++) {
		if (*c == '\n') {
			nlines++;
			if (*(c+1) == mark0 &&
			    strncmp(c+1, mark, marklen) == 0) {
				if (nlines > 0) { /* end prev block */
					top = c;
					if (new == (struct Block *)NULL) {
						write(nsf, base, top-base+1);
					}
					/*
					else {
						writeblk(nsf, *new);
					}
					*/
					nlines = -1;
				}
				new = updated(c+1);
				base = c+1;
			}
		}
	}
	if (nlines > 1) {
		top = c-2;
		if (new == (struct Block *)NULL) {
			write(nsf, base, top-base+1);
		}
		/*
		else {
			writeblk(nsf, *new);
		}
		*/
	}
	close(nsf);
}
