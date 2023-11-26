/*
 * nldiffs:
 *	Print out blocks in two files which are different
 *	A "block" is defined as a region of text surrounded
 *	by markers (the default markers are empty lines)
 *	Always prints first block on the assumption that it
 *	contains such things as operator definitions
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#ifndef BSD4
#include <fcntl.h>
#endif
#include <sys/file.h>
#include <sys/stat.h>

#define	TRUE	1
#define	FALSE	0

#define	STRLEN	100	/* probably no check for overflow */
#define	PATHLEN	200	/* probably no check for overflow */
#define	MAXBLKS	300	/* check added - last block can be BIG */

#define	or	else if

#define	writeblk(B)	write(1, (B).base, (B).top - (B).base + 1)

#ifndef BSD4
#define	index(x,y)	strchr(x,y)
#define	rindex(x,y)	strrchr(x,y)
#endif

char	oldfile[PATHLEN];
char	newfile[PATHLEN];

char	mark[STRLEN] = "\n";
int	marklen = 1;

char	filename[STRLEN];

struct	Block {
	char	*base;
	char	*top;
}
	oldblocks[MAXBLKS],
	newblocks[MAXBLKS];

main(argc, argv)
int argc;
char **argv;
{
	argc--; argv++; /* skip prog name */

	/*
	 * Check for resetting mark
	 */
	if (argv[0][0] == '-' && argv[0][1] == 'm') {
		strcpy(mark, *argv+2);
		marklen = strlen(mark);
		argc--; argv++;
	}

	if (argc != 2) {
		printf("nldiffs: two file names expected\n");
		exit(1);
	}
	diffs(*(argv+1), *argv);
}

/*
 * diffs:
 *	Grab text for current and previous versions of file,
 *	find blocks in old, and scan new, printing modified blocks
 */
diffs(oldfile, newfile)
char *oldfile,*newfile;
{
	register int	b,ob,nb;
	int	nob, nnb;
	char	*oldbuf, *newbuf;
	char	*gettext();

	if ((newbuf = gettext(newfile)) == NULL)
		return;
	if ((oldbuf = gettext(oldfile)) == NULL) {
		/* No old file, so dump everything */
		write(1, &newbuf[1], strlen(newbuf)-1);
		return;
	}
	
	if ((nob = getblocks(oldbuf, oldblocks)) == 0)
		return;
	if ((nnb = getblocks(newbuf, newblocks)) == 0)
		return;

	for (ob = 0, nb = 0; nb < nnb & ob < nob; nb++) {
		if (blkcmp(newblocks[nb], oldblocks[ob]) == 0) {
			if (nb == 0 && headerblk(newblocks[0]))
				writeblk(newblocks[0]);
			ob++;
		}
		else {
			for (b = ob; b < nob; b++)
				if (blkcmp(oldblocks[b], newblocks[nb]) == 0)
					break;
			if (b == nob)
				writeblk(newblocks[nb]);
			else
				ob = b+1;
		}
	}
	for (; nb < nnb; nb++)
		writeblk(newblocks[nb]);
}

/*
 * getblocks:
 *	Scan text of file and fill in a table of block location/length.
 *	Now checks for block table overflow (!) - if table fills up
 *	the rest of the file is considered one block.
 *	Should really double size of block table...
 */
int
getblocks(buf, table)
char *buf;
struct Block table[];
{
	register char	*c;
	register int	b, nlines;
	register char	mark0;

	b = 0;
	nlines = -1;
	mark0 = mark[0];
	for (c = buf; *c != '\0'; c++) {
		if (*c == '\n') {
			nlines++;
			if (*(c+1) == mark0 &&
			    b < MAXBLKS-1 &&	/* added - Aaahhhgg!!! */
			    strncmp(c+1, mark, marklen) == 0) {
				if (nlines > 0) { /* end prev block */
					table[b++].top = c;
					nlines = -1;
				}
				or (nlines == 0)
					nlines = -1;
			}
			or (nlines == 0) { /* start new block */
				table[b].base = c+1;
			}
		}
	}
	if (nlines > 0) /* end final block */
		table[b++].top = c-1;

	return(b);
}

/*
 * gettext:
 *	Grabs the entire text of a file into a buffer in memory
 *	Inserts '\n' at the beginning and terminates with '\0'
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
	if((buf = malloc(st.st_size+2)) == NULL) {
		fprintf(stderr, "No of memory for \"%s\"\n", file);
		close(fd);
		return(NULL);
	}
	buf[0] = '\n';
	read(fd, &buf[1], st.st_size);
	buf[st.st_size+1] = '\0';
	close(fd);
	return(buf);
}

/*
 * blkcmp:
 *	Compare two blocks
 */
int
blkcmp(b1, b2)
struct Block b1, b2;
{
	register char *c1, *c2;
	char	*t1, *t2;
	int	len1, len2, res;

	len1 = b1.top - b1.base;
	len2 = b2.top - b2.base;

	if (len1 != len2)
		return(len1 - len2);
	t1 = b1.top;
	t2 = b2.top;
	*t1 = *t2 = '\0';
#if 0
	res = strcmp(b1.base, b2.base);
#else
	for (c1 = b1.base, c2 = b2.base; *c1 == *c2; c1++, c2++)
		if (*c1 == '\0')
			break;
	res = *c1 - *c2;
#endif
	*t1 = *t2 = '\n';
	return(res);
}

/*
 * headerblk:
 *	Guess whether a block looks like a header block
 *	Measure for this is large proportion of ?-'s
 */
int
headerblk(b)
struct Block b;
{
	register char *c;
	int	 ncmds = 0;
	int	 nlines = 0;

	for (c = b.base; *c != '\0'; c++) {
		if (*c == '\n') {
			nlines++;
			if (*(c+1) == '?' && *(c+2) == '-')
				ncmds++;
		}
	}
	return(ncmds > 0);
}
