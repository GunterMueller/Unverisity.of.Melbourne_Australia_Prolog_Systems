/*
 * nlcp:
 *	Save a copy of a NU-Prolog source file at start of revise session
 *	Set up a tags file to assist in using vi(1) for revisions
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

#define	STRLEN	100
#define	PATHLEN	200
#define	MAXBLKS	100

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

FILE	*ftags;

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

	ftags = fopen("tags", "w");
		
	for ( ; argc > 0; argc--, argv++) {
		names(*argv, oldfile, newfile);
		copy(oldfile, newfile);
	}

	fclose(ftags);
}

/*
 * names:
 *	Generate oldfile name (prepend '.' to basename),
 *	and save oldfile and newfile names in buffers
 */
names(file, oldfile, newfile)
char *file,*oldfile,*newfile;
{
	char	*slash;

	strcpy(newfile, file);

	if ((slash = (char *)rindex(file,'/')) == (char *)0) {
		sprintf(oldfile, ".%s", file);
	}
	else {
		*slash = '\0';
		sprintf(oldfile, "%s/.%s", file, slash+1);
		*slash = '/';
	}
}

/*
 * copy:
 *	Grab text for current and previous versions of file,
 *	find blocks in old, and scan new, printing modified blocks
 */
copy(oldfile, newfile)
char *oldfile,*newfile;
{
	register char	*c;
	register char	mark0;
	register int	b,ob,nb;
	int	of, eof = 0;
	char	*line;
	char	*buf;
	char	*gettext();

	if ((buf = gettext(newfile)) == NULL)
		return;
	if ((of = open(oldfile, (O_WRONLY|O_CREAT), 0644)) < 0)
		return;
	write(of, &buf[1], strlen(buf)-1);

	mark0 = mark[0];
	if (mark0 != '\n') {
		for (c = buf; *c != '\0'; c++) {
			if (*c == '\n' && *(c+1) == mark0 &&
			    ftags != NULL && strncmp(c+1, mark, marklen) == 0) {
				line = c+1;
				c += marklen+1;
				while (*c == ' ' || *c == '\t')
					c++;
				if (*c == '\n') {
					c--;
					continue;
				}
				while (!isspace(*c) && *c != '\0')
					putc(*c++, ftags);
				while (*c != '\n' && *c != '\0')
					c++;
				if (*c == '\0')
					eof++;
				*c = '\0';
				fprintf(ftags, "\t%s\t?^%s$?\n", newfile, line);
				if (eof)
					break;
				*c = '\n';
				c--;
			}
		}
	}
	free(buf);
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
	if ((buf = malloc(st.st_size+2)) == NULL) {
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
