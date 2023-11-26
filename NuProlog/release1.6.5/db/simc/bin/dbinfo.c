/*
 * dbinfo.c  -  deductive database package (rel info test)
 *
 * This program prints relrmation about a relation
 *
 * Copyright (C) 1985, 1986, 1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include "simc.h"

String	my_name;
String	db_name;
String	rel_name;

File	descf;
Reln	rel;
Char	template[100];
Int	ncounters;
Int	*counters;

main(argc, argv)
Int argc;
String argv[];
{
	Int	i, size, nargs;
	Char	line[100];
	Char	filename[100];
	Char	*c, *op, *arg, *arg2;
	Bool	matches();
	void	show_counters();

	c = rindex(argv[0],'/');
	my_name = c == 0 ? argv[0] : (char *)(c+1);

	if (argc != 3) {
		fprintf(stderr, "Usage: %s db rel\n", my_name);
		exit(1);
	}

	db_name = argv[1];
	rel_name = argv[2];

	sprintf(filename, "%s/%s/desc", db_name, rel_name);
	if ((descf = open(filename,O_RDONLY,0)) < 0) {
		fprintf(stderr, "Can't open database/relation\n");
		exit(1);
	}

	size = info_bytes(&rel);
	if (read(descf, static_info(&rel), size) != size) {
		fprintf(stderr, "Can't read relation info for %s\n", rel_name);
		exit(1);
	}
	if (read(descf, template, 100) != 100) {
		fprintf(stderr, "Can't read relation info for %s\n", rel_name);
		exit(1);
	}

	ncounters = rel.rd_nsegs;
	size = (ncounters+2) * sizeof(Int);
	if ((counters = (Int *)malloc(size)) == (Int *)NULL) {
		exit(1);
	}

	while (printf("%s> ",my_name),fflush(stdout),gets(line) != NULL)
	{
		if (!isatty(0) || !isatty(1))
			fprintf(stdout,"%s\n",line);
		op = c = line;
		while (!isspace(*c) && *c != ChNULL)
			c++; /* skip op string */
		if (*c != ChNULL)
			*c++ = ChNULL;
		while (isspace(*c) && *c != ChNULL)
			c++; /* skip blanks */
		arg = c;
		while (!isspace(*c) && *c != ChNULL)
			c++; /* skip arg string */
		if (*c != ChNULL)
			*c++ = ChNULL;
		while (isspace(*c) && *c != ChNULL)
			c++; /* skip blanks */
		arg2 = c;
		if (*op == ChNULL)
			nargs = 0;
		else if (*arg == ChNULL)
			nargs = 1;
		else if (*arg2 == ChNULL)
			nargs = 2;
		else
			nargs = 3;

		if (matches(op, "info"))
			show_static_info();
		else if (matches(op,"level"))
			show_levels_info();
		else if (matches(op, "counter"))
			show_counters(nargs, arg, TRUE);
		else if (matches(op, "Counter"))
			show_counters(nargs, arg, FALSE);
		else if (matches(op, "slice")) {
			if (nargs != 2)
				fprintf(stderr, "usage: slice slice_number\n");
			else
				show_slice(arg);
		}
		else if (matches(op,"?") || matches(op,"help")) {
			printf("Commands for %s:\n", my_name);
			printf("\tinfo - print static info on relation\n");
			printf("\tcounters - print info on record counters\n");
			printf("\tslice num - print info about one slice\n");
			printf("\thelp - print this message\n");
		}
		else
			fprintf(stderr, "invalid command\n");
	}
	printf("end\n");
	exit(0);
}

Bool
matches(s1, s2)
String s1, s2;
{
	Int	l1, l2;

	l1 = strlen(s1);
	l2 = strlen(s2);

	return(strncmp(s1, s2, ((l1 < l2) ? l1 : l2)) == 0);
}

show_static_info()
{
	printf("%-30s: %s/%s\n", "database/relation", db_name, rel_name);
	printf("%-30s: %d\n", "number of segments (Ns)", rel.rd_nsegs);
	printf("%-30s: %d\n", "records per segment (Nr)", rel.rd_nrecs);
	printf("%-30s: %d\n", "number of data files", rel.rd_ndfiles);
	printf("%-30s: %d chars\n", "average rec length", rel.rd_avreclen);
	printf("%-30s: %d bits\n", "mask size (Ms)", rel.rd_masksz);
	printf("%-30s: %d\n", "rec counter offset", rel.rd_cntroffset);
	printf("%-30s: %d\n", "seg descriptor offset", rel.rd_descoffset);
	printf("%-30s: %d bits\n", "rec codeword size (Dr)", rel.rd_rcwordsz);
	printf("%-30s: %d bits\n", "rec desc size (Br)", rel.rd_rdescsz);
	printf("%-30s: %d\n", "bits set in rec desc (Kr)", rel.rd_rnbits);
	printf("%-30s: %d\n", "bits used in rec desc (Ur)", rel.rd_rubits);
	printf("%-30s: %d bits\n", "seg codeword size (Ds)", rel.rd_scwordsz);
	printf("%-30s: %d bits\n", "seg desc size (Bs)", rel.rd_sdescsz);
	printf("%-30s: %d\n", "bits set in seg desc (Ks)", rel.rd_snbits);
	printf("%-30s: %d\n", "bits used in seg desc (Us)", rel.rd_subits);
	printf("%-30s: %d\n", "segment info size", rel.rd_seg_info_size);
	printf("%-30s: %d\n", "segment size (+data)", rel.rd_seg_size);
	printf("%-30s: %d\n", "segs per data file", rel.rd_local_segs);
	printf("%-30s: %s\n", "template for keys", template);
	switch (rel.rd_index)
	{
	when SIMC_INDEX:
		printf("%-30s: SSIMC\n", "indexing scheme");
	when DSIMC_INDEX:
		printf("%-30s: DSIMC\n", "indexing scheme");
		printf("%-30s: %d\n", "segs per block",
			1 << rel.rd_sdesc_block);
	}
}

show_levels_info()
{
	int i, size;

	if (index_type(&rel) != DSIMC_INDEX) {
		fprintf(stderr,"simc databases don't have levels\n");
		return;
	}

	size = info_bytes(&rel);
	lseek(descf, (Faddr)0, L_SET);
	if (read(descf, static_info(&rel), size) != size) {
		fprintf(stderr, "Can't read relation info for %s\n", rel_name);
		exit(1);
	}
	if (read(descf, template, 100) != 100) {
		fprintf(stderr, "Can't read relation info for %s\n", rel_name);
		exit(1);
	}

	printf("%-20s:", "level numbers");
	for (i = 0; i < 10; i++)
		printf(" %4d",i);
	printf("\n");
	printf("%-20s:", "number of segments");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_nlevsegs[i]);
	printf("\n");
	printf("%-20s:", "number of records");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_nlevrecs[i]);
	printf("\n");
	printf("%-20s:", "load factor");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_loadfac[i]);
	printf("\n");
	printf("%-20s:", "load count");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_loadcnt[i]);
	printf("\n");
	printf("%-20s:", "split pointer");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_splitp[i]);
	printf("\n");
	printf("%-20s:", "split depth");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_depth[i]);
	printf("\n\n");
	printf("%-20s:", "magic number [0]");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_magic[i][0]);
	printf("\n");
	printf("%-20s:", "magic number [1]");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_magic[i][1]);
	printf("\n");
	printf("%-20s:", "magic number [2]");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_magic[i][2]);
	printf("\n");
	printf("%-20s:", "magic number [3]");
	for (i = 0; i < 10; i++)
		printf(" %4d",rel.rd_magic[i][3]);
	printf("\n");
}

void
show_counters(nargs, arg, show_em)
Int nargs;
String arg;
Bool show_em;
{
	int	lev;

	switch (rel.rd_index)
	{
	when SIMC_INDEX:
		show_simc_counters(show_em);
	when DSIMC_INDEX:
		if (nargs == 2) {
			sscanf(arg, "%d", &lev);
			show_dsimc_counters(lev,show_em);
		}
		else {
			for (lev = 0; lev < MAXLEVELS; lev++) {
				if (Nls(&rel, lev) == 0) continue;
				show_dsimc_counters(lev, show_em);
			}
		}
	}
}

show_simc_counters(show_em)
Bool show_em;
{
	int	i, j, size;
	int	addr, level;
	int	ov[16], nr[16];
	int	count;

	size = (ncounters) * sizeof(Word);
	lseek(descf, rel.rd_cntroffset, L_SET);
	if (read(descf, counters, size) != size) {
		fprintf(stderr, "Can't read counters\n");
		exit(1);
	}
	size = 0;
	j = 0;
	for (i = 0; i < 16; i++)
		nr[i] = ov[i] = 0;
	for (i = 0; i < ncounters-2; i++) {
		if (counters[i] == 0) j++;
		if (counters[i] > rel.rd_nrecs) {
			if (show_em)
				printf("%5d*%5d |", i, counters[i] - rel.rd_nrecs -1);
			counters[i] = rel.rd_nrecs;
			ov[i/rel.rd_local_segs]++;
		}
		else
			if (show_em) printf("%5d,%5d |", i, counters[i]);
		nr[i/rel.rd_local_segs] += counters[i];
		size += counters[i];
		if (((i+1) % 6) == 0 && show_em) putchar('\n');
	}
	if (show_em) printf("\nData files:\n");
	count = 0;
	for (i = 0; i < rel.rd_ndfiles; i++) {
		/*
		printf("#%-3d:%4d:%6d ",i,ov[i],nr[i]);
		*/
		if (show_em) printf("  #%d:%d",i,nr[i]);
		count += nr[i];
		if((i+1)%4 == 0 && show_em)putchar('\n');
	}
	putchar('\n');
	printf("Next seg to search   : %d\n", counters[ncounters]);
	printf("Total recs           : %d\n", count);
	printf("# segs with no recs  : %d\n", j) ;
}

show_dsimc_counters(level,show_em)
int level;
Bool show_em;
{
	int	i;
	int	fd, nfile, cfile;
	int	count, nrecs, nempty;
	long	off;
	Word	presence[10];
	char	fname[20];

	printf("\nCounters for level %d:\n=====\n", level);
	if (level < 0 || level >= MAXLEVELS) {
		fprintf(stderr, "invalid level\n");
		return;
	}
	if (Nls(&rel,level) == 0) {
		fprintf(stderr, "no segments on level %d\n", level);
		return;
	}
	cfile = -1;
	fd = nrecs = nempty = 0;

	/*
	 * For each segment in the data file ...
	 */
	for (i = 0; i < Nls(&rel,level); i++) {
		nfile = i / local_segs(&rel);
		if (nfile != cfile) {
			if (fd > 0) close(fd);
			sprintf(fname, "%s/%s/%02d.%04d",
					db_name, rel_name, level, nfile);
			if ((fd = open(fname, 0)) < 0) {
				printf("Can't open %s\n",fname);
				fflush(stdout);
				return;
			}
			cfile = nfile;
		}
		/*
		 * Compute file offset of presence bits
		 * Read in presence bits and count them
		 */
		off = (i % local_segs(&rel)) * seg_size(&rel);
		off += nBytes(Nr(&rel)) * Dr(&rel);
		lseek(fd, off, 0);
		read(fd, presence, nBytes(Nr(&rel)));
		count = b_count(presence, Nr(&rel));
		if (count == 0)
			nempty++;
		else
			nrecs += count;
		if (show_em) printf("%5d,%5d |", i, count);
		if (((i+1) % 6) == 0 && show_em) putchar('\n');
	}
	putchar('\n');
	printf("# recs at level %-4d: %d\n", level, nrecs);
	printf("# segs with no recs : %d\n", nempty) ;
	printf("average seg loading : %d (%0.2f)\n",
		nrecs/Nls(&rel,level),
		(float)(nrecs)/(float)(Nr(&rel)*Nls(&rel,level)));
}

show_slice(which)
String which;
{
	Word	word;
	long	off;
	int	i, slice_no, block_no, max_block;

	switch (rel.rd_index)
	{
	when SIMC_INDEX:
		sscanf(which,"%d",&slice_no);
		if (slice_no < 0 || slice_no >= rel.rd_sdescsz) {
			fprintf(stderr,"slice out of range, max is %d\n",
					rel.rd_sdescsz - 1);
			return;
		}
		off = rel.rd_descoffset + slice_no*nBytes(ncounters);
		lseek(descf, off, L_SET);
		for (i = 0; i < nWords(rel.rd_nsegs); i++) {
			read(descf, &word, sizeof(word));
			printf(" %08x",word);
		}
		putchar('\n');
	when DSIMC_INDEX:
		sscanf(which,"%d,%d",&block_no,&slice_no);
		max_block = rel.rd_nsegs >> rel.rd_sdesc_block;
		if (block_no < 0 || block_no >= max_block) {
			fprintf(stderr,"block out of range, max is %d\n",
					max_block - 1);
			return;
		}
		if (slice_no < 0 || slice_no >= rel.rd_sdescsz) {
			fprintf(stderr,"slice out of range, max is %d\n",
					rel.rd_sdescsz - 1);
			return;
		}
		off = rel.rd_descoffset + slice_no*nBytes(1 << rel.rd_sdesc_block)
					+ block_no*rel.rd_block_size;
		lseek(descf, off, L_SET);
		for (i = 0; i < nWords(1<<rel.rd_sdesc_block); i++) {
			read(descf, &word, sizeof(word));
			printf(" %08x",word);
		}
		putchar('\n');
	}
}

