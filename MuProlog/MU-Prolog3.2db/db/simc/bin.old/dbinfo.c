/*
 * info.c  -  deductive database package (rel info test)
 *
 * This program prints relrmation about a relation
 *
 * $Header: rel.c,v 1.2 85/06/08 16:53:50 jas Exp $
 * $Log:	rel.c,v $
 * Revision 1.2  85/06/08  16:53:50  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  13:03:56  jas
 * Initial revision
 * 
 *
 */

#include "muddlib.h"

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
	Char	*c, *op, *arg;

	my_name = argv[0];

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

	ncounters = rel.rd_total_segs;
	size = (ncounters+2) * sizeof(Int);
	if ((counters = (Int *)malloc(size)) == (Int *)NULL) {
		exit(1);
	}

	while (printf("%s> ",my_name),fflush(stdout),gets(line) != NULL)
	{
		if (!isatty(0))
			fprintf(stdout,"%s\n",line);
		op = c = line;
		while (!isspace(*c) && *c != ChNULL)
			c++; /* skip op string */
		*c++ = ChNULL;
		while (isspace(*c) && *c != ChNULL)
			c++; /* skip blanks */
		arg = c;
		if (*op == ChNULL)
			nargs = 0;
		or (*arg == ChNULL)
			nargs = 1;
		else
			nargs = 2;

		if (streq(op,"i") || streq(op, "info"))
			show_static_info();
		or (streq(op,"c") || streq(op, "counters")) {
			switch (rel.rd_index)
			{
			when SIMC_INDEX:
				list_counters(TRUE);
			when DSIMC_INDEX:
				if (nargs != 2)
					show_counters("0",TRUE);
				else
					show_counters(arg,TRUE);
			}
		}
		or (streq(op,"C") || streq(op, "Counters")) {
			switch (rel.rd_index)
			{
			when SIMC_INDEX:
				list_counters(FALSE);
			when DSIMC_INDEX:
				if (nargs != 2)
					show_counters("0",FALSE);
				else
					show_counters(arg,FALSE);
			}
		}
		or (streq(op,"s") || streq(op, "slice")) {
			if (nargs != 2)
				fprintf(stderr, "usage: slice slice_number\n");
			else
				show_slice(arg);
		}
		or (streq(op,"?") || streq(op,"help")) {
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

show_static_info()
{
	int i;

	printf("%-30s: %s %s\n", "database/relation", db_name, rel_name);
	printf("%-30s: %d\n", "number of segments (Ns)", rel.rd_nsegs);
	printf("%-30s: %d\n", "records per segment (Nr)", rel.rd_nrecs);
	printf("%-30s: %d\n", "number of data files", rel.rd_ndfiles);
	printf("%-30s: %d chars\n", "average rec length", rel.rd_avreclen);
	printf("%-30s: %d bits\n", "mask size (Ms)", rel.rd_masksz);
	printf("%-30s: %d\n", "rec counter offset", rel.rd_cntroffset);
	printf("%-30s: %d\n", "seg descriptor offset", rel.rd_descoffset);
	printf("%-30s: %d bits\n", "rec codeword size (Ds)", rel.rd_rcwordsz);
	printf("%-30s: %d bits\n", "rec desc size (Br)", rel.rd_rdescsz);
	printf("%-30s: %d\n", "bits set in rec desc (Kr)", rel.rd_rnbits);
	printf("%-30s: %d\n", "bits used in rec desc (Ur)", rel.rd_rubits);
	printf("%-30s: %d bits\n", "seg codeword size (Ds)", rel.rd_scwordsz);
	printf("%-30s: %d bits\n", "seg desc size (Bs)", rel.rd_sdescsz);
	printf("%-30s: %d\n", "bits set in seg desc (Ks)", rel.rd_snbits);
	printf("%-30s: %d\n", "bits used in seg desc (Us)", rel.rd_subits);
	printf("%-30s: %d\n", "segment buffer size", rel.rd_seg_buf_size);
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
		printf("%-30s:", "nsegs in ovflow levels");
		for (i = 0; i < MAXOVLEVELS; i++)
			printf(" %d",rel.rd_nlevsegs[i]);
		printf("\n");
	}
}

list_counters(show_em)
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

show_counters(lev,show_em)
char *lev;
Bool show_em;
{
	int	i, j, size;
	int	addr, level;
	int	ov[16], nr[16];
	int	count;

	sscanf(lev, "%d", &level);
	if (level < 0 || level >= MAXOVLEVELS) {
		fprintf(stderr, "invalid level\n");
		return;
	}
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
	for (i = 0; i < rel.rd_nlevsegs[level]; i++) {
		addr = addressofindex(i,level);
		if (counters[addr] == 0) j++;
		if (counters[addr] > rel.rd_nrecs) {
			if (show_em)
				printf("%5d*%5d |", i, counters[addr] - rel.rd_nrecs -1);
			counters[addr] = rel.rd_nrecs;
			ov[i/rel.rd_local_segs]++;
		}
		else
			if (show_em) printf("%5d,%5d |", i, counters[addr]);
		nr[addr/rel.rd_local_segs] += counters[addr];
		size += counters[addr];
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
		if((i+1)%4 == 0 && show_em) putchar('\n');
	}
	putchar('\n');
	/*
	printf("next seg to search: %d\n", counters[ncounters]);
	*/
	printf("Total recs at level %d: %d\n", level, count);
	printf("# segs with no recs  : %d\n", j) ;
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
		off = rel.rd_descoffset + slice_no*nbytes(ncounters);
		lseek(descf, off, L_SET);
		for (i = 0; i < nwords(rel.rd_nsegs); i++) {
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
		off = rel.rd_descoffset + slice_no*nbytes(1 << rel.rd_sdesc_block)
					+ block_no*rel.rd_block_size;
		lseek(descf, off, L_SET);
		for (i = 0; i < nwords(1<<rel.rd_sdesc_block); i++) {
			read(descf, &word, sizeof(word));
			printf(" %08x",word);
		}
		putchar('\n');
	}
}
