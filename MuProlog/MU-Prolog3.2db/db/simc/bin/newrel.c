/*
 * newrel.c  -  deductive database package (front-end)
 *
 * Given a complete set of database parameters (via dbparams?),
 * this program creates a relation within the specified database
 *
 * $Header: newrel.c,v 1.2 85/06/08 16:53:55 jas Exp $
 * $Log:	newrel.c,v $
 * Revision 1.2  85/06/08  16:53:55  jas
 * all buffered and cached
 * 
 * Revision 1.1  85/05/26  13:04:01  jas
 * Initial revision
 * 
 */

#include "simc.h"

String my_name;

main(argc, argv)
Int argc;
String argv[];
{
	r_Int	i;
	r_Char	*c;
	Word	ix;
	Reln	*rel;
	Char	rname[MAXRNAME];
	Char	reldir[MAXPATH];
	Char	cmd[MAXPATH+10];
	struct	stat status;
	Int	nkeys, br, bs, kr, ks, ur, us, nr, ns, nd, av;
	Bool	rel_create();

	c = rindex(argv[0],'/');
	my_name = c == 0 ? argv[0] : (char *)(c+1);

	if (argc < 15) {
		fprintf(stderr,
			"Usage: %s db rel index_scheme Br Bs Kr Ks Ur Us Nr Ns Nd Av template\n",
			my_name);
		exit(1);
	}
	if (streq(argv[3], "simc"))
		ix = SIMC_INDEX;
	or (streq(argv[3], "dsimc"))
		ix = DSIMC_INDEX;
	else {
		fprintf(stderr, "%s: bad index scheme\n", my_name);
		exit(1);
	}
	br = atoi(argv[4]);
	bs = atoi(argv[5]);
	kr = atoi(argv[6]);
	ks = atoi(argv[7]);
	ur = atoi(argv[8]);
	us = atoi(argv[9]);
	nr = atoi(argv[10]);
	ns = atoi(argv[11]);
	nd = atoi(argv[12]);
	av = atoi(argv[13]);

	if (! rel_create(argv[1],argv[2],ix,br,bs,kr,ks,ur,us,nr,ns,nd,av,argv[14])) {
		fprintf(stderr, "%s: Can't create relation %s in database %s\n",
				my_name, argv[2], argv[1]);
		exit(1);
	}
	exit(0);
}

/*
 * rel_create:
 *	Create a new relation in the specified database
 */
Bool
rel_create(db_name, rel_name, typ, dr, ds, kr, ks, ur, us, nr, ns, nd, av, skel)
String db_name;	/* name of directory containing database */
String rel_name;	/* relation name */
Word typ;	/* which indexing scheme to use */
Int dr, ds;	/* total # bits in record and segment descriptors */
Int kr, ks;	/* # bits to be set in rec and seg descriptors */
Int ur, us;	/* # bits to be used in rec and seg descriptors */
Int nr, ns;	/* records per segment, max segments in data file */
		/* for dsimc - "ns" contains # insertions bewteen splits */
Int nd, av;	/* number of data files, average record size */
String skel;	/* string describing structure of keys */
{
	r_Int	i, j;
	r_Char	*c;
	r_Reln	*info;
	r_Int	nbytes;
	r_Word	*b, *buf;
	Recptr	*rec_ptr;
	Segptr	*seg_ptr;
	Long	off;
	Int	total_segs;
	Int	nsegs_to_write;
	Int	deep, ldfac, ldcnt;
	File	segf, descf;
	Bool	status = FALSE;
	Char	filename[MAXPATH];
	Char	reldir[MAXPATH];
	Char	cmd[MAXPATH+10];
	struct	stat fstatus;
	extern	Int errno;
	Int	ceil();

	/*
	 * Allocate a static info descriptor for relation
	 */
	if ((info = mknew(Reln)) == RelnNULL) {
		error("Out of memory for relation descriptor");
		return(FALSE);
	}

	/*
	 * Create/open files & directories for relation
	 */
	sprintf(reldir, "%s/%s", db_name, rel_name);
	if (stat(reldir,&fstatus) < 0) {
		if (errno == ENOENT) {
			sprintf(cmd, "mkdir '%s'", reldir);
			system(cmd);
			if (stat(reldir,&fstatus) < 0) {
				fprintf(stderr, "Can't create %s\n", reldir);
				exit(1);
			}
		}		
		else {
			fprintf(stderr, "Can't find %s\n", reldir);
			exit(1);
		}
	}
	or ((fstatus.st_mode & S_IFDIR) == 0) {
		fprintf(stderr, "%s not a directory\n", reldir);
		exit(1);
	}

	sprintf(filename, "%s/%s/desc", db_name, rel_name);
	if ((info->rd_name = mkstr(strlen(filename))) == StrNULL) {
		error("Out of memory for relation name");
		free((char *)info);
		return(FALSE);
	}
	strcpy(info->rd_name, filename);
	if ((descf = open(filename, (O_WRONLY|O_CREAT|O_TRUNC), 0644)) < 0) {
		error("Can't create relation descriptor file");
		free((char *)info->rd_name);
		free((char *)info);
		return(FALSE);
	}	

	/*
	 * Set up indexing scheme type for relation
	 */
	switch (info->rd_index = typ)
	{
	when SIMC_INDEX:
		info->rd_sdesc_block = info->rd_block_size = 0;
		total_segs = ns;
		nsegs_to_write = info->rd_local_segs = ceil(total_segs,nd);
	when DSIMC_INDEX:
		i = 0;
		for (nbytes = PAGESIZE; i < 2; nbytes <<= 1) {
			i = 1; j = 0;
			while (ds*nBytes(i) <= nbytes) {
				i <<= 1; j++;
			}
		}
		info->rd_block_size = nbytes;
		info->rd_sdesc_block = j-1;

		/*
		 * Collect load factor & count values
		 * before they are overwritten by the
		 * computed #segs and #data files
		 */
		ldfac = ns;
		ldcnt = nd;

		/*
		 * Computes "depth" (i.e. amount of expansion) for level 0
		 * This determines initial # segments for level 0
		 * Higher levels have less "depth" => less segments
		 */
		if (nr == 0)
			total_segs = 1;
		else
			total_segs = nd / nr;
		for (i = 1, deep = 0; i < total_segs; i <<= 1, deep++)
			/* round #segs to power of two */;
		if (i > (4/3 * total_segs) && deep > 0)
			{ i >>= 1; deep--; } /* don't make toooo big */
		total_segs = ns = i;
		/*
		 * Initialise all level information
		 */
		for (i = 0; i < MAXLEVELS; i++)
		{
			info->rd_loadfac[i] = ldfac;
			info->rd_splitp[i] = 0;
			info->rd_nlevrecs[i] = 0;
			for (j = 0; j < MAXBITS; j++)
				info->rd_magic[i][j] = 0;
			
			if (deep < 0) {
				info->rd_depth[i] = 0;
				info->rd_nlevsegs[i] = 0;
				info->rd_loadcnt[i] = 0;
			}
			else {
				info->rd_depth[i] = deep;
				info->rd_nlevsegs[i] = j = (1 << deep);
				info->rd_loadcnt[i] = -nr * (j-1);
				deep -= 3;
			}
		}

	otherwise:
		error("Invalid indexing scheme");
		free((char *)info->rd_name);
		free((char *)info);
		return(FALSE);
	}

	/*
	 * Set up constant parameters for relation
	 * (describing descriptor sizes, #segs, #recs, etc.)
	 */
	info->rd_rdescsz = dr;
	info->rd_rnbits = kr;
	info->rd_rubits = ur;
	info->rd_nrecs = nr;
	info->rd_sdescsz = ds;
	info->rd_snbits = ks;
	info->rd_subits = us;
	info->rd_nsegs = ns;
	info->rd_avreclen = av;
	info->rd_seg_info_size = nr * sizeof(Recptr) + nBytes(nr) * (dr+1) + sizeof(Segptr);

	/*
	 * Round segment size up to nearest page boundary
	 */
	info->rd_seg_size = info->rd_seg_info_size + nr * av;
	info->rd_seg_size = alignup(info->rd_seg_size, PAGESIZE);
	if (info->rd_index == DSIMC_INDEX) {
		/* keep files < 10^6 bytes long */
		info->rd_local_segs = 1000000/info->rd_seg_size;
		if (total_segs < info->rd_local_segs)
			nsegs_to_write = total_segs;
		else
			nsegs_to_write = info->rd_local_segs;
		nd = ceil(ns, info->rd_local_segs);
	}
	info->rd_ndfiles = nd>ns ? ns : nd;

	/*
	 * Align record counters to start on a Word boundary
	 * Align segment descriptors to start on page boundary
	 */
	off = info_bytes(info) + strlen(skel) + 1;
	info->rd_cntroffset = off = alignup(off, sizeof(Word));
	off += (total_segs+2) * sizeof(Word);
	info->rd_descoffset = alignup(off, PAGESIZE);

	/*
	 * Set up skeleton, describing structures of keys
	 * Also, sets up mask size field in info structure
	 * and stores cluster masks
	 */
	if (!skel_parse(info,skel)) {
		error("Can't parse relation template");
		goto RelCreateFail;
	}
#ifdef DBUG
	debug(TREE_DBUG)
		skel_print("Unnormalised skel tree:\n",info->rd_skel);
#endif DBUG

	/*
	 * Set up size of codewords (excluding mask bits)
	 */
	info->rd_rcwordsz = dr - info->rd_masksz;
	info->rd_scwordsz = ds - info->rd_masksz;

#ifdef DBUG
#ifdef TODO
	Store normalised form of skeleton string in desc file
#endif TODO
	debug (TREE_DBUG) {
		if (!skel_normalise(info)) {
			error("Failure in template normalisation");
			goto RelCreateFail;
		}
		skel_print("Normalised skel tree:\n",info->rd_skel);
	}
#endif DBUG

	/*
	 * Write static relation info to the descriptor file,
	 */
	nbytes = info_bytes(info);
	if (write(descf, static_info(info), nbytes) != nbytes) {
		error("Can't write onto relation descriptor file");
		goto RelCreateFail;
	}
	nbytes = strlen(skel) + 1;
	if (write(descf, skel, nbytes) != nbytes) {
		error("Can't write onto relation descriptor file");
		goto RelCreateFail;
	}

	/*
	 * Initialise record counters to zeroes
	 */
	nbytes = (total_segs+2) * sizeof(Word);
	if ((buf = (Word *)malloc(nbytes)) == WordNULL) {
		error("Out of memory for clear segment descriptor");
		goto RelCreateFail;
	}
	for (i = 0, b = buf; i < total_segs; i++, b++)
		*b = 0;
	*b++ = total_segs-1;	/* first overflow segment */
	*b = 0;		/* count of records in db */
	lseek(descf, (Faddr)info->rd_cntroffset, L_SET);
	if (write(descf, (char *)buf, nbytes) != nbytes) {
		error("Can't write clear segment descriptor");
		goto RelCreateFail;
	}
	free((char *)buf);

	/*
	 * Initialise segment descriptors to zeroes
	 * This is a bit sneaky and relies on the fact that if
	 * we seek way beyond the end of the file and write a
	 * little piece of text, everything in between becomes zero
	 */
	switch (index_type(info))
	{
	when SIMC_INDEX:
		lseek(descf, (Faddr)(offset(info) + ds*nBytes(ns)), L_SET);
		write(descf, "End desc file", 13);
	when DSIMC_INDEX:
#ifdef TODO
		IS there anything to do here ????
		NOT if we assume that when the system first reads these
		descriptors (which don't exist yet), it will get all zeroes
#endif TODO
		/* keep the compiler happy with an empty statement*/;
	}

	/*
	 * Allocate a segment buffer and initialise it
	 */
	nbytes = info->rd_seg_size;
	if ((buf = (Word *)malloc(nbytes)) == WordNULL) {
		error("Out of memory for segment buffers");
		goto RelCreateFail;
	}
	clearSegBuf(info, buf, 0);
	rec_ptr = (Recptr *)&buf[nWords(nr) * (dr+1)];
	seg_ptr = (Segptr *)&rec_ptr[nr];

	/*
	 * Create segment data files
	 */
	for (i = 0; i < nd; i++)
	{
		if (i == (nd-1)) {
			j = total_segs % info->rd_local_segs;
			if (j != 0)
				nsegs_to_write = j;
		}
		sprintf(filename, "%s/%s/00.%04d", db_name, rel_name, i);
		if ((segf = open(filename,(O_WRONLY|O_CREAT|O_TRUNC),0644)) < 0) {
			error("Can't create data file");
			goto RelCreateFail;
		}
		seg_ptr->sp_freespace = info->rd_seg_info_size;
		for (j = 0; j < nsegs_to_write; j++) {
			lseek(segf, (Faddr)j*info->rd_seg_size, L_SET);
			if (write(segf, (char*)buf, nbytes) != nbytes) {
				error("Can't write onto data file");
				goto RelCreateFail;
			}
			seg_ptr->sp_freespace += info->rd_seg_size;
		}
		close(segf);
	}

	/*
	 * Create overflow level data files for DSIMC
	 */
	if (info->rd_index = DSIMC_INDEX) {
		r_Int lev;

		for (lev = 1; info->rd_nlevsegs[lev] > 0; lev++) {
			nsegs_to_write = info->rd_local_segs;
			total_segs = info->rd_nlevsegs[lev];
			nd = ceil(total_segs, info->rd_local_segs);
			for (i = 0; i < nd; i++)
			{
				if (i == (nd-1)) {
					j = total_segs % info->rd_local_segs;
					if (j != 0)
						nsegs_to_write = j;
				}
				sprintf(filename, "%s/%s/%02d.%04d",
						db_name, rel_name, lev, i);
				if ((segf = open(filename,(O_WRONLY|O_CREAT|O_TRUNC),0644)) < 0) {
					error("Can't create data file");
					goto RelCreateFail;
				}
				seg_ptr->sp_freespace = info->rd_seg_info_size;
				for (j = 0; j < nsegs_to_write; j++) {
					lseek(segf, (Faddr)j*info->rd_seg_size, L_SET);
					if (write(segf, (char*)buf, nbytes) != nbytes) {
						error("Can't write onto data file");
						goto RelCreateFail;
					}
					seg_ptr->sp_freespace += info->rd_seg_size;
				}
				close(segf);
			}
		}
	}

	status = TRUE;

RelCreateFail:
	skel_free(info->rd_skel);
	free((char *)info->rd_name);
	free((char *)buf);
	free((char *)info);
	if (isopen(segf)) close(segf);
	if (isopen(descf)) close(descf);
	return(status);
}

/*
 * ceil:         _   _
 *	Compute | x/y |
 */
Int
ceil(x,y)
{
	r_Int	divide, lower;

	divide = x / y;
	lower = divide * y;
	if (lower < x)
		return(divide + 1);
	else
		return(divide);
}

