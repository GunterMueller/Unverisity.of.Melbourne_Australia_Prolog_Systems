/*
 * newrel.c  -  deductive database package (front-end)
 *
 * This program creates relations within databases
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

#include "muddlib.h"

main(argc, argv)
Int argc;
String argv[];
{
	r_Int	i;
	Word	ix;
	Reln	*rel;
	Char	rname[MAXRNAME];
	Char	reldir[MAXPATH];
	Char	cmd[MAXPATH+10];
	struct	stat status;
	Int	nkeys, br, bs, kr, ks, ur, us, nr, ns, nd, av;

	if (argc < 15) {
		fprintf(stderr,
		"Usage: newrel db rel index_scheme Br Bs Kr Ks Ur Us Nr Ns Nd Av template\n");
		exit(1);
	}
	if (streq(argv[3], "simc"))
		ix = SIMC_INDEX;
	or (streq(argv[3], "dsimc"))
		ix = DSIMC_INDEX;
	else {
		fprintf(stderr, "%s: bad index scheme\n", argv[0]);
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
		fprintf(stderr, "Can't create relation %s in database %s\n",
				argv[2], argv[1]);
		exit(1);
	}
	exit(0);
}
