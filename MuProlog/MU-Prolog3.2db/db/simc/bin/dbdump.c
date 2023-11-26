/*
 * dbdump.c  -  deductive database package (dump a whole relation)
 *
 * Copyright (C) 1987, The University of Melbourne
 */

#include "simc.h"
#include <signal.h>

#ifdef DBUG
extern	Word	debug_flag;
#endif DBUG

Char	*my_name;
Char	*db_name;
Char	*rel_name;
Int	arity;

Char	qbuf[200];

Trans	*tr;

main(argc, argv)
Int argc;
String argv[];
{
	r_Char	*c;
	r_Int	i, qry;

	c = rindex(argv[0],'/');
	my_name = c == 0 ? argv[0] : (char *)(c+1);

	if (argc != 4) {
		fprintf(stderr, "Usage: %s db rel arity\n", my_name);
		exit(1);
	}
	db_name = argv[1];
	rel_name = argv[2];
	sscanf(argv[3], "%d", &arity);

	sprintf(qbuf, "%s(_", rel_name);
	for (i = 1; i < arity; i++)
		strcat(qbuf, ",_");
	strcat(qbuf, ")");

	if ((tr = trans_open(db_name, qbuf, opQUERY, 0)) == TransNULL) {
		fprintf(stderr, "can't query %s/%s/%d\n", db_name, rel_name, arity);
		exit(-1);
	}

	while (trans_fetch(tr) != StrNULL)
		printf("%s @ (%d,%d)\n", tr->rec_buf, tr->cur_seg, tr->cur_level);

	trans_close(tr);
	exit(0);
}

