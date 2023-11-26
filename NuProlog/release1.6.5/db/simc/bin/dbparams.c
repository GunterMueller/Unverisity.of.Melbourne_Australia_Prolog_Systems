/*
 * dbparams.c  -  compute nearly optimal database parameters
 *
 * This program computes near-optimal database parameters for a SIMC
 * database, given a small subset of the possible parameters
 *
 * Copyright (C) 1985, 1986, 1987, The University of Melbourne
 *
 * Code by Kotagiri Ramamohanarao, John Shepherd
 */

#include <stdio.h>
#include <math.h>

#define	SIMC_INDEX	0
#define	DSIMC_INDEX	1

#define	when		break; case

#define	USAGE		"Usage: %s [-v] index_scheme av_rec_len arity num_records seg_size num_data_files\n"

main(argc,argv)
int argc;
char **argv;
{
	/*
	 * Constants
	 */
	double	ln2;		/* natural logarithm of 2 */
	double	lnr;		/* natural logirthm of 2048 ... why 2048? */
	int	Rpb;		/* # bytes in a Recptr */
	int	Spb;		/* # bytes in a Segptr */
	char	*my_name;	/* program name */

	/*
	 * Inputs
	 */
	int	index_scheme;	/* indexing scheme (simc/dsimc) */
	int	Rb;		/* average # bytes per record */
	int	Sr;		/* # keys in predicate/relation */
	int	N;		/* total # records in database */
	int	Bb;		/* # bytes in file system blocks */
	int	Nd;		/* # data files */

	/*
	 * Outputs
	 */
	int	Br;		/* # bits in record codeword */
	int	Bs;		/* # bits in segment codeword */
	int	Kr;		/* # bits to set in record codeword */
	int	Ks;		/* # bits to set in segment codeword */
	int	Ur;		/* # bits to check in record codeword */
	int	Us;		/* # bits to check in segment codeword */
	int	Nr;		/* # bits to check in record codeword */
	int	Ns;		/* # bits to check in record codeword */
	/*int	Rb;		/* average # bytes per record */
	/*int	Nd;		/* # data files */


	/*
	 * Miscellaneous
	 */
	int	nr;		/* first guess at # records per segment */
	int	Nnr;		/* second guess at # records per segment */
	int	seg_size;	/* # bytes in segment using first guess */
	float	ovhead;		/* percentage overhead in database */
	int	verbose = 0;	/* flag for output format */
	double	x0, x1, x2;	/* temporaries */

	/*
	 * Process input args
	 */
	my_name = argv[0];
	if (argc < 7)
		fatal(USAGE, my_name);
	if (argc == 8 && strcmp(*++argv,"-v") == 0)
			verbose++;
	argv++;
	if (strcmp(*argv,"simc") == 0)
		index_scheme = SIMC_INDEX;
	else if (strcmp(*argv,"dsimc") == 0)
		index_scheme = DSIMC_INDEX;
	else
		fatal("%s: invalid index scheme: %s\n", my_name, *argv);
	Rb = atoi(*++argv);
	Sr = atoi(*++argv);
	N = atoi(*++argv);
	Bb = atoi(*++argv);
	Nd = atoi(*++argv);

	/*
	 * Compute constants
	 */
	ln2 = log((double)2.0);
	lnr = log((double)2048.0);
	Rpb = 12; /* sizeof(Recptr) */
	Spb = 8; /* sizeof(Segptr) */
	my_name = argv[0];

	/*
	 * Compute "optimal" record codeword size from number of keys
	 */
	Br = Sr * lnr/(ln2*ln2);

	/*
	 * Compute first guess at how many records per segment
	 * "nr" estimates the width (in words) of each slice
	 * "Nr" is the slice-width in bits == #records per segment
	 *
	 * Bb = nr*4*(Br+1) + nr*32*(Rpb+Rb) + Spb
	 * nr = (Bb - Spb) / (4*(Br+1) + 32*(Rpb+Rb))
	 */
	nr = (Bb-Spb) / (4*(Br+1) + 32*(Rpb+Rb));
	Nr = 32*nr;

	/*
	 * Given the first estimate, compute exactly how many
	 * records we can fit in a segment
	 */
	Nnr = (float)((Bb-Spb) - (Br+1)*(nr+1)*4) / (float)(Rpb+Rb);
	if (Nnr > Nr) {
		Nr = Nnr;
		nr++;
	}

	/*
	 * Now that we know exactly how many records there are,
	 * re-compute the segment size (seg_size) and compare it
	 * to the file system block size (Bb).
	 * Any excess Words (Bb-seg_size) are allocated for extra
	 * record descriptor slices
	 */
	seg_size = (Br+1)*nr*4 + Nr*Rpb + Spb + Nr*Rb + .9;
	Br += (Bb - seg_size)/(nr*4);

	/*
	 * Compute number of segments (Ns).
	 * This is the minimum number of segments required to hold
	 * at least the number of records specified by the user (N).
	 */
	Ns = (N - 1 + Nr)/Nr;

	/*
	 * ?? if (Ns > 16) don't round up too much ??
	 * !! What on earth is this all about????? !!
	 */
		Ns = ((Ns+31)/32)*32;

	/*
	 * Compute how many bits to set for each field in record
	 * !! Looks funny ... why is it always constant?????? !!
	 */
	Kr = lnr/ln2;

	/*
	 * Compute rest of parameters depending on index scheme
	 */
	switch (index_scheme) {
	when SIMC_INDEX:
		Ks = 4;
		x0 = 1.0 / (double)Ns;
		x1 = 1.0 / (double)Ks;
		x1 = 1.0 - pow(x0,x1);
		x2 = 1.0 / (double)(Sr * Nr);
		Bs = (double)Ks / (1.0 - pow(x1,x2));
		Us = Ks; Ur = Kr;
	when DSIMC_INDEX:
		Bs = 1024;
		Ks = Bs*ln2/(Nr*Sr) + .9;
		Us = Ks; Ur = Kr;
		Ns = 0.75 * Nr; /* load factor 0.75 */
	}
	if (verbose) {
		ovhead = (float)(Bb + Bs/8 - Rb*Nr) / (float)(Rb*Nr) * 100.0;
		fprintf(stderr,"overhead: %0.2f%%   ",ovhead);
		fprintf(stderr,"total space: %d\n",Ns*Bb + Ns*Bs/8 + Ns*4);
		printf("%-4s %-6s %-2s %-2s %-2s %-2s %-4s %-4s %-2s %-2s\n",
			"Br","Bs","Kr","Ks","Ur","Us","Nr","Ns","Nd","Av");
	}
	printf("%-4d %-6d %-2d %-2d %-2d %-2d %-4d %-4d %-2d %-2d\n",
					Br,Bs,Kr,Ks,Ur,Us,Nr,Ns,Nd,Rb);
}

fatal(fmt, a1, a2, a3, a4)
char *fmt, *a1, *a2, *a3, *a4;
{
	fprintf(stderr, fmt, a1, a2, a3, a4);
	exit(1);
}
