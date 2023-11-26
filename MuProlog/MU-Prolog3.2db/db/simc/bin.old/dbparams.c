/*
 * dbparams.c  -  deductive database package
 *
 * This program computes near-optimal database parameters
 *
 * $Header$
 * $Log$
 */

#include <stdio.h>
#include <math.h>

#define	SIMC_INDEX	0
#define	DSIMC_INDEX	1

#define	when		break; case


main(argc,argv)
int argc;
char **argv;
{
	int ix;
	double ln2,lnr;
	double x0, x1, x2;
	int br,nr,Nr,sr,seg_size;
	int b,block;
	int N,Nnr,Ns,bs,nd;
	int ks,kr;
	int av;
	int Us, Ur;
	float	ovhead;
	int	verbose = 0;

	if (argc < 7) {
		fprintf(stderr,"usage %s [-v] index_scheme av_rec_len arity num_records seg_size num_data_files\n", argv[0]);
		exit(1);
	}

	ln2 = log((double)2.0);
	lnr = log((double)2048.0);
	if (argc == 8)
		if (strcmp(*++argv,"-v") == 0)
			verbose++;
	argv++;
	if (strcmp(*argv,"simc") == 0)
		ix = SIMC_INDEX;
	else if (strcmp(*argv,"dsimc") == 0)
		ix = DSIMC_INDEX;
	else {
		fprintf(stderr, "invalid index scheme\n");
		exit(-1);
	}
	av = atoi(*++argv);
	sr = atoi(*++argv);
	N = atoi(*++argv);
	block = atoi(*++argv);
	nd = atoi(*++argv);

	br = sr * lnr/(ln2*ln2);
	b = block;
	nr = (b-8) / (br*4 + 260 + 32 * av);
	Nr = 32*nr;
	Nnr = (float)(b-8 -(br+1)*(nr+1)*4)/(float)(8+av);
	if(Nnr > Nr){
		Nr = Nnr;
		nr++;
	}
	seg_size = (br+1)*nr*4 + (Nr+1)*8+Nr*av + .9;
	br +=  (b - seg_size)/(nr*4);
	Ns = (N - 1 + Nr)/Nr;
	/* ?? if (Ns > 16) don't round up too much ?? */
		Ns = ((Ns+31)/32)*32;
	kr = lnr/ln2;
	switch (ix) {
	when SIMC_INDEX:
		ks = 4;
		x0 = 1.0 / (double)Ns;
		x1 = 1.0 / (double)ks;
		x1 = 1.0 - pow(x0,x1);
		x2 = 1.0 / (double)(sr * Nr);
		bs = (double)ks / (1.0 - pow(x1,x2));
		N = Ns*Nr;
		Us = ks; Ur = kr;
	when DSIMC_INDEX:
		bs = 1024;
		ks = bs*ln2/(Nr*sr) + .9;
		Us = ks; Ur = kr;
	}
	if (verbose) {
		ovhead = (float)(b + bs/8 - av*Nr) / (float)(av*Nr) * 100.0;
		fprintf(stderr,"overhead: %0.2f%%   ",ovhead);
		fprintf(stderr,"total space: %d\n",Ns*b + Ns*bs/8 + Ns*4);
		printf("%-4s %-6s %-2s %-2s %-2s %-2s %-4s %-4s %-2s %-2s\n",
			"Br","Bs","Kr","Ks","Ur","Us","Nr","Ns","Nd","Av");
	}
	printf("%-4d %-6d %-2d %-2d %-2d %-2d %-4d %-4d %-2d %-2d\n",
					br,bs,kr,ks,Ur,Us,Nr,Ns,nd,av);
}
