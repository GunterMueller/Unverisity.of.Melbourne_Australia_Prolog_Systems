BEGIN {
		n = 0;
		skip = 0;
		# Do you belive this?  It's the best I can come up with in portable
		# awk.  Incredible!
		tolower["_"] = "_";
		tolower["0"] = "0";
		tolower["1"] = "1";
		tolower["2"] = "2";
		tolower["3"] = "3";
		tolower["4"] = "4";
		tolower["5"] = "5";
		tolower["6"] = "6";
		tolower["7"] = "7";
		tolower["8"] = "8";
		tolower["9"] = "9";
		tolower["A"] = "a";
		tolower["B"] = "b";
		tolower["C"] = "c";
		tolower["D"] = "d";
		tolower["E"] = "e";
		tolower["F"] = "f";
		tolower["G"] = "g";
		tolower["H"] = "h";
		tolower["I"] = "i";
		tolower["J"] = "j";
		tolower["K"] = "k";
		tolower["L"] = "l";
		tolower["M"] = "m";
		tolower["N"] = "n";
		tolower["O"] = "o";
		tolower["P"] = "p";
		tolower["Q"] = "q";
		tolower["R"] = "r";
		tolower["S"] = "s";
		tolower["T"] = "t";
		tolower["U"] = "u";
		tolower["V"] = "v";
		tolower["W"] = "w";
		tolower["X"] = "x";
		tolower["Y"] = "y";
		tolower["Z"] = "z";
	}

/^{"/ {
		if(skip == 1)
			next;
		split($1, a, "\"")
		split($2, b, "}")
		bytecodes[n] = "";
		op = a[2];
		for(j = 1; j <= length(op); j++)
			bytecodes[n] = bytecodes[n] tolower[substr(op, j, 1)];
		optypes[n] = b[1];
		n++;
		next
	}

/^\#ifdef/ {
		if(skip == 1)
			exit;
		skip = 1;
		split(defines, da, " ");
		for(i in da) {
			if(("-D" $2) == da[i]) {
				skip = 0;
				next;
			}
		}
	}

/^\#endif/ {
		if(skip == 1) {
			skip = 0;
			next;
		} else
			skip = 0;
	}

/^opcode/ {
		next;
	}

END {
		printf "\t.PRED\t'$bytecode',3\n";
		printf "%d:\tsot\t0,&%d,&%d,&%d,&%d\n", n, n+1, n+2, n+3, n+3;
		printf "%d:\tt\t3,0,&0\n", n+1;
		for(i = 1; i < n-1; i++)
			printf "\tr\t3,0,&%d\n", i;
		printf "\ttr\t3,0,&%d\n", n-1;
		printf "%d:\tsoc\t0,&(", n+2;
		for(i = 0; i < n-1; i++)
			printf "$'%s':&%d,", bytecodes[i], i;
		printf "$'%s':&%d),&%d\n", bytecodes[n-1], n-1, n+3;
		printf "%d:\tfail\n", n+3;
		for(i = 0; i < n; i++) {
			printf "\t.CLAUSE\n";
			printf "%d:\tgc\t0,$'%s'\n", i, bytecodes[i];
			printf "\tgc\t1,$%d\n", i;
			printf "\tgc\t2,$'%s'\n", optypes[i];
			printf "\tpro\n";
		}
		printf "\tlast\n";
	}
