#! /bin/awk
#
# Please note that this code is the property of the University
# of Melbourne and is Copyright 1985, 1989 by it.
# 
# All rights are reserved.
#
# Author: Jeff Schultz
#
# Extract opcodes from "bytecodes.c"
#

BEGIN {
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
		split($1, a, "\"");
		op = a[2];
		printf "\t{\"%s\", c%s},\n", op, op;
		opl = op;
		printf "\t{\"";
		for(i = 1; i <= length(opl); i++)
			printf tolower[substr(opl, i, 1)];
		printf "\", c%s},\n", opl, op;
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
