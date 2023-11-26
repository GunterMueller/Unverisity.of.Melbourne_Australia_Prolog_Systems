BEGIN {
		n = 0;
		skip = 0;
		header = 1;
	}

/^{"/ {
		if(skip == 1)
			next;
		split($1, a, "\"");
		printf "#define c%s %d\n", a[2], n++;
		split($2, b, "}");
		i = substr(b[1], 3, 1) + 1;
		s = substr(b[1], 4, 2);
		if(s == "T")
			j = 4;
		else if(s == "IL" || s == "SL")
			j = 2;
		else if(s == "L" || s == "P" || s == "I" || s == "S")
			j = 1;
		else
			j = 0;
		printf "#define ni%s ((NWORDS(%d)+%d)*INSTSPERWORD)\n", a[2], i, j;
		next;
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
		header = 0;
		next;
	}

	{
		if(header)
			print;
	}

END {
		print "extern opcode bytecodes[];";
	}
