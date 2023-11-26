BEGIN {
		n = 0;
		skip = 0;
		header = 1;
	}

/^{"/ {
		if(skip == 1)
			next;
		split($1, a, "\"")
		split($2, b, "}")
		printf "$bytecode('%s', %d, %s).\n", a[2], n++, b[1]
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
		header = 0;
		next;
	}
