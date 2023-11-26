BEGIN {
		skip = 0;
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

/^\#else/ {
		if(skip == 1)
			skip = 0;
		else
			skip = 1;
		next;
	}

/^\#endif/ {
		skip = 0;
		next;
	}

{
		if(skip == 1)
			next;
		print;
}
