BEGIN {
	n = 0;
	print "#include \"mu.h\"";
}

/^DefineAtom/ {
#	split($1, a, "(")
#	split(a[2], b, ",")
#	names[n] = b[1]
	split($0, a, "\"")
	pnames[n] = a[2]
	n++
}

END {
	print "char *systemAtomNames[] = {"
	for(i = 0; i < n; i++)
		printf "\"%s\",\n", pnames[i]
	print "};"

	printf "Atom systemAtoms[%d] = {\n", n
	for(i = 0; i < n; i++)
		printf "	{0, %d, 0, MakeSmallInt(0), 0, 0},\n", i
	print "};"

	printf "int nSystemAtoms = %d;\n", n
}
