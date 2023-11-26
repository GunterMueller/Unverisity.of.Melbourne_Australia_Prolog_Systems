BEGIN {
	n = 0;
}

/^DefineAtom/ {
	split($1, a, "(")
	split(a[2], b, ",")
	printf "#define %s (systemAtoms[%d])\n", b[1], n
	printf "#define ind%s (sizeof(Atom) * %d)\n", b[1], n
	n++
}

END {
	print "extern Atom systemAtoms[];"
	print "extern char *systemAtomNames[];"
	print "extern int nSystemAtoms;"
}
