/^[a-zA-Z0-9_]*.ns:	\.pred	/ {
	for(i = 1; i < length($3); i++)
		if(substr($3, i, 1) == "$")
			next;
	printf "libraryPredicate(";
	for(i = 1; i < length($1) - 3; i++)
		printf "%s", substr($1, i, 1);
	printf ", %s).\n", $3;
}
