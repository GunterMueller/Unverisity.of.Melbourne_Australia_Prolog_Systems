%
% Load the database for the online manual
%

main(_) :-
%##	dbCons('LIB/man.NUVERSION'),
	dbCons('/mip/db'),
	repeat,
	read(X),
	(if X = end_of_file
	then
		exit(0)
	else
		X = man(A,B,C),
		assert(X)
		, writeln(asserted(A))
	),
	fail.
