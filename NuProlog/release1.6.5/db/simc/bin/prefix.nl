main(_) :-
	conv.
conv :-
	repeat,
	read(T),
	(	T = end_of_file
	;
		T = (?- op(A, B, C)),
		op(A, B, C),
		fail
	;
		writev([list,string,quote, prefix],T),
		put(46),
		nl,
		fail
	),
	!.
