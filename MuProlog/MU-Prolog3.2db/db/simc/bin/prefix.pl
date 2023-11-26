main(_) :-
	conv.
conv :-
	wflags(2'10111),
	repeat,
	read(T),
	(	eof(T)
	;
		T = (?- op(A, B, C)),
		op(A, B, C),
		fail
	;
		write(T),
		put(46),
		nl,
		fail
	),
	!.
