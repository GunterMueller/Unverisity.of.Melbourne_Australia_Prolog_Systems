%
% Create the database for the online manual
% (has to be careful whether it already exists, etc)
%

main(_) :-
%##	DB = 'LIB/man.NUVERSION',
	DB = '/mip/db',
	( dbCons(DB) ->
		( $onDisc(DB, man(_,_), _) ->
			dbUndefine(DB, man, 3)
		)
	;	dbCreate(DB)
	),
	dbDefine(DB, man, 3,
		[scheme=simc,
		 avrec=256,
		 nrec=900,
		 segsize=16384,
		 ndata=1,
		 template="g:0:0(g:8:7fffffff,g:0:0,g:0:0)"]).
