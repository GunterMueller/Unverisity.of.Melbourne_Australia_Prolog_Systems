/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog disgusting kernal hacks.

%	Destructive append.  Roughly equivalent to poking a screwdriver
%	at the cpu.  Voids maintenance contract.
$nconc(X, Y) :-
	X = _.XT,
	( XT == [] ->
		$replacn(2, X, Y)
	;	$nconc(XT, Y)
	).

%	Coerce one object into another.
$makeObject(CType, X, Object) :-
	( $ctag(CType, Type, CTag) ->
		$tag(Type, Tag)
	;	$tag(CType, Tag),
		CTag = 0
	),
	$mkObj(Tag, CTag, X, Object).

%	Coerce an object into a struct pointer.
%	Used a lot for converting between references and $bmt's
$makeStruct(X, Struct) :-
	$mkObj(6, 0, X, Struct).			% WARNING!  Built-in param.
