/*
 * Please note that this code is the property of
 * the University of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 *
 * Author: Jeff Schultz
 */

% Nepolog parameters.

maxint(X) :-
	X is maxint.

nuprolog.

muprolog :- fail.

%	The $ prefixed are retained here to be constitent with the type
%	testing predicates.
%
%	There are also built-in versions of these constants in dkh.nl.
$tag(ref,		2'000).
$tag($ref,		2'000).
$tag(del,		2'001).
$tag($del,		2'001).
$tag(icn,		2'010).
$tag(ucn,		2'011).
$tag(list,		2'100).
$tag($list,		2'100).
$tag(string,	2'101).
$tag($string,	2'101).
$tag(struct,	2'110).
$tag($struct,	2'110).
$tag(bmv,		2'111).
$tag($bmv,		2'111).

$ctag(integer,	icn, 2'00).
$ctag(atom,		icn, 2'01).
$ctag(block,	icn, 2'11).
$ctag($block,	icn, 2'11).
$ctag($i32,		ucn, 2'00).
$ctag(float,	ucn, 2'01).

%	$type/2 and $ctype/2 are for the use of the compiler
%	as arguments to the TYP[XY] and CTYP[XY] instructions.
$type(var,		2'00000011).
$type($ref,		2'00000001).
$type($del,		2'00000010).
$type(nonvar,	2'11111100).
$type($icn, 	2'00000100).
$type($ucn, 	2'00001000).
$type(compound,	2'01110000).			% Includes LST and CHR
$type(const, 	2'00001100).
$type(cons,		2'00110000).
$type($string,	2'00100000).			% Just CHR
$type(term,		2'01110000).			% Includes LST and CHR
$type($struct,	2'01000000).			% Just STR
$type($bmv,		2'10000000).

$ctype($smallInt,	2'00000001).
$ctype(integer,		2'00010001).
$ctype(atom,		2'00000010).
$ctype(atomic,		2'00110011).				% Not including BLK
$ctype($block,		2'00001000).
$ctype(float,		2'00100000).
$ctype($i32,		2'00010000).
$ctype(number,		2'00110001).

%	Code types.
$codeType(interpreted,	8'001).
$codeType(compiled,		8'002).
$codeType(native,		8'003).
$codeType(spypoint,		8'004).
