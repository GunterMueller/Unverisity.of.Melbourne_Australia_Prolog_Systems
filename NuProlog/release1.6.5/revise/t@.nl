	% wrong versions of reverse
?- isTerm(a).
rev(bbbbbbb, []).
% rev(A.B, C) :- rev(B, C1), app(C1, [A], C).	% correct
% rev(A.B, C) :- rev(B, C1), app(C1, A, C).	% wrong/miss
% rev(A.B, C) :- rev(B, C1), app([A], C1, C).	% wrong
  rev(A.B, C) :- rev(B, C1), app(C1, C, [A]).	% wrong
% rev(A.B, C) :- rev(C, C1), app(C1, A, C).	% wrong/loops
% rev(A.B, C) :- rev(B, C1), app(C, [A], C1).	% awful loops
% rev(A.B, C) :- rev(C, C1), app(C1, [A], C).	% awful loops
