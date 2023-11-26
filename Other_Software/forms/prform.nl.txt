% Test interface to HTML forms for NU-Prolog
% Adapted from C code from Andrew Davison (http://www.cs.mu.oz.au/~ad),
% who got it from elsewhere no doubt.
%
% Note: The Web tends to be 8-bit aware and supports character sets such
% as ISO Latin-1.  NU-Prolog is not really 8-bit aware.  Internally
% strings are implemented as lists of integers (plenty of bits) but various
% string-hendling builtins have their own idea of what lists of integers
% look like valid strings.  Eg, format (with ~s), and putl give error
% messages and write (and close relatives) will print a list of integers
% when given 8-bit strings.  put for single chars works ok, as does printf
% with %s.  Of course, even if your Prolog program handles 8-bit
% characters correctly, the chances are that if the output goes anywhere
% except /dev/null something else will break.  Hopefully NU-Prolog will
% be made fully 8-bit aware some time soon.

	% top level
main(_) :-
	html_form_interface.

	% Outputs the required "Content-type:" line + blank line,
	% checks that the apropriate environment variables are set
	% correctly, reads chars from standard input, forms list of
	% name-value pairs, fixes escape sequences etc and passes list
	% to html_form/1, which is expected to write a HTML page to stdout.
	%
	% The argument to html_form/1 is a list of Name-Value pairs.
	% The Names are atoms which have *not* had escape sequences
	% processed.  They are the names of the form fields, so you
	% had better not use any funny characters in the names!  The
	% Values are strings which have had escape sequences processed.
	% I have no idea how non-ascii characters entered into a form
	% turn out.
html_form_interface :-
	putl("Content-type: text/html\n\n"),
	(getenv('REQUEST_METHOD', "POST") ->
		true
	;
		putl("This script should be referenced with a METHOD of POST.\n"),
		putl("If you don't understand this, read "),
		putl("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.\n"),
		exit(1)
	),
	(getenv('CONTENT_TYPE', "application/x-www-form-urlencoded") ->
		true
	;
		putl("This script can only be used to decode form results.\n"),
		exit(1)
	),
	getenv('CONTENT_LENGTH', CLS),
	intToString(CL, CLS),
	read_all(CL, Input),		% read whole input
	inp_to_nvs(Input, NEVs),	% convert to name-escaped_value pairs
	expand_esc_plus_nv(NEVs, NVs),
	html_form(NVs).

	% Given list of Name-Value pairs from form,
	% output new HTML page
html_form(NVs) :-
	putl("<html>\n"),
	putl("<head>\n"),
	putl("<title>Form contents (produced by NU-Prolog)</title>\n"),
	putl("</head>\n"),
	putl("<body>\n"),
	putl("<H1>Form contents (produced by NU-Prolog)</H1>\n"),
	putl("The list of fields input from the form is as follows:<br>\n"),
	html_form_body(NVs),
	putl("Thats all folks!<br>\n"),
	putl("</body>\n"),
	putl("</html>\n").

	% version which echos name-value pairs
html_form_body([]).
html_form_body(N-V.NVs) :-
	printf("<pre>%s =\n", [N]),
	putl(V),
	putl("</pre><hr>\n"),
	html_form_body(NVs).

	% read N chars from input
	% (reading until EOF seems to just hang)
	% returns list of N+1 chars, including terminating "&"
read_all(N, Cs) :-
	(N == 0 ->
		Cs = "&"
	;
		get0(C),
		Cs = C.Cs1,
		N1 is N - 1,
		read_all(N1, Cs1)
	).

	% Converts string "name1=val1&name2=val2&" into
	% list of pairs [name1-"val1", name2="val2"] etc
	% Funny chars, eg = and & never occur in vals (they
	% appear as escape sequences)
inp_to_nvs(Cs, NVs) :-
	inp_to_nvs1(NVs, Cs, []).

	% as above using DCG
inp_to_nvs1([]) --> "".
inp_to_nvs1(N-V.NVs) -->
	non_eq(NS),
	{atomToString(N, NS)},
	[0'=],
	non_amp(V),
	[0'&],
	inp_to_nvs1(NVs).

	% reads string not containing "="
non_eq([]) --> "".
non_eq(C.Cs) -->
	[C],
	{C ~= 0'=},
	non_eq(Cs).

	% reads string not containing "&"
non_amp([]) --> "".
non_amp(C.Cs) -->
	[C],
	{C ~= 0'&},
	non_amp(Cs).

	% expands escape sequences and converts "+" back into " "
	% in value strings of name-value pairs
	% (NOTE its assumed that names don't have any funny chars!)
expand_esc_plus_nv([], []).
expand_esc_plus_nv(N-EV.NEVs, N-V.NVs) :-
	expand_esc_plus(EV, V),
	expand_esc_plus_nv(NEVs, NVs).

	% expand escape sequences and converts "+" back into " "
	% in a string
expand_esc_plus([], []).
expand_esc_plus(C0.Cs0, C.Cs) :-
	(C0 == 0'+ ->
		C = 0' ,
		expand_esc_plus(Cs0, Cs)
	; C0 == 0'% ->
		Cs0 = C1.C2.Cs1,
		expand_esc(C1, C2, C),
		expand_esc_plus(Cs1, Cs)
	;
		C = C0,
		expand_esc_plus(Cs0, Cs)
	).

	% expand escape sequence
	% translated from C code with no comments...
	% seems to work
expand_esc(C1, C2, C) :-
	(C1 >= 0'A ->
		Digit1 is ((C1 /\ 16'df) - 0'A) + 10
	;
		Digit1 is C1 - 0'0
	),
	(C2 >= 0'A ->
		Digit2 is ((C2 /\ 16'df) - 0'A) + 10
	;
		Digit2 is C2 - 0'0
	),
	C is Digit1 * 16 + Digit2.
