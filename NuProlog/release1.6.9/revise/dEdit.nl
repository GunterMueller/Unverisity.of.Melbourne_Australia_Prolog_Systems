	% edit predicate in the debugging environment
	% currently a bit of a hack for revise stuff
	% (much better than it was though)
dEdit(Pred, Arity) :-
	dFile(Pred, Arity, File),
	name(File, FileS),
	name(Pred, PredName),
	append("-p ", PredName, Flag),
	append(FileS1, ".nl", FileS),
	$runRevise(Flag, FileS1),
	append(FileS1, "@.nl", FileS2),
	name(File2, FileS2),
	dConsult(File2),
	dLastPreds(Preds),		% should be able to delete
	$change_file(File, Preds).	% when @ is handled by dConsult

	% hack to change file names from *@.nl to something sensible
$change_file(File, []).
$change_file(File, (F/A).Preds) :-
        remprop(F, $dFile(A)),
        putprop(F, $dFile(A), File),
	$change_file(File, Preds).

