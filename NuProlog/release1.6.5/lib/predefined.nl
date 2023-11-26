/*
 * Please note that this code is the property of the University
 * of Melbourne and is Copyright 1985, 1986, 1987 by it.
 * 
 * All rights are reserved.
 */

libraryPredicate(Lib, F) :-
	functor(F, P, N),
	libraryPredicate(Lib, P, N).

nonlogicalPredicate(F) :-
	functor(F, P, N),
	nonlogicalPredicate(P, N).

nonlogicalPredicate( (gAll), 2).
nonlogicalPredicate( (gSome), 2).
nonlogicalPredicate( (!), 0).
nonlogicalPredicate( (\+), 1).
nonlogicalPredicate( (->), 2).
nonlogicalPredicate( (;), 2).
nonlogicalPredicate( (once), 1).
nonlogicalPredicate( (setof), 3).
nonlogicalPredicate( (bagof), 3).
nonlogicalPredicate( (clearIOError), 1).
nonlogicalPredicate( (close), 1).
nonlogicalPredicate( (currentInput), 1).
nonlogicalPredicate( (currentOutput), 1).
nonlogicalPredicate( (currentStream), 3).
nonlogicalPredicate( (fileErrors), 0).
nonlogicalPredicate( (noFileErrors), 0).
nonlogicalPredicate( (open), 3).
nonlogicalPredicate( (openNullStream), 1).
nonlogicalPredicate( (pipe), 2).
nonlogicalPredicate( (setInput), 1).
nonlogicalPredicate( (setOutput), 1).
nonlogicalPredicate( (eof), 1).
nonlogicalPredicate( (get), 1).
nonlogicalPredicate( (get), 2).
nonlogicalPredicate( (get0), 1).
nonlogicalPredicate( (get0), 2).
nonlogicalPredicate( (getToken), 2).
nonlogicalPredicate( (getToken), 3).
nonlogicalPredicate( (read), 1).
nonlogicalPredicate( (read), 2).
nonlogicalPredicate( (readTerm), 3).
nonlogicalPredicate( (readTerm), 4).
nonlogicalPredicate( (skip), 1).
nonlogicalPredicate( (skip), 2).
nonlogicalPredicate( (ttyget), 1).
nonlogicalPredicate( (ttyget0), 1).
nonlogicalPredicate( (ttyskip), 1).
nonlogicalPredicate( (display), 1).
nonlogicalPredicate( (display), 2).
nonlogicalPredicate( (flushOutput), 1).
nonlogicalPredicate( (nl), 0).
nonlogicalPredicate( (nl), 1).
nonlogicalPredicate( (portraycl), 1).
nonlogicalPredicate( (portraygoals), 1).
nonlogicalPredicate( (print), 1).
nonlogicalPredicate( (printf), 2).
nonlogicalPredicate( (printf), 3).
nonlogicalPredicate( (put), 1).
nonlogicalPredicate( (put), 2).
nonlogicalPredicate( (putl), 1).
nonlogicalPredicate( (putl), 2).
nonlogicalPredicate( (tab), 1).
nonlogicalPredicate( (tab), 2).
nonlogicalPredicate( (ttyflush), 0).
nonlogicalPredicate( (ttynl), 0).
nonlogicalPredicate( (ttyput), 1).
nonlogicalPredicate( (ttytab), 1).
nonlogicalPredicate( (write), 1).
nonlogicalPredicate( (write), 2).
nonlogicalPredicate( (writeln), 1).
nonlogicalPredicate( (writeln), 2).
nonlogicalPredicate( (writev), 2).
nonlogicalPredicate( (writev), 3).
nonlogicalPredicate( (atom), 1).
nonlogicalPredicate( (atomic), 1).
nonlogicalPredicate( (cons), 1).
nonlogicalPredicate( (const), 1).
nonlogicalPredicate( (ground), 1).
nonlogicalPredicate( (int), 1).
nonlogicalPredicate( (nonvar), 1).
nonlogicalPredicate( (occurs), 2).
nonlogicalPredicate( (term), 1).
nonlogicalPredicate( (termToString), 2).
nonlogicalPredicate( (termToString), 3).
nonlogicalPredicate( (var), 1).
nonlogicalPredicate( (\=), 2).
nonlogicalPredicate( (==), 2).
nonlogicalPredicate( (\==), 2).
nonlogicalPredicate( (@<), 2).
nonlogicalPredicate( (@=<), 2).
nonlogicalPredicate( (@>), 2).
nonlogicalPredicate( (@>=), 2).
nonlogicalPredicate( (compare), 3).
nonlogicalPredicate( (sread), 2).
nonlogicalPredicate( (sreadTerm), 4).
nonlogicalPredicate( (tokenize), 4).
nonlogicalPredicate( (compile), 1).
nonlogicalPredicate( (consult), 1).
nonlogicalPredicate( (lib), 1).
nonlogicalPredicate( (libdirectory), 1).
nonlogicalPredicate( (load), 1).
nonlogicalPredicate( (.), 2).
nonlogicalPredicate( (restore), 1).
nonlogicalPredicate( (save), 1).
nonlogicalPredicate( (assert), 1).
nonlogicalPredicate( (assert), 2).
nonlogicalPredicate( (asserta), 1).
nonlogicalPredicate( (asserta), 2).
nonlogicalPredicate( (assertz), 1).
nonlogicalPredicate( (assertz), 2).
nonlogicalPredicate( (clause), 2).
nonlogicalPredicate( (clause), 3).
nonlogicalPredicate( (clauses), 3).
nonlogicalPredicate( (dynamic), 1).
nonlogicalPredicate( (erase), 1).
nonlogicalPredicate( (instance), 2).
nonlogicalPredicate( (retract), 1).
nonlogicalPredicate( (retractall), 1).
nonlogicalPredicate( (retractall), 2).
nonlogicalPredicate( (addprop), 3).
nonlogicalPredicate( (addpropa), 3).
nonlogicalPredicate( (addpropz), 3).
nonlogicalPredicate( (getprop), 3).
nonlogicalPredicate( (properties), 3).
nonlogicalPredicate( (putprop), 3).
nonlogicalPredicate( (remprop), 2).
nonlogicalPredicate( (remprop), 3).
nonlogicalPredicate( (db_cons), 1).
nonlogicalPredicate( (db_create), 1).
nonlogicalPredicate( (db_define), 4).
nonlogicalPredicate( (db_rules), 2).
nonlogicalPredicate( (db_undefine), 2).
nonlogicalPredicate( (abort), 0).
nonlogicalPredicate( (ancestors), 1).
nonlogicalPredicate( (break), 0).
nonlogicalPredicate( (depth), 1).
nonlogicalPredicate( (restart), 0).
nonlogicalPredicate( (debugging), 0).
nonlogicalPredicate( (nodebug), 0).
nonlogicalPredicate( (nospy), 1).
nonlogicalPredicate( (spy), 1).
nonlogicalPredicate( (csh), 0).
nonlogicalPredicate( (exit), 1).
nonlogicalPredicate( (fork), 0).
nonlogicalPredicate( (fork), 1).
nonlogicalPredicate( (sh), 0).
nonlogicalPredicate( (system), 1).
nonlogicalPredicate( (signal), 2).
nonlogicalPredicate( (system), 2).

libraryPredicate(antiUnify, 'antiUnify',3).
libraryPredicate(antiUnify, 'antiUnify',5).
libraryPredicate(arg, 'arg0',3).
libraryPredicate(arg, 'genarg',3).
libraryPredicate(arg, 'genarg0',3).
libraryPredicate(arg, 'args',3).
libraryPredicate(arg, 'args0',3).
libraryPredicate(arg, 'pathArg',3).
libraryPredicate(changeArg, 'changeArg',5).
libraryPredicate(changeArg, 'copyArgs',4).
libraryPredicate(changeArg, 'changeArg',4).
libraryPredicate(changeArg, 'changeFunctor',5).
libraryPredicate(changeArg, 'swapArgs',6).
libraryPredicate(changeArg, 'swapArgs',4).
libraryPredicate(changeArg, 'changePathArg',5).
libraryPredicate(changeArg, 'changePathArg',4).
libraryPredicate(compat, 'int',1).
libraryPredicate(compat, '.agnot2',1).
libraryPredicate(compat, 'gnot',2).
libraryPredicate(compat, 'putatom',2).
libraryPredicate(compat, 'putatom',1).
libraryPredicate(curses, 'foreignFile',2).
libraryPredicate(curses, 'foreign',2).
libraryPredicate(curses, 'my_term',1).
libraryPredicate(curses, 'addch',1).
libraryPredicate(curses, 'waddch',2).
libraryPredicate(curses, 'addstr',1).
libraryPredicate(curses, 'waddstr',2).
libraryPredicate(curses, 'clear',0).
libraryPredicate(curses, 'clrtobot',0).
libraryPredicate(curses, 'clrtoeol',0).
libraryPredicate(curses, 'delch',0).
libraryPredicate(curses, 'deleteln',0).
libraryPredicate(curses, 'erase',0).
libraryPredicate(curses, 'insch',1).
libraryPredicate(curses, 'winsch',2).
libraryPredicate(curses, 'insertln',0).
libraryPredicate(curses, 'move',2).
libraryPredicate(curses, 'wmove',3).
libraryPredicate(curses, 'refresh',0).
libraryPredicate(curses, 'wrefresh',1).
libraryPredicate(curses, 'standout',0).
libraryPredicate(curses, 'standend',0).
libraryPredicate(curses, 'crmode',0).
libraryPredicate(curses, 'nocrmode',0).
libraryPredicate(curses, 'getch',1).
libraryPredicate(curses, 'wgetch',2).
libraryPredicate(curses, 'getstr',1).
libraryPredicate(curses, 'wgetstr',2).
libraryPredicate(curses, 'inch',1).
libraryPredicate(curses, 'initscr',0).
libraryPredicate(curses, 'longname',1).
libraryPredicate(curses, 'fullname',1).
libraryPredicate(curses, 'mvwin',3).
libraryPredicate(curses, 'writew',1).
libraryPredicate(curses, 'writew',2).
libraryPredicate(curses, 'writelnw',1).
libraryPredicate(curses, 'writelnw',2).
libraryPredicate(curses, 'writevw',2).
libraryPredicate(curses, 'writevw',3).
libraryPredicate(curses, 'nlw',0).
libraryPredicate(curses, 'nlw',1).
libraryPredicate(curses, 'formatw',2).
libraryPredicate(curses, 'formatw',3).
libraryPredicate(curses, 'mvaddch',3).
libraryPredicate(curses, 'mvwaddch',4).
libraryPredicate(curses, 'mvgetch',3).
libraryPredicate(curses, 'mvwgetch',4).
libraryPredicate(curses, 'mvgetstr',3).
libraryPredicate(curses, 'mvwgetstr',4).
libraryPredicate(curses, 'mvaddstr',3).
libraryPredicate(curses, 'mvwaddstr',4).
libraryPredicate(curses, 'mvwritew',3).
libraryPredicate(curses, 'mvwritew',4).
libraryPredicate(curses, 'mvwriteln',3).
libraryPredicate(curses, 'mvwriteln',4).
libraryPredicate(curses, 'mvformatw',4).
libraryPredicate(curses, 'mvformatw',5).
libraryPredicate(curses, 'mvinch',3).
libraryPredicate(curses, 'mvwinch',4).
libraryPredicate(curses, 'mvdelch',2).
libraryPredicate(curses, 'mvwdelch',3).
libraryPredicate(curses, 'mvinsch',3).
libraryPredicate(curses, 'mvwinsch',4).
libraryPredicate(db, '.aassert1',4).
libraryPredicate(db, 'assert',1).
libraryPredicate(db, 'assert',2).
libraryPredicate(db, '.basserta1',3).
libraryPredicate(db, '.aasserta1',4).
libraryPredicate(db, 'asserta',1).
libraryPredicate(db, 'asserta',2).
libraryPredicate(db, '.bassertz1',3).
libraryPredicate(db, '.aassertz1',4).
libraryPredicate(db, 'assertz',1).
libraryPredicate(db, 'assertz',2).
libraryPredicate(db, 'retract',1).
libraryPredicate(db, 'retractall',1).
libraryPredicate(db, 'clause',2).
libraryPredicate(db, 'clauses',3).
libraryPredicate(db, 'dbCons',1).
libraryPredicate(db, '.adbCreate1',1).
libraryPredicate(db, '.bdbCreate1',1).
libraryPredicate(db, 'dbCreate',1).
libraryPredicate(db, 'dbRules',2).
libraryPredicate(db, 'dbDefine',4).
libraryPredicate(db, '.ddbDefine3',4).
libraryPredicate(db, '.cdbDefine3',3).
libraryPredicate(db, '.adbDefine3',1).
libraryPredicate(db, '.bdbDefine3',1).
libraryPredicate(db, 'dbDefine',3).
libraryPredicate(db, 'listToHex',2).
libraryPredicate(db, 'dbRedefine',3).
libraryPredicate(db, 'dbRedefine',4).
libraryPredicate(db, 'dbUndefine',3).
libraryPredicate(db, 'dbBackup',2).
libraryPredicate(db, '.adbRestore2',3).
libraryPredicate(db, 'dbRestore',2).
libraryPredicate(db, 'dbParam',4).
libraryPredicate(db, 'simc_hash',3).
libraryPredicate(debug, 'dConsult',1).
libraryPredicate(debug, 'dLoad',1).
libraryPredicate(debug, 'dPred',2).
libraryPredicate(debug, 'dPreds',1).
libraryPredicate(debug, 'dClause',3).
libraryPredicate(debug, 'dClauses',3).
libraryPredicate(debug, 'dFile',3).
libraryPredicate(debug, 'dGoal',2).
libraryPredicate(debug, 'dGoals',2).
libraryPredicate(debug, 'dGoals',1).
libraryPredicate(debug, 'dDec',4).
libraryPredicate(debug, 'dDecs',4).
libraryPredicate(debug, 'dProp',4).
libraryPredicate(debug, 'dProps',4).
libraryPredicate(debug, 'dDCall',4).
libraryPredicate(debug, 'dDCalls',3).
libraryPredicate(debug, 'dDAncs',3).
libraryPredicate(debug, 'dICall',4).
libraryPredicate(debug, 'dICalls',3).
libraryPredicate(debug, 'dGoalDCall',3).
libraryPredicate(debug, 'dGoalICall',3).
libraryPredicate(debug, 'dLastPreds',1).
libraryPredicate(debug, 'dAddPred',2).
libraryPredicate(debug, 'dAddPred',3).
libraryPredicate(debug, 'dAddPreds',1).
libraryPredicate(debug, 'dAddPreds',2).
libraryPredicate(debug, 'dAbolishPred',2).
libraryPredicate(debug, 'dAbolishPreds',1).
libraryPredicate(debug, 'dAddClause',1).
libraryPredicate(debug, 'dAddClauses',1).
libraryPredicate(debug, 'dRmClause',3).
libraryPredicate(debug, 'dRmClauses',2).
libraryPredicate(debug, 'dAddDec',4).
libraryPredicate(debug, 'dAddDecs',4).
libraryPredicate(debug, 'dRmDec',4).
libraryPredicate(debug, 'dRmDecs',3).
libraryPredicate(debug, 'dRmDecs',2).
libraryPredicate(debug, 'dAddGoal',2).
libraryPredicate(debug, 'dAddGoals',2).
libraryPredicate(debug, 'dRmGoal',2).
libraryPredicate(debug, 'dRmGoals',1).
libraryPredicate(debug, 'dRmGoals',0).
libraryPredicate(debug, 'dPutProp',4).
libraryPredicate(debug, 'dAddProp',4).
libraryPredicate(debug, 'dAddProps',4).
libraryPredicate(debug, 'dRmProp',4).
libraryPredicate(debug, 'dRmProps',3).
libraryPredicate(debug, 'dRmProps',2).
libraryPredicate(debug, 'sub_goal',2).
libraryPredicate(debug, 'applySuffix',4).
libraryPredicate(debug, 'forall',2).
libraryPredicate(debug, 'dPortraycl',1).
libraryPredicate(debug, 'mk_uscore',1).
libraryPredicate(debug, 'name_vars',2).
libraryPredicate(debug, 'dListing',2).
libraryPredicate(debug, 'dListing',0).
libraryPredicate(debug, 'print_dec',2).
libraryPredicate(debug, 'dEdit',2).
libraryPredicate(dsimc, 'dsimcQuery',1).
libraryPredicate(dsimc, 'startQstats',1).
libraryPredicate(dsimc, 'endQstats',1).
libraryPredicate(dsimc, 'startPhase',2).
libraryPredicate(dsimc, 'endPhase',2).
libraryPredicate(fromonto, 'copyBytes',0).
libraryPredicate(fromonto, 'fromStream',2).
libraryPredicate(fromonto, 'fromFile',2).
libraryPredicate(fromonto, 'fromChars',2).
libraryPredicate(fromonto, 'ontoStream',2).
libraryPredicate(fromonto, 'ontoFile',2).
libraryPredicate(fromonto, 'ontoChars',2).
libraryPredicate(lineio, 'getLine',1).
libraryPredicate(lineio, 'getLine',2).
libraryPredicate(lineio, 'fGetLine',2).
libraryPredicate(lineio, 'fGetLine',3).
libraryPredicate(lineio, 'putChars',1).
libraryPredicate(lineio, 'putChars',2).
libraryPredicate(lineio, 'putLine',1).
libraryPredicate(lineio, 'putLine',2).
libraryPredicate(lineio, 'skipLine',0).
libraryPredicate(lineio, 'skipLine',1).
libraryPredicate(logarr, 'newArray',1).
libraryPredicate(logarr, 'newArray',2).
libraryPredicate(logarr, 'newLastUseArray',1).
libraryPredicate(logarr, 'newLastUseArray',2).
libraryPredicate(logarr, 'isArray',1).
libraryPredicate(logarr, 'isArray',2).
libraryPredicate(logarr, 'asize',2).
libraryPredicate(logarr, 'aref',3).
libraryPredicate(logarr, 'arefl',3).
libraryPredicate(logarr, 'aset',4).
libraryPredicate(logarr, 'alist',2).
libraryPredicate(occurs, 'noCommonVariable',2).
libraryPredicate(occurs, 'containsTerm',2).
libraryPredicate(occurs, 'containsVar',2).
libraryPredicate(occurs, 'freeOfTerm',2).
libraryPredicate(occurs, 'freeOfVar',2).
libraryPredicate(occurs, 'occurrencesOfTerm',3).
libraryPredicate(occurs, 'occurrencesOfVar',3).
libraryPredicate(occurs, 'occurrencesOfMatch',3).
libraryPredicate(occurs, 'containsMatch',2).
libraryPredicate(occurs, 'freeOfMatch',2).
libraryPredicate(occurs, 'subTerm',2).
libraryPredicate(osets, 'listToSet',2).
libraryPredicate(osets, 'addElement',3).
libraryPredicate(osets, 'delElement',3).
libraryPredicate(osets, 'setMember',2).
libraryPredicate(osets, 'disjoint',2).
libraryPredicate(osets, 'intersect',2).
libraryPredicate(osets, 'intersect',3).
libraryPredicate(osets, 'setEq',2).
libraryPredicate(osets, 'subset',2).
libraryPredicate(osets, 'subtract',3).
libraryPredicate(osets, 'symdiff',3).
libraryPredicate(osets, 'union',3).
libraryPredicate(predefined, 'libraryPredicate',2).
libraryPredicate(predefined, 'nonlogicalPredicate',1).
libraryPredicate(predefined, 'nonlogicalPredicate',2).
libraryPredicate(predefined, 'libraryPredicate',3).
libraryPredicate(prompt, 'prompt',1).
libraryPredicate(prompt, 'promptedChar',2).
libraryPredicate(prompt, 'promptedLine',2).
libraryPredicate(prompt, 'promptedLine',3).
libraryPredicate(sql, 'sqlQuery',2).
libraryPredicate(sql, 'sqlModify',2).
libraryPredicate(sql, 'sqlAccess',3).
libraryPredicate(termdepth, 'termDepth',2).
libraryPredicate(termdepth, 'depthBound',2).
libraryPredicate(termdepth, 'termSize',2).
libraryPredicate(termdepth, 'sizeBound',2).
libraryPredicate(termdepth, 'lengthBound',2).
libraryPredicate(unify, 'unify',2).
