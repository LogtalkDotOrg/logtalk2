
:- object(help).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2010/06/15,
		comment is 'Command-line help for Logtalk built-in predicates and methods.']).

	:- public(help/0).
	:- mode(help, one).
	:- info(help/0, [
		comment is 'Description']).

	help :-
		write('On-line help is available for Logtalk built-in control constructs,'), nl,
		write('built-in predicates, built-in non-terminals, and built-in methods.'), nl, nl,
		write('  Type help::Functor/Arity.'), nl,
		write('  Or   help::Functor//Arity. '), nl, nl,
		write('  A web page for the selected built-in feature will open in your'), nl,
		write('  default web browser.'), nl, nl.

	:- public('/'/2).
	:- mode('/'(+atom, +integer), zero_or_one).
	:- info('/'/2, [
		comment is 'Description',
		argnames is ['Functor', 'Arity']]).

	PredicateFunctor/Arity :-
		(	built_in_method(PredicateFunctor, Arity, Path, File) ->
			true
		;	built_in_predicate(PredicateFunctor, Arity, Path, File) ->
			true
		;	control(PredicateFunctor, Arity, Path, File) ->
			true
		),
		open(Path, File).

	:- public('//'/2).
	:- mode('//'(+atom, +integer), zero_or_one).
	:- info('//'/2, [
		comment is 'Description',
		argnames is ['Functor', 'Arity']]).

	NonTerminalFunctor//Arity :-
		(	built_in_non_terminal(NonTerminalFunctor, Arity, Path, File) ->
			open(Path, File)
		;	fail
		).

	built_in_predicate(current_category, 1, '/manuals/refman/builtins/', 'current_category1.html').
	built_in_predicate(current_object, 1, '/manuals/refman/builtins/', 'current_object1.html').
	built_in_predicate(current_protocol, 1, '/manuals/refman/builtins/', 'current_protocol1.html').

	built_in_predicate(category_property, 2, '/manuals/refman/builtins/', 'category_property2.html').
	built_in_predicate(object_property, 2, '/manuals/refman/builtins/', 'object_property2.html').
	built_in_predicate(protocol_property, 2, '/manuals/refman/builtins/', 'protocol_property2.html').

	built_in_predicate(create_category, 4, '/manuals/refman/builtins/', 'create_category4.html').
	built_in_predicate(create_object, 4, '/manuals/refman/builtins/', 'create_object4.html').
	built_in_predicate(create_protocol, 3, '/manuals/refman/builtins/', 'create_protocol3.html').

	built_in_predicate(abolish_category, 1, '/manuals/refman/builtins/', 'abolish_category1.html').
	built_in_predicate(abolish_object, 1, '/manuals/refman/builtins/', 'abolish_object1.html').
	built_in_predicate(abolish_protocol, 1, '/manuals/refman/builtins/', 'abolish_protocol1.html').

	built_in_predicate(extends_object, 2, '/manuals/refman/builtins/', 'extends_object2_3.html').
	built_in_predicate(extends_object, 3, '/manuals/refman/builtins/', 'extends_object2_3.html').
	built_in_predicate(extends_protocol, 2, '/manuals/refman/builtins/', 'extends_protocol2_3.html').
	built_in_predicate(extends_protocol, 3, '/manuals/refman/builtins/', 'extends_protocol2_3.html').
	built_in_predicate(extends_category, 2, '/manuals/refman/builtins/', 'extends_category2_3.html').
	built_in_predicate(extends_category, 3, '/manuals/refman/builtins/', 'extends_category2_3.html').
	built_in_predicate(implements_protocol, 2, '/manuals/refman/builtins/', 'implements_protocol2_3.html').
	built_in_predicate(implements_protocol, 3, '/manuals/refman/builtins/', 'implements_protocol2_3.html').
	built_in_predicate(imports_category, 2, '/manuals/refman/builtins/', 'imports_category2_3.html').
	built_in_predicate(imports_category, 3, '/manuals/refman/builtins/', 'imports_category2_3.html').
	built_in_predicate(instantiates_class, 2, '/manuals/refman/builtins/', 'instantiates_class2_3.html').
	built_in_predicate(instantiates_class, 3, '/manuals/refman/builtins/', 'instantiates_class2_3.html').
	built_in_predicate(specializes_class, 2, '/manuals/refman/builtins/', 'specializes_class2_3.html').
	built_in_predicate(specializes_class, 3, '/manuals/refman/builtins/', 'specializes_class2_3.html').
	built_in_predicate(complements_object, 2, '/manuals/refman/builtins/', 'complements_object2.html').

	built_in_predicate(abolish_events, 5, '/manuals/refman/builtins/', 'abolish_events5.html').
	built_in_predicate(current_event, 5, '/manuals/refman/builtins/', 'current_event5.html').
	built_in_predicate(define_events, 5, '/manuals/refman/builtins/', 'define_events5.html').

	built_in_predicate(threaded, 1, '/manuals/refman/builtins/', 'threaded1.html').
	built_in_predicate(threaded_call, 1, '/manuals/refman/builtins/', 'threaded_call1_2.html').
	built_in_predicate(threaded_call, 2, '/manuals/refman/builtins/', 'threaded_call1_2.html').
	built_in_predicate(threaded_once, 1, '/manuals/refman/builtins/', 'threaded_once1_2.html').
	built_in_predicate(threaded_once, 2, '/manuals/refman/builtins/', 'threaded_once1_2.html').
	built_in_predicate(threaded_ignore, 1, '/manuals/refman/builtins/', 'threaded_ignore1.html').
	built_in_predicate(threaded_exit, 1, '/manuals/refman/builtins/', 'threaded_exit1_2.html').
	built_in_predicate(threaded_exit, 2, '/manuals/refman/builtins/', 'threaded_exit1_2.html').
	built_in_predicate(threaded_peek, 1, '/manuals/refman/builtins/', 'threaded_peek1_2.html').
	built_in_predicate(threaded_peek, 2, '/manuals/refman/builtins/', 'threaded_peek1_2.html').
	built_in_predicate(threaded_wait, 1, '/manuals/refman/builtins/', 'threaded_wait1.html').
	built_in_predicate(threaded_notify, 1, '/manuals/refman/builtins/', 'threaded_notify1.html').

	built_in_predicate(logtalk_compile, 1, '/manuals/refman/builtins/', 'logtalk_compile1.html').
	built_in_predicate(logtalk_compile, 2, '/manuals/refman/builtins/', 'logtalk_compile2.html').
	built_in_predicate(logtalk_load, 1, '/manuals/refman/builtins/', 'logtalk_load1.html').
	built_in_predicate(logtalk_load, 2, '/manuals/refman/builtins/', 'logtalk_load2.html').
	built_in_predicate(logtalk_library_path, 2, '/manuals/refman/builtins/', 'logtalk_library_path2.html').

	built_in_predicate(current_logtalk_flag, 2, '/manuals/refman/builtins/', 'current_logtalk_flag2.html').
	built_in_predicate(set_logtalk_flag, 2, '/manuals/refman/builtins/', 'set_logtalk_flag2.html').

	built_in_method(parameter, 2, '/manuals/refman/methods/', 'parameter2.html').
	built_in_method(self, 1, '/manuals/refman/methods/', 'self1.html').
	built_in_method(sender, 1, '/manuals/refman/methods/', 'sender1.html').
	built_in_method(this, 1, '/manuals/refman/methods/', 'this1.html').

	built_in_method(current_predicate, 1, '/manuals/refman/methods/', 'current_predicate1.html').
	built_in_method(predicate_property, 2, '/manuals/refman/methods/', 'predicate_property2.html').

	built_in_method(abolish, 1, '/manuals/refman/methods/', 'abolish1.html').
	built_in_method(asserta, 1, '/manuals/refman/methods/', 'asserta1.html').
	built_in_method(assertz, 1, '/manuals/refman/methods/', 'assertz1.html').
	built_in_method(clause, 2, '/manuals/refman/methods/', 'clause2.html').
	built_in_method(retract, 1, '/manuals/refman/methods/', 'retract1.html').
	built_in_method(retractall, 1, '/manuals/refman/methods/', 'retractall1.html').

	built_in_method(call, N, '/manuals/refman/methods/', 'callN.html') :-
		N > 0.
	built_in_method(once, 1, '/manuals/refman/methods/', 'once1.html').
	built_in_method((\+), 1, '/manuals/refman/methods/', 'not1.html').

	built_in_method(catch, 3, '/manuals/refman/methods/', 'catch3.html').
	built_in_method(throw, 1, '/manuals/refman/methods/', 'throw1.html').

	built_in_method(bagof, 3, '/manuals/refman/methods/', 'bagof3.html').
	built_in_method(findall, 3, '/manuals/refman/methods/', 'findall3.html').
	built_in_method(forall, 2, '/manuals/refman/methods/', 'forall2.html').
	built_in_method(setof, 3, '/manuals/refman/methods/', 'setof3.html').

	built_in_method(before, 3, '/manuals/refman/methods/', 'before3.html').
	built_in_method(after, 3, '/manuals/refman/methods/', 'after3.html').

	built_in_method(phrase, 2, '/manuals/refman/methods/', 'phrase2.html').
	built_in_method(phrase, 3, '/manuals/refman/methods/', 'phrase3.html').

	built_in_method(expand_term, 2, '/manuals/refman/methods/', 'expand_term2.html').
	built_in_method(term_expansion, 2, '/manuals/refman/methods/', 'term_expansion2.html').
	built_in_method(expand_goal, 2, '/manuals/refman/methods/', 'expand_goal2.html').
	built_in_method(goal_expansion, 2, '/manuals/refman/methods/', 'goal_expansion2.html').

	control((::), 2, '/manuals/refman/control/', 'to_object2.html').
	control((::), 1, '/manuals/refman/control/', 'to_self1.html').
	control((^^), 1, '/manuals/refman/control/', 'to_super1.html').
	control(({}), 1, '/manuals/refman/control/', 'external1.html').
	control((<<), 2, '/manuals/refman/control/', 'context2.html').
	control((:), 1, '/manuals/refman/control/', 'direct1.html').

	built_in_non_terminal(call, 1, '/manuals/refman/methods/', 'call1.html').

	open(Path, File) :-
		(	\+ os::environment_variable('LOGTALKHOME', _) ->
			write('The environment variable LOGTALKHOME must be defined and pointing to your'), nl,
			write('Logtalk installation folder in order for on-line help to be available.'), nl, nl
		;	os::environment_variable('HOMEDRIVE', _) ->
			% assume we're running on Windows
			convert_file_path(Path, ConvertedPath),
			atom_concat('%LOGTALKHOME%', ConvertedPath, FullPath),
			atom_concat('cmd /c start /D"', FullPath, Command0),
			atom_concat(Command0, '" ', Command1),
			atom_concat(Command1, File, Command),
			os::shell(Command)
		;	os::shell('uname -s | grep Darwin 1> /dev/null') ->
			% assume we're running on MacOS X
			atom_concat('open "$LOGTALKHOME', Path, Command0),
			atom_concat(Command0, File, Command1),
			atom_concat(Command1, '"', Command),
			os::shell(Command)
		;	os::shell('uname -s | grep Linux 1> /dev/null') ->
			% assume we're running on Linux
			atom_concat('xdg-open "$LOGTALKHOME', Path, Command0),
			atom_concat(Command0, File, Command1),
			atom_concat(Command1, '"', Command),
			os::shell(Command)
		;	% we couldn't find which operating-system are we running on
			write('Unsupported operating-system.'), nl
		).

	convert_file_path(File, Converted) :-
		atom_chars(File, FileChars),
		reverse_slashes(FileChars, ConvertedChars),
		atom_chars(Converted, ConvertedChars).

	reverse_slashes([], []).
	reverse_slashes([Char| Chars], [ConvertedChar| ConvertedChars]) :-
		(	Char == '/' ->
			ConvertedChar = ('\\')
		;	ConvertedChar = Char
		),
		reverse_slashes(Chars, ConvertedChars).

:- end_object.
