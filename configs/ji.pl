%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.37.6
%
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  configuration file for JIProlog 3.0.2-6 or later versions
%
%  last updated: August 7, 2009
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  ISO Prolog Standard predicates that we must define because they are
%  not built-in
%
%  add a clause for '$lgt_iso_predicate'/1 declaring each ISO predicate that
%  we must define; there must be at least one clause for this predicate
%  whose call should fail if we don't define any ISO predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initialization(Goal) :-
	Goal.


% '$lgt_iso_predicate'(?callable).
%
% table of definition for missing ISO predicates

'$lgt_iso_predicate'(_) :-			% remove this clause if you need 
	fail.							% to define any ISO predicate



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(Pred, Prop) :-
	functor(Pred, Functor, Arity),
	predicate_property(Functor/Arity, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% call_cleanup(+callable, +callable)

call_cleanup(_, _) :-
	throw(not_supported(call_cleanup/2)).


% forall(+callable, +callable) -- built-in


% retractall(+callable) -- built-in


% call/2-5 -- built-in

% call/6-9

call(F, A1, A2, A3, A4, A5) :-
	Call =.. [F, A1, A2, A3, A4, A5],
	Call.

call(F, A1, A2, A3, A4, A5, A6) :-
	Call =.. [F, A1, A2, A3, A4, A5, A6],
	Call.

call(F, A1, A2, A3, A4, A5, A6, A7) :-
	Call =.. [F, A1, A2, A3, A4, A5, A6, A7],
	Call.

call(F, A1, A2, A3, A4, A5, A6, A7, A8) :-
	Call =.. [F, A1, A2, A3, A4, A5, A6, A7, A8],
	Call.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog built-in meta-predicates
%
%  (excluding ISO Prolog Standard meta-predicates)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_meta_predicate'(?callable, ?atom).

'$lgt_pl_meta_predicate'(ignore(::), predicate).
'$lgt_pl_meta_predicate'(not(::), predicate).
'$lgt_pl_meta_predicate'(one(::), predicate).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file extension predicates
%
%  these extensions are used by Logtalk load/compile predicates
%
%  you may want to change the extension for Prolog files to match 
%  the one expected by your Prolog compiler
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_file_extension'(?atom, ?atom)

'$lgt_file_extension'(logtalk, '.lgt').
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(xml, '.xml').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features

'$lgt_prolog_feature'(prolog_dialect, ji).
'$lgt_prolog_feature'(prolog_version, _) :-
	fail.
'$lgt_prolog_feature'(prolog_compatible_version, @>=((3,0,2))).

'$lgt_prolog_feature'(break_predicate, unsupported).
'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(multifile_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
%
%  if your Prolog compiler supports the ISO definition of the 
%  initialization/1 then change the default value below to true
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_flag'(?atom, ?atom)
%
% default values for all flags

'$lgt_default_flag'(xmldocs, on).
'$lgt_default_flag'(xslfile, 'lgtxml.xsl').
'$lgt_default_flag'(xmlspec, dtd).
'$lgt_default_flag'(xmlsref, local).

'$lgt_default_flag'(unknown, warning).
'$lgt_default_flag'(misspelt, warning).
'$lgt_default_flag'(singletons, warning).
'$lgt_default_flag'(lgtredef, warning).
'$lgt_default_flag'(plredef, silent).
'$lgt_default_flag'(portability, silent).

'$lgt_default_flag'(report, on).

'$lgt_default_flag'(smart_compilation, off).
'$lgt_default_flag'(reload, always).

'$lgt_default_flag'(startup_message, flags(compact)).

'$lgt_default_flag'(underscore_variables, singletons).

'$lgt_default_flag'(code_prefix, '$').

'$lgt_default_flag'(debug, off).

'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).

'$lgt_default_flag'(altdirs, off).
'$lgt_default_flag'(tmpdir, 'lgt_tmp/').
'$lgt_default_flag'(xmldir, 'xml_docs/').

'$lgt_default_flag'(context_switching_calls, allow).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  list predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_append'([], List, List).
'$lgt_append'([Head| Tail], List, [Head| Tail2]) :-
	'$lgt_append'(Tail, List, Tail2).


'$lgt_member'(Head, [Head| _]).
'$lgt_member'(Head, [_| Tail]) :-
	'$lgt_member'(Head, Tail).


'$lgt_member_var'(V, [H| _]) :-
	V == H.
'$lgt_member_var'(V, [_| T]) :-
	'$lgt_member_var'(V, T).


'$lgt_is_list'([]) :-
    !.
'$lgt_is_list'([_| Tail]) :-
    '$lgt_is_list'(Tail).


'$lgt_is_proper_list'(List) :-
    List == [], !.
'$lgt_is_proper_list'([_| Tail]) :-
    nonvar(Tail),
    '$lgt_is_proper_list'(Tail).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_expand_path'(+nonvar, -atom)
%
% checks if a file exist in the current directory

'$lgt_expand_path'(_, _) :-
	fail.


% '$lgt_file_exists'(+atom)
%
% checks if a file exist in the current directory

'$lgt_file_exists'(File) :-
	exists_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	exists_directory(Directory).


% '$lgt_delete_file'(+atom)
%
% deletes a file in the current directory

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Directory, Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	chdir(Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	exists_directory(Directory) ->
		true
	;	make_directory(Directory)
	).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, _, _) :-
	consult(File).


% '$lgt_compare_file_mtimes'(?atom, +atom, +atom)
%
% compare file modification times
%
% should fail if file modification times cannot be retrived 
% or if one of the files does not exist

'$lgt_compare_file_mtimes'(Result, File1, File2) :-
	exists_file(File1),
	exists_file(File2),
	file_attributes(File1, _, _, _, _, _, Time1),
	file_attributes(File2, _, _, _, _, _, Time2),
	compare(Result, Time1, Time2).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(_, _) :-
	fail.


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory; fails if unknwon 

'$lgt_startup_directory'(_) :-
	fail.


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknwon

'$lgt_user_directory'(_) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  sorting predicates
%
%  note that sort/2 and keysort/2 are buitl-in predicates in most Prolog 
%  compilers
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_keysort'(+list, -list)

'$lgt_keysort'(_, _) :-
	fail.


% '$lgt_sort'(+list, -list)

'$lgt_sort'(List, Sorted) :-
	sort(List, Sorted).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_current_date'(?Year, ?Month, ?Day)

'$lgt_current_date'(Year, Month, Day) :-
	time(Year, Month, Day, _, _, _, _).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	time(_, _, _, Hours, Mins, Secs, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  timing predicate
%
%  if your Prolog compiler does not provide access to a timing predicate 
%  just write dummy definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_cpu_time'(-Seconds)

'$lgt_cpu_time'(Seconds) :-
	Seconds is cputime.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  comparison predicate
%
%  the usual compare/3 definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% compare(?atom, @term, @term) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  callable predicate
%
%  the usual callable/1 definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% callable(@term) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  read character predicate
%
%  read a single character echoing it and writing a newline after
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_read_single_char'(Char) :-
	get_char(Char),
	(	peek_code(10) ->	% hack to workaround the lack of built-in
		get_code(_)			% support for unbuffered character input
	;	true
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pretty print a term by naming its free variables
%  (avoid instantiating variables in term by using double negation if necessary)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_pretty_print_vars'(Stream, Term) :-
	write(Stream, Term).


'$lgt_pretty_print_vars_quoted'(Stream, Term) :-
	writeq(Stream, Term).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the line
%  where the term starts (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, Options, -1) :-
	read_term(Stream, Term, Options).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  handling of Prolog-proprietary directives on Logtalk source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_ignore_pl_directive'(@callable)

'$lgt_ignore_pl_directive'(_) :-
	fail.


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(_, _) :-
	fail.


% '$lgt_rewrite_and_recompile_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_recompile_pl_directive'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Shortcut to the Logtalk built-in predicate logtalk_load/1
%
%  defined in the config files in order to be able to comment it out in case
%  of conflict with some Prolog native feature; it implies conformance with
%  the ISO Prolog standard regarding the definition of the {}/1 syntax
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{File, Files} :-
	!,
	logtalk_load(File),
	{Files}.
{File} :-
	logtalk_load(File).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'(_, _, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
