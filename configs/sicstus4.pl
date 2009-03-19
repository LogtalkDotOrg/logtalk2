%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.35.2
%
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  configuration file for SICStus Prolog 4.0 and later versions
%
%  last updated: March 18, 2009
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(file_systems)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  ISO Prolog Standard predicates that we must define because they are
%  not built-in
%
%  add a clause for lgt_iso_predicate/1 declaring each ISO predicate that
%  we must define; there must be at least one clause for this predicate
%  whose call should fail if we don't define any ISO predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(_) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(':'(_,_), built_in) :-
	!.

'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% call_cleanup(+callable, +callble) -- built-in


% forall(+callable, +callble) -- built-in

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% retractall(+callable) -- built-in


% call/2-9 -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog built-in meta-predicates
%
%  (excluding ISO Prolog Standard meta-predicates)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_meta_predicate'(?callable, ?atom).

'$lgt_pl_meta_predicate'(call_cleanup(::, ::), predicate).
'$lgt_pl_meta_predicate'(call_residue_vars(::, *), predicate).
'$lgt_pl_meta_predicate'(findall(*, ::, *, *), predicate).
'$lgt_pl_meta_predicate'(freeze(*, ::), predicate).
'$lgt_pl_meta_predicate'(if(::, ::, ::), predicate).
'$lgt_pl_meta_predicate'(on_exception(*, ::, ::), predicate).
'$lgt_pl_meta_predicate'(undo(::), predicate).
'$lgt_pl_meta_predicate'(when(*, ::), predicate).



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

'$lgt_prolog_feature'(prolog, sicstus).
'$lgt_prolog_feature'(break_predicate, supported).
'$lgt_prolog_feature'(encoding_directive, source).
'$lgt_prolog_feature'(multifile_directive, supported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
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

'$lgt_default_flag'(startup_message, flags(verbose)).

'$lgt_default_flag'(underscore_variables, singletons).

'$lgt_default_flag'(code_prefix, '').

'$lgt_default_flag'(debug, off).

'$lgt_default_flag'(complements, off).
'$lgt_default_flag'(dynamic_declarations, off).
'$lgt_default_flag'(events, off).

'$lgt_default_flag'(altdirs, off).
'$lgt_default_flag'(tmpdir, 'lgt_tmp/').
'$lgt_default_flag'(xmldir, 'xml_docs/').

'$lgt_default_flag'(context_switching_calls, allow).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  list predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_append'(List1, List2, List3) :-
	append(List1, List2, List3).


'$lgt_member'(Element, List) :-
	member(Element, List).


'$lgt_member_var'(Var, [Head| _]) :-
	Var == Head.
'$lgt_member_var'(Var, [_| Tail]) :-
	'$lgt_member_var'(Var, Tail).


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


% '$lgt_file_exists'(+atom)
%
% checks if a file exist in the current directory

'$lgt_file_exists'(File) :-
	absolute_file_name(File, Path),
	file_exists(Path).


% '$lgt_delete_file'(+atom)
%
% deletes a file in the current directory

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	absolute_file_name(Directory, Path),
	directory_exists(Path).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	current_directory(Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	current_directory(_, Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	absolute_file_name(Directory, Path),
	(	directory_exists(Path) ->
		true
	;	make_directory(Path)
	).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, _, Options) :-
	load_files(File, [compilation_mode(compile)| Options]).


% '$lgt_compare_file_mtimes'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_mtimes'(Result, File1, File2) :-
	file_property(File1, modify_time, Time1),
	file_property(File2, modify_time, Time2),
	compare(Result, Time1, Time2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  sorting predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_keysort'(+list, -list)

'$lgt_keysort'(List, Sorted) :-
	keysort(List, Sorted).


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
	datime(datime(Year, Month, Day, _, _, _)).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	datime(datime(_, _, _, Hours, Mins, Secs)).



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
	statistics(runtime, [Miliseconds| _]),
	Seconds is Miliseconds / 1000.



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
	get_code(Code), char_code(Char, Code),
	(	Code =:= 10 ->
		true
	;	peek_code(10) ->	% hack to workaround the lack of built-in
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
	\+ \+ (
		numbervars(Term, 0, _),
		write_term(Stream, Term, [numbervars(true)])).


'$lgt_pretty_print_vars_quoted'(Stream, Term) :-
	\+ \+ (
		numbervars(Term, 0, _),
		write_term(Stream, Term, [numbervars(true), quoted(true)])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	stream_property(Stream, position(Position)),
	stream_position_data(line_count, Position, Line).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the line
%  where the term starts (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -integer)

'$lgt_read_term'(Stream, Term, Options, -1) :-
	read_term(Stream, Term, Options).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  handling of Prolog-proprietary directives on Logtalk source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_ignore_pl_directive'(@callable)

'$lgt_ignore_pl_directive'(mode(_)).


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(volatile(PIs), volatile(CPIs)) :-
	'$lgt_rewrite_and_copy_pl_directive_pis'(PIs, CPIs).


'$lgt_rewrite_and_copy_pl_directive_pis'(PIs, _) :-
	var(PIs),
	throw(instantiation_error).
'$lgt_rewrite_and_copy_pl_directive_pis'([], []) :-
	!.
'$lgt_rewrite_and_copy_pl_directive_pis'([PI| PIs], [CPI| CPIs]) :-
	!,
	'$lgt_rewrite_and_copy_pl_directive_pis'(PI, CPI),
	'$lgt_rewrite_and_copy_pl_directive_pis'(PIs, CPIs).
'$lgt_rewrite_and_copy_pl_directive_pis'((PI, PIs), (CPI, CPIs)) :-
	!,
	'$lgt_rewrite_and_copy_pl_directive_pis'(PI, CPI),
	'$lgt_rewrite_and_copy_pl_directive_pis'(PIs, CPIs).
'$lgt_rewrite_and_copy_pl_directive_pis'(Functor/Arity, TFunctor/TArity) :-
	'$lgt_pp_entity'(_, _, Prefix, _, _),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity).


% '$lgt_rewrite_and_recompile_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_recompile_pl_directive'(module(Module, Exports, _), module(Module, Exports)).



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('US-ASCII', 'ANSI_X3.4-1968', _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-1', 'ISO-8859-1', _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-2', 'ISO-8859-2', _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-15', 'ISO-8859-15', _).
'$lgt_logtalk_prolog_encoding'('UTF-8', 'UTF-8', _).
'$lgt_logtalk_prolog_encoding'('UCS-2', Encoding, Stream) :-	% UTF-16 subsumes UCS-2
	'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream).
'$lgt_logtalk_prolog_encoding'('UCS-2BE', 'UTF-16BE', _).
'$lgt_logtalk_prolog_encoding'('UCS-2LE', 'UTF-16LE', _).
'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream) :-	% BOM optional but strongly recommended
	(	stream_property(Stream, encoding('UTF-16BE')) ->
		Encoding = 'UTF-16BE'
	;	stream_property(Stream, encoding('UTF-16LE')) ->
		Encoding = 'UTF-16LE'
	).
'$lgt_logtalk_prolog_encoding'('UTF-16BE', 'UTF-16BE', _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-16LE', 'UTF-16LE', _).
'$lgt_logtalk_prolog_encoding'('UTF-32', Encoding, Stream) :-	% BOM mandatory
	(	stream_property(Stream, encoding('UTF-32BE')) ->
		Encoding = 'UTF-32BE'
	;	stream_property(Stream, encoding('UTF-32LE')) ->
		Encoding = 'UTF-32LE'
	).
'$lgt_logtalk_prolog_encoding'('UTF-32BE', 'UTF-32BE', _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-32LE', 'UTF-32LE', _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
