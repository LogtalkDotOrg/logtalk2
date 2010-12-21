%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.42.1
%
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  configuration file for ECLiPSe 6.0#141 and later versions
%
%  last updated: December 21, 2010
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- pragma(system).
:- pragma(nodebug).

:- use_module(library(numbervars)).
:- use_module(library(multifile)).


:- set_event_handler(134, '$lgt_eclipse_discontiguous_predicate_handler'/2).

'$lgt_eclipse_discontiguous_predicate_handler'(Err, Goal) :-
	'$lgt_inc_load_warnings_counter',
	error(default(Err), Goal).



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


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(_) :-
	fail.


:- use_module(library(iso)).
:- import abolish/1 from iso.



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
'$lgt_predicate_property'(numbervars(_,_,_), built_in) :-
	!.

'$lgt_predicate_property'(Predicate, built_in) :-
	functor(Predicate, Functor, Arity),
	current_built_in(Functor/Arity).

'$lgt_predicate_property'(Predicate, dynamic) :-
	functor(Predicate, Functor, Arity),
	current_predicate(Functor/Arity),
	is_dynamic(Functor/Arity).

'$lgt_predicate_property'(Predicate, static) :-
	functor(Predicate, Functor, Arity),
	current_built_in(Functor/Arity).

'$lgt_predicate_property'(Predicate, static) :-
	functor(Predicate, Functor, Arity),
	current_predicate(Functor/Arity),
	\+ is_dynamic(Functor/Arity).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% call_cleanup(+callable, +callable)

call_cleanup(_, _) :-
	throw(not_supported(call_cleanup/2)).


% setup_call_cleanup(+callable, +callable, +callable)

setup_call_cleanup(_, _, _) :-
	throw(not_supported(setup_call_cleanup/3)).


% forall(+callable, +callable)

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% call/2-9

:- export call/2.	% avoid conflict with obsolete built-in predicate
call(F, A) :-
	Call =.. [F, A],
	call(Call).

call(F, A1, A2) :-
	Call =.. [F, A1, A2],
	call(Call).

call(F, A1, A2, A3) :-
	Call =.. [F, A1, A2, A3],
	call(Call).

call(F, A1, A2, A3, A4) :-
	Call =.. [F, A1, A2, A3, A4],
	call(Call).

call(F, A1, A2, A3, A4, A5) :-
	Call =.. [F, A1, A2, A3, A4, A5],
	call(Call).

call(F, A1, A2, A3, A4, A5, A6) :-
	Call =.. [F, A1, A2, A3, A4, A5, A6],
	call(Call).

call(F, A1, A2, A3, A4, A5, A6, A7) :-
	Call =.. [F, A1, A2, A3, A4, A5, A6, A7],
	call(Call).

call(F, A1, A2, A3, A4, A5, A6, A7, A8) :-
	Call =.. [F, A1, A2, A3, A4, A5, A6, A7, A8],
	call(Call).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog non-standard built-in meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_meta_predicate'(+callable, ?callable, ?atom)
%
% table of meta-predicate patterns for proprietary built-in predicates;
% the third argument, which must be either "predicate" or "control_construct",
% is used to guide the compilation of these meta-predicates in debug mode

'$lgt_pl_meta_predicate'(*->(_, _), *->(0, 0), control_construct).
'$lgt_pl_meta_predicate'(~(_), ~(0), control_construct).
'$lgt_pl_meta_predicate'(block(_, _, _), block(0, *, 0), predicate).
'$lgt_pl_meta_predicate'(call_priority(_, _), call_priority(0, *), predicate).
'$lgt_pl_meta_predicate'(coverof(_, _, _), coverof(*, 0, *), predicate).
'$lgt_pl_meta_predicate'(do(_, _), do(*, 0), predicate).
'$lgt_pl_meta_predicate'(event_create(_, _, _), event_create(0, *, *), predicate).
'$lgt_pl_meta_predicate'(fail_if(_), fail_if(0), predicate).
'$lgt_pl_meta_predicate'(make_suspension(_, _, _), make_suspension(0, *, *), predicate).
'$lgt_pl_meta_predicate'(mutex(_, _), mutex(*, 0), predicate).
'$lgt_pl_meta_predicate'(not(_), not(0), predicate).
'$lgt_pl_meta_predicate'(subcall(_, _), subcall(0, *), predicate).
'$lgt_pl_meta_predicate'(suspend(_, _, _), suspend(0, *, *), predicate).
'$lgt_pl_meta_predicate'(suspend(_, _, _, _), suspend(0, *, *, *), predicate).



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

'$lgt_prolog_feature'(prolog_dialect, eclipse).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Build)) :-
	get_flag(version_as_list, [Major, Minor, Build]).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((6,0,141))).

'$lgt_prolog_feature'(break_predicate, supported).
'$lgt_prolog_feature'(encoding_directive, unsupported).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, supported).
'$lgt_prolog_feature'(coinduction, supported).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_flag'(?atom, ?atom)
%
% default values for all flags

% documenting compilation flags:
'$lgt_default_flag'(xmldocs, on).
'$lgt_default_flag'(xslfile, 'lgtxml.xsl').
'$lgt_default_flag'(xmlspec, dtd).
'$lgt_default_flag'(xmlsref, local).
% lint compilation flags:
'$lgt_default_flag'(unknown, warning).
'$lgt_default_flag'(misspelt, warning).
'$lgt_default_flag'(singletons, warning).
'$lgt_default_flag'(lgtredef, warning).
'$lgt_default_flag'(plredef, silent).
'$lgt_default_flag'(portability, silent).
'$lgt_default_flag'(underscore_variables, dont_care).
'$lgt_default_flag'(missing_directives, warning).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% directories compilation flags:
'$lgt_default_flag'(altdirs, on).
'$lgt_default_flag'(tmpdir, TmpDir) :-
	(	get_flag(hostarch, HostArch), atom_string(i386_nt, HostArch) ->
		TmpDir = 'lgt_tmp/'
	;	TmpDir = '.lgt_tmp/'
	).
'$lgt_default_flag'(xmldir, 'xml_docs/').
% other compilation flags:
'$lgt_default_flag'(report, on).
'$lgt_default_flag'(clean, on).
'$lgt_default_flag'(smart_compilation, off).
'$lgt_default_flag'(reload, always).
'$lgt_default_flag'(startup_message, flags(compact)).
'$lgt_default_flag'(code_prefix, '$').
'$lgt_default_flag'(debug, off).



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
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	atom_string(Path, PathString),
	canonical_path_name(PathString, ExpandedPathString),
	atom_string(ExpandedPath, ExpandedPathString).


% '$lgt_file_exists'(+atom)
%
% checks if a directory exists

'$lgt_file_exists'(File) :-
	exists(File).


% '$lgt_delete_file'(+atom)
%
% deletes a file in the current directory

'$lgt_delete_file'(File) :-
	delete(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	atom_string(Directory, DirectoryString),
	canonical_path_name(DirectoryString, Path),
	exists(Path).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	getcwd(DirectoryString),
	atom_string(Directory, DirectoryString).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	cd(Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	'$lgt_directory_exists'(Directory) ->
		true
	;	mkdir(Directory)
	).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, _, _) :-
	compile(File, [debug:off]).


% '$lgt_compare_file_mtimes'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_mtimes'(Result, File1, File2) :-
	get_file_info(File1, mtime, Time1),
	get_file_info(File2, mtime, Time2),
	compare(Result, Time1, Time2).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	getenv(Variable, ValueString),
	atom_string(Value, ValueString).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory; fails if unknwon 

'$lgt_startup_directory'(Directory) :-
	(	getenv('LOGTALK_STARTUP_DIRECTORY', DirectoryString) ->
		true
	;	getcwd(DirectoryString)
	),
	atom_string(Directory, DirectoryString).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknwon

'$lgt_user_directory'(Directory) :-
	getenv('LOGTALKUSER', DirectoryString),
	atom_string(Directory, DirectoryString).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(calendar)).


% '$lgt_current_date'(?Year, ?Month, ?Day)

'$lgt_current_date'(Year, Month, Day) :-
	mjd_now(MJD),
	mjd_to_date(MJD, Day/Month/Year).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	mjd_now(MJD),
	mjd_to_time(MJD, Hours:Mins:FloatSecs),
	Secs is integer(floor(FloatSecs)).



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
	cputime(Seconds).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  read character predicate
%
%  read a single character echoing it and writing a newline after
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_read_single_char'(Char) :-
	flush(user), tyi(Code), put(Code), nl, char_code(Char, Code).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pretty print a term by naming its free variables
%  (avoid instantiating variables in term by using double negation if necessary)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_pretty_print_vars'(Stream, Term) :-
	write_term(Stream, Term, [numbervars(true)]).


'$lgt_pretty_print_vars_quoted'(Stream, Term) :-
	write_term(Stream, Term, [numbervars(true), quoted(true)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(Stream, Line) :-
	get_stream_info(Stream, line, Last),
	Line is Last + 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd) :-
	get_stream_info(Stream, line, LineBegin),
	(	read_term(Stream, Term, Options) ->
		get_stream_info(Stream, line, LineEnd)
	;	throw(syntax_error)
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  handling of Prolog-proprietary directives on Logtalk source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_meta_directive'(@callable)

'$lgt_pl_meta_directive'(_) :-
	fail.


% '$lgt_ignore_pl_directive'(@callable)

'$lgt_ignore_pl_directive'(mode(_)).
'$lgt_ignore_pl_directive'(comment(_, _)).


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(demon(PIs), demon(CPIs)) :-
	'$lgt_compile_predicate_indicators'(PIs, CPIs).

'$lgt_rewrite_and_copy_pl_directive'(export(chtab(Char, Class)), export(chtab(Char, Class))).
'$lgt_rewrite_and_copy_pl_directive'(export(domain(Domain)), export(domain(Domain))).
'$lgt_rewrite_and_copy_pl_directive'(export(struct(Struct)), export(struct(Struct))).
'$lgt_rewrite_and_copy_pl_directive'(export(syntax_option(SyntaxOption)), export(syntax_option(SyntaxOption))).

'$lgt_rewrite_and_copy_pl_directive'(inline(PI1, PI2), inline(CPI1, CPI2)) :-
	'$lgt_compile_predicate_indicators'(PI1, CPI1),
	'$lgt_compile_predicate_indicators'(PI2, CPI2).

'$lgt_rewrite_and_copy_pl_directive'(pragma(Pragma), pragma(Pragma)).

'$lgt_rewrite_and_copy_pl_directive'(set_error_handler(Event, Functor/Arity), set_error_handler(Event, CFunctor/CArity)) :-
	'$lgt_compile_predicate_indicators'(Functor/Arity, CFunctor/CArity).
'$lgt_rewrite_and_copy_pl_directive'(set_event_handler(Event, defers(Functor/Arity)), set_event_handler(Event, defers(CFunctor/CArity))) :-
	'$lgt_compile_predicate_indicators'(Functor/Arity, CFunctor/CArity).
'$lgt_rewrite_and_copy_pl_directive'(set_event_handler(Event, Functor/Arity), set_event_handler(Event, CFunctor/CArity)) :-
	'$lgt_compile_predicate_indicators'(Functor/Arity, CFunctor/CArity).

'$lgt_rewrite_and_copy_pl_directive'(set_flag(PI, Flag, Value), set_flag(CPI, Flag, Value)) :-
	'$lgt_compile_predicate_indicators'(PI, CPI).

'$lgt_rewrite_and_copy_pl_directive'(skipped(PIs), skipped(CPIs)) :-
	'$lgt_compile_predicate_indicators'(PIs, CPIs).


% '$lgt_rewrite_and_recompile_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_recompile_pl_directive'(import(from(Conjunction, Module)), use_module(Module, Imports)) :-
	'$lgt_flatten_list'([Conjunction], Imports).

'$lgt_rewrite_and_recompile_pl_directive'(local(Functor/Arity), private(Functor/Arity)).
'$lgt_rewrite_and_recompile_pl_directive'(local(op(Priority, Spec, Operators)), op(Priority, Spec, Operators)).

'$lgt_rewrite_and_recompile_pl_directive'(lib(Library), use_module(Module, Exports)) :-
	'$lgt_eclipse_list_of_exports'(library(Library), Module, Exports).

'$lgt_rewrite_and_recompile_pl_directive'(reexport(File), reexport(Module, Exports)) :-
	atom(File),
	'$lgt_eclipse_list_of_exports'(File, Module, Exports).

'$lgt_rewrite_and_recompile_pl_directive'(reexport(from(Conjunction, Module)), reexport(Module, Exports)) :-
	'$lgt_flatten_list'([Conjunction], Exports).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(File), use_module(Module, Imports)) :-
	'$lgt_eclipse_list_of_exports'(File, Module, Imports).


'$lgt_eclipse_list_of_exports'(File, Module, Exports) :-
	(	get_flag(prolog_suffix, Suffixes), existing_file(File, Suffixes, [], ExtRel) ->
		true
	;	% we may be compiling Prolog module files as Logtalk objects
		existing_file(File, [`.lgt`], [], ExtRel) ->
		true
	;	ExtRel = File
	),
	canonical_path_name(ExtRel, Path),
	open(Path, read, In),
	catch(read(In, ModuleDecl), _, (close(In), fail)),
	(	ModuleDecl = (:- module(Module, Interface)) ->
		true
	;	ModuleDecl = (:- module(Module)) ->
		(	current_module(Module) ->
			true
		;	ensure_loaded(File)
		),
		get_module_info(Module, interface, Interface)
	),
	'$lgt_eclipse_filter_exports'(Interface, Exports),
	!.


'$lgt_eclipse_filter_exports'([], []).

'$lgt_eclipse_filter_exports'([Functor/Arity| Interface], [Functor/Arity| Exports]) :-
	'$lgt_eclipse_filter_exports'(Interface, Exports).

'$lgt_eclipse_filter_exports'([op(Priority, Spec, Operators)| Interface], [op(Priority, Spec, Operators)| Exports]) :-
	'$lgt_eclipse_filter_exports'(Interface, Exports).

'$lgt_eclipse_filter_exports'([export(Functor/Arity)| Interface], [Functor/Arity| Exports]) :-
	'$lgt_eclipse_filter_exports'(Interface, Exports).

'$lgt_eclipse_filter_exports'([export(op(Priority, Spec, Operators))| Interface], [op(Priority, Spec, Operators)| Exports]) :-
	'$lgt_eclipse_filter_exports'(Interface, Exports).



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% experimental lambda support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term(Term, Copy, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
