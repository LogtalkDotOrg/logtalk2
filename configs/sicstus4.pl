%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.43.0
%
%  Copyright (c) 1998-2011 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  configuration file for SICStus Prolog 4.0.3 and later versions
%
%  last updated: April 9, 2011
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(file_systems)).
:- use_module(library(system)).
:- use_module(library(system3), [shell/1, shell/2]).
:- use_module(library(terms), [term_variables/2]).


:- multifile(message_hook/3).							% SICStus Prolog hook predicate

message_hook(warning, clauses_not_together(_), _) :-	% SICStus Prolog discontiguous predicate
	'$lgt_inc_load_warnings_counter',					% clauses warning; hack to increment
	fail.												% the Logtalk warnings counter



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

'$lgt_iso_predicate'(term_variables(_, _)).



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


% call_cleanup(+callable, +callable) -- built-in


% setup_call_cleanup(+callable, +callable, +callable)

setup_call_cleanup(Setup, Call, Cleanup) :-
	call(Setup),
	call_cleanup(Call, Cleanup).


% forall(+callable, +callable)

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% call/2-9 -- built-in



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

'$lgt_pl_meta_predicate'(call_cleanup(_, _), call_cleanup(0, 0), predicate).
'$lgt_pl_meta_predicate'(call_residue_vars(_, _), call_residue_vars(0, *), predicate).
'$lgt_pl_meta_predicate'(do(_, _), do(*, 0), predicate) :-
	predicate_property(do(_, _), built_in).
'$lgt_pl_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_pl_meta_predicate'(freeze(_, _), freeze(*, 0), predicate).
'$lgt_pl_meta_predicate'(if(_, _, _), if(0, 0, 0), predicate).
'$lgt_pl_meta_predicate'(on_exception(_, _, _), on_exception(*, 0, 0), predicate).
'$lgt_pl_meta_predicate'(save_predicates(_, _), save_predicates([/], *), predicate).
'$lgt_pl_meta_predicate'(undo(_), undo(0), predicate).
'$lgt_pl_meta_predicate'(when(_, _), when(*, 0), predicate).
% workaround problematic meta-predicate declarations:
'$lgt_pl_meta_predicate'(compile(_), compile(*), predicate).
'$lgt_pl_meta_predicate'(consult(_), consult(*), predicate).
'$lgt_pl_meta_predicate'(ensure_loaded(_), ensure_loaded(*), predicate).
'$lgt_pl_meta_predicate'(format(_, _), format(*, *), predicate).
'$lgt_pl_meta_predicate'(format(_, _, _), format(*, *, *), predicate).
'$lgt_pl_meta_predicate'(load_files(_), load_files(*), predicate).
'$lgt_pl_meta_predicate'(load_files(_, _), load_files(*, *), predicate).
'$lgt_pl_meta_predicate'(load_foreign_resource(_), load_foreign_resource(*), predicate).
'$lgt_pl_meta_predicate'(use_module(_), use_module(*), predicate).
'$lgt_pl_meta_predicate'(use_module(_, _), use_module(*, *), predicate).
'$lgt_pl_meta_predicate'(use_module(_, _, _), use_module(*, *, *), predicate).



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
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, sicstus).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	catch(current_prolog_flag(version_data, sicstus(Major, Minor, Patch, _, _)), _, fail).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((4,0,3))).

'$lgt_prolog_feature'(break_predicate, supported).
'$lgt_prolog_feature'(encoding_directive, source).
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
'$lgt_default_flag'(underscore_variables, singletons).
'$lgt_default_flag'(missing_directives, warning).
% optional features compilation flags:
'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).
'$lgt_default_flag'(context_switching_calls, allow).
% directories compilation flags:
'$lgt_default_flag'(altdirs, on).
'$lgt_default_flag'(tmpdir, TmpDir) :-
	(	environ('COMSPEC', _) ->	% Windows systems define this environment variable...
		TmpDir = 'lgt_tmp/'
	;	TmpDir = '.lgt_tmp/'		% ... but not POSIX systems
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


% '$lgt_expand_path'(+nonvar, -atom)
%
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	current_directory(Directory),
	absolute_file_name(Path, ExpandedPath, [relative_to(Directory)]).


% '$lgt_file_exists'(+atom)
%
% checks if a file exist in the current directory

'$lgt_file_exists'(File) :-
	current_directory(Directory),
	absolute_file_name(File, Path, [relative_to(Directory)]),
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


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	environ(Variable, Value).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory; fails if unknwon 

'$lgt_startup_directory'(Directory) :-
	(	environ('LOGTALK_STARTUP_DIRECTORY', Directory) ->
		true
	;	current_directory(Directory)
	).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknwon

'$lgt_user_directory'(Directory) :-
	environ('LOGTALKUSER', Directory).



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
	stream_position_data(line_count, Position, LineCount),
	Line is LineCount + 1.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd) :-
	stream_position(Stream, PositionBegin),
	stream_position_data(line_count, PositionBegin, LineCountBegin),
	LineBegin is LineCountBegin + 1,
	read_term(Stream, Term, Options),
	stream_position(Stream, PositionEnd),
	stream_position_data(line_count, PositionEnd, LineCountEnd),
	LineEnd is LineCountEnd + 1.



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
'$lgt_ignore_pl_directive'(public(_)) :-	% used to provide info to the cross-referencer
	'$lgt_pp_module_'(_).					% only when we're compiling a module as an object!


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(block(Heads), block(THeads)) :-
	'$lgt_compile_predicate_heads'(Heads, THeads, '?').

'$lgt_rewrite_and_copy_pl_directive'(load_foreign_resource(Resource), initialization(load_foreign_resource(Resource))) :-
	load_foreign_resource(Resource).

'$lgt_rewrite_and_copy_pl_directive'(volatile(PIs), volatile(CPIs)) :-
	'$lgt_compile_predicate_indicators'(PIs, CPIs).


% '$lgt_rewrite_and_recompile_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_recompile_pl_directive'(ensure_loaded(File), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, module),
	% ensure_loaded/1 directive used within a module (sloppy replacement for the use_module/1-2 directives)
	'$lgt_sicstus_list_of_exports'(File, Module, Imports).

'$lgt_rewrite_and_recompile_pl_directive'(module(Module, Exports, _), module(Module, Exports)).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(File, Imports), use_module(Module, Imports)) :-
	'$lgt_sicstus_list_of_exports'(File, Module, _).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(File), use_module(Module, Imports)) :-
	'$lgt_sicstus_list_of_exports'(File, Module, Imports).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(Module, File, Imports), Directive) :-
	(	var(Module) ->
		'$lgt_rewrite_and_recompile_pl_directive'(use_module(File, Imports), Directive)
	;	Directive = use_module(Module, Imports)
	).


'$lgt_sicstus_list_of_exports'(File, Module, Exports) :-
	nonvar(File),
	absolute_file_name(File, Path, [extensions(['.pl', '.pro']), access(read), file_errors(fail)]),
	current_module(Module, Path),	% this only succeeds for already loaded modules
	findall(
		Functor/Arity,
		(predicate_property(Module:Predicate, exported), functor(Predicate, Functor, Arity)),
		Exports),
	!.

'$lgt_sicstus_list_of_exports'(File, Module, Exports) :-
	(	absolute_file_name(File, Path, [extensions(['.pl', '.pro']), access(read), file_errors(fail)])
	;	% we may be compiling Prolog module files as Logtalk objects
		absolute_file_name(File, Path, [extensions(['.lgt']), access(read), file_errors(fail)])
	),
	open(Path, read, In),
	call_cleanup(read(In, ModuleDecl), close(In)),
	ModuleDecl = (:- module(Module, Exports)).



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
%  converts between Prolog stream encoding names and XML encoding names
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  experimental lambda support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term(Term, Copy, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  utility predicates used to construct execution context terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_exec_ctx'(c(This, r(Sender, Self, MetaCallCtx, Stack)), Sender, This, Self, MetaCallCtx, Stack).

'$lgt_exec_ctx_this_rest'(c(This, Ctx), This, Ctx).	% inheritance only requires updating "this"

'$lgt_exec_ctx_this'(c(This, _), This).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  hook predicate for writing compiled entity terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_write_entity_term_hook'(@stream, @callable, @callable)

'$lgt_write_entity_term_hook'(_, _, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
