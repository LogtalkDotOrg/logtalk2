%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.43.1
%
%  Copyright (c) 1998-2011 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  configuration file for SWI Prolog 5.8.0 and later versions
%
%  last updated: July 14, 2011
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%:- set_prolog_flag(iso, true).	% commented due to all the SWI-Prolog libraries that don't compile/work in "iso" mode!
:- set_prolog_flag(generate_debug_info, false).
:- set_prolog_flag(optimise, true).
:- system_module.


:- multifile(message_hook/3).				% SWI-Prolog hook predicate
:- dynamic(message_hook/3).

message_hook(discontiguous(_), _, _) :-		% SWI-Prolog discontiguous predicate
	'$lgt_inc_load_warnings_counter',		% clauses warning; hack to increment
	fail.									% the Logtalk warnings counter



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
	current_prolog_flag(autoload, Value),
	setup_call_cleanup(
		set_prolog_flag(autoload, false),
		predicate_property(Pred, Prop),
		set_prolog_flag(autoload, Value)).

'$lgt_predicate_property'(thread_sleep(_), built_in).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% call_cleanup(+callable, +callable) -- built-in


% setup_call_cleanup(+callable, +callable, +callable) -- built-in


% forall(+callable, +callable) -- built-in


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

'$lgt_pl_meta_predicate'(*->(_, _), *->(0, 0), control_construct).
:- if(predicate_property(block(_, _, _), built_in)).
	'$lgt_pl_meta_predicate'(block(_, _, _), block(*, 0, *), predicate).
:- endif.
'$lgt_pl_meta_predicate'(call_cleanup(_, _), call_cleanup(0, 0), predicate).
'$lgt_pl_meta_predicate'(call_cleanup(_, _, _), call_cleanup(0, *, 0), predicate).
'$lgt_pl_meta_predicate'(call_with_depth_limit(_, _, _), call_with_depth_limit(0, *, *), predicate).
'$lgt_pl_meta_predicate'(compile_predicates(_), compile_predicates([/]), predicate).
'$lgt_pl_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_pl_meta_predicate'(freeze(_, _), freeze(*, 0), predicate).
'$lgt_pl_meta_predicate'(not(_), not(0), predicate).
'$lgt_pl_meta_predicate'(notrace(_), notrace(0), predicate).
'$lgt_pl_meta_predicate'(on_signal(_, _, _), on_signal(*, *, 0), predicate).
'$lgt_pl_meta_predicate'(setup_call_cleanup(_, _, _), setup_call_cleanup(0, 0, 0), predicate).
'$lgt_pl_meta_predicate'(setup_call_catcher_cleanup(_, _, _, _), setup_call_catcher_cleanup(0, 0, *, 0), predicate).
'$lgt_pl_meta_predicate'(thread_initialization(_), thread_initialization(0), predicate).
'$lgt_pl_meta_predicate'(thread_at_exit(_), thread_at_exit(0), predicate).
'$lgt_pl_meta_predicate'(thread_create(_, _, _), thread_create(0, *, *), predicate).
'$lgt_pl_meta_predicate'(thread_signal(_, _), thread_signal(*, 0), predicate).
'$lgt_pl_meta_predicate'(trace(_), trace(0), predicate).
'$lgt_pl_meta_predicate'(trace(_, _), trace(0, *), predicate).
:- if(predicate_property(win_insert_menu_item(_, _, _, _), built_in)).
	'$lgt_pl_meta_predicate'(win_insert_menu_item(_, _, _, _), win_insert_menu_item(*, *, *, 0), predicate).
:- endif.
'$lgt_pl_meta_predicate'(with_mutex(_, _), with_mutex(*, 0), predicate).
'$lgt_pl_meta_predicate'(with_output_to(_, _), with_output_to(*, 0), predicate).
% workaround problematic meta-predicate declarations:
'$lgt_pl_meta_predicate'(consult(_), consult(*), predicate).
'$lgt_pl_meta_predicate'(ensure_loaded(_), ensure_loaded(*), predicate).
'$lgt_pl_meta_predicate'(format(_, _), format(*, *), predicate).
'$lgt_pl_meta_predicate'(format(_, _, _), format(*, *, *), predicate).
'$lgt_pl_meta_predicate'(load_files(_, _), load_files(*, *), predicate).
'$lgt_pl_meta_predicate'(use_module(_), use_module(*), predicate).
'$lgt_pl_meta_predicate'(use_module(_, _), use_module(*, *), predicate).



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
'$lgt_file_extension'(tmp, '.qlf').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  back-end Prolog features
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_prolog_feature'(?atom, ?atom)
%
% back-end Prolog compiler supported features (that are compatible with Logtalk)

'$lgt_prolog_feature'(prolog_dialect, swi).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	current_prolog_flag(version_data, swi(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, @>=((5,8,0))).

'$lgt_prolog_feature'(break_predicate, supported).
'$lgt_prolog_feature'(encoding_directive, full).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, Threads) :-
	(	current_prolog_flag(threads, true) ->
		Threads = supported
	;	Threads = unsupported
	).
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
	(	current_prolog_flag(unix, true) ->
		TmpDir = '.lgt_tmp/'
	;	TmpDir = 'lgt_tmp/'
	).
'$lgt_default_flag'(xmldir, 'xml_docs/').
% other compilation flags:
'$lgt_default_flag'(report, on).
'$lgt_default_flag'(clean, on).
'$lgt_default_flag'(smart_compilation, off).
'$lgt_default_flag'(reload, always).
'$lgt_default_flag'(startup_message, flags(compact)).
'$lgt_default_flag'(code_prefix, '$').
'$lgt_default_flag'(optimize, on).
:- if((current_prolog_flag(version_data, swi(Major,Minor,Patch,_)), ((Major,Minor) == (5,10) -> Patch >= 5; (Major,Minor,Patch) @>= (5,11,20)))).
	'$lgt_default_flag'(source_data, on).
:- else.
	'$lgt_default_flag'(source_data, off).
:- endif.
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


'$lgt_is_list_or_partial_list'(Var) :-
    var(Var),
	!.
'$lgt_is_list_or_partial_list'([]).
'$lgt_is_list_or_partial_list'([_| Tail]) :-
    '$lgt_is_list_or_partial_list'(Tail).


'$lgt_is_list'(-) :-
	!,
	fail.
'$lgt_is_list'([]).
'$lgt_is_list'([_| Tail]) :-
    '$lgt_is_list'(Tail).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_expand_path'(+nonvar, -atom)
%
% expands a file path to a full path

'$lgt_expand_path'(Path, ExpandedPath) :-
	working_directory(Current, Current),
	(	absolute_file_name(Path, [expand(true), relative_to(Current), file_errors(fail)], ExpandedPath) ->
		true
	;	absolute_file_name(Path, [expand(true), relative_to(Current), file_type(directory), file_errors(fail)], ExpandedPath)
	).


% '$lgt_file_exists'(+atom)
%
% checks if a file exist in the current directory

'$lgt_file_exists'(File) :-
	exists_file(File).


% '$lgt_delete_file'(+atom)
%
% deletes a file in the current directory

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	expand_file_name(Directory, [Path]),
	exists_directory(Path).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Directory, Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	prolog_to_os_filename(Directory, Path),		% fix possible mix of forward and backward slashes
	expand_file_name(Path, [Expanded]),			% expand environment variables
	prolog_to_os_filename(Fixed, Expanded),		% convert to SWI-Prolog notation for paths
	working_directory(_, Fixed).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	prolog_to_os_filename(Directory, Path),		% fix possible mix of forward and backward slashes
	expand_file_name(Path, [Expanded]),			% expand environment variables
	prolog_to_os_filename(Fixed, Expanded),		% convert to SWI-Prolog notation for paths
	(	exists_directory(Fixed) ->
		true
	;	make_directory(Fixed)
	).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, Source, Options) :-
	load_files(File, [derived_from(Source)| Options]).


% '$lgt_compare_file_mtimes'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_mtimes'(Result, File1, File2) :-
	time_file(File1, Time1),
	time_file(File2, Time2),
	compare(Result, Time1, Time2).


% '$lgt_environment_variable'(?atom, ?atom)
%
% access to operating-system environment variables

'$lgt_environment_variable'(Variable, Value) :-
	getenv(Variable, Value).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory 

'$lgt_startup_directory'(Directory) :-
	(	getenv('LOGTALK_STARTUP_DIRECTORY', Path) ->
		prolog_to_os_filename(Directory, Path)
	;	working_directory(Directory, Directory)
	).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	getenv('LOGTALKUSER', Path),
	prolog_to_os_filename(Directory, Path).



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
	get_time(Time),
	convert_time(Time, Year, Month, Day, _, _, _, _).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	get_time(Time),
	convert_time(Time, _, _, _, Hours, Mins, Secs, _).



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
%  read character predicate
%
%  read a single character echoing it and writing a newline after
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_read_single_char'(Char) :-
	get_single_char(Code), put_code(Code), nl, char_code(Char, Code).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pretty print a term by naming its free variables
%  (avoid instantiating variables in term by using double negation if necessary)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_pretty_print_vars'(Stream, Term) :-
	\+ \+ (
		numbervars(Term, 0, _, [singletons(true)]),
		write_term(Stream, Term, [numbervars(true)])).


'$lgt_pretty_print_vars_quoted'(Stream, Term) :-
	\+ \+ (
		numbervars(Term, 0, _, [singletons(true)]),
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
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

:- if(current_op(_, _, (public))).
	% workaround SWI-Prolog 5.11.9 operator clash
	'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd) :-
		(	current_op(Priority, Specifier, (public)),
			\+ '$lgt_pp_module_'(_) ->
			setup_call_cleanup(
				op(0, Specifier, (public)),
				read_term(Stream, Term, [term_position(PositionBegin)| Options]),
				op(Priority, Specifier, (public))
			)
		;	read_term(Stream, Term, [term_position(PositionBegin)| Options])
		),
		stream_position_data(line_count, PositionBegin, LineBegin),
		stream_property(Stream, position(PositionEnd)),
		stream_position_data(line_count, PositionEnd, LineEnd).
:- else.
	'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd) :-
		read_term(Stream, Term, [term_position(PositionBegin)| Options]),
		stream_position_data(line_count, PositionBegin, LineBegin),
		stream_property(Stream, position(PositionEnd)),
		stream_position_data(line_count, PositionEnd, LineEnd).
:- endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  handling of Prolog-proprietary directives on Logtalk source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_meta_directive'(@callable)

'$lgt_pl_meta_directive'(at_halt(0)).
'$lgt_pl_meta_directive'(initialization(0, *)).
'$lgt_pl_meta_directive'(thread_initialization(0)).


% '$lgt_ignore_pl_directive'(@callable)

'$lgt_ignore_pl_directive'(public(_)) :-	% used to provide info to the cross-referencer
	'$lgt_pp_module_'(_).					% only when we're compiling a module as an object!
'$lgt_ignore_pl_directive'(style_check(Option)) :-
	style_check(Option).


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(arithmetic_function(Functor/Arity), arithmetic_function(Functor/Arity)) :-
	'$lgt_compile_predicate_indicators'(Functor/Arity, TFunctor/TArity),
	functor(Term, Functor, TArity),
	Term =.. [_| Args],
	TArity2 is TArity + 1,
	functor(TTerm, TFunctor, TArity2),
	TTerm =.. [_| TArgs],
	'$lgt_swi_unify_head_thead_args'(Args, TArgs),
	'$lgt_compile_aux_clauses'([({Term} :- {TTerm})]).
'$lgt_rewrite_and_copy_pl_directive'(create_prolog_flag(Key, Value, Options), create_prolog_flag(Key, Value, Options)).
'$lgt_rewrite_and_copy_pl_directive'(expects_dialect(Dialect), expects_dialect(Dialect)) :-
	expects_dialect(Dialect).
'$lgt_rewrite_and_copy_pl_directive'(format_predicate(Char, Head), format_predicate(Char, THead)) :-
	'$lgt_compile_predicate_heads'(Head, THead).
'$lgt_rewrite_and_copy_pl_directive'(license(License), license(License)).
'$lgt_rewrite_and_copy_pl_directive'(set_prolog_flag(generate_debug_info, false), set_prolog_flag(generate_debug_info, false)).
'$lgt_rewrite_and_copy_pl_directive'(thread_local(PIs), thread_local(CPIs)) :-
	'$lgt_compile_predicate_indicators'(PIs, CPIs).
'$lgt_rewrite_and_copy_pl_directive'(index(Head), index(THead)) :-
	'$lgt_compile_predicate_heads'(Head, THead, 0).
'$lgt_rewrite_and_copy_pl_directive'(hash(Head), hash(THead)) :-
	'$lgt_compile_predicate_heads'(Head, THead).
'$lgt_rewrite_and_copy_pl_directive'(noprofile(PIs), noprofile(CPIs)) :-
	'$lgt_compile_predicate_indicators'(PIs, CPIs).
'$lgt_rewrite_and_copy_pl_directive'(use_foreign_library(File), use_foreign_library(File)) :-
	load_foreign_library(File).
'$lgt_rewrite_and_copy_pl_directive'(use_foreign_library(File, Entry), use_foreign_library(File, Entry)) :-
	load_foreign_library(File, Entry).
'$lgt_rewrite_and_copy_pl_directive'(volatile(PIs), volatile(CPIs)) :-
	'$lgt_compile_predicate_indicators'(PIs, CPIs).


% '$lgt_rewrite_and_recompile_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_recompile_pl_directive'(encoding(Encoding1), encoding(Encoding2)) :-
	nonvar(Encoding1),
	'$lgt_rewrite_and_recompile_pl_encoding_directive'(Encoding1, Encoding2).

'$lgt_rewrite_and_recompile_pl_directive'(ensure_loaded(File), use_module(Module, Imports)) :-
	logtalk_load_context(entity_type, module),
	% ensure_loaded/1 directive used within a module (sloppy replacement for the use_module/1-2 directives)
	'$lgt_swi_list_of_exports'(File, Module, Imports).

'$lgt_rewrite_and_recompile_pl_directive'(reexport(File), reexport(Module, Exports)) :-
	'$lgt_swi_list_of_exports'(File, Module, Exports).

'$lgt_rewrite_and_recompile_pl_directive'(reexport(File, Exports), reexport(Module, Exports)) :-
	'$lgt_swi_list_of_exports'(File, Module, _).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(File, Imports), use_module(Module, Imports)) :-
	'$lgt_swi_list_of_exports'(File, Module, _).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(File), use_module(Module, Imports)) :-
	'$lgt_swi_list_of_exports'(File, Module, Imports).


'$lgt_swi_unify_head_thead_args'([], [_]).
'$lgt_swi_unify_head_thead_args'([Arg| Args], [Arg| ExtArgs]) :-
	'$lgt_swi_unify_head_thead_args'(Args, ExtArgs).


'$lgt_swi_list_of_exports'(File, Module, Exports) :-
	absolute_file_name(File, Path, [file_type(prolog), access(read), file_errors(fail)]),
	module_property(Module, file(Path)),	% only succeeds for loaded modules
	module_property(Module, exports(Exports)),
	!.
'$lgt_swi_list_of_exports'(File, Module, Exports) :-
	(	absolute_file_name(File, Path, [file_type(prolog), access(read), file_errors(fail)])
	;	% we may be compiling Prolog module files as Logtalk objects
		absolute_file_name(File, Path, [extensions(['.lgt']), access(read), file_errors(fail)])
	),
	open(Path, read, In),
	(	peek_char(In, #) ->					% deal with #! script; if not present
		skip(In, 10)						% assume that the module declaration
	;	true								% is the first directive on the file
	),
	setup_call_cleanup(true, read(In, ModuleDecl), close(In)),
	ModuleDecl = (:- module(Module, Exports)),
	(	var(Module) ->
		file_base_name(Path, Base),
		file_name_extension(Module, _, Base)
	;	true
	).


'$lgt_rewrite_and_recompile_pl_encoding_directive'(ascii, 'US-ASCII').
'$lgt_rewrite_and_recompile_pl_encoding_directive'(iso_latin_1, 'ISO-8859-1').
'$lgt_rewrite_and_recompile_pl_encoding_directive'(utf8, 'UTF-8').
'$lgt_rewrite_and_recompile_pl_encoding_directive'(unicode_be, 'UCS-2BE').
'$lgt_rewrite_and_recompile_pl_encoding_directive'(unicode_le, 'UCS-2LE').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  multi-threading predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% thread_property(+atom, ?nonvar) -- built-in


% thread_self(?atom) -- built-in


% thread_create(@callable, -thread_id, +list) -- built-in


% thread_join(+atom, -nonvar) -- built-in


% thread_detach(+atom) -- built-in


% thread_exit(@term) -- built-in


% thread_send_message(+atom, @callable) -- built-in


% thread_peek_message(+atom, ?callable) -- built-in


% thread_get_message(+atom, ?callable) -- built-in


% thread_get_message(?callable) -- built-in


% thread_sleep(+number)

thread_sleep(Time) :-
	sleep(Time).



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

'$lgt_logtalk_prolog_encoding'('US-ASCII', ascii, _).
'$lgt_logtalk_prolog_encoding'('ISO-8859-1', iso_latin_1, _).
'$lgt_logtalk_prolog_encoding'('UTF-8', utf8, _).
'$lgt_logtalk_prolog_encoding'('UCS-2', Encoding, Stream) :-	% BOM mandatory
	(	stream_property(Stream, encoding(unicode_be)) ->
		Encoding = unicode_be
	;	stream_property(Stream, encoding(unicode_le)) ->
		Encoding = unicode_le
	).
'$lgt_logtalk_prolog_encoding'('UCS-2BE', unicode_be, _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UCS-2LE', unicode_le, _).
'$lgt_logtalk_prolog_encoding'('UTF-16', Encoding, Stream) :-	% BOM optional but strongly recommended
	(	stream_property(Stream, encoding(unicode_be)) ->		% not true of course but usually we can get away with it
		Encoding = unicode_be
	;	stream_property(Stream, encoding(unicode_le)) ->
		Encoding = unicode_le
	).
'$lgt_logtalk_prolog_encoding'('UTF-16BE', unicode_be, _).		% BOM forbidden
'$lgt_logtalk_prolog_encoding'('UTF-16LE', unicode_le, _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  experimental lambda support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term_nat(Term, Copy).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  hack to allow calling the Prolog built-in predicates phrase/2-3 with a
%  Object::GRBody as the first argument
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic(user:goal_expansion/2).
:- multifile(user:goal_expansion/2).

user:goal_expansion(phrase(Rule, Input, Rest), '$lgt_phrase'(Rule, Input, Rest, ExCtx)) :-
	nonvar(Rule),
	functor(Rule, '::', 2),
	!,
	'$lgt_exec_ctx'(ExCtx, user, user, user, [], []).
user:goal_expansion(phrase(Rule, Input), '$lgt_phrase'(Rule, Input, ExCtx)) :-
	nonvar(Rule),
	functor(Rule, '::', 2),
	!,
	'$lgt_exec_ctx'(ExCtx, user, user, user, [], []).



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
%  hooks predicates for writing and assert compiled entity terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_write_term_and_source_location'(@stream, @callable, +atom, @callable)

'$lgt_write_term_and_source_location'(Stream, '$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags), _, Path+Source+Line) :-
	!,
	'$lgt_swi_write_hide_directive'(Stream, Dcl/4),
	'$lgt_swi_write_hide_directive'(Stream, Dcl/6),
	'$lgt_swi_write_hide_directive'(Stream, Def/3),
	'$lgt_swi_write_hide_directive'(Stream, Def/4),
	'$lgt_swi_write_hide_directive'(Stream, Super/4),
	'$lgt_swi_write_hide_directive'(Stream, IDcl/6),
	'$lgt_swi_write_hide_directive'(Stream, IDef/4),
	'$lgt_swi_write_hide_directive'(Stream, DDcl/2),
	'$lgt_swi_write_hide_directive'(Stream, DDef/3),
	'$lgt_swi_write_hide_directive'(Stream, Rnm/3),
	atom_concat(Path, Source, File),
	write_canonical(Stream, '$source_location'(File,Line):'$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)),
	write(Stream, '.'),
	nl(Stream).

'$lgt_write_term_and_source_location'(Stream, '$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags), _, Path+Source+Line) :-
	!,
	'$lgt_swi_write_hide_directive'(Stream, Dcl/4),
	'$lgt_swi_write_hide_directive'(Stream, Dcl/5),
	'$lgt_swi_write_hide_directive'(Stream, Def/3),
	'$lgt_swi_write_hide_directive'(Stream, Rnm/3),
	atom_concat(Path, Source, File),
	write_canonical(Stream, '$source_location'(File,Line):'$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)),
	write(Stream, '.'),
	nl(Stream).

'$lgt_write_term_and_source_location'(Stream, '$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags), _, Path+Source+Line) :-
	!,
	'$lgt_swi_write_hide_directive'(Stream, Dcl/4),
	'$lgt_swi_write_hide_directive'(Stream, Dcl/5),
	'$lgt_swi_write_hide_directive'(Stream, Rnm/3),
	atom_concat(Path, Source, File),
	write_canonical(Stream, '$source_location'(File,Line):'$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)),
	write(Stream, '.'),
	nl(Stream).

'$lgt_write_term_and_source_location'(Stream, (:- Directive), _, _) :-	% to cope with {(:- Directive)} entity terms
	!,
	write_canonical(Stream, (:- Directive)),
	write(Stream, '.'),
	nl(Stream).

'$lgt_write_term_and_source_location'(Stream, Term, Kind, Path+Source+Line) :-
	(	Kind == aux ->
		(	Term = (Head :- _) ->
			true
		;	Term \= (:- _),
			Term = Head
		),
		functor(Head, Functor, Arity),
		'$lgt_swi_write_hide_directive'(Stream, Functor/Arity)
	;	true
	),
	atom_concat(Path, Source, File),
	write_canonical(Stream, '$source_location'(File,Line):Term),
	write(Stream, '.'),
	nl(Stream).


'$lgt_swi_write_hide_directive'(Stream, Functor/Arity) :-
	write_canonical(Stream, (:- '$hide'(user:Functor/Arity))),
	write(Stream, '.'),
	nl(Stream).


% '$lgt_assertz_entity_clause'(@clause, +atom)

'$lgt_assertz_entity_clause'('$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags), _) :-
	!,
	'$hide'(user:Dcl/4),
	'$hide'(user:Dcl/6),
	'$hide'(user:Def/3),
	'$hide'(user:Def/4),
	'$hide'(user:Super/4),
	'$hide'(user:IDcl/6),
	'$hide'(user:IDef/4),
	'$hide'(user:DDcl/2),
	'$hide'(user:DDef/3),
	'$hide'(user:Rnm/3),
	assertz('$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)).

'$lgt_assertz_entity_clause'('$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags), _) :-
	!,
	'$hide'(user:Dcl/4),
	'$hide'(user:Dcl/5),
	'$hide'(user:Def/3),
	'$hide'(user:Rnm/3),
	assertz('$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)).

'$lgt_assertz_entity_clause'('$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags), _) :-
	!,
	'$hide'(user:Dcl/4),
	'$hide'(user:Dcl/5),
	'$hide'(user:Rnm/3),
	assertz('$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)).

'$lgt_assertz_entity_clause'(Term, Kind) :-
	(	Kind == aux ->
		(	Term = (Head :- _) ->
			true
		;	Term \= (:- _),
			Term = Head
		),
		functor(Head, Functor, Arity),
		'$hide'(user:Functor/Arity)
	;	true
	),
	assertz(Term).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  annotations
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_value_annotation'(@callable, -atom, -callable, -callable, -callable)

'$lgt_default_value_annotation'(_, _, _, _, _) :-
	fail.


% '$lgt_default_goal_annotation'(@callable, -atom, -callable, -callable, -callable)

'$lgt_default_goal_annotation'(_, _, _, _, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
