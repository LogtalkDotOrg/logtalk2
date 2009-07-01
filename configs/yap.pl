%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.37.3
%
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  configuration file for YAP Prolog 5.1.3 and later versions
%
%  last updated: June 10, 2009
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% the following language initialization is not needed to run Logtalk altough is 
% higly recommended; you can comment out the set_prolog_flag/2 calls if needed

:- initialization((
	use_module(library(system)),
	'$lgt_hide_predicates',
	set_prolog_flag(language, iso),		% comment out if you need to use YAP libraries that fail in "iso" mode!
	set_prolog_flag(update_semantics, logical),
	set_prolog_flag(unknown, error),
	set_prolog_flag(syntax_errors, error))).


'$lgt_hide_predicates' :-
	(	predicate_property(hide_predicate(_), built_in) ->
		dynamic('$lgt_before_'/5), hide_predicate('$lgt_before_'/5),
		dynamic('$lgt_after_'/5), hide_predicate('$lgt_after_'/5),
		dynamic('$lgt_current_protocol_'/5), hide_predicate('$lgt_current_protocol_'/5),
		dynamic('$lgt_current_category_'/6), hide_predicate('$lgt_current_category_'/6),
		dynamic('$lgt_current_object_'/11), hide_predicate('$lgt_current_object_'/11),
		dynamic('$lgt_entity_property_'/2), hide_predicate('$lgt_entity_property_'/2),
		dynamic('$lgt_implements_protocol_'/3), hide_predicate('$lgt_implements_protocol_'/3),
		dynamic('$lgt_imports_category_'/3), hide_predicate('$lgt_imports_category_'/3),
		dynamic('$lgt_instantiates_class_'/3), hide_predicate('$lgt_instantiates_class_'/3),
		dynamic('$lgt_specializes_class_'/3), hide_predicate('$lgt_specializes_class_'/3),
		dynamic('$lgt_extends_protocol_'/3), hide_predicate('$lgt_extends_protocol_'/3),
		dynamic('$lgt_extends_object_'/3), hide_predicate('$lgt_extends_object_'/3),
		dynamic('$lgt_extends_category_'/3), hide_predicate('$lgt_extends_category_'/3),
		dynamic('$lgt_complemented_object_'/4), hide_predicate('$lgt_complemented_object_'/4),
		dynamic('$lgt_loaded_file_'/2), hide_predicate('$lgt_loaded_file_'/2),
		dynamic('$lgt_debugging_'/1), hide_predicate('$lgt_debugging_'/1),
		dynamic('$lgt_dbg_debugging_'/0), hide_predicate('$lgt_dbg_debugging_'/0),
		dynamic('$lgt_dbg_tracing_'/0), hide_predicate('$lgt_dbg_tracing_'/0),
		dynamic('$lgt_dbg_skipping_'/0), hide_predicate('$lgt_dbg_skipping_'/0),
		dynamic('$lgt_dbg_spying_'/2), hide_predicate('$lgt_dbg_spying_'/2),
		dynamic('$lgt_dbg_spying_'/4), hide_predicate('$lgt_dbg_spying_'/4),
		dynamic('$lgt_dbg_leashing_'/1), hide_predicate('$lgt_dbg_leashing_'/1),
		dynamic('$lgt_current_flag_'/2), hide_predicate('$lgt_current_flag_'/2),
		dynamic('$lgt_static_binding_entity_'/1), hide_predicate('$lgt_static_binding_entity_'/1),
		dynamic('$lgt_obj_static_binding_cache_'/4), hide_predicate('$lgt_obj_static_binding_cache_'/4),
		dynamic('$lgt_ctg_static_binding_cache_'/4), hide_predicate('$lgt_ctg_static_binding_cache_'/4),
		dynamic('$lgt_pp_warnings_top_argument_'/1), hide_predicate('$lgt_pp_warnings_top_argument_'/1),
		dynamic('$lgt_pp_comp_warnings_counter_'/1), hide_predicate('$lgt_pp_comp_warnings_counter_'/1),
		dynamic('$lgt_pp_load_warnings_counter_'/1), hide_predicate('$lgt_pp_load_warnings_counter_'/1),
		dynamic('$lgt_pp_entity_warnings_flag_'/0), hide_predicate('$lgt_pp_entity_warnings_flag_'/0),
		dynamic('$lgt_pp_load_warnings_flag_'/0), hide_predicate('$lgt_pp_load_warnings_flag_'/0),
		dynamic('$lgt_hook_term_expansion_'/2), hide_predicate('$lgt_hook_term_expansion_'/2),
		dynamic('$lgt_hook_goal_expansion_'/2), hide_predicate('$lgt_hook_goal_expansion_'/2),
		dynamic('$lgt_threaded_tag_counter'/1), hide_predicate('$lgt_threaded_tag_counter'/1),
		dynamic('$lgt_dbg_invocation_number_'/1), hide_predicate('$lgt_dbg_invocation_number_'/1),
		dynamic('$lgt_settings_file_loaded'/1), hide_predicate('$lgt_settings_file_loaded'/1),
		dynamic('$lgt_send_to_obj_'/4), hide_predicate('$lgt_send_to_obj_'/4),
		dynamic('$lgt_send_to_obj_ne_'/4), hide_predicate('$lgt_send_to_obj_ne_'/4),
		dynamic('$lgt_send_to_self_'/4), hide_predicate('$lgt_send_to_self_'/4),
		dynamic('$lgt_obj_super_call_same_'/4), hide_predicate('$lgt_obj_super_call_same_'/4),
		dynamic('$lgt_obj_super_call_other_'/4), hide_predicate('$lgt_obj_super_call_other_'/4),
		dynamic('$lgt_ctg_super_call_same_'/4), hide_predicate('$lgt_ctg_super_call_same_'/4),
		dynamic('$lgt_ctg_super_call_other_'/4), hide_predicate('$lgt_ctg_super_call_other_'/4),
		dynamic('$lgt_ctg_call_'/4), hide_predicate('$lgt_ctg_call_'/4),
		dynamic('$lgt_db_lookup_cache_'/5), hide_predicate('$lgt_db_lookup_cache_'/5)
	;	true
	).



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
	predicate_property(Pred, Prop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% call_cleanup(+callable, +callble) -- built-in


% forall(+callable, +callble) -- built-in


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

'$lgt_pl_meta_predicate'(*->(::, ::), control_construct).
'$lgt_pl_meta_predicate'(all(*, ::, *), predicate).
'$lgt_pl_meta_predicate'(call_cleanup(::), predicate).
'$lgt_pl_meta_predicate'(call_cleanup(::, ::), predicate).
'$lgt_pl_meta_predicate'(call_residue(::, *), predicate).
:- if(predicate_property(call_residue_vars(_, _), built_in)).
	'$lgt_pl_meta_predicate'(call_residue_vars(::, *), predicate).
:- endif.
:- if(predicate_property(depth_bound_call(_, _), built_in)).
	'$lgt_pl_meta_predicate'(depth_bound_call(::, *), predicate).
:- endif.
'$lgt_pl_meta_predicate'(if(::, ::, ::), predicate).
'$lgt_pl_meta_predicate'(ignore(::), predicate).
'$lgt_pl_meta_predicate'(findall(*, ::, *, *), predicate).
'$lgt_pl_meta_predicate'(freeze(*, ::), predicate).
'$lgt_pl_meta_predicate'(time_out(::, *, *), predicate).
'$lgt_pl_meta_predicate'(when(*, ::), predicate).
:- if(predicate_property(setup_call_cleanup(_, _, _), built_in)).
	'$lgt_pl_meta_predicate'(setup_call_cleanup(::, ::, ::), predicate).
:- endif.
:- if(predicate_property(setup_call_catcher_cleanup(_, _, _, _), built_in)).
	'$lgt_pl_meta_predicate'(setup_call_catcher_cleanup(::, ::, *, ::), predicate).
:- endif.
'$lgt_pl_meta_predicate'(thread_initialization(::), predicate).
'$lgt_pl_meta_predicate'(thread_at_exit(::), predicate).
'$lgt_pl_meta_predicate'(thread_create(::, *, *), predicate).
'$lgt_pl_meta_predicate'(thread_create(::, *), predicate).
'$lgt_pl_meta_predicate'(thread_create(::), predicate).
'$lgt_pl_meta_predicate'(thread_signal(*, ::), predicate).
'$lgt_pl_meta_predicate'(with_mutex(*, ::), predicate).
% workaround broken meta-predicate declarations:
'$lgt_pl_meta_predicate'(format(*, *), predicate).
'$lgt_pl_meta_predicate'(format(*, *, *), predicate).



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

'$lgt_prolog_feature'(prolog_dialect, yap).
'$lgt_prolog_feature'(prolog_version, Version) :-
	current_prolog_flag(version_data, yap(Major, Minor, Patch, _)),
	atomic_concat([Major, '.', Minor, '.', Patch], Version).
'$lgt_prolog_feature'(prolog_compatible_version, '@>='('5.1.3')).

'$lgt_prolog_feature'(break_predicate, supported).
'$lgt_prolog_feature'(encoding_directive, full).
'$lgt_prolog_feature'(multifile_directive, supported).
'$lgt_prolog_feature'(tabling, Tabling) :-
	(	current_prolog_flag(system_options, tabling) ->
		Tabling = supported
	;	Tabling = unsupported
	).
'$lgt_prolog_feature'(threads, Threads) :-
	(	current_prolog_flag(system_options, threads) ->
		Threads = supported
	;	Threads = unsupported
	).



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

'$lgt_default_flag'(startup_message, flags(compact)).

'$lgt_default_flag'(underscore_variables, singletons).

'$lgt_default_flag'(code_prefix, '$').

'$lgt_default_flag'(debug, off).

'$lgt_default_flag'(complements, deny).
'$lgt_default_flag'(dynamic_declarations, deny).
'$lgt_default_flag'(events, deny).

'$lgt_default_flag'(altdirs, off).
'$lgt_default_flag'(tmpdir, TmpDir) :-
	(	current_prolog_flag(unix, true) ->
		TmpDir = '.lgt_tmp/'
	;	TmpDir = 'lgt_tmp/'
	).
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


% '$lgt_file_exists'(+atom)
%
% checks if a file exist in the current directory

'$lgt_file_exists'(File) :-
	file_exists(File).


% '$lgt_delete_file'(+atom)
%
% deletes a file in the current directory

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	(	atom_concat(Directory2, '/', Directory) ->
		true
	;	Directory2 = Directory
	),
	absolute_file_name(Directory2, Path),
	file_exists(Path),
	file_property(Path, type(directory)).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	getcwd(Directory).


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
	;	make_directory(Directory)
	).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, _, Options) :-
	load_files(File, Options).


% '$lgt_compare_file_mtimes'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_mtimes'(Result, File1, File2) :-
	file_property(File1, mod_time(Time1)),
	file_property(File2, mod_time(Time2)),
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
	;	getcwd(Directory)
	).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknwon

'$lgt_user_directory'(Directory) :-
	environ('LOGTALKUSER', Directory).



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
	statistics(cputime, [Miliseconds, _]),
	Seconds is Miliseconds/1000.



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
	stream_position(Stream, Position),
	stream_position_data(line_count, Position, Line).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the line
%  where the term starts (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, Options, LineBegin-LineEnd) :-
	read_term(Stream, Term, [term_position(PositionBegin)| Options]),
	stream_position_data(line_count, PositionBegin, LineBegin),
	stream_position(Stream, PositionEnd),
	stream_position_data(line_count, PositionEnd, LineEnd).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  handling of Prolog-proprietary directives on Logtalk source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_ignore_pl_directive'(@callable)

'$lgt_ignore_pl_directive'(_) :-
	fail.


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(load_foreign_files(Files, Libs, InitRoutine), initialization(load_foreign_files(Files, Libs, InitRoutine))) :-
	load_foreign_files(Files, Libs, InitRoutine).

'$lgt_rewrite_and_copy_pl_directive'(table(PIs), table(CPIs)) :-
	'$lgt_rewrite_and_copy_pl_directive_pis'(PIs, CPIs).

'$lgt_rewrite_and_copy_pl_directive'(thread_local(PIs), thread_local(CPIs)) :-
	'$lgt_rewrite_and_copy_pl_directive_pis'(PIs, CPIs).

'$lgt_rewrite_and_copy_pl_directive'(thread_initialization(Goal), thread_initialization(CGoal)) :-
	'$lgt_pp_entity'(_, Entity, Prefix, _, _),
	'$lgt_comp_ctx'(Ctx, _, Entity, Entity, Entity, Prefix, [], _, ExCtx),
	'$lgt_exec_ctx'(ExCtx, Entity, Entity, Entity, []),		% MetaVars = [] as we're compiling a local call
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_compiler_flag'(debug, on) ->
		CGoal = DGoal
	;	CGoal = TGoal
	).


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

'$lgt_rewrite_and_recompile_pl_directive'(encoding(Encoding1), encoding(Encoding2)) :-
	nonvar(Encoding1),
	'$lgt_rewrite_and_recompile_pl_encoding_directive'(Encoding1, Encoding2).

'$lgt_rewrite_and_recompile_pl_directive'(ensure_loaded(File), use_module(File)) :-
	'$lgt_pp_module_'(_).	% ensure_loaded/1 directive used within a module (sloppy replacement for the use_module/1 directive)

'$lgt_rewrite_and_recompile_pl_directive'(reexport(File), reexport(Module, Exports)) :-
	'$lgt_yap_list_of_exports'(File, Module, Exports).

'$lgt_rewrite_and_recompile_pl_directive'(use_module(File), use_module(Module, Exports)) :-
	'$lgt_yap_list_of_exports'(File, Module, Exports).


'$lgt_yap_list_of_exports'(File, Module, Exports) :-
	(	absolute_file_name(File, Path, [file_type(prolog), access(read), file_errors(fail)])
	;	absolute_file_name(File, Path, [extensions(['.lgt']), access(read), file_errors(fail)])
	),
	open(Path, read, In),
	(	peek_char(In, #) ->		% deal with #! script; if not present
		skip(In, 10)			% assume that the module declaration
	;	true					% is the first directive on the file
	),
	call_cleanup(read(In, ModuleDecl), close(In)),
	ModuleDecl = (:- module(Module, Exports)).


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


% thread_sleep(+number) -- built-in



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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
