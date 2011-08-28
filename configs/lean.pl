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
%  configuration file for Lean Prolog 2.18.5 and later versions
%
%  last updated: August 28, 2011
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_quickfail(0, _).



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
%
% table of missing ISO predicates which are defined in this file

'$lgt_iso_predicate'(open(_, _, _, _)).
'$lgt_iso_predicate'(stream_property(_, _)).
'$lgt_iso_predicate'(write_term(_, _)).
'$lgt_iso_predicate'(write_term(_, _, _)).


open(Source, Mode, Stream, Options) :-
	(	'$lgt_member'(alias(Alias), Options) ->
		open(Source, Mode, Stream),
		set_alias(Stream, Alias)
	;	open(Source, Mode, Stream)
	).

stream_property(Stream, alias(Alias)) :-
	get_alias(Stream, Alias),
	!.


write_term(Term, Options) :-
	current_output(Stream),
	write_term(Stream, Term, Options).


write_term(Stream, Term, Options) :-
	'$lgt_member'(quoted(true), Options),
	!,
	current_output(Current),
	set_output(Stream),
	writeq(Term),
	set_output(Current).

write_term(Stream, Term, _) :-
	current_output(Current),
	set_output(Stream),
	write(Term),
	set_output(Current).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(Pred, static) :-
	predicate_property(Pred, compiled).
'$lgt_predicate_property'(Pred, ('dynamic')) :-
	predicate_property(Pred, interpreted).
'$lgt_predicate_property'(Pred, Prop) :-
	predicate_property(Pred, Prop).



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


% forall(+callable, +callable) -- built-in


% retractall(+callable) -- built-in


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

'$lgt_pl_meta_predicate'('?'(_), '?'(0), predicate).
'$lgt_pl_meta_predicate'(':'(_, _), ':'(*, 0), predicate).
'$lgt_pl_meta_predicate'('@'(_, _), '@'(*, 0), predicate).
'$lgt_pl_meta_predicate'(add_task(_), add_task(0), predicate).
'$lgt_pl_meta_predicate'(add_task(_, _), add_task(*, 0), predicate).
'$lgt_pl_meta_predicate'(add_task(_, _, _), add_task(*, 0, *), predicate).
'$lgt_pl_meta_predicate'(all(_, _), all(0, *), predicate).
'$lgt_pl_meta_predicate'(all(_, _, _, _), all(*, *,0, *), predicate).
'$lgt_pl_meta_predicate'(alt_findall(_, _, _), alt_findall(*, 0, *), predicate).
'$lgt_pl_meta_predicate'(ask_agent(_, _), ask_agent(*, 0), predicate).
'$lgt_pl_meta_predicate'(ask_broker(_), ask_broker(0), predicate).
'$lgt_pl_meta_predicate'(ask_engine(_, _, _), ask_engine(* ,0, *), predicate).
'$lgt_pl_meta_predicate'(ask_interactor(_, _), ask_interactor(*, 0), predicate).
'$lgt_pl_meta_predicate'(ask_linda(_, _,_), ask_linda(*, *, 0), predicate).
'$lgt_pl_meta_predicate'(ask_peer(_, _, _, _, _), ask_peer(*, *, *, 0, *), predicate).
'$lgt_pl_meta_predicate'(bg(_, _), bg(0, *), predicate).
'$lgt_pl_meta_predicate'(bg_clone(_, _), bg_clone(0, *), predicate).
'$lgt_pl_meta_predicate'(bg_with(_, _, _), bg_with(*, 0, *), predicate).
'$lgt_pl_meta_predicate'(call_ifdef(_, _), call_ifdef(0, 0), predicate).
'$lgt_pl_meta_predicate'(call_with_time_limit(_, _), call_with_time_limit(*, 0), predicate).
'$lgt_pl_meta_predicate'(ccall(_, _, _, _, _), ccall(*, *, *, 0, *), predicate).
'$lgt_pl_meta_predicate'(det_call(_), det_call(0), predicate).
'$lgt_pl_meta_predicate'(drop_at_least(_, _), drop_at_least(*, 0), predicate).
'$lgt_pl_meta_predicate'(find_at_most(_, _, _, _), find_at_most(*, *, 0, *), predicate).
'$lgt_pl_meta_predicate'(find_while(_, _, _, _), find_while(*, *, 0, *), predicate).
'$lgt_pl_meta_predicate'(findall(_, _, _, _), findall(*, 0, *, *), predicate).
'$lgt_pl_meta_predicate'(foreach(_, _), foreach(0, 0), predicate).
'$lgt_pl_meta_predicate'(init_engine(_, _, _, _), init_engine(*, *, 0, *), predicate).
'$lgt_pl_meta_predicate'(linda_client(_, _, _,_ , _), linda_client(*, *, *, *, 0), predicate).
'$lgt_pl_meta_predicate'(mbg(_), mbg([0]), predicate).
'$lgt_pl_meta_predicate'(new_engine(_, _, _), new_engine(*, 0, *), predicate).
'$lgt_pl_meta_predicate'(nth_answer(_, _), nth_answer(*, 0), predicate).
'$lgt_pl_meta_predicate'(rli_call(_, _, _, _, _), rli_call(*, *, _, 0, *), predicate).
'$lgt_pl_meta_predicate'(take_at_most(_, _), take_at_most(*, 0), predicate).
'$lgt_pl_meta_predicate'(tell_agent_at(_, _), tell_agent_at(*, 0), predicate).
'$lgt_pl_meta_predicate'(time(_), time(0), predicate).
'$lgt_pl_meta_predicate'(time(_, _), time(0, *), predicate).
'$lgt_pl_meta_predicate'(to_engine(_, _, _), to_engine(*, *, 0), predicate).
'$lgt_pl_meta_predicate'(topcall(_), topcall(0), predicate).
'$lgt_pl_meta_predicate'(while(_, _), while(0, 0), predicate).

to_engine(Interactor, Pattern, Goal) :-
	to_engine(Interactor, (Pattern:-Goal)).



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

'$lgt_prolog_feature'(prolog_dialect, lean).
'$lgt_prolog_feature'(prolog_version, (Major, Minor, Patch)) :-
	current_prolog_flag(version_data, lprolog(Major, Minor, Patch, _)).
'$lgt_prolog_feature'(prolog_compatible_version, '@>='((2, 19, 0))).

'$lgt_prolog_feature'(break_predicate, unsupported).
'$lgt_prolog_feature'(encoding_directive, source).
'$lgt_prolog_feature'(tabling, unsupported).
'$lgt_prolog_feature'(threads, unsupported).
'$lgt_prolog_feature'(modules, unsupported).
'$lgt_prolog_feature'(coinduction, unsupported).



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
	(	getenv('COMSPEC', _) ->		% Windows systems define this environment variable...
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
'$lgt_default_flag'(optimize, on).
'$lgt_default_flag'(source_data, on).
'$lgt_default_flag'(debug, off).
% Prolog compiler and loader flags:
'$lgt_default_flag'(prolog_compiler, []).
'$lgt_default_flag'(prolog_loader, []).



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


'$lgt_is_list'(('-')) :-
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
	(	\+ atom_concat('/', _, Path),
		\+ atom_concat('$', _, Path),
		working_directory(Current, Current),
		atom_concat(Current, '/', ExpandedPath0),
		atom_concat(ExpandedPath0, Path, ExpandedPath)
	;	absolute_file_name(Path, ExpandedPath)
	).


% '$lgt_file_exists'(+atom)
%
% see if a file exist in the current directory

'$lgt_file_exists'(File) :-
	working_directory(Path0, Path0),
	atom_concat(Path0, '/', Path1),
	atom_concat(Path1, File, Path),
	exists_file(Path).


% '$lgt_delete_file'(+atom)
%
% deletes a file

'$lgt_delete_file'(File) :-
	delete_file(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	exists_dir(Directory).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	working_directory(Directory0, Directory0),
	atom_concat(Directory0, '/', Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	absolute_file_name(Directory, Path),
	working_directory(_, Path).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(Directory) :-
	(	exists_dir(Directory) ->
		true
	;	make_directory(Directory)
	).


% '$lgt_compile_prolog_code'(+atom, +atom, +list)
%
% compile to disk a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_compile_prolog_code'(File, _, Options) :-
	(	'$lgt_member'(wam, Options) ->
		'$lgt_file_extension'(prolog, Extension),
		atom_concat(Source, Extension, File),
		atomic_list_concat(['fcompile(\'', Source, '\')'], Arg),
		atom_concat(Source, '.wam', WamFile),
		bg(system([lprolog, Arg], _)),
		repeat,
		exists_file(WamFile),
		!
	;	true
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
	newer_file_of(File1, File2, File),
	(	File == File1 ->
		Result = (>)
	;	Result = (<)
	).


% '$lgt_startup_directory'(-atom)
%
% returns the Logtalk startup directory 

'$lgt_startup_directory'(Directory) :-
	(	getenv('LOGTALK_STARTUP_DIRECTORY', Directory) ->
		true
	;	working_directory(Directory, Directory)
	).


% '$lgt_user_directory'(-atom)
%
% returns the Logtalk user directory; fails if unknown

'$lgt_user_directory'(Directory) :-
	getenv('LOGTALKUSER', Directory).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_current_date'(?Year, ?Month, ?Day)

'$lgt_current_date'(2011, 8, 26).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(15, 00, 00).



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
	kbd_wait(Code),
	put_code(Code),
	nl,
	char_code(Char, Code).



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
%  customized version of the read_term/3 predicate for returning the term
%  position (start and end lines; needed for improved error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -position)

'$lgt_read_term'(Stream, Term, [singletons([])], LineBegin-LineEnd) :-
	read_term(Stream, yes, Term, _, LineBegin, LineEnd).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  handling of Prolog-proprietary directives on Logtalk source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_meta_directive'(@callable)

'$lgt_pl_meta_directive'(_) :-
	fail.


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
%  multi-threading predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% thread_property(+atom, ?nonvar)

thread_property(_, _) :-
	fail.


% thread_self(?atom)

thread_self(_) :-
	fail.


% thread_create(@callable, -thread_id, +list)

thread_create(_, _, _) :-
	fail.


% thread_join(+atom, -nonvar)

thread_join(_, _) :-
	fail.


% thread_detach(+atom)

thread_detach(_) :-
	fail.


% thread_exit(@term)

thread_exit(_) :-
	fail.


% thread_send_message(+atom, @callable)

thread_send_message(_, _) :-
	fail.


% thread_peek_message(+atom, ?callable)

thread_peek_message(_, _) :-
	fail.


% thread_get_message(+atom, ?callable)

thread_get_message(_, _) :-
	fail.


% thread_get_message(?callable)

thread_get_message(_) :-
	fail.


% thread_sleep(+number)

thread_sleep(_) :-
	fail.


mutex_create(_, _) :-
	fail.


with_mutex(_, _) :-
	fail.


mutex_lock(_) :-
	fail.


mutex_unlock(_) :-
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  converts between Prolog stream encoding names and XML encoding names
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_logtalk_prolog_encoding'(?atom, ?atom, +stream)

'$lgt_logtalk_prolog_encoding'('UTF-8', 'UTF-8', _).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  experimental lambda support predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_copy_term_without_constraints'(@term, ?term)

'$lgt_copy_term_without_constraints'(Term, Copy) :-
	copy_term(Term, Copy).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  utility predicates used to construct execution context terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_exec_ctx'([This, Sender, Self, MetaCallCtx, Stack], Sender, This, Self, MetaCallCtx, Stack).

'$lgt_exec_ctx_this_rest'([This| Ctx], This, Ctx).	% inheritance only requires updating "this"

'$lgt_exec_ctx_this'([This| _], This).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  hooks predicates for writing and assert compiled entity terms
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_write_term_and_source_location'(@stream, @callable, +atom, @callable)

'$lgt_write_term_and_source_location'(Stream, Term, _Kind, _Location) :-
	write_canonical(Stream, Term),
	write(Stream, '.'),
	nl(Stream).


% '$lgt_assertz_entity_clause'(@clause, +atom)

'$lgt_assertz_entity_clause'(Clause, _Kind) :-
	assertz(Clause).



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
%  dummy definitions just to avoid warnings at startup when using qcompile/1
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


abort :-
	fail.


break :-
	fail.


ensure_loaded(_) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
