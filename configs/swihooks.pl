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
%  integration code for SWI Prolog 5.8.0 and later versions to compile and
%  load Logtalk files using SWI Prolog consult/1, to support edit/1 and
%  make/0, and to improve usability when using the XPCE profiler
%
%  last updated: April 10, 2011
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- multifile(user:prolog_load_file/2).
:- dynamic(user:prolog_load_file/2).

user:prolog_load_file(_:Spec, Options) :-
	\+ '$lgt_member'(must_be_module(true), Options),	% exclude calls to use_module/1-2
	\+ '$lgt_member'(if(not_loaded), Options),			% exclude calls to ensure_loaded/1
	\+ absolute_file_name(Spec, [extensions([pl]), access(read), file_errors(fail)], Path),
	(	atom(Spec) ->
		expand_file_name(Spec, [SpecExp]),
		absolute_file_name(SpecExp, [extensions([lgt]), access(read), file_errors(fail)], Path)
	;	Spec =.. [Library, File],
		atom(File),										% no paths instead of a file name for Logtalk
		'$lgt_expand_library_path'(Library, LibPath),
		atom_concat(LibPath, File, Spec2),
		expand_file_name(Spec2, [SpecExp]),
		absolute_file_name(SpecExp, [extensions([lgt]), access(read), file_errors(fail)], Path)
	),
	file_directory_name(Path, Dir),
	file_base_name(Path, BaseName),
	file_name_extension(Entity, _, BaseName),
	working_directory(Old, Dir),
	'$lgt_swi_filter_compiler_options'(Options, Options2),
	setup_call_cleanup(true, logtalk_load(Entity, Options2), working_directory(_, Old)).


'$lgt_swi_filter_compiler_options'([], []).

'$lgt_swi_filter_compiler_options'([Option| Options], [Option| Options2]) :-
	functor(Option, Functor, 1),
	'$lgt_valid_flag'(Functor),
	!,
	'$lgt_swi_filter_compiler_options'(Options, Options2).

'$lgt_swi_filter_compiler_options'([_| Options], Options2) :-
	'$lgt_swi_filter_compiler_options'(Options, Options2).


:- multifile(prolog_edit:locate/3).

prolog_edit:locate(Name, source_file(Source), [file(Source)]) :-
	atom(Name),
	source_file(Path),
	file_base_name(Path, File),
	file_name_extension(Name, 'pl', File),
	file_name_extension(Plain, _, Path),
	file_name_extension(Plain, 'lgt', Source),
	exists_file(Source),
	!.


:- multifile(user:prolog_predicate_name/2).

user:prolog_predicate_name(user:'$lgt_send_to_obj_'(_, _, _), '::/2 (cached; event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne_'(_, _, _), '::/2 (cached; not event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self_'(_, _, _), '::/1 (cached)') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_same_'(_, _, _), '^^/2 (cached; from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_other_'(_, _, _), '^^/2 (cached; from obj; diff pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_same_'(_, _, _), '^^/2 (cached; from ctg; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_other_'(_, _, _), '^^/2 (cached; from ctg; diff pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_call_'(_, _, _), ':/1 (cached)') :- !.

user:prolog_predicate_name(user:'$lgt_send_to_obj'(_, _, _), '::/2 (not cached; event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne'(_, _, _), '::/2 (not cached; not event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self'(_, _, _), '::/1 (not cached)') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_same'(_, _, _), '^^/2 (not cached; from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_other'(_, _, _), '^^/2 (not cached; from obj; diff pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_same'(_, _, _), '^^/2 (not cached; from ctg; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_other'(_, _, _), '^^/2 (not cached; from ctg; diff pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_call'(_, _, _), ':/1 (not cached)') :- !.

user:prolog_predicate_name(Goal, Label) :-
	Goal \= _::_,
	(	Goal = Module:THead ->
		Module == user
	;	Goal = THead
	),
	functor(THead, TFunctor, TArity),
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, Functor/Arity),
	(	atom(Entity) ->
		atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
	;	functor(Entity, EFunctor, EArity),
		atomic_list_concat([EFunctor, '/', EArity, '::', Functor, '/', Arity], Label)
	).


:- multifile(prolog_clause:unify_clause_hook/5).

prolog_clause:unify_clause_hook((Head :- Body), (user:THead :- TBody), _, TermPos0, TermPos) :-
	functor(THead, TFunctor, _),
	'$lgt_default_flag'(code_prefix, CodePrefix),
	atom_concat('.', CodePrefix, DebugCodePrefix),
	atom_concat(DebugCodePrefix, _, TFunctor),
	'$lgt_decompile_debug_clause'((THead :- TBody), (Head :- Body)),
	TermPos0 = term_position(From, To, FFrom, FTo, [H, B0]),
	TermPos = term_position(From, To, FFrom, FTo, [H, B]),
	B = term_position(0,0,0,0,[0-0,B0]).
prolog_clause:unify_clause_hook(Head, (user:THead :- TBody), _, TermPos0, TermPos) :-
	functor(THead, TFunctor, _),
	'$lgt_default_flag'(code_prefix, CodePrefix),
	atom_concat('.', CodePrefix, DebugCodePrefix),
	atom_concat(DebugCodePrefix, _, TFunctor),
	'$lgt_decompile_debug_clause'((THead :- TBody), Head),
	(	TermPos0 = term_position(From, To, FFrom, FTo, [H, B0]) ->
		TermPos = term_position(From, To, FFrom, FTo, [H, B]),
		B = term_position(0,0,0,0,[0-0,B0])
	;	TermPos0 = term_position(From, To, FFrom, FTo, [H| B0]),
		TermPos = term_position(From, To, FFrom, FTo, [H, B]),
		B = term_position(0,0,0,0,[0-0| B0])
	).


:- multifile(prolog_clause:make_varnames_hook/5).

prolog_clause:make_varnames_hook(_, (user:THead :- _), Offsets, Names, Bindings) :-
	functor(THead, TFunctor, Arity),
	'$lgt_default_flag'(code_prefix, CodePrefix),
	atom_concat('.', CodePrefix, DebugCodePrefix),
	atom_concat(DebugCodePrefix, _, TFunctor),
	N is Arity - 1,
	memberchk(N=IVar, Offsets),
	Names1 = ['[Sender, This, Self, MetaVars, CoinductionStack]'=IVar| Names],
	prolog_clause:make_varnames(xx, xx, Offsets, Names1, Bindings).


:- multifile(user:portray/1).
:- dynamic(user:portray/1).

user:portray(c(This, r(Sender, Self, MetaVars, CoinductionStack))) :-
	writeq([Sender, This, Self, MetaVars, CoinductionStack]).
