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
%  last updated: April 17, 2011
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- multifile(user:prolog_load_file/2).
:- dynamic(user:prolog_load_file/2).

user:prolog_load_file(_:Spec, Options) :-
	\+ '$lgt_member'(must_be_module(true), Options),	% exclude calls to use_module/1-2
	\+ '$lgt_member'(if(not_loaded), Options),			% exclude calls to ensure_loaded/1
	\+ absolute_file_name(Spec, [extensions([pl]), access(read), file_errors(fail)], _),
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
	file_name_extension(Name, 'pl', PrologFile),
	source_file(Path),
	file_base_name(Path, PrologFile),
	'$derived_source'(Path, Source, _),
	file_base_name(Source, LogtalkFile),
	file_name_extension(Name, 'lgt', LogtalkFile),
	!.

prolog_edit:locate(Spec, source_file(Source), [file(Source)]) :-
	compound(Spec),
	Spec =.. [Library, Name],
	atom(Name),
	file_name_extension(Name, 'pl', PrologFile),
	source_file(Path),
	file_base_name(Path, PrologFile),
	'$derived_source'(Path, Source, _),
	file_base_name(Source, LogtalkFile),
	file_name_extension(Name, 'lgt', LogtalkFile),
	'$lgt_expand_library_path'(Library, LibraryPath),
	atom_concat(LibraryPath, LogtalkFile, Source),
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

user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_this'(_, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_sender'(_, _, _, _, _), 'call/N') :- !.

user:prolog_predicate_name(user:'$lgt_expand_term'(_, _, _, _, _), 'expand_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_expand_goal'(_, _, _, _, _), 'expand_goal/2') :- !.

user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _), 'phrase/2') :- !.
user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _, _), 'phrase/3') :- !.

user:prolog_predicate_name(user:'$lgt_abolish_chk'(_, _, _, _), 'abolish/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_fact_chk'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_rule_chk'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_fact_chk'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_rule_chk'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_clause_chk'(_, _, _, _, _), 'clause/2') :- !.
user:prolog_predicate_name(user:'$lgt_retract_fact_chk'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retract_rule_chk'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retractall_chk'(_, _, _, _), 'retractall/1') :- !.

user:prolog_predicate_name(user:'$lgt_iso_read_term'(_, _, _, _), 'read_term/3') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read_term'(_, _, _), 'read_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read'(_, _, _), 'read/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_read'(_, _), 'read/1') :- !.

user:prolog_predicate_name(user:'$lgt_iso_write_term'(_, _, _, _), 'write_term/3') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write_term'(_, _, _), 'write_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write'(_, _, _), 'write/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_write'(_, _), 'write/1') :- !.
user:prolog_predicate_name(user:'$lgt_iso_writeq'(_, _, _), 'writeq/2') :- !.
user:prolog_predicate_name(user:'$lgt_iso_writeq'(_, _), 'writeq/1') :- !.

user:prolog_predicate_name(user:'$lgt_ctg_parameter'(_, _, _, _), 'parameter/2') :- !.

user:prolog_predicate_name(user:'$lgt_threaded_or'(_, _, _), 'threaded/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_and'(_, _, _), 'threaded/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_ignore'(_), 'threaded_ignore/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call'(_, _, _), 'threaded_call/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call'(_, _, _, _), 'threaded_call/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once'(_, _, _), 'threaded_once/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once'(_, _, _, _), 'threaded_once/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call_tagged'(_, _, _, _), 'threaded_call/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_call_tagged'(_, _, _, _, _), 'threaded_call/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once_tagged'(_, _, _, _), 'threaded_once/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_once_tagged'(_, _, _, _, _), 'threaded_once/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek'(_, _, _, _), 'threaded_peek/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek'(_, _, _, _, _), 'threaded_peek/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek_tagged'(_, _, _, _, _), 'threaded_peek/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_peek_tagged'(_, _, _, _, _, _), 'threaded_peek/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit'(_, _, _, _), 'threaded_exit/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit'(_, _, _, _, _), 'threaded_exit/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit_tagged'(_, _, _, _, _), 'threaded_exit/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_exit_tagged'(_, _, _, _, _, _), 'threaded_exit/2') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_synch_ctg'(_, _, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_synch'(_, _, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait_ctg'(_, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_wait'(_, _), 'threaded_wait/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_notify_ctg'(_, _), 'threaded_notify/1') :- !.
user:prolog_predicate_name(user:'$lgt_threaded_notify'(_, _), 'threaded_notify/1') :- !.

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

/*
:- multifile(user:prolog_trace_interception/4).
:- dynamic(user:prolog_trace_interception/4).

user:prolog_trace_interception(unify, Frame, _, continue) :-
	prolog_frame_attribute(Frame, goal, Goal),
	user:prolog_predicate_name(user:Goal, _), !.
*/

:- multifile(prolog:term_compiled/2).

prolog:term_compiled(Entity::Head, QHead) :-
	(	callable(Entity), callable(Head) ->
		'$lgt_compile_predicate_heads'(Head, Entity, THead, _),
		QHead = user:THead
	;	callable(QHead) ->
		(	QHead = Module:THead ->
			Module == user
		;	QHead = THead
		),
		'$lgt_decompile_predicate_heads'(THead, Entity, Head)
	;	fail
	).


:- multifile(prolog_clause:unify_clause_hook/5).

prolog_clause:unify_clause_hook(Clause, QClause, Module, TermPos0, TermPos) :-
	(	QClause = (M:THead :- TBody) ->
		M == user,
		TClause = (THead :- TBody)
	;	QClause = (THead :- TBody) ->
		TClause = QClause
	;	QClause = M:THead ->
		M == user,
		TClause = THead
	;	QClause = THead ->
		TClause = QClause
	),
	functor(THead, TFunctor, _),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	'$lgt_swi_prolog_clause:unify_clause_hook'(Clause, TClause, Module, TermPos0, TermPos).

'$lgt_swi_prolog_clause:unify_clause_hook'((NonTerminal --> GRBody), (THead :- TBody), Module, TermPos0, TermPos) :-
	logtalk::expand_term((NonTerminal --> GRBody), Clause),
	'$lgt_swi_prolog_clause:unify_clause_hook'(Clause, (THead :- TBody), Module, TermPos0, TermPos).
'$lgt_swi_prolog_clause:unify_clause_hook'((Head :- Body), (THead :- TBody), _, TermPos0, TermPos) :-
	(	'$lgt_swi_unify_clause'((Head :- Body), (THead :- TBody), TermPos0, TermPos) ->
		true
	;	writeq((THead :- TBody)), nl
	).
'$lgt_swi_prolog_clause:unify_clause_hook'((Head :- Body), THead, _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'((Head :- Body), (THead :- true), TermPos0, TermPos).
'$lgt_swi_prolog_clause:unify_clause_hook'(Head, THead, _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'(Head, THead, TermPos0, TermPos).


:- multifile(prolog_clause:make_varnames_hook/5).

prolog_clause:make_varnames_hook((Head --> _), (user:THead :- _), Offsets, Names, Bindings) :-
	functor(THead, TFunctor, THeadArity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is THeadArity - 1,
	memberchk(N=EVar, Offsets),
	Names1 = ['<Sender, This, Self, MetaVars, CoinductionStack>'=EVar| Names],
	functor(Head, _, HeadArity),
	In is HeadArity,
	memberchk(In=IVar, Offsets),
	Names2 = ['<DCG_list>'=IVar|Names1],
	Out is HeadArity + 1,
	memberchk(Out=OVar, Offsets),
	Names3 = ['<DCG_tail>'=OVar|Names2],
	prolog_clause:make_varnames(xx, xx, Offsets, Names3, Bindings).
prolog_clause:make_varnames_hook(_, (user:THead :- _), Offsets, Names, Bindings) :-
	functor(THead, TFunctor, Arity),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	N is Arity - 1,
	memberchk(N=IVar, Offsets),
	Names1 = ['<Sender, This, Self, MetaVars, CoinductionStack>'=IVar| Names],
	prolog_clause:make_varnames(xx, xx, Offsets, Names1, Bindings).


:- multifile(user:portray/1).
:- dynamic(user:portray/1).

user:portray(c(This, r(Sender, Self, MetaVars, CoinductionStack))) :-
	write('<'),
	writeq(Sender), write(','),
	writeq(This), write(','),
	writeq(Self), write(','),
	writeq(MetaVars), write(','),
	writeq(CoinductionStack), write(','),
	write('>').


'$lgt_swi_unify_clause'((Head :- Body), (THead :- TBody), TermPos0, TermPos) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, Entity, Head),
	'$lgt_swi_unify_clause_body'(Body, Entity, TBody, TermPos0, TermPos).

'$lgt_swi_unify_clause'(Head, THead, TermPos, TermPos) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, Head).


'$lgt_swi_unify_clause_body'((Goal1, Goal2), Entity, (TGoal1, TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Goal2, Entity, TGoal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'((Goal1; Goal2), Entity, (TGoal1; TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Goal2, Entity, TGoal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'((Goal1 -> Goal2), Entity, (TGoal1 -> TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Goal2, Entity, TGoal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'(Var^Goal, Entity, Var^TGoal, TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_ne_nv'(Obj, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_ne'(Obj, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_ne_'(Obj, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::Msg, _, '$lgt_send_to_obj_'(Obj, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self_nv'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::Msg, _, '$lgt_send_to_self_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call_same'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call_same_'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call_other'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_obj_super_call_other_'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call_same'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call_same_'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call_other'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(^^Msg, _, '$lgt_ctg_super_call_other_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(:Msg, _, '$lgt_ctg_call_'(_, Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(call(Goal), Entity, call(TGoal), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(once(Goal), Entity, (TGoal -> true; fail), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(ignore(Goal), Entity, (TGoal -> true; true), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(catch(Goal, Catcher, Recover), Entity, catch(TGoal, Catcher, TRecover), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(Recover, Entity, TRecover, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'(CallN, _, '$lgt_metacall'(Closure, ExtraArgs, _, _, _, _), TermPos, TermPos) :- !,
	functor(CallN, call, Arity),
	!,
	length(ExtraArgs, N),
	Arity is N + 1,
	arg(1, CallN, Closure),
	'$lgt_swi_call_n_args'(ExtraArgs, 2, CallN).
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_metacall'(Goal, _, _, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_metacall_this'(Goal, _, _, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Goal, _, '$lgt_metacall_sender'(Goal, _, _, _, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(bagof(Term, Goal, List), Entity, bagof(Term, TGoal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(setof(Term, Goal, List), Entity, setof(Term, TGoal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(findall(Term, Goal, List), Entity, findall(Term, TGoal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(abolish(PI), Entity, abolish(TPI), TermPos, TermPos) :-
	'$lgt_decompile_predicate_indicators'(TPI, Entity, PI), !.
'$lgt_swi_unify_clause_body'(asserta(Clause), Entity, asserta(TClause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, Clause), !.
'$lgt_swi_unify_clause_body'(assertz(Clause), Entity, assertz(TClause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, Clause), !.
'$lgt_swi_unify_clause_body'(retract(Clause), Entity, retract(TClause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, Clause), !.

'$lgt_swi_unify_clause_body'(Obj::expand_term(Term, Clause), _, '$lgt_expand_term'(Obj, Term, Clause, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(expand_term(Term, Clause), _, '$lgt_expand_term'(This, Term, Clause, This, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::expand_term(Term, Clause), _, '$lgt_expand_term'(_, Term, Clause, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::expand_goal(Goal, EGoal), _, '$lgt_expand_goal'(Obj, Goal, EGoal, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(expand_goal(Goal, EGoal), _, '$lgt_expand_goal'(This, Goal, EGoal, This, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::expand_goal(Goal, EGoal), _, '$lgt_expand_goal'(_, Goal, EGoal, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(phrase(GRBody, Input), _, '$lgt_phrase'(GRBody, Input, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(phrase(GRBody, Input, Rest), _, '$lgt_phrase'(GRBody, Input, Rest, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::abolish(PI), _, '$lgt_abolish_chk'(Obj, PI, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(abolish(PI), _, '$lgt_abolish_chk'(_, PI, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::abolish(PI), _, '$lgt_abolish_chk'(_, PI, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(asserta(Clause), _, '$lgt_asserta_fact_chk'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::asserta(Clause), _, '$lgt_asserta_fact_chk'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::asserta(Clause), _, '$lgt_asserta_fact_chk'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(asserta(Clause), _, '$lgt_asserta_rule_chk'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::asserta(Clause), _, '$lgt_asserta_rule_chk'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::asserta(Clause), _, '$lgt_asserta_rule_chk'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(assertz(Clause), _, '$lgt_assertz_fact_chk'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::assertz(Clause), _, '$lgt_assertz_fact_chk'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::assertz(Clause), _, '$lgt_assertz_fact_chk'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(assertz(Clause), _, '$lgt_assertz_rule_chk'(_, Clause, _, _, p), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::assertz(Clause), _, '$lgt_assertz_rule_chk'(_, Clause, _, _, p(p)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::assertz(Clause), _, '$lgt_assertz_rule_chk'(Obj, Clause, _, _, p(p(p))), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::clause(Head, Body), _, '$lgt_clause_chk'(Obj, Head, Body, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(clause(Head, Body), _, '$lgt_clause_chk'(_, Head, Body, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::clause(Head, Body), _, '$lgt_clause_chk'(_, Head, Body, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::retract(Head), _, '$lgt_retract_fact_chk'(Obj, Head, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retract(Head), _, '$lgt_retract_fact_chk'(_, Head, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retract(Head), _, '$lgt_retract_fact_chk'(_, Head, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(Obj::retract(Clause), _, '$lgt_retract_rule_chk'(Obj, Clause, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retract(Clause), _, '$lgt_retract_rule_chk'(_, Clause, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retract(Clause), _, '$lgt_retract_rule_chk'(_, Clause, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Obj::retractall(Head), _, '$lgt_retractall_chk'(Obj, Head, _, p(p(p))), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(retractall(Head), _, '$lgt_retractall_chk'(_, Head, _, p(_)), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(::retractall(Head), _, '$lgt_retractall_chk'(_, Head, _, p(_)), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(read_term(Stream, Term, Options), _, '$lgt_iso_read_term'(Stream, Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(read_term(Term, Options), _, '$lgt_iso_read_term'(Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(read(Stream, Term), _, '$lgt_iso_read'(Stream, Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(read(Term), _, '$lgt_iso_read'(Term, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(write_term(Stream, Term, Options), _, '$lgt_iso_write_term'(Stream, Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(write_term(Term, Options), _, '$lgt_iso_write_term'(Term, Options, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(write(Stream, Term), _, '$lgt_iso_write'(Stream, Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(write(Term), _, '$lgt_iso_write'(Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(writeq(Stream, Term), _, '$lgt_iso_writeq'(Stream, Term, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(writeq(Term), _, '$lgt_iso_writeq'(Term, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(parameter(Arg, Value), _, '$lgt_ctg_parameter'(_, _, Arg, Value), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(parameter(_, _), _, true, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(sender(_), _, true, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(self(_), _, true, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(this(_), _, true, TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(threaded(Goals), Entity, '$lgt_threaded_or'(_, MTGoals, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_threaded_goals'(Goals, Entity, MTGoals, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded(Goals), Entity, '$lgt_threaded_and'(_, MTGoals, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_threaded_goals'(Goals, Entity, MTGoals, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded(Goal), Entity, (TGoal -> true; fail), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_ignore(Goal), Entity, '$lgt_threaded_ignore'(TGoal), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_call(Goal), Entity, '$lgt_threaded_call'(TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_call(Goal), Entity, '$lgt_threaded_call'(_, TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_once(Goal), Entity, '$lgt_threaded_once'(TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_once(Goal), Entity, '$lgt_threaded_once'(_, TGoal, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_call(Goal, Tag), Entity, '$lgt_threaded_call_tagged'(TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_call(Goal, Tag), Entity, '$lgt_threaded_call_tagged'(_, TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_once(Goal, Tag), Entity, '$lgt_threaded_once_tagged'(TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_once(Goal, Tag), Entity, '$lgt_threaded_once_tagged'(_, TGoal, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_peek(Goal), Entity, '$lgt_threaded_peek'(TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_peek(Goal), Entity, '$lgt_threaded_peek'(_, TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_peek(Goal, Tag), Entity, '$lgt_threaded_peek_tagged'(TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_peek(Goal, Tag), Entity, '$lgt_threaded_peek_tagged'(_, TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_exit(Goal), Entity, '$lgt_threaded_exit'(TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_exit(Goal), Entity, '$lgt_threaded_exit'(_, TGoal, _, _, _), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_exit(Goal, Tag), Entity, '$lgt_threaded_exit_tagged'(TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(threaded_exit(Goal, Tag), Entity, '$lgt_threaded_exit_tagged'(_, TGoal, _, _, _, Tag), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_synch_ctg'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_synch'(_, Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait_ctg'(Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_wait(Msg), _, '$lgt_threaded_wait'(Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(threaded_notify(Msg), _, '$lgt_threaded_notify_ctg'(Msg, _), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'(threaded_notify(Msg), _, '$lgt_threaded_notify'(Msg, _), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Goal, Entity, with_mutex(_, TGoal), TermPos0, TermPos) :-
	\+ functor(Goal, with_mutex, 2),								% synchronized predicates
	!,
	writeq('HERE'-Goal-TGoal), nl,
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(Goal, _, Goal, TermPos, TermPos) :-	% built-in predicates
	!.

'$lgt_swi_unify_clause_body'(Goal, _, TGoal, TermPos, TermPos) :-	% object and category user predicates
	'$lgt_decompile_predicate_heads'(TGoal, GoalEntity, Goal0),
	(	functor(Goal0, Functor0, _),
		atom_concat(Functor1, '__sync', Functor0) ->
		% synchronized predicate
		Goal0 =.. [Functor0| Args],
		Goal1 =.. [Functor1| Args]
	;	Goal1 = Goal0
	),
	(	Goal = Goal1
	;	Goal = GoalEntity::Goal1
	),
	!.

'$lgt_swi_unify_clause_body'(_, _, _, TermPos, TermPos).			% just in case...


'$lgt_swi_unify_threaded_goals'((Goal1, Goal2), Entity, (TGoal1, TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_threaded_goal'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_threaded_goals'(Goal2, Entity, TGoal2, TermPos1, TermPos).
'$lgt_swi_unify_threaded_goals'((Goal1; Goal2), Entity, (TGoal1; TGoal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_threaded_goal'(Goal1, Entity, TGoal1, TermPos0, TermPos1),
	'$lgt_swi_unify_threaded_goals'(Goal2, Entity, TGoal2, TermPos1, TermPos).
'$lgt_swi_unify_threaded_goals'(Goal, Entity, TGoal, TermPos0, TermPos) :-
	'$lgt_swi_unify_threaded_goal'(Goal, Entity, TGoal, TermPos0, TermPos).

'$lgt_swi_unify_threaded_goal'(Goal, Entity, '$lgt_threaded_goal'(TGoal, _, _, _), TermPos0, TermPos) :-
	'$lgt_swi_unify_clause_body'(Goal, Entity, TGoal, TermPos0, TermPos).


'$lgt_swi_call_n_args'([], _, _).
'$lgt_swi_call_n_args'([Arg| Args], N, CallN) :-
	arg(N, CallN, Arg),
	N2 is N + 1,
	'$lgt_swi_call_n_args'(Args, N2, CallN).


% the following directives are necessary when using the SWI-Prolog
% graphical tracer as predicates whose name start with a $ have by
% default a "notrace" property
:- '$set_predicate_attribute'('$lgt_send_to_self_nv'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_self'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_self_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_nv'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_ne_nv'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_ne'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_send_to_obj_ne_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_obj_super_call_same'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_obj_super_call_same_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_obj_super_call_other'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_obj_super_call_other_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_super_call_same'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_super_call_same_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_super_call_other'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_super_call_other_'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_call'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_ctg_call_'/3, trace, 1).

:- '$set_predicate_attribute'('$lgt_metacall'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall'/6, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall_this'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_metacall_sender'/5, trace, 1).

:- '$set_predicate_attribute'('$lgt_expand_term'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_expand_goal'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_phrase'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_phrase'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_abolish_chk'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_asserta_fact_chk'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_asserta_rule_chk'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_assertz_fact_chk'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_assertz_rule_chk'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_clause_chk'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_retract_fact_chk'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_retract_rule_chk'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_retractall_chk'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_iso_read_term'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_read_term'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_read'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_read'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write_term'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write_term'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_write'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_writeq'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_iso_writeq'/2, trace, 1).

:- '$set_predicate_attribute'('$lgt_ctg_parameter'/4, trace, 1).

:- '$set_predicate_attribute'('$lgt_threaded_or'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_and'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_ignore'/1, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call_tagged'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_call_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once_tagged'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_once_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_peek_tagged'/6, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit'/4, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit_tagged'/5, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_exit_tagged'/6, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_synch_ctg'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_synch'/3, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait_ctg'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_wait'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_notify_ctg'/2, trace, 1).
:- '$set_predicate_attribute'('$lgt_threaded_notify'/2, trace, 1).
