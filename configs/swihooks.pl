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

prolog_clause:unify_clause_hook(Clause, TClause, Module, TermPos0, TermPos) :-
	(	TClause = (M:THead :- TBody) ->
		M == user
	;	TClause = (THead :- TBody) ->
		true
	;	TClause = M:THead ->
		M == user,
		TBody = true
	;	TClause = THead,
		TBody = true
	),
	functor(THead, TFunctor, _),
	'$lgt_current_flag_'(code_prefix, CodePrefix),
	atom_concat(CodePrefix, _, TFunctor),
	'$lgt_swi_prolog_clause:unify_clause_hook'(Clause, (THead :- TBody), Module, TermPos0, TermPos).

'$lgt_swi_prolog_clause:unify_clause_hook'((NonTerminal --> GRBody), (THead :- TBody), Module, TermPos0, TermPos) :-
	logtalk::expand_term((NonTerminal --> GRBody), Clause),
	'$lgt_swi_prolog_clause:unify_clause_hook'(Clause, (THead :- TBody), Module, TermPos0, TermPos).
'$lgt_swi_prolog_clause:unify_clause_hook'((Head :- Body), (THead :- TBody), _, TermPos, TermPos) :-
	(	'$lgt_swi_unify_clause'((THead :- TBody), (Head :- Body), TermPos, TermPos) ->
		true
	;	writeq((THead :- TBody)), nl
	).
%	TermPos0 = term_position(From, To, FFrom, FTo, [H, B0]),
%	TermPos  = term_position(From, To, FFrom, FTo, [H, term_position(0,0,0,0,[0-0,0-0,0-0,B0])]).
'$lgt_swi_prolog_clause:unify_clause_hook'(Head, (THead :- TBody), _, TermPos0, TermPos) :-
	'$lgt_swi_unify_clause'((THead :- TBody), Head, TermPos, TermPos),
	TermPos0 = term_position(From, To, FFrom, FTo, P),
	TermPos  = term_position(From, To, FFrom, FTo, [term_position(From, To, FFrom, FTo, P), term_position(0,0,0,0,[0-0,0-0,0-0])]).


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


'$lgt_swi_unify_clause'((THead :- true), (Head :- Body), TermPos0, TermPos) :-
	'$lgt_swi_unify_clause_in_line_goal'(Body),
	'$lgt_decompile_predicate_heads'(THead, Head, TermPos0, TermPos),
	!.

'$lgt_swi_unify_clause'((THead :- TBody), (Head :- Body), TermPos0, TermPos) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, Entity, Head),
	'$lgt_swi_unify_clause_body'(TBody, Entity, Body, TermPos0, TermPos).

'$lgt_swi_unify_clause'((THead :- true), Head, TermPos, TermPos) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, Head).


'$lgt_swi_unify_clause_body'(TGoal2, Entity, (Goal1, Goal2), TermPos0, TermPos) :-
	nonvar(Goal1),
	'$lgt_swi_unify_clause_in_line_goal'(Goal1),
	!,
	'$lgt_swi_unify_clause_body'(TGoal2, Entity, Goal2, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(TGoal1, Entity, (Goal1, Goal2), TermPos0, TermPos) :-
	nonvar(Goal2),
	'$lgt_swi_unify_clause_in_line_goal'(Goal2),
	!,
	'$lgt_swi_unify_clause_body'(TGoal1, Entity, Goal1, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'((TGoal1, TGoal2), Entity, (Goal1, Goal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(TGoal1, Entity, Goal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(TGoal2, Entity, Goal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'((TGoal1; TGoal2), Entity, (Goal1; Goal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(TGoal1, Entity, Goal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(TGoal2, Entity, Goal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'((TGoal1 -> TGoal2), Entity, (Goal1 -> Goal2), TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(TGoal1, Entity, Goal1, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(TGoal2, Entity, Goal2, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'(Var^TGoal, Entity, Var^Goal, TermPos0, TermPos) :-
	!,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(TGoal, _Entity, Goal, TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TGoal, GoalEntity, Goal0),
	(	Goal = Goal0
	;	Goal = GoalEntity::Goal0
	),
	!.

'$lgt_swi_unify_clause_body'('$lgt_send_to_obj_ne_nv'(Obj, Msg, _), _, Obj::Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_send_to_obj_ne'(Obj, Msg, _), _, Obj::Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_send_to_obj_ne_'(Obj, Msg, _), _, Obj::Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_send_to_obj_'(Obj, Msg, _), _, Obj::Msg, TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_send_to_self'(_, Msg, _), _, ::Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_send_to_self_nv'(_, Msg, _), _, ::Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_send_to_self_'(_, Msg, _), _, ::Msg, TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_obj_super_call_same'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_obj_super_call_same_'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_obj_super_call_other'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_obj_super_call_other_'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_ctg_super_call_same'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_ctg_super_call_same_'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_ctg_super_call_other'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_ctg_super_call_other_'(_, Msg, _), _, ^^Msg, TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_ctg_call_'(_, Msg, _), _, :Msg, TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(call(TGoal), Entity, call(Goal), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos1).
'$lgt_swi_unify_clause_body'(TGoal -> true; fail), Entity, once(Goal), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos1).
'$lgt_swi_unify_clause_body'(TGoal -> true; true), Entity, ignore(Goal), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos1).
'$lgt_swi_unify_clause_body'(catch(TGoal, Catcher, TRecover), Entity, catch(Goal, Catcher, Recover), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos1),
	'$lgt_swi_unify_clause_body'(TRecover, Entity, Recover, TermPos1, TermPos).

'$lgt_swi_unify_clause_body'(bagof(Term, TGoal, List), Entity, bagof(Term, Goal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(setof(Term, TGoal, List), Entity, setof(Term, Goal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos).
'$lgt_swi_unify_clause_body'(findall(Term, TGoal, List), Entity, findall(Term, Goal, List), TermPos0, TermPos) :- !,
	'$lgt_swi_unify_clause_body'(TGoal, Entity, Goal, TermPos0, TermPos).

'$lgt_swi_unify_clause_body'(abolish(TPI), Entity, _, abolish(PI), TermPos, TermPos) :-
	'$lgt_decompile_predicate_indicators'(TPI, Entity, PI), !.
'$lgt_swi_unify_clause_body'(asserta(TClause), Entity, _, asserta(Clause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, Clause), !.
'$lgt_swi_unify_clause_body'(assertz(TClause), Entity, _, assertz(Clause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, Clause, TermPos, TermPos), !.
'$lgt_swi_unify_clause_body'(retract(TClause), Entity, _, retract(Clause), TermPos, TermPos) :-
	'$lgt_decompile_predicate_heads'(TClause, Entity, Clause), !.

'$lgt_swi_unify_clause_body'('$lgt_expand_term'(Obj, Term, Clause, _, p(p(p))), _, Obj::expand_term(Term, Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_expand_term'(This, Term, Clause, This, p(_)), _, expand_term(Term, Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_expand_term'(_, Term, Clause, _, p(_)), _, ::expand_term(Term, Clause), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_expand_goal'(Obj, Goal, EGoal, _, p(p(p))), _, Obj::expand_goal(Goal, EGoal), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_expand_goal'(This, Goal, EGoal, This, p(_)), _, expand_goal(Goal, EGoal), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_expand_goal'(_, Goal, EGoal, _, p(_)), _, ::expand_goal(Goal, EGoal), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_phrase'(GRBody, Input, _), _, phrase(GRBody, Input), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_phrase'(GRBody, Input, Rest, _), _, phrase(GRBody, Input, Rest), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_abolish_chk'(Obj, PI, _, p(p(p))), _, Obj::abolish(PI), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_abolish_chk'(_, PI, _, p(_)), _, abolish(PI), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_abolish_chk'(_, PI, _, p(_)), _, ::abolish(PI), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_asserta_fact_chk'(_, Clause, _, _, p), _, asserta(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_asserta_fact_chk'(_, Clause, _, _, p(p)), _, ::asserta(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_asserta_fact_chk'(Obj, Clause, _, _, p(p(p))), _, Obj::asserta(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_asserta_rule_chk'(_, Clause, _, _, p), _, asserta(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_asserta_rule_chk'(_, Clause, _, _, p(p)), _, ::asserta(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_asserta_rule_chk'(Obj, Clause, _, _, p(p(p))), _, Obj::asserta(Clause), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_assertz_fact_chk'(_, Clause, _, _, p), _, assertz(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_assertz_fact_chk'(_, Clause, _, _, p(p)), _, ::assertz(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_assertz_fact_chk'(Obj, Clause, _, _, p(p(p))), _, Obj::assertz(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_assertz_rule_chk'(_, Clause, _, _, p), _, assertz(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_assertz_rule_chk'(_, Clause, _, _, p(p)), _, ::assertz(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_assertz_rule_chk'(Obj, Clause, _, _, p(p(p))), _, Obj::assertz(Clause), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_clause_chk'(Obj, Head, Body, _, p(p(p))), _, Obj::clause(Head, Body), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_clause_chk'(_, Head, Body, _, p(_)), _, clause(Head, Body), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_clause_chk'(_, Head, Body, _, p(_)), _, ::clause(Head, Body), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_retract_fact_chk'(Obj, Head, _, p(p(p))), _, Obj::retract(Head), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_retract_fact_chk'(_, Head, _, p(_)), _, retract(Head), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_retract_fact_chk'(_, Head, _, p(_)), _, ::retract(Head), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_retract_rule_chk'(Obj, Clause, _, p(p(p))), _, Obj::retract(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_retract_rule_chk'(_, Clause, _, p(_)), _, retract(Clause), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_retract_rule_chk'(_, Clause, _, p(_)), _, ::retract(Clause), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'('$lgt_retractall_chk'(Obj, Head, _, p(p(p))), _, Obj::retractall(Head), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_retractall_chk'(_, Head, _, p(_)), _, retractall(Head), TermPos, TermPos) :- !.
'$lgt_swi_unify_clause_body'('$lgt_retractall_chk'(_, Head, _, p(_)), _, ::retractall(Head), TermPos, TermPos) :- !.

'$lgt_swi_unify_clause_body'(Goal, _, Goal, TermPos, TermPos).


'$lgt_swi_unify_clause_in_line_goal'(sender(_)).
'$lgt_swi_unify_clause_in_line_goal'(self(_)).
'$lgt_swi_unify_clause_in_line_goal'(this(_)).
'$lgt_swi_unify_clause_in_line_goal'(parameter(_, _)).
'$lgt_swi_unify_clause_in_line_goal'(true).
