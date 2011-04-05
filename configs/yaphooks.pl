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
%  integration code for YAP 6.0.2 and later versions to improve
%  usability when using the YAP profilers
%
%  last updated: February 24, 2011
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- op(600, xfy, ::).	% message-sending operator


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
