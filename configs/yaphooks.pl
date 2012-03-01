%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.44.0
%
%  Copyright (c) 1998-2012 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  integration code for YAP 6.0.2 and later versions to improve
%  usability when using the YAP profilers
%
%  last updated: August 13, 2011
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
user:prolog_predicate_name(user:'$lgt_call_in_this'(_, _), 'call/1') :- !.

user:prolog_predicate_name(user:'$lgt_send_to_obj'(_, _, _), '::/2 (not cached; event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_obj_ne'(_, _, _), '::/2 (not cached; not event-aware)') :- !.
user:prolog_predicate_name(user:'$lgt_send_to_self'(_, _, _), '::/1 (not cached)') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_same'(_, _, _), '^^/2 (not cached; from obj; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_obj_super_call_other'(_, _, _), '^^/2 (not cached; from obj; diff pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_same'(_, _, _), '^^/2 (not cached; from ctg; same pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_super_call_other'(_, _, _), '^^/2 (not cached; from ctg; diff pred)') :- !.
user:prolog_predicate_name(user:'$lgt_ctg_call'(_, _, _), ':/1 (not cached)') :- !.

user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall'(_, _, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_this'(_, _, _, _, _), 'call/N') :- !.
user:prolog_predicate_name(user:'$lgt_metacall_sender'(_, _, _), 'call/N') :- !.

user:prolog_predicate_name(user:'$lgt_expand_term'(_, _, _, _, _), 'expand_term/2') :- !.
user:prolog_predicate_name(user:'$lgt_expand_goal'(_, _, _, _, _), 'expand_goal/2') :- !.

user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _), 'phrase/2') :- !.
user:prolog_predicate_name(user:'$lgt_phrase'(_, _, _, _), 'phrase/3') :- !.

user:prolog_predicate_name(user:'$lgt_abolish_checked'(_, _, _, _), 'abolish/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_fact_checked'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_asserta_rule_checked'(_, _, _, _, _), 'asserta/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_fact_checked'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_assertz_rule_checked'(_, _, _, _, _), 'assertz/1') :- !.
user:prolog_predicate_name(user:'$lgt_clause_checked'(_, _, _, _, _), 'clause/2') :- !.
user:prolog_predicate_name(user:'$lgt_retract_fact_checked'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retract_rule_checked'(_, _, _, _), 'retract/1') :- !.
user:prolog_predicate_name(user:'$lgt_retractall_checked'(_, _, _, _), 'retractall/1') :- !.

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
