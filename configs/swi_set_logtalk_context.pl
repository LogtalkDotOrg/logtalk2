%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.31.7
%
%  a useful but fragile hack for SWI Prolog 5.6.x that allows setting the 
%  Logtalk top-level context to other object than the pseudo-object "user"
%  
%  this is useful mostly when debugging as all queries will be interpreted 
%  as being made within the current context (thus, plain Prolog calls must 
%  be made using the {}/1 control construct)
%
%  usage is simple: call the predicate set_logtalk_context(Obj) to switch 
%  the context to "Obj"; call set_logtalk_context(user) to return to the 
%  normal Logtak/Prolog top-level
%
%  as of Logtalk version 2.30.1 you may use in alternative the <</2 control
%  construct, which provides a more robust solution for making queries in 
%  the context of objects other than "user"
%
%  last updated: May 18, 2008
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- system_module.

:- dynamic('$lgt_current_context_'/2).

'$lgt_current_context_'(user, _).


set_logtalk_context(Obj) :-
	var(Obj),
	throw(error(instantiation_error, set_logtalk_context(Obj))).

set_logtalk_context(Obj) :-
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), set_logtalk_context(Obj))).

set_logtalk_context(Obj) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _, _, _),
	throw(error(existence_error(object, Obj), set_logtalk_context(Obj))).

set_logtalk_context(Obj) :-
	catch('$lgt_set_logtalk_context'(Obj), Error, (writeq(Error), nl, fail)).

'$lgt_set_logtalk_context'(user) :-
	!,
	retract('$lgt_current_context_'(_, HookRef)),
	(	nonvar(HookRef) -> 
		erase(HookRef)
	;	true
	),
	assertz('$lgt_current_context_'(user, _)),
	'$set_prompt'('?- ').

'$lgt_set_logtalk_context'(Obj) :-
	retract('$lgt_current_context_'(Old, _)),
	(	Old == user ->
		'$lgt_expand_query_hook'(Hook),
		asserta(Hook, HookRef)
	;	true
	),
	assertz('$lgt_current_context_'(Obj, HookRef)),
	term_to_atom(Obj, ObjAtom),
	concat_atom(['[', ObjAtom, '] ?- '], Prompt),
	'$set_prompt'(Prompt).

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).

'$lgt_expand_query_hook'((user:expand_query(Query, Expanded, Bindings, Bindings) :-
	'$lgt_current_context_'(Obj, _),
	'$lgt_current_object_'(Obj, Prefix, _, _, _, _, _, _),
	'$lgt_ctx_ctx'(Ctx, _, Obj, Obj, Obj, Prefix, [], _),
	'$lgt_tr_body'(Query, TQuery, DQuery, Ctx),
	(	'$lgt_dbg_debugging_', '$lgt_debugging_'(Obj) ->
		Expanded = catch(DQuery, Error, '$lgt_runtime_error_handler'(Error))
	;	Expanded = catch(TQuery, Error, '$lgt_runtime_error_handler'(Error))
	))).
