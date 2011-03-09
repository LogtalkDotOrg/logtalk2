
:- object(attvars_hook,
	implements(expanding)).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2011/03/09,
		comment is 'Hook object for compiling objects and categories using attributed variables.']).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		term_expansion((attr_unify_hook(Att, Var) :- Body), [{(:- multifile(attr_unify_hook/3))}, ({attr_unify_hook(Var, Prefix, Att)} :- Body)]) :-
			logtalk_load_context(entity_prefix, Prefix).
		term_expansion((attribute_goals(_) --> _), []).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		term_expansion((attr_unify_hook(Att, Var) :- Body), [{(:- import(from(/(install_verify_attribute_handler,4), machine)))},{(:- install_verify_attribute_handler(Prefix,Att,Var,TAttrUnifyHookHead))},(attr_unify_hook(Att, Var) :- Body)]) :-
			logtalk_load_context(entity_prefix, Prefix),
			logtalk::compile_predicate_heads(attr_unify_hook(Att, Var), TAttrUnifyHookHead).
		term_expansion((attribute_goals(X) --> Body), [{(:- import(from(/(install_attribute_portray_hook,3), machine)))},{(:- install_attribute_portray_hook(Prefix,X,TAttrUnifyHookHead))},(attribute_goals(X) --> Body)]) :-
			logtalk_load_context(entity_prefix, Prefix),
			logtalk::expand_term((attribute_goals(X) --> Body), (AttrUnifyHookHead :- _)),
			logtalk::compile_predicate_heads(AttrUnifyHookHead, TAttrUnifyHookHead).

	:- else.

		term_expansion((attr_unify_hook(Att, Var) :- Body), [(:- multifile(Prefix:attr_unify_hook/2)), (Prefix:attr_unify_hook(Att, Var) :- Body)]) :-
			logtalk_load_context(entity_prefix, Prefix).
		term_expansion((attribute_goals(X) --> Body), [(:- multifile(Prefix:attribute_goals//1)), (Prefix:attribute_goals(X) --> Body)]) :-
			logtalk_load_context(entity_prefix, Prefix).

	:- endif.

	goal_expansion(get_attr(Var, Entity, Value), get_attr(Var, Prefix, Value)) :-
		logtalk_load_context(entity_name, Entity),
		logtalk_load_context(entity_prefix, Prefix),
		Entity \= Prefix.
	goal_expansion(put_attr(Var, Entity, Value), put_attr(Var, Prefix, Value)) :-
		logtalk_load_context(entity_name, Entity),
		logtalk_load_context(entity_prefix, Prefix),
		Entity \= Prefix.
	goal_expansion(del_attr(Var, Entity), del_attr(Var, Prefix)) :-
		logtalk_load_context(entity_name, Entity),
		logtalk_load_context(entity_prefix, Prefix),
		Entity \= Prefix.

:- end_object.
