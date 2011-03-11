
:- object(attvars_hook,
	implements(expanding)).

	:- info([
		version is 0.5,
		author is 'Paulo Moura',
		date is 2011/03/11,
		comment is 'Hook object for compiling objects and categories using attributed variables.']).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		term_expansion((attr_unify_hook(Att, Var) :- Body), [{(:- multifile(attr_unify_hook/3))}, ({attr_unify_hook(Var, Entity, Att)} :- Body)]) :-
			logtalk_load_context(entity_name, Entity).
		term_expansion((attribute_goals(_) --> _), []).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		term_expansion((attr_unify_hook(Att, Var) :- Body), [{(:- import(from(/(install_verify_attribute_handler,4), machine)))},{(:- install_verify_attribute_handler(Entity,Att,Var,TAttrUnifyHookHead))},(attr_unify_hook(Att, Var) :- Body)]) :-
			logtalk_load_context(entity_name, Entity),
			logtalk::compile_predicate_heads(attr_unify_hook(Att, Var), TAttrUnifyHookHead).
		term_expansion((attribute_goals(X) --> Body), [{(:- import(from(/(install_attribute_portray_hook,3), machine)))},{(:- install_attribute_portray_hook(Entity,X,TAttrUnifyHookHead))},(attribute_goals(X) --> Body)]) :-
			logtalk_load_context(entity_name, Entity),
			logtalk::expand_term((attribute_goals(X) --> Body), (AttrUnifyHookHead :- _)),
			logtalk::compile_predicate_heads(AttrUnifyHookHead, TAttrUnifyHookHead).

	:- else.

		term_expansion((attr_unify_hook(Att, Var) :- Body), [(:- multifile(Entity:attr_unify_hook/2)), (Entity:attr_unify_hook(Att, Var) :- Body)]) :-
			logtalk_load_context(entity_name, Entity).
		term_expansion((attribute_goals(X) --> Body), [(:- multifile(Entity:attribute_goals//1)), (Entity:attribute_goals(X) --> Body)]) :-
			logtalk_load_context(entity_name, Entity).

	:- endif.

:- end_object.
