
:- object(attvars_hook,
	implements(expanding)).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2011/03/09,
		comment is 'Hook object for compiling objects and categories using attributed variables.']).

	term_expansion((attr_unify_hook(Att, Var) :- Body), [(:- multifile(Module:attr_unify_hook/2)), (Module:attr_unify_hook(Att, Var) :- Body)]) :-
		logtalk_load_context(entity_name, Module).

	term_expansion((attr_unify_hook(This, Att, Var) :- Body), [(:- multifile(Module:attr_unify_hook/3)), (Module:attr_unify_hook(This, Att, Var) :- Body)]) :-
		logtalk_load_context(entity_name, Entity),
		functor(Entity, Module, _).

	term_expansion((attribute_goals(X) --> Body), [(:- multifile(Module:attribute_goals//1)), (Module:attribute_goals(X) --> Body)]) :-
		logtalk_load_context(entity_name, Module).

	term_expansion((attribute_goals(This, X) --> Body), [(:- multifile(Module:attribute_goals//2)), (Module:attribute_goals(This, X) --> Body)]) :-
		logtalk_load_context(entity_name, Entity),
		functor(Entity, Module, _).

:- end_object.
