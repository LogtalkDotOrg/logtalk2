
:- object(attvars_hook,
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2011/02/16,
		comment is 'Hook object for compiling objects and categories using attributed variables.']).

	term_expansion((attr_unify_hook(Att, Var) :- Body), [(:- multifile(Module:attr_unify_hook/2)), (Module:attr_unify_hook(Att, Var) :- Body)]) :-
		logtalk_load_context(entity_name, Module).

	term_expansion((attribute_goals(X) --> Body), [(:- multifile(Module:attribute_goals//1)), (Module:attribute_goals(X) --> Body)]) :-
		logtalk_load_context(entity_name, Module).

:- end_object.
