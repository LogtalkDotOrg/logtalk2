
:- object(action_rules_hook,
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2010/11/12,
		comment is 'Hook object for compiling objects and categories containing B-Prolog action rules.']).

	term_expansion((:- Directive), [(:- Directive), (:- annotation('=>'(0,0)))]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		).

:- end_object.
