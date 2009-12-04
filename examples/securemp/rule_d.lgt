
:- object(wrong_arity).

	:- public(map/2).
	:- meta_predicate(map(2, *)).		% wrong meta-predicate arity

	map(Closure, Element) :-
		call(Closure, Element, _).

:- end_object.
