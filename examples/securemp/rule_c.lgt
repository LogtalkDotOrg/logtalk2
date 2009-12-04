
:- object(illegal_call).

	:- public(map/2).
	:- meta_predicate(map(1, *)).

	map(Closure, Element) :-
		call(Closure, Element, _).	% illegal call; results in a compile time error

:- end_object.
