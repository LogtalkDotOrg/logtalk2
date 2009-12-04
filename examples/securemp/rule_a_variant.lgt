
:- initialization(catch((client_a_variant::double([1,2,3], Doubles), write(Doubles)), Error, writeq(Error))).


:- object(library_a_variant).

	:- meta_predicate(map_(*, 2, *)).
	map_([], _, []).
	map_([X| Xs], Closure, [Y| Ys]) :-
		call(Closure, X, Y),
		map_(Xs, Closure, Ys).

	:- public(map/3).
	:- meta_predicate(map(*, 2, *)).
	map(In, Closure, Out) :- 
		(	Closure = scale(_) -> 
			map_(In, scale(3), Out)		% the second argument will trigger a
		;	map_(In, Closure, Out)		% runtime error because scale/3 is not
		).								% defined in "library_a_variant"

:- end_object.


:- object(client_a_variant).

	:- public(double/2).
	double(Ints, Doubles) :-
		library_a_variant::map(Ints, scale(2), Doubles).

	scale(Scale, X, Xscaled) :-
		Xscaled is X*Scale.

:- end_object.
