
bar :-
	write('bar predicate called'), nl.


:- object(bypass).

	:- public(foo//0).

	foo --> {{bar}}.

:- end_object.
