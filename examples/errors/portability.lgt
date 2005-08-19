
:- object(portability).


	:- public(predicate/0).

	% clause with calls to non-ISO Prolog standard predicates
	predicate :-
		compare(Result, first, second),
		retractall(result(Result, _)),
		sort([], []),
		consult(file).


:- end_object.
