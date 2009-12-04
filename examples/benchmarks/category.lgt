
:- category(category).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/06/10,
		comment is 'Defines a simple predicate for comparing performance of calls from within an object to imported category predicates.']).

	:- public(ctg_pred/0).

	ctg_pred :-
		{generate_list(20, List)},
		length(List, _).

	length(List, Length) :-
		length(List, 0, Length).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

:- end_category.
