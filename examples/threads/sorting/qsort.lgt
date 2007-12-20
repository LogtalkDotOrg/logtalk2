
:- object(qsort(_Threads)).

	:- info([
		version is 1.1,
		author is 'Paul Croker and Paulo Moura',
		date is 2007/12/19,
		comment is 'Multi-threaded version of the quick sort algorithm.',
		parameters is ['Threads'- 'Number of threads to use in sorting. Valid values are 1, 2, 4, 8, 16, 32, etc.']]).

	:- threaded.

	:- public(qsort/2).
	:- mode(qsort(+list, -list), one).
	:- info(qsort/2, [
		comment is 'Sorts a list of terms into ascending order.',
		argnames is ['List', 'Sorted']]).

	qsort(List, Sorted) :-
		parameter(1, Threads),
		Forks is Threads//2 + 1,
		qsort(List, [], Sorted, Forks).

	qsort([], Sorted, Sorted, _).
	qsort([Pivot| Rest], Acc, Sorted, Forks) :-
		(	Forks =:= 1 ->
			quicksort([Pivot| Rest], Acc, Sorted)
		;	Forks > 1,
			Forks1 is Forks//2,
			Forks2 is Forks - Forks1,
			partition(Rest, Pivot, Smaller0, Bigger0),
			threaded((
				qsort(Smaller0, [Pivot| Bigger], Sorted, Forks1),
				qsort(Bigger0, Acc, Bigger, Forks2)
			))
		).

    partition([], _, [], []).
    partition([X| Xs], Pivot, Smalls, Bigs) :-
		(	X < Pivot ->
			Smalls = [X| Rest],
			partition(Xs, Pivot, Rest, Bigs)
		;	Bigs = [X| Rest],
			partition(Xs, Pivot, Smalls, Rest)
		).

    quicksort([], Sorted, Sorted).
    quicksort([Pivot| Rest], Acc, Sorted) :- 
		partition(Rest, Pivot, Smaller0, Bigger0),
		quicksort(Smaller0, [Pivot| Bigger], Sorted),
		quicksort(Bigger0, Acc, Bigger).

:- end_object.
