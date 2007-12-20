
:- object(hanoi(_Threads)).

	:- info([
		version is 1.0,
		date is 2007/12/19,
		author is 'Paulo Moura',
		comment is 'Multi-threaded version of the Towers of Hanoi problem.',
		parameters is ['Threads'- 'Number of threads to use. Valid values are 1, 2, 4, 8, 16, etc.']]).

	:- threaded.

	:- public(run/1).
	:- mode(run(+integer), one).
	:- info(run/1, [
		comment is 'Solves the towers of Hanoi problem for the specified number of disks.',
		argnames is ['Disks']]).

	run(Disks) :-
		parameter(1, Threads),
		Forks is Threads//2 + 1,
		mt_move(Forks, Disks, left, middle, right).

	mt_move(1, Disks, Left, Aux, Right) :-
		st_move(Disks, Left, Aux, Right).
	mt_move(Forks, Disks, Left, Aux, Right) :-
		Forks > 1,
		Forks1 is Forks//2,
		Forks2 is Forks - Forks1,
		Disks2 is Disks - 1,
		threaded((
			mt_move(Forks1, Disks2, Left, Right, Aux),
			mt_move(Forks2, Disks2, Aux, Left, Right)
		)).

	st_move(1, _Left, _, _Right):-
		!.
	st_move(Disks, Left, Aux, Right):-
		Disks2 is Disks - 1,
		st_move(Disks2, Left, Right, Aux),
		st_move(Disks2, Aux, Left, Right).

:- end_object.
