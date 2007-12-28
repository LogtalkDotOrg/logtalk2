
:- object(hanoi(_Threads)).

	:- info([
		version is 1.1,
		date is 2007/12/27,
		author is 'Paulo Moura',
		comment is 'Multi-threaded version of the Towers of Hanoi problem.',
		parameters is ['Threads'- 'Number of threads to use. Valid values are 1, 2, 4, 8, 16, etc.']]).

	:- threaded.

	:- public(run/2).
	:- mode(run(+integer, -list), one).
	:- info(run/2, [
		comment is 'Solves the towers of Hanoi problem for the specified number of disks.',
		argnames is ['Disks', 'Moves']]).

	run(Disks, Moves) :-
		parameter(1, Threads),
		mt_move(Threads, Disks, left, middle, right, [], Moves).

	mt_move(1, Disks, Left, Aux, Right, Acc, Moves) :- !,
		st_move(Disks, Left, Aux, Right, Acc, Moves).
	mt_move(Threads, Disks, Left, Aux, Right, Acc, Moves) :-
		Threads > 1,
		Threads2 is Threads//2,
		Disks > 1,
		Disks2 is Disks - 1,
		threaded((
			mt_move(Threads2, Disks2, Left, Right, Aux, [Left-Right| Acc2], Moves),
			mt_move(Threads2, Disks2, Aux, Left, Right, Acc, Acc2)
		)).

	st_move(1, Left, _, Right, Acc, [Left-Right| Acc]) :- !.
	st_move(Disks, Left, Aux, Right, Acc, Moves) :-
		Disks > 1,
		Disks2 is Disks - 1,
		st_move(Disks2, Left, Right, Aux, [Left-Right| Acc2], Moves),
		st_move(Disks2, Aux, Left, Right, Acc, Acc2).

	write_moves([]).
	write_moves([Move| Moves]) :-
		write_move(Move), nl,
		write_moves(Moves).

	write_move(Pole1-Pole2) :-
		write('Move a disk from '),
		writeq(Pole1),
		write(' to '),
		writeq(Pole2),
		write('.'),
		nl.

:- end_object.
