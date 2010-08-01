
:- object(streams).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Coinduction infinite streams example.']).

	:- public(nat_stream/1).
	:- coinductive1(nat_stream/1).

	nat_stream([H| T]) :-
		nat(H),
		nat_stream(T).

	nat(0).
	nat(s(N)) :-
		nat(N).

	:- public(bit_stream/1).
	:- coinductive1(bit_stream/1).

	bit_stream([H| T]) :-
		bit(H),
		bit_stream(T).

	bit(0).
	bit(1).

:- end_object.
