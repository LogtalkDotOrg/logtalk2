
:- object(streams).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Coinduction infinite streams example.']).

	% A stream of numbers.
	:- public(stream/1).
	:- coinductive(stream/1).

	stream([H| T]) :-
		num(H),
		stream(T).

	% A number.
	num(0).
	num(s(N)) :-
		num(N).

:- end_object.
