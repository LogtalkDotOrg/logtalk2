
:- object(cp1).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/24,
		comment is 'Coinduction example of finding the cyclic paths in a graph.']).

	:- public(path/2).
	:- coinductive(path/2).

	path(From, [From| Path]) :-
		arc(From, Next),
		path(Next, Path).

	arc(a, b).
	arc(b, b).	arc(b, c).
	arc(c, d).	arc(c, a).
	arc(d, d).

:- end_object.



:- object(cp2).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/24,
		comment is 'Coinduction example of finding the cyclic paths in a graph.']).

	:- public(path/2).
	:- coinductive(path/2).

	path(From, [From| Path]) :-
		arc(From, Next),
		path(Next, Path).

	arc(a, b).
	arc(b, c).
	arc(c, a).
	arc(c, d).
	arc(d, a).

:- end_object.
