
:- object(automata).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Coinduction omega-automaton example.']).

	:- public(automata/2).
	:- coinductive1(automata/2).

	automata([X| T], St) :-
		trans(St, X, NewSt),
		automata(T, NewSt).
%	automata([], St) :-		% we drop the base case in order
%		final(St).			% to get an omega-automaton

	trans(s0, a, s1).
	trans(s1, b, s2).
	trans(s2, c, s3).
	trans(s2, e, s0).
	trans(s3, d, s0).

	final(s2).

:- end_object.
