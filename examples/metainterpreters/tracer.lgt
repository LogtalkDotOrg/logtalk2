
:- object(tracer).

	:- info([
		version is 1.0,
		date is 2004/5/2,
		author is 'Paulo Moura',
		comment is 'Tracer meta-interpreter for pure Prolog.']).

	:- public(trace/1).
	:- mode(trace(+goal), zero_or_more).
	:- info(trace/1, [
		comment is 'Traces goal proof.',
		argnames is ['Goal']]).

	trace(Goal) :-
		trace(Goal, 1).

	trace(true, _).
	trace((A, B), Depth) :-
		!, trace(A, Depth), trace(B, Depth). 
	trace(A, Depth) :-
		clause(A, B),
		write_trace(A, Depth),
		Depth2 is Depth + 1,
		trace(B, Depth2).

	write_trace(Goal, Depth) :-
		write(Depth), write(': '), writeq(Goal), nl.

:- end_object.
