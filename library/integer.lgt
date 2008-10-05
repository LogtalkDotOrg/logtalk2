
:- object(integer,
	extends(number)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/10/5,
		comment is 'Integer data type predicates.']).

	:- public(between/3).
	:- mode(between(+integer, +integer, +integer), zero_or_one).
	:- mode(between(+integer, +integer, -integer), zero_or_more).
	:- info(between/3, [
		comment is 'Returns integers in the interval defined by the two first arguments.',
		argnames is ['Lower', 'Upper', 'Integer']]).

	:- public(plus/3).
	:- mode(plus(+integer, +integer, ?integer), zero_or_one).
	:- mode(plus(+integer, ?integer, +integer), zero_or_one).
	:- mode(plus(?integer, +integer, +integer), zero_or_one).
	:- info(plus/3, [
		comment is 'Reversible integer sum. At least two of the arguments must be instantiated to integers.',
		argnames is ['I', 'J', 'Sum']]).

	:- public(succ/2).
	:- mode(succ(+integer, ?integer), zero_or_one).
	:- mode(succ(?integer, +integer), zero_or_one).
	:- info(plus/2, [
		comment is 'Successor of a natural number. At least one of the arguments must be instantiated to a natural number.',
		argnames is ['I', 'J']]).

	between(Lower, Upper, Integer) :-
		integer(Lower),
		integer(Upper),
		(	var(Integer) ->
			Lower =< Upper,
			generate(Lower, Upper, Integer)
		;	integer(Integer),
			Lower =< Integer,
			Integer =< Upper
		).

	generate(Lower, _, Lower).
	generate(Lower, Upper, Integer) :-
		Lower < Upper,
		Next is Lower + 1,
		generate(Next, Upper, Integer).

    plus(I, J, Sum) :-
		integer(I),
		integer(J), !,
		Sum is I + J.
    plus(I, J, Sum) :-
		integer(I),
		integer(Sum), !,
		J is Sum - I.
    plus(I, J, Sum) :-
		integer(J),
		integer(Sum), !,
		I is Sum - J.

	succ(I, J) :-
		integer(I), !,
		I >= 0,
		J is I + 1.
	succ(I, J) :-
		integer(J),
		(	J =:= 0 ->
		   	fail
		;	J > 0,
			I is J - 1
		).

	valid(Integer) :-
		integer(Integer).

:- end_object.
