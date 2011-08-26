
:- object(numberlist,
	implements(numberlistp),
	extends(list)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'List of numbers predicates.']).

	average([], 0.0).
	average([N| Ns], Average) :-
		average(Ns, 1, N, Average).

	average([], Length, Sum, Average) :-
		Average is Sum / Length.
	average([N| Ns], Lacc, Sacc, Average) :-
		Lacc2 is Lacc + 1,
		Sacc2 is Sacc + N,
		average(Ns, Lacc2, Sacc2, Average).

	min([X| Xs], Min) :-
		min(Xs, X, Min).

	min([], Min, Min).
	min([X| Xs], Aux, Min) :-
		(	X < Aux ->
			min(Xs, X, Min)
		;	min(Xs, Aux, Min)
		).

	max([X| Xs], Max) :-
		max(Xs, X, Max).

	max([], Max, Max).
	max([X| Xs], Aux, Max) :-
		(	X > Aux ->
			max(Xs, X, Max)
		;	max(Xs, Aux, Max)
		).

	product([X| Xs], Product) :-
		product(Xs, X, Product).

	product([], Product, Product).
	product([X| Xs], Acc, Product) :-
		Acc2 is Acc * X,
		product(Xs, Acc2, Product).

	sum(List, Sum) :-
		sum(List, 0, Sum).

	sum([], Sum, Sum).
	sum([X| Xs], Acc, Sum) :-
		Acc2 is Acc + X,
		sum(Xs, Acc2, Sum).

	valid((-)) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		number(Element),
		valid(List).

	check(Term) :-
		this(This),
		sender(Sender),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
