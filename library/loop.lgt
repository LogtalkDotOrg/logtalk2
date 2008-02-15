 
:- object(loop,
	implements(loopp)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/2/15,
		comment is 'Loop control structures predicates.']).

	:- meta_predicate(whiledo(::, ::)).
	whiledo(Condition, Action) :-
		(	call(Condition) ->
			\+ \+ call(Action),
			whiledo(Condition, Action)
		;	true
		).

	:- meta_predicate(dowhile(::, ::)).
	dowhile(Action, Condition) :-
		\+ \+ call(Action),
		whiledo(Condition, Action).

	:- meta_predicate(forto(*, *, ::)).
	forto(FirstExp, LastExp, Call) :-
		First is FirstExp,
		Last is LastExp,
		(	First =< Last ->
			\+ \+ call(Call),
			Next is First + 1,
			forto(Next, Last, Call)
		;	true
		).

	:- meta_predicate(forto(*, *, *, ::)).
	forto(Count, FirstExp, LastExp, Call) :-
		First is FirstExp,
		Last is LastExp,
		(	First =< Last ->
			\+ \+ (Count = First, call(Call)),
			Next is First + 1,
			forto(Count, Next, Last, Call)
		;	true
		).

	:- meta_predicate(fordownto(*, *, ::)).
	fordownto(FirstExp, LastExp, Call) :-
		First is FirstExp,
		Last is LastExp,
		(	First >= Last ->
			\+ \+ call(Call),
			Next is First - 1,
			fordownto(Next, Last, Call)
		;	true
		).

	:- meta_predicate(fordownto(*, *, *, ::)).
	fordownto(Count, FirstExp, LastExp, Call) :-
		First is FirstExp,
		Last is LastExp,
		(	First >= Last ->
			\+ \+ (Count = First, call(Call)),
			Next is First - 1,
			fordownto(Count, Next, Last, Call)
		;	true
		).

:- end_object.
