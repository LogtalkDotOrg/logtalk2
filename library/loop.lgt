 
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

	:- meta_predicate(foreach(*, *, ::)).
	foreach(Count, List, Goal) :-
		foreach_inv(List, Count, Goal).

	foreach_inv([], _, _).
	foreach_inv([Element| List], Count, Goal) :-
		\+ \+ (Count = Element, call(Goal)),
		foreach_inv(List, Count, Goal).

	:- meta_predicate(forto(*, *, ::)).
	forto(FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		(	First =< Last ->
			\+ \+ call(Goal),
			Next is First + 1,
			forto(Next, Last, Goal)
		;	true
		).

	:- meta_predicate(forto(*, *, *, ::)).
	forto(Count, FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		(	First =< Last ->
			\+ \+ (Count = First, call(Goal)),
			Next is First + 1,
			forto(Count, Next, Last, Goal)
		;	true
		).

	:- meta_predicate(fordownto(*, *, ::)).
	fordownto(FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		(	First >= Last ->
			\+ \+ call(Goal),
			Next is First - 1,
			fordownto(Next, Last, Goal)
		;	true
		).

	:- meta_predicate(fordownto(*, *, *, ::)).
	fordownto(Count, FirstExp, LastExp, Goal) :-
		First is FirstExp,
		Last is LastExp,
		(	First >= Last ->
			\+ \+ (Count = First, call(Goal)),
			Next is First - 1,
			fordownto(Count, Next, Last, Goal)
		;	true
		).

:- end_object.
