
:- object(meta,
	implements(metap)).

	:- info([
		version is 2.0,
		date is 2006/9/11,
		author is 'Paulo Moura',
		comment is 'Some useful meta-predicates.']).

	callable(Term) :-
		nonvar(Term),
		functor(Term, Functor, _),
		atom(Functor).

	:- metapredicate(filter(1, *, *)).
	filter(_, [], []) :- !.
	filter(Closure, [Arg| Args], List) :-
		(	call(Closure, Arg) ->
			List = [Arg| Args2]
		;	List = Args2
		),
		filter(Closure, Args, Args2).

	:- metapredicate(ignore(::)).
	ignore(Goal) :-
		(	call(Goal) ->
			true
		;	true
		).

	:- metapredicate(map(2, *, *)).
	map(_, [], []).
	map(Closure, [Old| Olds], [New| News]) :-
		call(Closure, Old, New),
		map(Closure, Olds, News).

	:- metapredicate(succeeds(1, *)).
	succeeds(_, []).
	succeeds(Closure, [Head| Tail]) :-
		call(Closure, Head),
		succeeds(Closure, Tail).

:- end_object.
