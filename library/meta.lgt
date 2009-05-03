
:- object(meta,
	implements(metap)).

	:- info([
		version is 2.5,
		date is 2009/5/2,
		author is 'Paulo Moura',
		comment is 'Some useful meta-predicates.']).

	:- alias(metap, map/2, succeeds/2).
	:- alias(metap, include/3, filter/3).

	callable(Term) :-
		nonvar(Term),
		functor(Term, Functor, _),
		atom(Functor).

	:- meta_predicate(include_(*, 1, *)).
	include_([], _, []).
	include_([Arg| Args], Closure, Included) :-
		(	call(Closure, Arg) ->
			Included = [Arg| Rest]
		;	Included = Rest
		),
		include_(Args, Closure, Rest).

	:- meta_predicate(include(1, *, *)).
	include(Closure, List, Included) :-
		include_(List, Closure, Included).

	:- meta_predicate(filter(1, *, *)).
	filter(Closure, List, Included) :-
		include_(List, Closure, Included).

	:- meta_predicate(exclude_(*, 1, *)).
	exclude_([], _, []).
	exclude_([Arg| Args], Closure, Excluded) :-
		(	call(Closure, Arg) ->
			Excluded = Rest
		;	Excluded = [Arg| Rest]
		),
		exclude_(Args, Closure, Rest).

	:- meta_predicate(exclude(1, *, *)).
	exclude(Closure, List, Excluded) :-
		exclude_(List, Closure, Excluded).

	:- meta_predicate(partition_(*, 1, *, *)).
	partition_([], _, [], []).
	partition_([Arg| Args], Closure, Included, Excluded) :-
		(   call(Closure, Arg) ->
			Included = [Arg| RestIncluded],
			Excluded = RestExcluded
		;	Included = RestIncluded,
			Excluded = [Arg| RestExcluded]
		),
		partition_(Args, Closure, RestIncluded, RestExcluded).

	:- meta_predicate(partition(1, *, *, *)).
    partition(Closure, List, Included, Excluded) :-
		partition_(List, Closure, Included, Excluded).

	:- meta_predicate(ignore(::)).
	ignore(Goal) :-
		(	call(Goal) ->
			true
		;	true
		).

	:- meta_predicate(fold_left_(*, 3, *, *)).
	fold_left_([], _, Result, Result).
	fold_left_([Arg| Args], Closure, Acc, Result) :-
		call(Closure, Acc, Arg, Acc2),
		fold_left_(Args, Closure, Acc2, Result).

	:- meta_predicate(fold_left(3, *, *, *)).
	fold_left(Closure, Acc, List, Result) :-
		fold_left_(List, Closure, Acc, Result).

	:- meta_predicate(scan_left_(*, 3, *, *)).
	scan_left_([], _, Result, [Result]).
	scan_left_([Arg| Args], Closure, Acc, [Acc| Results]) :-
		call(Closure, Acc, Arg, Acc2),
		scan_left_(Args, Closure, Acc2, Results).

	:- meta_predicate(scan_left(3, *, *, *)).
	scan_left(Closure, Acc, List, Results) :-
		scan_left_(List, Closure, Acc, Results).

	:- meta_predicate(fold_right_(*, 3, *, *)).
	fold_right_([], _, Result, Result).
	fold_right_([Arg| Args], Closure, Acc, Result) :-
		fold_right_(Args, Closure, Acc, Acc2),
		call(Closure, Arg, Acc2, Result).

	:- meta_predicate(fold_right(3, *, *, *)).
	fold_right(Closure, Acc, List, Result) :-
		fold_right_(List, Closure, Acc, Result).

	:- meta_predicate(scan_right_(*, 3, *, *)).
	scan_right_([], _, Result, [Result]).
	scan_right_([Arg| Args], Closure, Acc, [Result, Acc2| Results]) :-
		scan_right_(Args, Closure, Acc, [Acc2| Results]),
		call(Closure, Arg, Acc2, Result).

	:- meta_predicate(scan_right(3, *, *, *)).
	scan_right(Closure, Acc, List, Results) :-
		scan_right_(List, Closure, Acc, Results).

	:- meta_predicate(map_(*, 1)).
	map_([], _).
	map_([Head| Tail], Closure) :-
		call(Closure, Head),
		map_(Tail, Closure).

	:- meta_predicate(map(1, *)).
	map(Closure, List) :-
		map_(List, Closure).

	:- meta_predicate(succeeds(1, *)).
	succeeds(Closure, List) :-
		map_(List, Closure).

	:- meta_predicate(map_(*, 2, *)).
	map_([], _, []).
	map_([A| As], Closure, [B| Bs]) :-
		call(Closure, A, B),
		map_(As, Closure, Bs).

	:- meta_predicate(map(2, *, *)).
	map(Closure, As, Bs) :-
		map_(As, Closure, Bs).

	:- meta_predicate(map_(*, 3, *, *)).
	map_([], _, [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs]) :-
		call(Closure, A, B, C),
		map_(As, Closure, Bs, Cs).

	:- meta_predicate(map(3, *, *, *)).
	map(Closure, As, Bs, Cs) :-
		map_(As, Closure, Bs, Cs).

	:- meta_predicate(map_(*, 4, *, *, *)).
	map_([], _, [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds]) :-
		call(Closure, A, B, C, D),
		map_(As, Closure, Bs, Cs, Ds).

	:- meta_predicate(map(4, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds) :-
		map_(As, Closure, Bs, Cs, Ds).

	:- meta_predicate(map_(*, 5, *, *, *, *)).
	map_([], _, [], [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds], [E| Es]) :-
		call(Closure, A, B, C, D, E),
		map_(As, Closure, Bs, Cs, Ds, Es).

	:- meta_predicate(map(5, *, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds, Es) :-
		map_(As, Closure, Bs, Cs, Ds, Es).

	:- meta_predicate(map_(*, 6, *, *, *, *, *)).
	map_([], _, [], [], [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds], [E| Es], [F| Fs]) :-
		call(Closure, A, B, C, D, E, F),
		map_(As, Closure, Bs, Cs, Ds, Es, Fs).

	:- meta_predicate(map(6, *, *, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds, Es, Fs) :-
		map_(As, Closure, Bs, Cs, Ds, Es, Fs).

	:- meta_predicate(map_(*, 7, *, *, *, *, *, *)).
	map_([], _, [], [], [], [], [], []).
	map_([A| As], Closure, [B| Bs], [C| Cs], [D| Ds], [E| Es], [F| Fs], [G| Gs]) :-
		call(Closure, A, B, C, D, E, F, G),
		map_(As, Closure, Bs, Cs, Ds, Es, Fs, Gs).

	:- meta_predicate(map(7, *, *, *, *, *, *, *)).
	map(Closure, As, Bs, Cs, Ds, Es, Fs, Gs) :-
		map_(As, Closure, Bs, Cs, Ds, Es, Fs, Gs).

:- end_object.
