
:- object(meta,
	implements(metap)).

	:- info([
		version is 2.2,
		date is 2008/10/5,
		author is 'Paulo Moura',
		comment is 'Some useful meta-predicates.']).

	callable(Term) :-
		nonvar(Term),
		functor(Term, Functor, _),
		atom(Functor).

	:- meta_predicate(filter(1, *, *)).
	filter(_, [], []) :- !.
	filter(Closure, [Arg| Args], In) :-
		(	call(Closure, Arg) ->
			In = [Arg| RIn]
		;	In = RIn
		),
		filter(Closure, Args, RIn).

	:- meta_predicate(partition(1, *, *, *)).
    partition(_, [], [], []) :- !.
    partition(Closure, [Arg| Args], In, Out) :-
        (   call(Closure, Arg) ->
	        In = [Arg| RIn],
	        Out = ROut
        ;   In = RIn,
            Out = [Arg| ROut]
        ),
        partition(Closure, Args, RIn, ROut).

	:- meta_predicate(ignore(::)).
	ignore(Goal) :-
		(	call(Goal) ->
			true
		;	true
		).

	:- meta_predicate(map(2, *, *)).
	map(_, [], []).
	map(Closure, [A| As], [B| Bs]) :-
		call(Closure, A, B),
		map(Closure, As, Bs).

	:- meta_predicate(map(3, *, *, *)).
	map(_, [], [], []).
	map(Closure, [A| As], [B| Bs], [C| Cs]) :-
		call(Closure, A, B, C),
		map(Closure, As, Bs, Cs).

	:- meta_predicate(map(4, *, *, *, *)).
	map(_, [], [], [], []).
	map(Closure, [A| As], [B| Bs], [C| Cs], [D| Ds]) :-
		call(Closure, A, B, C, D),
		map(Closure, As, Bs, Cs, Ds).

	:- meta_predicate(succeeds(1, *)).
	succeeds(_, []).
	succeeds(Closure, [Head| Tail]) :-
		call(Closure, Head),
		succeeds(Closure, Tail).

:- end_object.
