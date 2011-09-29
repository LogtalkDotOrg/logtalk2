
:- module(meta, [meta/1]).

:- meta_predicate(meta(0)).

meta(Goal) :-
	call(Goal).
