
:- module(test, [test/0, test/1, names/0, names/1]).

:- use_module(lists, [contained/2]).
:- use_module(metapreds, [meta/1]).

names :-
	contained(P, [paulo, carlos, helena]),
	write(P), nl,
	fail.
names.

names(Names) :-
	findall(Name, contained(Name, [paulo, carlos, helena]), Names).

test :-
	meta(names).

test(Names) :-
	meta(names(Names)).
