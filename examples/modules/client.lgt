
:- module(client, [test/0, test/1, names/0, names/1]).

:- use_module(lists).	% not all back-end Prolog compilers allow the
:- use_module(meta).	% use_module/1 directive to be supported

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
