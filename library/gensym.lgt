

:- object(gensym).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/10/06,
		comment is 'Predicates for generating unique atoms. Object protocol based on the "gensym" module of SWI-Prolog.']).

	:- public(reset_gensym/0).
	:- synchronized(reset_gensym/0).
	:- mode(reset_gensym, one).
	:- info(reset_gensym/0, [
		comment is 'Resets the generator counter for all bases.']).

	:- public(reset_gensym/1).
	:- synchronized(reset_gensym/1).
	:- mode(reset_gensym(-atom), one).
	:- info(reset_gensym/1, [
		comment is 'Resets the generator counter for a given base.',
		argnames is ['Base']]).

	:- public(gensym/2).
	:- synchronized(gensym/2).
	:- mode(gensym(+atom, -atom), one).
	:- info(gensym/2, [
		comment is 'Returns a new unique atom with a given base (prefix).',
		argnames is ['Base', 'Unique']]).

	:- private(base_/2).
	:- dynamic(base_/2).
	:- mode(base_(?atom, ?integer), zero_or_more).
	:- info(base_/2, [
		comment is 'Table of gebnerator bases and respective counters.',
		argnames is ['Base', 'Counter']]).

	reset_gensym :-
		retract(base_(Base, _)),
		asserta(base_(Base, 0)),
		fail.
	reset_gensym.

	reset_gensym(Base) :-
		retractall(base_(Base, _)),
		asserta(base_(Base, 0)).

	gensym(Base, Unique) :-
		(	retract(base_(Base, OldInt)) ->
			NewInt is OldInt + 1
		;	NewInt is 0
		),
		asserta(base_(Base, NewInt)),
		number_codes(NewInt, NewCodes),
		atom_codes(NewAtom, NewCodes),
		atom_concat(Base, NewAtom, Unique).

:- end_object.
