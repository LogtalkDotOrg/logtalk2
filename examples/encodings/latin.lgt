
:- encoding(iso_latin_1).	% this directive, when present, must be the first term in a source file


:- object(latin).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2005/04/24,
		comment is 'Simple test of the encoding/1 directive.']).

	:- public(name/1).
	:- mode(name(?atom), zero_or_more).
	:- info(name/1, [
		comment is 'Table of person names.',
		argnames is ['Name']]).

	name('Ant�nio Sim�es').
	name('C�tia Concei��o').
	name('Jo�o Ra�nho').
	name('Lu�s Ara�jo').

:- end_object.
