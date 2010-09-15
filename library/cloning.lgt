
:- protocol(cloning).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/09/14,
		comment is 'Object cloning protocol.']).

	:- public(clone/1).
	:- mode(clone(?object), zero_or_one).
	:- info(clone/1, [
		comment is 'Clones an object, returning the identifier of the new object if none is given.',
		argnames is ['Clone']]).

:- end_protocol.
