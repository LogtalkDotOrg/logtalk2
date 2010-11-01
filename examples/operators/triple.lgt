
:- object(triple).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2010/11/01,
		comment is 'Read and asserts a simple table of facts from a file for testing operator handling code.']).

	:- public(triple/2).
	:- dynamic(triple/2).

	:- op(500, xfx, triple).

	:- initialization((
		retractall(triple(_, _)),	% for avoiding unit test failures when reloading
		read_from_file				% load the data from the "triple.txt" text file
	)).

	read_from_file :-
		open('triple.txt', read, Stream),
		read(Stream, Term),
		process(Stream, Term).

	process(Stream, end_of_file) :-
		close(Stream),
		!.
	process(Stream, Term) :-
		assertz(Term),
		read(Stream, Next),
		process(Stream, Next).

:- end_object.
