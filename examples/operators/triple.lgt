
:- object(triple).


	:- public(triple/2).
	:- dynamic(triple/2).

	:- op(500, xfx, triple).

	:- initialization(read_from_file).


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
