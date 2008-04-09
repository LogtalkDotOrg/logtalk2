
:- object(object).

	:- info([
		version is 1.0,
		author is pm,
		date is 2008/4/9,
		comment is 'Simple example of using compilation hooks and term expansion for conditional compilation of debug statements.']).

	:- public(append/3).

	append([], List, List) :-
		debug((write('Base case: '), writeq(append([], List, List)), nl)).
	append([Head| Tail], List, [Head| Tail2]) :-
		debug((write('Recursive case: '), writeq(append(Tail, List, Tail2)), nl)),
		append(Tail, List, Tail2).

:- end_object.
