
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "operators" example.']).

	test(operators_1) :-
		findall(I-J, double::double(I, J), Solutions),
		Solutions == [1-2, 2-4, 3-6].

	test(operators_2) :-
		triple::read_from_file,
		findall(I-J, triple::triple(I, J), Solutions),
		Solutions == [1-3, 2-6, 3-9].

	% test 3.  % couldn't really test the interesting cases because of compilation errors
	test(operators_3) :-
		findall(N1-N2, graph1::edge(N1, N2), Solutions),
		Solutions == [a-b, a-c, b-d, c-d].

	test(operators_4) :-
		findall(Path, graph1::path(a, d, Path), Solutions),
		Solutions == [[a,b,d], [a,c,d]].

	test(operators_5) :-
		\+ current_op(_P, _T, edge).

:- end_object.
