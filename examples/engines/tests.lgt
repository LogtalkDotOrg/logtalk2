
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "engines" example.']).

	test(engines_1) :-
		findall(P, sedan::current_predicate(P), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == [bore_stroke/2, capacity/1, cylinders/1, fuel/1, horsepower_rpm/2, reference/1].

	test(engines_2) :-
		findall(P, coupe::current_predicate(P), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == [bore_stroke/2, capacity/1, cylinders/1, fuel/1, horsepower_rpm/2, reference/1].

	test(engines_3) :-
		findall(Name-Cylinders-HP-RPM, sedan::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == ['M180.940'-6-94-4800].

	test(engines_4) :-
		findall(Name-Cylinders-HP-RPM, coupe::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == ['M180.941'-6-115-3657].

:- end_object.
