
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "roots" example.']).

	test(roots_1) :-
		object::ancestors(Ancestors),
		list::msort(Ancestors,AncestorsSorted),
		AncestorsSorted == [abstract_class,class, object].

	test(roots_2) :-
		class::instances(Instances),
		list::msort(Instances,InstancesSorted),
		InstancesSorted == [abstract_class,class, object].

	test(roots_3) :-
		findall(Super, class::superclass(Super), Solutions),
		list::msort(Solutions,SolutionsSorted),
		SolutionsSorted == [abstract_class,object].

:- end_object.
