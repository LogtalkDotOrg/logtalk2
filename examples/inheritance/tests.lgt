
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "inheritance" example.']).

	test(inheritance_1) :-
		parent::current_predicate(public/0),
		\+ parent::current_predicate(protected/0),
		\+ parent::current_predicate(private/0),
		parent << predicate_property(public, public),
		parent << predicate_property(protected, protected),
		parent << predicate_property(private, private).

	test(inheritance_2) :-
		prototype1::current_predicate(public/0),
		\+ prototype1::current_predicate(protected/0),
		\+ prototype1::current_predicate(private/0),
		prototype1 << predicate_property(public, public),
		prototype1 << predicate_property(protected, protected),
		\+ prototype1 << predicate_property(private, _).

	test(inheritance_3) :-
		\+ prototype2::current_predicate(public/0),
		\+ prototype2::current_predicate(protected/0),
		\+ prototype2::current_predicate(private/0),
		prototype2 << predicate_property(public, protected),
		prototype2 << predicate_property(protected, protected),
		\+ prototype1 << predicate_property(private, _).

	test(inheritance_4) :-
		\+ prototype3::current_predicate(public/0),
		\+ prototype3::current_predicate(protected/0),
		\+ prototype3::current_predicate(private/0),
		prototype3 << predicate_property(public, private),
		prototype3 << predicate_property(protected, private),
		\+ prototype3 << predicate_property(private, _).

	test(inheritance_5) :-
		descendant1::current_predicate(public/0),
		\+ descendant1::current_predicate(protected/0),
		\+ descendant1::current_predicate(private/0),
		descendant1 << predicate_property(public, public),
		descendant1 << predicate_property(protected, protected),
		\+ descendant1 << predicate_property(private, _).

	test(inheritance_6) :-
		\+ descendant2::current_predicate(public/0),
		\+ descendant2::current_predicate(protected/0),
		\+ descendant2::current_predicate(private/0),
		descendant2 << predicate_property(public, protected),
		descendant2 << predicate_property(protected, protected),
		\+ descendant2 << predicate_property(private, _).

	test(inheritance_8) :-
		\+ descendant3::current_predicate(public/0),
		\+ descendant3::current_predicate(protected/0),
		\+ descendant3::current_predicate(private/0),
		\+ descendant3 << predicate_property(public, _),
		\+ descendant3 << predicate_property(protected, _),
		\+ descendant3 << predicate_property(private, _).

	test(inheritance_8) :-
		root::current_predicate(public/0),
		\+ root::current_predicate(protected/0),
		\+ root::current_predicate(private/0),
		root << predicate_property(public, public),
		root << predicate_property(protected, protected),
		root << predicate_property(private, private).

	test(inheritance_9) :-
		instance1::current_predicate(public/0),
		\+ instance1::current_predicate(protected/0),
		\+ instance1::current_predicate(private/0),
		instance1 << predicate_property(public, public),
		instance1 << predicate_property(protected, protected),
		\+ instance1 << predicate_property(private, _).

	test(inheritance_10) :-
		\+ instance2::current_predicate(public/0),
		\+ instance2::current_predicate(protected/0),
		\+ instance2::current_predicate(private/0),
		instance2 << predicate_property(public, protected),
		instance2 << predicate_property(protected, protected),
		\+ instance2 << predicate_property(private, _).

	test(inheritance_11) :-
		\+ instance3::current_predicate(public/0),
		\+ instance3::current_predicate(protected/0),
		\+ instance3::current_predicate(private/0),
		\+ instance3 << predicate_property(public, _),
		\+ instance3 << predicate_property(protected, _),
		\+ instance3 << predicate_property(private, _).

:- end_object.
