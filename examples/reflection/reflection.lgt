
:- object(object,
	instantiates(class)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Inheritance root for all objects.']).

	:- public(strict_instance/0).
	:- mode(strict_instance, zero_or_one).

	:- public(print/0).
	:- mode(print, one).

	strict_instance.

	print :-
		self(Self),
		write('Object: '), writeq(Self), nl, nl,
		write('  interface:'), nl,
		forall(
			::current_predicate(Predicate),
			(write('    '), writeq(Predicate), nl)),
		nl.

:- end_object.


:- object(class,
	instantiates(class),
	specializes(abstract_class)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Instantiation root and default metaclass for all classes.']).

	:- public(new/1).
	:- mode(new(+object), zero_or_one).

	:- public(delete/1).
	:- mode(delete(+object), zero_or_one).

	:- public(instances/1).
	:- mode(instances(-list), one).

	new(Object) :-
		self(Self),
		create_object(Object, [instantiates(Self)], [], []).

	delete(Object) :-
		self(Self),
		instantiates_class(Object, Self),
		\+ instantiates_class(_, Object),
		\+ specializes_class(_, Object),
		abolish_object(Object).

	instances(Instances) :-
		self(Self),
		findall(Instance, instantiates_class(Instance, Self), Instances).

	abstract_class :-
		fail.

:- end_object.


:- object(abstract_class,
	instantiates(class),
	specializes(object)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Default metaclass for all abstract classes.']).

	:- public(metaclass/0).
	:- mode(metaclass, zero_or_one).

	:- public(abstract_class/0).
	:- mode(abstract_class, zero_or_one).

	abstract_class :-
		self(Self),
		Self \= abstract_class.

	metaclass :-
		self(Self),
		once((
			instantiates_class(Class, Self),
			Class::current_predicate(abstract_class/0))).

	strict_instance :-
		fail.

:- end_object.
