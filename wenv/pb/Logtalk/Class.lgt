
:- object(Class,
	instantiates(Metaclass),
	specializes(Superclass)).

	:- info([
		version is 1.0,
		author is '',
		date is 2003/02/01,
		comment is '']).

	:- public(Functor/Arity).

	:- protected(Functor/Arity).

	:- private(Functor/Arity).


:- end_object.
