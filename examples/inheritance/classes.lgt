/*
This source file defines the following class-based hierarchy:

	root
		subclass1
			instance1
		subclass2
			instance2
		subclass3
			instance3

All objects import the category "interface", which defines a predicate 
(interface/0) for listing the object interface.
*/


:- object(root,
	imports(predicates, interface),
	instantiates(root)).


:- end_object.


% public inheritance:
% root predicates will be inherited without scope changes
:- object(subclass1,
	imports(interface),
	specializes(public::root)).

:- end_object.


:- object(instance1,
	instantiates(subclass1)).

:- end_object.


% protected inheritance:
% root public predicates will be inherited as protected predicates
:- object(subclass2,
	imports(interface),
	specializes(protected::root)).

:- end_object.


:- object(instance2,
	instantiates(subclass2)).

:- end_object.


% private inheritance:
% root predicates will be inherited as private predicates
:- object(subclass3,
	imports(interface),
	specializes(private::root)).

:- end_object.


:- object(instance3,
	instantiates(subclass3)).

:- end_object.
