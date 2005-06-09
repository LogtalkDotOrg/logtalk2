/*
This source file defines the following class-based hierarchy:

	root
		subclass1
			instance1
		subclass2
			instance2
		subclass3
			instance3


*/


:- object(root,
	imports(predicates, interface),
	instantiates(root)).


:- end_object.


:- object(subclass1,
	imports(interface),
	specializes(public::root)).

:- end_object.


:- object(instance1,
	instantiates(subclass1)).

:- end_object.


:- object(subclass2,
	imports(interface),
	specializes(protected::root)).

:- end_object.


:- object(instance2,
	instantiates(subclass2)).

:- end_object.


:- object(subclass3,
	imports(interface),
	specializes(private::root)).

:- end_object.


:- object(instance3,
	instantiates(subclass3)).

:- end_object.
