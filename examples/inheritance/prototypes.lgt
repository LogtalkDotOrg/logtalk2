/*
This source file defines the following prototype-based hierarchy:

	parent
		prototype1
			descendant1
		prototype2
			descendant2
		prototype3
			descendant3


*/


:- object(parent,
	imports(predicates, interface)).

:- end_object.


:- object(prototype1,
	imports(interface),
	extends(public::parent)).

:- end_object.


:- object(descendant1,
	imports(interface),
	extends(prototype1)).
	
:- end_object.


:- object(prototype2,
	imports(interface),
	extends(protected::parent)).

:- end_object.


:- object(descendant2,
	imports(interface),
	extends(prototype2)).
	
:- end_object.



:- object(prototype3,
	imports(interface),
	extends(private::parent)).

:- end_object.


:- object(descendant3,
	imports(interface),
	extends(prototype3)).
	
:- end_object.
