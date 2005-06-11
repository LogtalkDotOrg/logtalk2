
:- object(root,
	instantiates(root)).

	:- public(method/0).

	method :-
		this(This),
		write('This is the default definition for the method, stored in class '),
		writeq(This), write('.'), nl.

:- end_object.



:- object(instance1,
	instantiates(root)).

:- end_object.



:- object(instance2,
	instantiates(root)).

	method :-
		this(This),
		write('This is an overriding definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl.

:- end_object.



:- object(instance3,
	instantiates(root)).

	method :-
		this(This),
		write('This is a specializing definition stored in the '),
		writeq(This),
		write(' instance itself.'), nl,
		write('It makes a super call to execute the default definition:'), nl, nl,
		^^method.

:- end_object.
