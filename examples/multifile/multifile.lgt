
:- object(main).

	:- public(a/1).
	:- multifile(a/1).
	a(1).

:- end_object.


:- object(other).

	:- multifile(main::a/1).
	main::a(2).
	main::a(3).

:- end_object.


:- category(more).

	:- multifile(main::a/1).
	main::a(4).
	main::a(5).

:- end_category.
