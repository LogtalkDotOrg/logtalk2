
:- object(other).

	:- multifile(main::a/1).
	main::a(2).
	main::a(3).

	:- multifile(main::b/1).
	main::b(X) :-	% the head of this clause exists in the object "main" but
		c(X).		% the body is compiled as local to the object "other"

	c(two).
	c(three).

:- end_object.
