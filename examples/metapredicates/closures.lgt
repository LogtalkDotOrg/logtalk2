
/*	Logtalk meta-predicates accept not only goals but also closures
	as meta-arguments as illustrated in this example
*/


:- object(meta).

	% the meta_predicate/1 directive below changes the interpretation of meta-calls on apply/2
	% clauses; the integer argument ("1") implies that the first argument is a closure that will 
	% be used to construct a goal by appending exactly one additional argument

	:- public(apply/2).
	:- mode(apply(+callable, ?term), zero_or_more).
	:- meta_predicate(apply(1, *)).

	apply(Closure, Arg) :-		% the Logtalk compiler verifies that any closure which is a
		call(Closure, Arg).		% meta-argument is used within a call/N method honors the
								% meta-predicate directive (in this case, apply(1, *) => call/2)

	:- public(test_this/0).		% simple predicate for testing calls to a local meta-predicate

	test_this :-
		apply(foo(X), Y),		% call in the context of "this"
		write((X, Y)), nl.

	:- public(test_self/0).		% simple predicate for testing calls to a meta-predicate
								% defined in the object itself or in an ancestor object
	test_self :-
		::apply(foo(X), Y),		% call in the context of "self"; note that foo/2 is private 
		write((X, Y)), nl.		% but the validity of the message depends only on the context
								% where the call to the meta-predicate is made 
	:- private(foo/2).

	foo(2, b).

:- end_object.


:- object(desc,					% this prototype simply extends the "meta" prototype; it's used
	extends(meta)).				% to illustrate the behavior of the inherited test_self/0 predicate
								% when "self" is a descendant object instead of the object where			
	foo(3, c).					% the meta-predicate is defined (note that the local definition of 
								% predicate foo/2 is not the one that's called othwerwise it would 
:- end_object.					% be possible for an ancestor to call private and local predicates
								% in a descendant object)

:- object(test).

	:- public(test_obj/0).		% simple predicate for testing calls to a meta-predicate
								% defined in another object
	test_obj :-
		meta::apply(bar(X), Y),	% the call to bar/2 is made on the context of
		write((X, Y)), nl.		% the sender (the object "test" in this case)

	:- private(bar/2).

	bar(1, w).

:- end_object.
