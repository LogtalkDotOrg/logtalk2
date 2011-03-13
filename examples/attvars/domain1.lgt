
% example adapted from the SWI-Prolog documentation on attributed variables

:- object(domain(_Type)).

	:- public(domain/2).

	:- uses(set, [insert_all/3, intersection/3, memberchk/2, new/1]).

	domain(X, Dom) :-
		var(Dom),
		!,
		parameter(1, Type),
		get_attr(X, domain(Type), Dom).
	domain(X, List) :-
		check_domain(List),
		new(Set),
		insert_all(List, Set, Domain),
		parameter(1, Type),
		put_attr(Y, domain(Type), Domain),
		X = Y.

	check_domain([]).
	check_domain([Head| Tail]) :-
		parameter(1, Type),		% compiled in-line to head unification
		Type::valid(Head),
		check_domain(Tail).

	% Var, ?Domain
	%	An attributed variable with attribute value Domain has been
	%	assigned the value Y
	attr_unify_hook(Domain, Y) :-
		(	var(Y), get_attr(Y, domain(Type), Dom2) ->
			intersection(Domain, Dom2, NewDomain),
			(	NewDomain == [] ->
				fail
			;	NewDomain = [Value] ->
				Y = Value
			;	put_attr(Y, domain(Type), NewDomain)
			)
		;	var(Y) ->
			parameter(1, Type),
			put_attr(Y, domain(Type), Domain)
		;	memberchk(Y, Domain)
		).

	%	Translate attributes from this module to residual goals
	attribute_goals(X) -->
		{ var(X), get_attr(X, domain(_), List) },
		[domain(X, List)].

:- end_object.
