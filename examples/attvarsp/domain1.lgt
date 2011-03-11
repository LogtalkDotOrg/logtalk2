
% example adapted from the SWI-Prolog documentation on attributed variables

:- object(domain(_Type)).

	:- public(domain/2).

	:- uses(set, [insert_all/3, intersection/3, memberchk/2, new/1]).

	domain(X, Dom) :-
		var(Dom),
		!,
		get_attr(X, domain(_), Dom).
	domain(X, List) :-
		check_domain(List),
		new(Set),
		insert_all(List, Set, Domain),
		put_attr(Y, domain(_), Domain),
		X = Y.

	check_domain([]).
	check_domain([Head| Tail]) :-
		parameter(1, Type),		% compiled in-line to head unification
		Type::valid(Head),
		check_domain(Tail).

	% Var, ?Domain
	%	An attributed variable with attribute value Domain has been
	%	assigned the value Y
	attr_unify_hook(domain(_), Domain, Y) :-
		(	var(Y), get_attr(Y, domain(_), Dom2) ->
			intersection(Domain, Dom2, NewDomain),
			(	NewDomain == [] ->
				fail
			;	NewDomain = [Value] ->
				Y = Value
			;	put_attr(Y, domain(_), NewDomain)
			)
		;	var(Y) ->
			put_attr(Y, domain(_), Domain)
		;	memberchk(Y, Domain)
		).

	%	Translate attributes from this module to residual goals
	attribute_goals(domain(_), X) -->
		{ var(X), get_attr(X, domain(_), List) },
		[domain(X, List)].

:- end_object.
