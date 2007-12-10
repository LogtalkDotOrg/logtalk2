
:- object(employee).

	:- public([name/1, age/1, salary/1]).

	name(john).
	age(42).
	salary(23500).

:- end_object.



:- category(logging,
	implements(monitoring),
	complements(employee)).

	:- initialization(define_events(before, employee, _, _, employee)).	% implies that the category must be loaded *after*
																		% the object due to the define_events/5 goal
	before(_, Message, Sender) :-
		write('Received message '), writeq(Message), write(' from '), writeq(Sender), nl.

	:- public(predicates/0).

	predicates :-	% list only public predicates
		this(This),
		setof(Predicate, This::current_predicate(Predicate), Predicates),
		write(Predicates), nl. 

:- end_category.
