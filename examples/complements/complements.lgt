
:- initialization(define_events(before, employee, _, _, employee)).


:- object(employee).

	:- public([name/1, age/1, salary/1]).

	name(john).
	age(42).
	salary(23500).

:- end_object.


:- category(logging,
	implements(monitoring),
	complements(employee)).

	before(_, Message, Sender) :-
		write('Received message '), writeq(Message), write(' from '), writeq(Sender), nl.

	% add a new method to the object:

	:- public(predicates/1).

	predicates(Predicates) :-
		setof(Predicate, ::current_predicate(Predicate), Predicates). 

:- end_category.
