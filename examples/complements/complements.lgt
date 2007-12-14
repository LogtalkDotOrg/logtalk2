
:- object(employee).

	:- public([name/1, age/1, salary/1]).

	name(john).
	age(42).
	salary(23500).

:- end_object.



:- category(logging,
	implements(monitoring),
	complements(employee)).

	% add a simple logging functionality by defining a "before" event and 
	% the corresponding event handler (implies that the category must be 
	% loaded *after* the object due to the define_events/5 goal):

	:- initialization(define_events(before, employee, _, _, employee)).

	before(_, Message, Sender) :-
		write('Received message '), writeq(Message), write(' from '), writeq(Sender), nl.

	% add a new method to the object:

	:- public(predicates/1).

	predicates(Predicates) :-
		setof(Predicate, ::current_predicate(Predicate), Predicates). 

:- end_category.
