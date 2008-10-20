
% setup the object employee as a monitor for any message sent to itself:
:- initialization(define_events(before, employee, _, _, employee)).


:- object(employee).

	:- public([name/1, age/1, salary/1]).

	name(john).
	age(42).
	salary(23500).

:- end_object.


:- category(logging,
	implements(monitoring),		% built-in protocol for event handler methods
	complements(employee)).		% add the category predicates to the employee object

	% define a "before" event handler for the complemented object:
	before(This, Message, Sender) :-
		this(This),
		write('Received message '), writeq(Message), write(' from '), writeq(Sender), nl.

	% add a new method to the complemented object:
	:- public(predicates/1).

	predicates(Predicates) :-
		setof(Predicate, ::current_predicate(Predicate), Predicates). 

:- end_category.
