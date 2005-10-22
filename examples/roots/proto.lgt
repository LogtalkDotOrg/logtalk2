
:- object(proto,
	implements(protop, event_handlersp),
	imports(initialization, proto_hierarchy)).


	:- info([
		version is 1.1,
		date is 2005/10/22,
		author is 'Paulo Moura',
		comment is 'Minimal predicates for all prototypes. Default root of the extension graph.']).


	:- uses(event_registry, [del_monitors/4]).


	clone(Clone) :-
		self(Self),
		sender(Sender),
		throw(error(descendant_responsability, Self::clone(Clone), Sender)).


	default_free_option(del_monitors).


	process_free_option(del_monitors) :-
		self(Self),
		del_monitors(Self, _, _, _),
		del_monitors(_, _, Self, _),
		del_monitors(_, _, _, Self).


	print :-
		self(Self),
		writeq(Self), nl, nl,
		forall(
			::current_predicate(Predicate),
			(writeq(Predicate), nl)),
		nl.


	before(_, _, _).


	after(_, _, _).


:- end_object.
