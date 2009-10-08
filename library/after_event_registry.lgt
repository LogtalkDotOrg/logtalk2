
:- object(after_event_registry,
	implements(event_registryp)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/10/8,
		comment is 'After events registry predicates.']).

	monitor(Monitor) :-
		current_event(after, _, _, _, Monitor).

	monitors(Monitors) :-
		(	setof(Monitor, monitor(Monitor), Monitors) ->
			true
		;	Monitors = []
		).

	monitored(Objects) :-
		(	setof(Object, Message^Sender^Monitor^current_event(after, Object, Message, Sender, Monitor), Objects) ->
			true
		;	Objects = []
		).

	monitor(Object, Message, Sender, Monitor) :-
		current_event(after, Object, Message, Sender, Monitor).

	set_monitor(Object, Message, Sender, Monitor) :-
		define_events(after, Object, Message, Sender, Monitor).

	del_monitors(Object, Message, Sender, Monitor) :-
		abolish_events(after, Object, Message, Sender, Monitor).

	del_monitors :-
		abolish_events(after, _, _, _, _).

:- end_object.
