
:- protocol(event_registryp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/10/8,
		comment is 'Event registry protocol.']).

	:- public(monitors/1).
	:- mode(monitors(-list(object_identifier)), one).
	:- info(monitors/1, [
		comment is 'Returns a list of all current monitors.',
		argnames is ['Monitors']]).

	:- public(monitor/1).
	:- mode(monitor(-object_identifier), zero_or_more).
	:- mode(monitor(+object_identifier), zero_or_one).
	:- info(monitor/1, [
		comment is 'Monitor is an object playing the role of a monitor.',
		argnames is ['Monitor']]).

	:- public(monitored/1).
	:- mode(monitored(-list(object_identifier)), one).
	:- info(monitored/1, [
		comment is 'Returns a list of all currently monitored objects.',
		argnames is ['Objects']]).

	:- public(monitor/4).
	:- mode(monitor(?object_identifier, ?nonvar, ?object_identifier, ?object_identifier), zero_or_more).
	:- info(monitor/4, [
		comment is 'True if the arguments describe a currently defined monitored event.',
		argnames is ['Object', 'Message', 'Sender', 'Monitor']]).

	:- public(set_monitor/4).
	:- mode(set_monitor(?object_identifier, ?nonvar, ?object_identifier, +object_identifier), zero_or_one).
	:- info(set_monitor/4, [
		comment is 'Sets a monitor for the set of matching events.',
		argnames is ['Object', 'Message', 'Sender', 'Monitor']]).

	:- public(del_monitors/4).
	:- mode(del_monitors(?object_identifier, ?nonvar, ?object_identifier, ?object_identifier), one).
	:- info(del_monitors/4, [
		comment is 'Deletes all matching monitored events.',
		argnames is ['Object', 'Message', 'Sender', 'Monitor']]).

	:- public(del_monitors/0).
	:- mode(del_monitors, one).
	:- info(del_monitors/0, [
		comment is 'Deletes all monitored events.']).

:- end_protocol.
