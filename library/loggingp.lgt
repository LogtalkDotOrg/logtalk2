
:- protocol(loggingp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/01/06,
		comment is 'Logging events to files protocol.']).

	:- public(log_file/2).
	:- mode(log_file(?atom, ?atom), zero_or_more).
	:- info(log_file/2, [
		comment is 'Access to the table of log files.',
		argnames is ['Alias', 'File']]).

	:- public(define_log_file/2).
	:- mode(define_log_file(+atom, +atom), zero_or_one).
	:- info(define_log_file/2, [
		comment is 'Defines a log file with alias Alias and file name File. If the log file already exists, its contents are kept. Logging is enabled by default.',
		argnames is ['Alias', 'File']]).

	:- public(init_log_file/2).
	:- mode(init_log_file(+atom, +atom), zero_or_one).
	:- info(init_log_file/2, [
		comment is 'Initializes a new log file with alias Alias and file name File. If the log file already exists, its contents are erased. Logging is enabled by default.',
		argnames is ['Alias', 'File']]).

	:- public(log_event/2).
	:- mode(log_event(+atom, +nonvar), one).
	:- info(log_event/2, [
		comment is 'Logs an event Event to a log file with alias Alias. Fails if a log file with alias Alias is not defined.',
		argnames is ['Alias', 'Event']]).

	:- public(logging/1).
	:- mode(logging(+atom), zero_or_one).
	:- info(logging/1, [
		comment is 'True if logging to file with alias Alias is enabled.',
		argnames is ['Alias']]).

	:- public(enable_logging/1).
	:- mode(enable_logging(+atom), zero_or_one).
	:- info(enable_logging/1, [
		comment is 'Enables logging to file with alias Alias.',
		argnames is ['Alias']]).

	:- public(disable_logging/1).
	:- mode(disable_logging(+atom), zero_or_one).
	:- info(disable_logging/1, [
		comment is 'Disables logging to file with alias Alias.',
		argnames is ['Alias']]).

:- end_protocol.
