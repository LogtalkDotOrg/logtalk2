
:- category(initialization).

	:- info([
		version is 1.11,
		author is 'Paulo Moura',
		date is 2008/10/10,
		comment is 'Object initialization protocol.']).

	:- public(init/1).
	:- mode(init(+list), zero_or_one).
	:- info(init/1,
		[comment is 'Intialize an object with the given list of options.',
		 argnames is ['Options']]).

	:- public(valid_init_option/1).
	:- mode(valid_init_option(+nonvar), zero_or_one).
	:- info(valid_init_option/1,
		[comment is 'True if the argument is a valid initialization option.',
		 argnames is ['Option']]).

	:- public(valid_init_options/1).
	:- mode(valid_init_options(+list), zero_or_one).
	:- info(valid_init_options/1,
		[comment is 'True if the argument is list of valid initialization options.',
		 argnames is ['Options']]).

	:- public(default_init_options/1).
	:- mode(default_init_options(+list), one).
	:- info(default_init_options/1,
		[comment is 'List of default initilization options.',
		 argnames is ['Options']]).

	:- public(default_init_option/1).
	:- mode(default_init_option(?nonvar), zero_or_more).
	:- info(default_init_option/1,
		[comment is 'Default initilization option.',
		 argnames is ['Option']]).

	:- protected(process_init_option/1).
	:- mode(process_init_option(?nonvar), zero_or_one).
	:- info(process_init_option/1,
		[comment is 'Process initilization option.',
		 argnames is ['Option']]).

	:- private(process_init_options/1).
	:- mode(process_init_options(+list), zero_or_one).
	:- info(process_init_options/1,
		[comment is 'Process a list of initilization options.',
		 argnames is ['Options']]).

	:- public(free/1).
	:- mode(free(+list), zero_or_one).
	:- info(free/1,
		[comment is 'Release an object with the given list of options.',
		 argnames is ['Options']]).

	:- public(valid_free_option/1).
	:- mode(valid_free_option(+nonvar), zero_or_one).
	:- info(valid_init_option/1,
		[comment is 'True if the argument is a valid delete option.',
		 argnames is ['Option']]).

	:- public(valid_free_options/1).
	:- mode(valid_free_options(+list), zero_or_one).
	:- info(valid_free_options/1,
		[comment is 'True if the argument is list of valid delete options.',
		 argnames is ['Options']]).

	:- public(default_free_options/1).
	:- mode(default_free_options(+list), one).
	:- info(default_free_options/1,
		[comment is 'List of default delete options.',
		 argnames is ['Options']]).

	:- public(default_free_option/1).
	:- mode(default_free_option(?nonvar), zero_or_more).
	:- info(default_free_option/1,
		[comment is 'Default delete option.',
		 argnames is ['Option']]).

	:- protected(process_free_option/1).
	:- mode(process_free_option(?nonvar), zero_or_one).
	:- info(process_free_option/1,
		[comment is 'Process delete option.',
		 argnames is ['Option']]).

	:- private(process_free_options/1).
	:- mode(process_free_options(+list), zero_or_one).
	:- info(process_free_options/1,
		[comment is 'Process a list of delete options.',
		 argnames is ['Options']]).

	:- private(merge_options/3).
	:- mode(merge_options(+list, +list, -list), one).
	:- info(merge_options/3,
		[comment is 'Constructs a complete list of options complementing the explicit options with the default ones.',
		 argnames is ['Options', 'Defaults', 'Result']]).

	:- uses(list, [select/3]).

	init(Options) :-
		valid_init_options(Options),
		default_init_options(Defaults),
		merge_options(Options, Defaults, Options2),
		process_init_options(Options2).

	default_init_options(Defaults) :-
		findall(Default, ::default_init_option(Default), Defaults).

	valid_init_options([]).
	valid_init_options([Option| Options]) :-
		::valid_init_option(Option) ->
		valid_init_options(Options).

	valid_init_option(_).

	process_init_options([]).
	process_init_options([Option| Options]) :-
		::process_init_option(Option) ->
		process_init_options(Options).

	process_init_option(Option) :-
		self(Self),
		sender(Sender),
		throw(error(existence_error(predicate_definition), Self::process_init_option(Option), Sender)).

	free(Options) :-
		valid_free_options(Options),
		default_free_options(Defaults),
		merge_options(Options, Defaults, Options2),
		process_free_options(Options2).

	default_free_options(Defaults) :-
		findall(Default, ::default_free_option(Default), Defaults).

	valid_free_options([]).
	valid_free_options([Option| Options]) :-
		::valid_free_option(Option) ->
		valid_free_options(Options).

	valid_free_option(_).

	process_free_options([]).
	process_free_options([Option| Options]) :-
		::process_free_option(Option) ->
		process_free_options(Options).

	process_free_option(Option) :-
		self(Self),
		sender(Sender),
		throw(error(existence_error(predicate_definition), Self::process_free_option(Option), Sender)).

	merge_options([], Defaults, Defaults).
	merge_options([Option-Value| Options], Defaults, [Option-Value| Options2]) :-
		!,
		(	select(Option-_, Defaults, Defaults2) ->
			merge_options(Options, Defaults2, Options2)
		;	merge_options(Options, Defaults, Options2)
		).
	merge_options([Option| Options], Defaults, [Option| Options2]) :-
		merge_options(Options, Defaults, Options2).

:- end_category.
