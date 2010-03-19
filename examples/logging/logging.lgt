
:- category(logging).

	:- public([
		init_log/0,
		add_log_entry/1,
		print_log/0,
		log_entry/2]).

	:- private(log_/2).
	:- dynamic(log_/2).

	:- op(600, xfy, :).

	init_log :-
		retractall(log_(_, _)),
		add_log_entry(start).

	add_log_entry(Entry) :-
		date::today(Year, Month, Day),
		time::now(Hours, Mins, Secs),
		assertz(log_(Year/Month/Day-Hours:Mins:Secs, Entry)).

	print_log :-
		this(This),
			This::log_(Date, Entry),
			write(Date), write(' - '), write(Entry), nl,
		fail.
	print_log.

	log_entry(Date, Entry) :-
		this(This),
		This::log_(Date, Entry).

:- end_category.


:- object(object,
	imports(logging)).

	:- initialization(:init_log).

:- end_object.
