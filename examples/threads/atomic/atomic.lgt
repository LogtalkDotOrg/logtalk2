
:- object(nasty).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2006/04/14,
		comment is 'Simple example for comparing single and multi-threading calculation of prime numbers.']).

	:- threaded.

	:- public(update_db/1).
	:- mode(update_db(-integer), one).
	:- info(update_db/1, [
		comment is '.',
		argnames is ['New']]).

	:- private(db/1).
	:- dynamic(db/1).

	:- public(io/1).
	:- mode(io(+atom), one).
	:- info(io/1, [
		comment is '.',
		argnames is ['Chars']]).

	db(0).

	update_db(New) :-
		retract(db(Old)),
		New is Old + 1,
		assertz(db(New)).

	io(alpha) :-
		write(a), waste_time, write(b), waste_time, write(c), waste_time, write(d), waste_time, write(e),
		write(f), waste_time, write(g), waste_time, write(h), waste_time, write(i), waste_time, write(j),
		write(k), waste_time, write(l), waste_time, write(m), waste_time, write(n), waste_time, write(o),
		write(p), waste_time, write(q), waste_time, write(r), waste_time, write(s), waste_time, write(t),
		write(z), waste_time, write(y), waste_time, write(x), waste_time, write(w), waste_time, write(u),
		write(v), nl.

	io(digit) :-
		write(0), waste_time, write(1), waste_time, write(2), waste_time, write(3), waste_time, write(4),
		write(5), waste_time, write(6), waste_time, write(7), waste_time, write(8), waste_time, write(9), nl.

	waste_time :-
		between(1, 10000, _),
		fail.
	waste_time.
	
	between(Lower, _, Lower).
	between(Lower, Upper, Integer) :-
		Lower < Upper,
		Next is Lower + 1,
		between(Next, Upper, Integer).

:- end_object.
