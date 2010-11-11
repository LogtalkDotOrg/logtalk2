
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/11/09,
		comment is 'Unit tests for the "chr" example.']).

	:- if(current_object(dom)).
	test(chr_dom_1) :-
		dom::dom(A, [1,2,3]), dom::dom(A, [3,4,5]),
		A =:= 3.
	:- endif.

	:- if(current_object(fib)).
	test(chr_fib_1) :-
		fib::fib(5, M),
		M =:= 8.
	:- endif.

	:- if(current_object(gcd)).
	test(chr_gcd_1) :-
		gcd::gcd(2), gcd::gcd(3).

	test(chr_gcd_2) :-
		X is 37*11*11*7*3, Y is 11*7*5*3, Z is 37*11*5, gcd::gcd(X), gcd::gcd(Y), gcd::gcd(Z).
	:- endif.

	:- if(current_object(leq)).
	test(chr_leq_1) :-
		leq::leq(X, Y), leq::leq(Y, Z).
	:- endif.

	:- if(current_object(primes)).
	test(chr_primes_1) :-
		primes::candidate(29).
	:- endif.

:- end_object.
