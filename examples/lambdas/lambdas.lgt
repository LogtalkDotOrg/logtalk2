
:- object(countries).

	:- info([
		version is 1.0,
		date is 2009/11/28,
		author is 'Paulo Moura',
		comment is 'Simple example of using lambda expressions to simplify setof/3 and similar calls.']).

	:- public(currencies_wrong/1).
	currencies_wrong(Currencies) :-
		setof(Currency, country(_, _, _, Currency), Currencies).

	:- public(currencies_no_lambda/1).
	currencies_no_lambda(Currencies) :-
		setof(Currency, Country^Capital^Population^country(Country, Capital, Population, Currency), Currencies).

	:- public(currencies_lambda/1).
	currencies_lambda(Currencies) :-
		setof(Currency, Currency+\country(_, _, _, Currency), Currencies).

	% country(Country, Capital, Population, Currency)
	country(portugal, lisbon, 10, euro).
	country(spain, madrid, 46, euro).
	country(england, london, 51, pound_sterling).
	country(malaysia, kuala_lumpur, 28, ringgit).
	country(iraq, baghdad, 31, dinar).
	country(tunisia, tunis, 10, dinar).

:- end_object.
