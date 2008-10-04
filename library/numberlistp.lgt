
:- protocol(numberlistp).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/9/15,
		comment is 'List of numbers protocol.']).

	:- public(product/2).
	:- mode(product(+list(number), ?number), zero_or_one).
	:- info(product/2,
		[comment is 'Calculates the product of all list numbers. Fails if the list is empty.',
		 argnames is ['List', 'Product']]).

	:- public(sum/2).
	:- mode(sum(+list(number), ?number), one).
	:- info(sum/2,
		[comment is 'Calculates the sum of all list numbers. Returns zero if the list is empty.',
		 argnames is ['List', 'Sum']]).

	:- public(average/2).
	:- mode(average(+list(number), ?float), zero_or_one).
	:- info(average/2,
		[comment is 'Calculates the average (i.e. arithmetic mean) of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Average']]).

:- end_protocol.
