
:- protocol(numberlistp).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2011/12/04,
		comment is 'List of numbers protocol.']).

	:- public(product/2).
	:- mode(product(+list(number), -number), zero_or_one).
	:- info(product/2,
		[comment is 'Calculates the product of all list numbers. Fails if the list is empty.',
		 argnames is ['List', 'Product']]).

	:- public(sum/2).
	:- mode(sum(+list(number), -number), one).
	:- info(sum/2,
		[comment is 'Calculates the sum of all list numbers. Returns zero if the list is empty.',
		 argnames is ['List', 'Sum']]).

	:- public(average/2).
	:- mode(average(+list(number), -float), zero_or_one).
	:- info(average/2,
		[comment is 'Calculates the average (i.e. arithmetic mean) of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Average']]).

	:- public(euclidean_norm/2).
	:- mode(euclidean_norm(+list(number), -float), one).
	:- info(euclidean_norm/2,
		[comment is 'Calculates the Euclidean norm of a list of numbers.',
		 argnames is ['List', 'Norm']]).

	:- public(manhattan_norm/2).
	:- mode(manhattan_norm(+list(number), -float), one).
	:- info(manhattan_norm/2,
		[comment is 'Calculates the Manhattan norm of a list of numbers.',
		 argnames is ['List', 'Norm']]).

	:- public(scalar_product/3).
	:- mode(scalar_product(+list(number), +list(number), -number), zero_or_one).
	:- info(scalar_product/3,
		[comment is 'Calculates the scalar product of two lists of numbers. Fails if the two lists are not of the same length.',
		 argnames is ['List1', 'List2', 'Product']]).

:- end_protocol.
