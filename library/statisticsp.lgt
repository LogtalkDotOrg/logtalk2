
:- protocol(statisticsp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/10/4,
		comment is 'Statistical calculations over a list of numbers protocol.']).

	:- public(product/2).
	:- mode(product(+list(number), -number), zero_or_one).
	:- info(product/2,
		[comment is 'Calculates the product of all list numbers. Fails if the list is empty.',
		 argnames is ['List', 'Product']]).

	:- public(sum/2).
	:- mode(sum(+list(number), -number), zero_or_one).
	:- info(sum/2,
		[comment is 'Calculates the sum of all list numbers. Fails if the list is empty.',
		 argnames is ['List', 'Sum']]).

	:- public(max/2).
	:- mode(max(+list, -number), zero_or_one).
	:- info(max/2,
		[comment is 'Determines the list maximum value in a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Maximum']]).

	:- public(min/2).
	:- mode(min(+list, -number), zero_or_one).
	:- info(min/2,
		[comment is 'Determines the minimum value in a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Minimum']]).

	:- public(range/2).
	:- mode(range(+list, -number), zero_or_one).
	:- info(range/2,
		[comment is 'Range is the length of the smallest interval which contains all the numbers in List. Fails if the list is empty.',
		 argnames is ['List', 'Range']]).

	:- public(arithmetic_mean/2).
	:- mode(arithmetic_mean(+list(number), -float), zero_or_one).
	:- info(arithmetic_mean/2,
		[comment is 'Calculates the arithmetic mean of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Mean']]).

	:- public(geometric_mean/2).
	:- mode(geometric_mean(+list(number), -float), zero_or_one).
	:- info(geometric_mean/2,
		[comment is 'Calculates the geometric mean of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Mean']]).

	:- public(harmonic_mean/2).
	:- mode(harmonic_mean(+list(number), -float), zero_or_one).
	:- info(harmonic_mean/2,
		[comment is 'Calculates the harmonic mean of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Mean']]).

	:- public(median/2).
	:- mode(median(+list(number), -float), zero_or_one).
	:- info(median/2,
		[comment is 'Calculates the median of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Median']]).

	:- public(average_deviation/3).
	:- mode(average_deviation(+list(number), +float, -float), zero_or_one).
	:- info(average_deviation/3,
		[comment is 'Calculates the average absolute deviation of a list of numbers given a central tendency (e.g. mean, median, or mode). Fails if the list is empty.',
		 argnames is ['List', 'CentralTendency', 'Deviation']]).

	:- public(mean_deviation/2).
	:- mode(mean_deviation(+list(number), -float), zero_or_one).
	:- info(mean_deviation/2,
		[comment is 'Calculates the mean absolute deviation of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Deviation']]).

	:- public(median_deviation/2).
	:- mode(median_deviation(+list(number), -float), zero_or_one).
	:- info(median_deviation/2,
		[comment is 'Calculates the median absolute deviation of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Deviation']]).

	:- public(standard_deviation/2).
	:- mode(standard_deviation(+list(number), -float), zero_or_one).
	:- info(standard_deviation/2,
		[comment is 'Calculates the standard deviation of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Deviation']]).

	:- public(coefficient_of_variation/2).
	:- mode(coefficient_of_variation(+list(number), -float), zero_or_one).
	:- info(coefficient_of_variation/2,
		[comment is 'Calculates the coefficient of variation of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Coefficient']]).

	:- public(relative_standard_deviation/2).
	:- mode(relative_standard_deviation(+list(number), -float), zero_or_one).
	:- info(relative_standard_deviation/2,
		[comment is 'Calculates the relative standard deviation of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Percentage']]).

	:- public(skewness/2).
	:- mode(skewness(+list(number), -float), zero_or_one).
	:- info(skewness/2,
		[comment is 'Calculates the skewness of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Deviation']]).

	:- public(variance/2).
	:- mode(variance(+list(number), -float), zero_or_one).
	:- info(variance/2,
		[comment is 'Calculates the unbiased variance of a list of numbers. Fails if the list is empty.',
		 argnames is ['List', 'Variance']]).

	:- public(valid/1).
	:- mode(valid(@nonvar), zero_or_one).
	:- info(valid/1, [
		comment is 'Term is a closed list of numbers.',
		argnames is ['Term']]).

    :- private(arithmetic_mean/5).
    :- private(squares_and_cubes/6).
	:- private(variance/6).

:- end_protocol.
