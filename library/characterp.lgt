
:- protocol(characterp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/10/7,
		comment is 'Character protocol.']).

	:- public(is_alphanumeric/1).
	:- mode(is_alphanumeric(+char), zero_or_one).
	:- info(is_alphanumeric/1, [
		comment is 'True if the argument is an alphanumeric character.',
		argnames is ['Char']]).

	:- public(is_alpha/1).
	:- mode(is_alpha(+char), zero_or_one).
	:- info(is_alpha/1, [
		comment is 'True if the argument is a letter or an underscore.',
		argnames is ['Char']]).

	:- public(is_letter/1).
	:- mode(is_letter(+char), zero_or_one).
	:- info(is_letter/1, [
		comment is 'True if the argument is a letter.',
		argnames is ['Char']]).

	:- public(is_bin_digit/1).
	:- mode(is_bin_digit(+char), zero_or_one).
	:- info(is_bin_digit/1, [
		comment is 'True if the argument is a binary digit.',
		argnames is ['Char']]).

	:- public(is_octal_digit/1).
	:- mode(is_octal_digit(+char), zero_or_one).
	:- info(is_octal_digit/1, [
		comment is 'True if the argument is an octal digit.',
		argnames is ['Char']]).

	:- public(is_dec_digit/1).
	:- mode(is_dec_digit(+char), zero_or_one).
	:- info(is_dec_digit/1, [
		comment is 'True if the argument is a decimal digit.',
		argnames is ['Char']]).

	:- public(is_hex_digit/1).
	:- mode(is_hex_digit(+char), zero_or_one).
	:- info(is_hex_digit/1, [
		comment is 'True if the argument is an hexadecimal digit.',
		argnames is ['Char']]).

	:- public(is_lower_case/1).
	:- mode(is_lower_case(+char), zero_or_one).
	:- info(is_lower_case/1, [
		comment is 'True if the argument is a lower case letter.',
		argnames is ['Char']]).

	:- public(is_upper_case/1).
	:- mode(is_upper_case(+char), zero_or_one).
	:- info(is_upper_case/1, [
		comment is 'True if the argument is a upper case letter.',
		argnames is ['Char']]).

	:- public(is_vowel/1).
	:- mode(is_vowel(+char), zero_or_one).
	:- info(is_vowel/1, [
		comment is 'True if the argument is a vowel.',
		argnames is ['Char']]).

	:- public(is_layout/1).
	:- mode(is_layout(+char), zero_or_one).
	:- info(is_layout/1, [
		comment is 'True if the argument is a layout character.',
		argnames is ['Char']]).

	:- public(is_quote/1).
	:- mode(is_quote(+char), zero_or_one).
	:- info(is_quote/1, [
		comment is 'True if the argument is a quote character.',
		argnames is ['Char']]).

	:- public(is_punctation/1).
	:- mode(is_punctation(+char), zero_or_one).
	:- info(is_punctation/1, [
		comment is 'True if the argument is a punctation character.',
		argnames is ['Char']]).

	:- public(lower_upper/2).
	:- mode(lower_upper(?char, ?char), zero_or_more).
	:- mode(lower_upper(+char, ?char), zero_or_one).
	:- mode(lower_upper(?char, +char), zero_or_one).
	:- info(lower_upper/2, [
		comment is 'Converts between lower and upper case letters.',
		argnames is ['Char1', 'Char2']]).

:- end_protocol.
