:- encoding('UTF-32').		% this directive, when present, must be the first
							% term, in the first line, of a source file

:- object(tests_utf_32,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "encodings" example.']).

	test(encodings_utf_32_1) :-
		findall(Greek-English, mythology::divinity(English, Greek), Solutions),
		Solutions == ['Ηρα'-hera, 'Καλυψω'-kalypso, 'Μορφευς'-morpheus, 'Ποσειδων'-poseidon, 'Ζευς'-zeus].

:- end_object.
