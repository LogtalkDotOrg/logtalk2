:- encoding('ISO-8859-1').	% this directive, when present, must be the first
							% term, in the first line, of a source file


:- object(tests_iso_8859_1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "encodings" example.']).

	test(encodings_iso_8859_1_1) :-
		findall(Name, latin::name(Name), Solutions),
		Solutions == ['António Simões', 'Cátia Conceição', 'João Raínho', 'Luís Araújo'].

:- end_object.
