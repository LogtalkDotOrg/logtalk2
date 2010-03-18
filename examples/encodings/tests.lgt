
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "encodings" example.']).

	test(encodings_1) :-
		findall(Code-Text, babel::hello_world(Code, Text), Solutions),
		Solutions == [el-'Γειάσου κόσμος!', en-'Hello world!', es-'¡Hola mundo!', ja-'こんにちは世界!', ko-'여보세요 세계!', nl-'Hallo wereld!', pt-'Olá mundo!', ru-'Здравствулте! мир!', zh-'你好世界!'].

	test(encodings_2) :-
		findall(Name, latin::name(Name), Solutions),
		Solutions == ['António Simões', 'Cátia Conceição', 'João Raínho', 'Luís Araújo'].

	test(encodings_3) :-
		findall(Name-Capital-Country, asian::country(Country, Name, Capital), Solutions),
		Solutions == ['中国'-'北京'-china, '日本'-'東京'-japan, 'Монгол Улс'-'Улаанбатаар'-mongolia, '臺灣'-'臺北'-taiwan, 'Тоҷикистон'-'Душанбе'-tajikistan].

	% test 4. only do this test for some dialects
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

		test(encodings_4,true).

	:- else.

		test(encodings_4) :-
			findall(Greek-English, mythology::divinity(English, Greek), Solutions),
			Solutions == ['Ηρα'-hera, 'Καλυψω'-kalypso, 'Μορφευς'-morpheus, 'Ποσειδων'-poseidon, 'Ζευς'-zeus].

	:- endif.

:- end_object.
