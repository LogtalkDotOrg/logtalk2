
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "dcgs" example.']).

	test(dcgs_1) :-
		findall(Result,calculator::parse("1+2-3*4", Result), Solutions),
		Solutions == [-9].

	test(dcgs_2) :-
		macaddr::valid("00:1e:4a:ef:72:8b").

	test(dcgs_3) :-
		findall(Message, morse::phrase(morse(Message), "... --- ..."), Solutions),
		Solutions == [[sos]].

	test(dcgs_4) :-
		findall(Message, enigma::solve("4 96853 5683 86 4283 346637 9484 968 8664448", Message), Solutions),
		Solutions == [[i, would, love, to, have, dinner, with, you, tonight]].

	test(dcgs_5) :-
    	sentence::parse([the, girl, likes, the, boy], Result),
		Result == true.

	test(dcgs_6) :-
		sentence::parse([the, girl, scares, the, boy], Result),
		Result == false.

	test(dcgs_7) :-
		findall(Tree, parsetree::parse([the, girl, likes, the, boy], Tree), Trees),
		Trees == [s(np(d(the), n(girl)), vp(v(likes), np(d(the), n(boy))))].

	test(dcgs_8) :-
		findall(L, bom::parts(bike, L), Solutions),
		Solutions == [[frame, crank, pedal, pedal, chain, spokes, rim, hub, spokes, rim, hub]].

	test(dcgs_9) :-
		findall(L,bom::parts(wheel, L), Solutions),
		Solutions == [[spokes, rim, hub]].

	test(dcgs_10) :-
		findall(L, shell::parse("pwd; cd ..; ls -a", L), Solutions),
		Solutions == [[pwd,'cd ..','ls -a']].

	% test 11.  % complicated because of comparison of floats
	test(dcgs_11) :-
		tokenizer::tokens(" We owe $1,048,576.24 to Agent 007 for Version 3.14159! ", Tokens),
		Tokens = [Tok1,Tok2, Tok3, Number| Rest],
		Error is abs(1048576.24 - Number),	% Wow the error is huge (>3)
		Tok1 == we, Tok2 == owe, Tok3 == '$', Error < 5, Rest == [to,agent,7,for,version,3.14159,!].

	test(dcgs_12) :-
		findall(Ending, walker::walk([n(5), e(4), s(2), nw(8), s(5), se(1), n(4)], Ending), Endings),
		Endings = [(A, B)], 
		Error1 is abs(A - -0.94974746830583223), 
		Error2 is abs(B - 6.9497474683058318),
		Error1 < 0.00001, 
		Error2 < 0.00001.

	test(dcgs_13) :-
		findall(XML, xml::convert(word(child, children), word(singular, plural), XML), Solutions),
		Solutions == ['<word><singular>child</singular><plural>children</plural></word>'].

	test(dcgs_14) :-
		findall(Term-Interpretation, xml::convert(Term, Interpretation, '<word><singular>child</singular><plural>children</plural></word>'), Solutions),
		Solutions == [word(child, children)-word(singular, plural)].

	test(dcgs_15) :-
		findall(Components, url::parse("http://logtalk.org", Components), Solutions),
		Solutions == [[protocol(http), address([logtalk, org]), path([]), file('')]].

	test(dcgs_16) :-
		findall(Components, url::parse("http://logtalk.org/", Components), Solutions),
		Solutions == [[protocol(http), address([logtalk, org]), path(['']), file('')]].

	test(dcgs_17) :-
		findall(Components, url::parse("http://logtalk.org/cvs", Components), Solutions),
		Solutions == [[protocol(http), address([logtalk, org]), path([cvs]), file('')]].

	test(dcgs_18) :-
		findall(Components, url::parse("http://logtalk.org/cvs.html", Components), Solutions),
		Solutions == [[protocol(http), address([logtalk, org]), path([]), file('cvs.html')]].

	test(dcgs_19) :-
		findall(Components, url::parse("http://logtalk.org/files/update", Components), Solutions),
		Solutions == [[protocol(http), address([logtalk, org]), path([files,update]), file('')]].

	test(dcgs_20) :-
		bypass::phrase(foo, _, _).

:- end_object.
