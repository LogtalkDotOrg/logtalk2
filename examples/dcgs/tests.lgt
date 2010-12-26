
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.21,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/07/10,
		comment is 'Unit tests for the "dcgs" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(dcgs_01) :-
		findall(Result,calculator::parse("1+2-3*4", Result), Solutions),
		Solutions == [-9].

	test(dcgs_02) :-
		macaddr::valid("00:1e:4a:ef:72:8b").

	test(dcgs_03) :-
		findall(Message, logtalk << phrase(morse::morse(Message), "... --- ..."), Solutions),
		Solutions == [[sos]].

	test(dcgs_04) :-
		findall(Message, enigma::solve("4 96853 5683 86 4283 346637 9484 968 8664448", Message), Solutions),
		Solutions == [[i, would, love, to, have, dinner, with, you, tonight]].

	test(dcgs_05) :-
    	sentence::parse([the, girl, likes, the, boy], Result),
		Result == true.

	test(dcgs_06) :-
		sentence::parse([the, girl, scares, the, boy], Result),
		Result == false.

	test(dcgs_07) :-
		findall(Tree, parsetree::parse([the, girl, likes, the, boy], Tree), Trees),
		Trees == [s(np(d(the), n(girl)), vp(v(likes), np(d(the), n(boy))))].

	test(dcgs_08) :-
		findall(L, bom::parts(bike, L), Solutions),
		Solutions == [[frame, crank, pedal, pedal, chain, spokes, rim, hub, spokes, rim, hub]].

	test(dcgs_09) :-
		findall(L,bom::parts(wheel, L), Solutions),
		Solutions == [[spokes, rim, hub]].

	test(dcgs_10) :-
		findall(L, shell::parse("pwd; cd ..; ls -a", L), Solutions),
		Solutions == [[pwd,'cd ..','ls -a']].

	% test 11.  % complicated because of comparison of floats
	test(dcgs_11) :-
		tokenizer::tokens(" We owe $1,048,576.24 to Agent 007 for Version 3.14159! ", Tokens),
		Tokens = [Tok1, Tok2, Tok3, Number| Rest],
		Number =~= 1048576.24,	% Wow the error is huge (>3)
		Tok1 == we, Tok2 == owe, Tok3 == ('$'), Rest == [to,agent,7,for,version,3.14159,!].

	test(dcgs_12) :-
		findall(Ending, walker::walk([n(5), e(4), s(2), nw(8), s(5), se(1), n(4)], Ending), Endings),
		Endings = [(A, B)],
		A =~= -0.94974746830583223,
		B =~=  6.9497474683058318.

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
		logtalk << phrase(bypass::foo, _, _).

	% test access to the grammar rule implicit list of tokens using the call//1 built-in
	% non-terminal and lambda expressions:

	test(dcgs_21) :-
		logtalk << phrase(call([[], []]>>true), [], []).

	test(dcgs_22) :-
		logtalk << phrase(call([[], []]>>true), Input, Rest),
		Input == [], Rest == [].

	test(dcgs_23) :-
		logtalk << phrase(call([Input, Rest]>>(set::subtract(Input, Rest, [1]))), [1,2,3], [2,3]).

	% cuts in the first argument of phrase/2-3 calls must be local and not extend outside:

	test(dcgs_24) :-
		findall(X, (list::member(X, [1,2,3]), logtalk << phrase(!, _)), Xs),
		Xs == [1,2,3].

	test(dcgs_25) :-
		findall(X, (list::member(X, [1,2,3]), logtalk << phrase(!, _, _)), Xs),
		Xs == [1,2,3].

:- end_object.
