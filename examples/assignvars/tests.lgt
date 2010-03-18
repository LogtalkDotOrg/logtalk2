
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "assignvars" example.']).

	test(assignvars_1) :-
		rectangle(2, 3, _)::(init, position(X0, Y0), move(3, 7), position(X1, Y1), move(2, 5), position(X2, Y2)),
		X0==0, Y0==0, X1==3, Y1==7, X2==2, Y2==5.

	test(assignvars_2) :-
		findall(T-I-F, {fsm(T, I, F)}::recognise([0,1,1,2,1,2,0]), Solutions),
		Solutions == [[red-0-red, red-1-green, red-2-red, yellow-0-red, yellow-1-green, yellow-2-red, green-0-yellow, green-1-yellow, green-2-red]-red-[red]].

	test(assignvars_3) :-
		\+ {fsm(_T, _I, _F)}::recognise([0,1,1,2,1,2,1,0]).

:- end_object.
