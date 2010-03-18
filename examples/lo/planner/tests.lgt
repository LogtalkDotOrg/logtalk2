
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "lo/planner" example.']).

	test(lo_planner_1) :-
		plan(london)::from(imperial, aiai, L),
		L == [[taxi(imperial,lhr)]-[fly(lhr,edin)]-[taxi(edin,aiai)]].

:- end_object.
