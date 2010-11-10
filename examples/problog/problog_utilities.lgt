
:- object(problog_utilities).

	:- info([
		version is 0.3,
		author is 'Paulo Moura',
		date is 2010/11/09,
		comment is 'Utility predicates for supporting the ProbLog integration.']).

	:- public(decompile_facts/3).

	decompile_facts([], _, []).
	decompile_facts([TFact| TFacts], Obj, [Fact| Facts]) :-
		logtalk::decompile_predicate_head(TFact, Obj, _, Fact),
		decompile_facts(TFacts, Obj, Facts).

:- end_object.

