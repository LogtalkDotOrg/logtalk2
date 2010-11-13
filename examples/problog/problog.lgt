
:- category(problog_inference).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2010/11/13,
		comment is 'Interface predicates for ProbLog integration.']).

	:- public(problog_exact/3).

	problog_exact(problog_neg(A), B, C) :-
		!,
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_exact(tabling:problog_neg(user:TA), B, C).
	problog_exact(A, B, C) :-
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_exact(TA, B, C).

	:- public(problog_max/3).

	problog_max(A,B,C) :-
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_max(TA, B, TC),
		problog_utilities::decompile_facts(TC, This, C).

	:- public(problog_kbest/4).

	problog_kbest(A, B, C, D) :-
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_kbest(TA, B, C, D).

	:- public(problog_montecarlo/3).

	problog_montecarlo(A, B, C) :-
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_montecarlo(TA, B, C).

	:- public(problog_delta/5).

	problog_delta(A, B, C, D, E) :-
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_delta(TA, B, C, D, E).

	:- public(problog_threshold/5).

	problog_threshold(A, B, C, D, E) :-
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_threshold(TA, B, C, D, E).

	:- public(problog_low/4).

	problog_low(A, B, C, D) :-
		this(This),
		logtalk::compile_predicate_heads(A, This, TA, _),
		problog:problog_low(TA, B, C, D).

:- end_category.



:- category(problog_decision_theory).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2010/11/13,
		comment is 'Interface predicates for ProbLog integration.']).

	:- public(dtproblog_solve/2).

	dtproblog_solve(Strategy, ExpectedValue) :-
		this(This),
		dtproblog:dtproblog_solve(TStrategy, ExpectedValue),
		problog_utilities::decompile_facts(TStrategy, This, Strategy).

	:- public(dtproblog_ev/2).

	dtproblog_ev(A, B) :-
		this(This),		
		logtalk::compile_predicate_heads(A, This, TA, _),
		dtproblog:dtproblog_ev(TA, B).

	:- public(dtproblog_utility_facts/1).

	dtproblog_utility_facts(Facts) :-
		dtproblog:dtproblog_utility_facts(TFacts),
		decompile_imp_facts(TFacts, Facts).

	decompile_imp_facts([], []).
	decompile_imp_facts(['=>'(TFact,Value)| TFacts], ['=>'(Fact,Value)| Facts]) :-
		this(This),
		problog_utilities::decompile_facts([TFact], This, [Fact]),
		decompile_imp_facts(TFacts, Facts).

	:- public(dtproblog_decisions/1).

	dtproblog_decisions(Decisions) :-
		this(This),
		dtproblog:dtproblog_decisions(TDecisions),
		problog_utilities::decompile_facts(TDecisions, This, Decisions).

:- end_category.



:- category(problog_learning).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2010/11/13,
		comment is 'Interface predicates for ProbLog integration.']).

	:- public(do_learning/1).

	do_learning(A) :-
		learning:do_learning(A).

:- end_category.
