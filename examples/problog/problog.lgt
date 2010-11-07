
:- category(utilities).

	:- private(decompile_facts/2).

	decompile_facts([], []).
	decompile_facts([TFact| TFacts], [Fact| Facts]) :-
		this(This),		
		logtalk::decompile_predicate_head(TFact, This, _, Fact),
		decompile_facts(TFacts, Facts).

:- end_category.



:- category(problog,
	extends(utilities)).

	:- public(problog_exact/3).

	problog_exact(problog_neg(A),B,C) :-
		!,
		this(This),
		problog:problog_exact(tabling:problog_neg(This::A),B,C).
	problog_exact(A,B,C) :-
		this(This),
		problog:problog_exact(This::A,B,C).

	:- public(problog_max/3).

	problog_max(A,B,C) :-
		this(This),
		problog:problog_max(This::A,B,TC),
		::decompile_facts(TC, C).

	:- public(problog_kbest/4).

	problog_kbest(A, B, C, D) :-
		this(This),
		problog:problog_kbest(This::A, B, C, D).

	:- public(problog_montecarlo/3).

	problog_montecarlo(A,B,C) :-
		this(This),
		problog:problog_montecarlo(This::A,B,C).

	:- public(problog_delta/5).

	problog_delta(A, B, C, D, E) :-
		this(This),
		problog:problog_delta(This::A, B, C, D, E).

	:- public(problog_threshold/5).

	problog_threshold(A, B, C, D, E) :-
		this(This),
		problog:problog_threshold(This::A, B, C, D, E).

	:- public(problog_low/4).

	problog_low(A, B, C, D) :-
		this(This),
		problog:problog_low(This::A, B, C, D).

:- end_category.



:- category(dtproblog,
	extends(utilities)).

	:- public(dtproblog_solve/2).

	dtproblog_solve(Strategy, ExpectedValue) :-
		dtproblog:dtproblog_solve(TStrategy, ExpectedValue),
		::decompile_facts(TStrategy, Strategy).

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
		::decompile_facts([TFact], [Fact]),
		decompile_imp_facts(TFacts, Facts).

	:- public(dtproblog_decisions/1).

	dtproblog_decisions(Decisions) :-
		dtproblog:dtproblog_decisions(TDecisions),
		::decompile_facts(TDecisions, Decisions).

:- end_category.



:- category(problog_learning).

	:- public(do_learning/1).

	do_learning(A) :-
		learning:do_learning(A).

:- end_category.
