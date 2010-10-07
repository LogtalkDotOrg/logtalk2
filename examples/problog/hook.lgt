
:- use_module(library(problog), []).
:- use_module(library(dtproblog), []).
:- use_module(library(problog_learning), []).

:- op(550, yfx, ~).		% alternative to ProbLog (::)/2 operator
:- op(550, yfx, =>).


:- object(hook,
	implements(expanding)).

	term_expansion((:- set_problog_flag(Flag, Value)), [{(:- flags:set_problog_flag(Flag, Value))}]).

	term_expansion((:- problog_table(PI)), [{(:- tabling:problog_table(user:TPI))}]) :-
		{'$lgt_tr_predicate_indicators'(PI, TPI)}.

	term_expansion(('=>'(Head,N) :- Body), [(:- multifile(user::'=>'/2)), (user::'=>'(THead,N) :- {TBody})]) :-
		{'$lgt_tr_predicate_heads'(Head, THead, context)},
		{'$lgt_tr_predicate_heads'(Body, TBody, context)}.

	term_expansion((Prob~Head :- Body), [ExpandedClause]) :-
		{'$lgt_tr_predicate_heads'(Head, THead, context)},
		{'$lgt_tr_predicate_heads'(Body, TBody, context)},
		problog:term_expansion_intern((Prob::THead :- TBody), user, ExpandedClause).

	term_expansion(Prob~Fact, []) :-
		{'$lgt_tr_predicate_heads'(Fact, TFact)},
		problog:problog_assert((Prob::TFact)).

:- end_object.
