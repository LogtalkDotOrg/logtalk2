
:- use_module(library(problog), []).
:- use_module(library(dtproblog), []).
:- use_module(library(problog_learning), []).

:- op(550, yfx, ~).
:- op(550, yfx, =>).


:- object(hook,
	implements(expanding)).

	term_expansion(('=>'(Head,N) :- Body), [(:- multifile(user::'=>'/2)), (user::'=>'(THead,N) :- Body)]) :-
		{'$lgt_tr_predicate_heads'(Head, THead)}.

	term_expansion((Prob~Head :- Body), [ExpandedClause]) :-
		{'$lgt_tr_predicate_heads'(Head, THead)},
		{'$lgt_tr_predicate_heads'(Body, TBody)},
		problog:term_expansion_intern((Prob::THead :- TBody), user, ExpandedClause).

	term_expansion(Prob~Fact, []) :-
		{'$lgt_tr_predicate_heads'(Fact, TFact)},
		problog:problog_assert((Prob::TFact)).

:- end_object.
