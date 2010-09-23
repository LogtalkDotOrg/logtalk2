
:- use_module(library(problog), []).
:- use_module(library(dtproblog), []).
:- use_module(library(problog_learning), []).

:- op(600, xfx, #).
:- op(550, yfx, =>).


:- object(hook,
	implements(expanding)).

	:- private(probabilistic/3).
	:- dynamic(probabilistic/3).

	term_expansion((:- probabilistic(Functor/Arity)), []) :-
		logtalk_load_context(entity_prefix, Prefix),
		atom_concat(Prefix, Functor, TFunctor),
		assertz(probabilistic(Functor, Arity, TFunctor)).

	term_expansion('=>'(A,B), [user:'=>'(A,B)]).

	term_expansion((Prob#Head :-Body), []) :-
		functor(Head, Functor, Arity),
		probabilistic(Functor, Arity, TFunctor),
		Head =.. [Functor| Args],
		THead =.. [TFunctor| Args],
		problog:problog_assert((Prob::THead:-Body)).

	term_expansion(Prob#Fact, []) :-
		functor(Fact, Functor, Arity),
		probabilistic(Functor, Arity, TFunctor),
		Fact =.. [Functor| Args],
		TFact =.. [TFunctor| Args],
		problog:problog_assert((Prob::TFact)).

	term_expansion((:- end_object), [(:- end_object)]) :-
		retractall(probabilistic(_, _, _)).

	goal_expansion(Goal, Expanded) :-
		functor(Goal, Functor, Arity),
		(	probabilistic(Functor, Arity, TFunctor) ->
			Goal =.. [Functor| Args],
			TGoal =.. [TFunctor| Args],
			Expanded = {TGoal}
		;	problog:goal_expansion(Goal, Expanded)
		).

:- end_object.
