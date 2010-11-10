
:- use_module(library(problog), []).
:- use_module(library(dtproblog), []).
:- use_module(library(problog_learning), []).


:- multifile(user:problog_user_ground/1).
user:problog_user_ground(THead) :-
	logtalk::decompile_predicate_head(THead, _, _, Head),
	ground(Head).


:- op(550, yfx, ~).		% alternative to ProbLog (::)/2 operator


:- object(problog_hook,
	implements(expanding)).

	:- info([
		version is 0.3,
		author is 'Paulo Moura',
		date is 2010/11/10,
		comment is 'Hook object for compiling objects and categories containing ProbLog code.']).

	term_expansion((:- set_problog_flag(Flag, Value)), [{(:- flags:set_problog_flag(Flag, Value))}]).

	term_expansion((:- problog_table(PI)), [{(:- problog:problog_table(user:TPI))}]) :-
		logtalk::compile_predicate_indicators(PI, TPI).

	term_expansion((:- Directive), [(:- Directive)| Annotations]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		problog_annotations(Annotations).

	problog_annotations([
		(:- annotation('=>'(0, *))),
		(:- annotation('~'(*, 0)))
	]).

	:- multifile(user:term_expansion/2).
	:- dynamic(user:term_expansion/2).

	user:term_expansion('~'(Prob, Fact), Expansion) :-
		user:term_expansion('::'(Prob, Fact), Expansion).

	user:term_expansion(('~'(Prob, Head) :- Body), Expansion) :-
		user:term_expansion(('::'(Prob, Head) :- Body), Expansion).

:- end_object.
