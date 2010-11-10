
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
		date is 2010/11/09,
		comment is 'Hook object for compiling objects and categories containing ProbLog code.']).

	term_expansion((:- set_problog_flag(Flag, Value)), [{(:- flags:set_problog_flag(Flag, Value))}]).

	term_expansion((:- problog_table(PI)), [{(:- problog:problog_table(user:TPI))}]) :-
		logtalk::compile_predicate_indicators(PI, TPI).

	term_expansion(('=>'(Head,N) :- Body), [(:- annotation('=>'(0, *))), ('=>'(Head,N) :- Body)]).

	term_expansion(('~'(Prob, Head) :- Body), [(:- annotation('~'(*, 0))), ('~'(Prob, Head) :- Body)]).

	term_expansion('~'(Prob, Fact), [(:- annotation('~'(*, 0))), '~'(Prob, Fact)]).

	:- multifile(user:term_expansion/2).
	:- dynamic(user:term_expansion/2).

	user:term_expansion('~'(Prob, Fact), Expansion) :-
		user:term_expansion('::'(Prob, Fact), Expansion).

	user:term_expansion(('~'(Prob, Head) :- Body), Expansion) :-
		user:term_expansion(('::'(Prob, Head) :- Body), Expansion).

:- end_object.
