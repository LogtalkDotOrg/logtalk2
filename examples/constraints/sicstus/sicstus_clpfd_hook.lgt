
:- object(sicstus_clpfd_hook,
	implements(expanding)).

	:- info([
		version is 0.2,
		author is 'Paulo Moura',
		date is 2011/02/14,
		comment is 'Hook object for compiling objects and categories containing CLP(FD) code when using SICStus Prolog.']).

	term_expansion((:- Directive), [(:- Directive)| Annotations]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		clpfd_annotations(Annotations).

	term_expansion('+:'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).
	term_expansion('-:'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).
	term_expansion('+?'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).
	term_expansion('-?'(Head, Body), [{'+:'(THead, Body)}, (Head :- {THead})]) :-
		compile_indexical_head(Head, THead).

	compile_indexical_head(Head, THead) :-
		logtalk::compile_predicate_heads(Head, CHead),	% remove execution-context argument
		CHead =.. [CFunctor| CArgs],
		copy_args_except_last(CArgs, TArgs),
		THead =.. [CFunctor| TArgs].

	copy_args_except_last([_], []) :-
		!.
	copy_args_except_last([Arg| CArgs], [Arg| TArgs]) :-
		copy_args_except_last(CArgs, TArgs).

	clpfd_annotations([
		(:- meta_predicate(clpfd:labeling(*,*))),
		(:- meta_predicate(clpfd:'#\\'(*))),
		(:- meta_predicate(clpfd:'#/\\'(*,*))),
		(:- meta_predicate(clpfd:'#\\'(*,*))),
		(:- meta_predicate(clpfd:'#\\/'(*,*))),
		(:- meta_predicate(clpfd:'#=>'(*,*))),
		(:- meta_predicate(clpfd:'#<='(*,*))),
		(:- meta_predicate(clpfd:'#<=>'(*,*)))
	]).

	:- multifile(user::portray/1).
	:- dynamic(user::portray/1).
	user::portray(THead) :-
		callable(THead),
		logtalk::decompile_predicate_head(THead, Entity, _, Head),
		writeq(Entity::Head).

:- end_object.
