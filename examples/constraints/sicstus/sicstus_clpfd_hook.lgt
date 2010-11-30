
:- object(sicstus_clpfd_hook,
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2010/11/12,
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

	clpfd_annotations([
		(:- meta_predicate(clpfd:labeling(*,*))),
		(:- meta_predicate(clpfd:'#\\'(*))),
		(:- meta_predicate(clpfd:'#/\\'(*,*))),
		(:- meta_predicate(clpfd:'#\\'(*,*))),
		(:- meta_predicate(clpfd:'#\\/'(*,*))),
		(:- meta_predicate(clpfd:'#=>'(*,*))),
		(:- meta_predicate(clpfd:'#<='(*,*))),
		(:- meta_predicate(clpfd:'#<=>'(*,*))),
		(:- annotation('+:'(0,*))),
		(:- annotation('-:'(0,*))),
		(:- annotation('+?'(0,*))),
		(:- annotation('+?'(0,*)))
	]).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap; Dialect == sicstus))).
		:- multifile(user:portray/1).
		:- dynamic(user:portray/1).
		user:portray(THead) :-
			callable(THead),
			logtalk::decompile_predicate_head(THead, Entity, _, Head),
			writeq(Entity::Head).
	:- endif.

:- end_object.
