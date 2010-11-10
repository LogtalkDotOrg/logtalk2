
:- object(chr_hook,
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2010/11/09,
		comment is 'Hook object for compiling objects and categories containing CHR code.']).

	term_expansion((:- chr_constraint(F/A)), [{(:- chr_constraint(TF/TA))}, (:- dynamic(F/A))]) :-
		logtalk::compile_predicate_indicators(F/A, TF/TA).

	term_expansion((:- chr_constraint(H)), [{(:- chr_constraint(TH))}, (:- dynamic(F/A))]) :-
		functor(H, F, A),
		logtalk::compile_predicate_heads(H, TH, '?'(any)).

	% for now, CHR type declarations are global
	term_expansion((:- chr_type(T)), [{(:- chr_type(T))}]).

	term_expansion((:- Directive), [(:- Directive)| Annotations]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		chr_annotations(Annotations).

	chr_annotations([
		(:- annotation('@'(*,0))),
		(:- annotation('==>'(0,0))),
		(:- annotation('<=>'(0,0))),
		(:- annotation('|'(0,0))),
		(:- annotation('\\'(0,0)))
	]).

%	:- multifile(user:portray/1).
%	:- dynamic(user:portray/1).
%	user:portray(THead) :-
%		logtalk::decompile_predicate_head(THead, Entity, _, Head),
%		writeq(Entity::Head).

:- end_object.
