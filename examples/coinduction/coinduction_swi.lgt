
:- object(coinduction_swi,
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2010/07/23,
		comment is 'Wrapper for the SWI-Prolog "coinduction" library. Supports coinductive code in Logtalk objects when used as a hook object.']).

	term_expansion((:- coinductive(Spec)), [(:- multifile(coinduction:coinductive_declaration/2))| Expanded]) :-
		{'$lgt_tr_predicate_indicators'(Spec, SpecFixed)},
		system:term_expansion((:- coinductive(SpecFixed)), Expanded).

:- end_object.
