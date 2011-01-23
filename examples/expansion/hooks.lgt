
% we start by defining three hook objects; each hook object defines a term_expansion/2
% clause that expands a fact and a term_expansion/2 clause that adds a new clause to
% the expanded object

:- object(ha,
	implements(expanding)).		% hook objects should always reference
								% the "expanding" built-in protocol
	term_expansion(a, [aa]).
	term_expansion((:- end_object), [ha, (:- end_object)]).

:- end_object.


:- object(hb,
	implements(expanding)).

	% term_expansion/2 clauses can return a list of terms
	term_expansion(b, [bb]).
	term_expansion((:- end_object), [hb, (:- end_object)]).

:- end_object.


:- object(hc,
	implements(expanding)).

	% term_expansion/2 clauses may also return a single term
	term_expansion(c, cc).
	term_expansion((:- end_object), [hc, (:- end_object)]).

:- end_object.


% we can also combine hook objects by defining a new hook object whose term_expansion/2
% and goal_expansion/2 clauses reference the term_expansion/2 and goal_expansion/2 clauses
% in other hook objects

:- object(hh,
	implements(expanding)).

	% the term-expansion mechanism tries term_expansion/2 clauses until it finds one
	% that succeeds (or until all term_expansion/2 clauses are tried); thus more specifc
	% clauses should be listed before more general ones

	term_expansion((:- object(raw)), (:- object(cooked))).

	term_expansion((:- end_object), Expansion) :-
		ha::term_expansion((:- end_object), HA0),
		list::append(HA, [(:- end_object)], HA0),
		hb::term_expansion((:- end_object), HB0),
		list::append(HB, [(:- end_object)], HB0),
		hc::term_expansion((:- end_object), HC0),
		list::append(HC, [(:- end_object)], HC0),		
		list::append([HA, HB, HC, [(:- end_object)]], Expansion).

	% here we just try each individual hook object in succesion but more elaborate
	% combining schemes could be implemented if necessary

	term_expansion(Term, Expansion) :-
		ha::term_expansion(Term, Expansion).
	term_expansion(Term, Expansion) :-
		hb::term_expansion(Term, Expansion).
	term_expansion(Term, Expansion) :-
		hc::term_expansion(Term, Expansion).

:- end_object.
