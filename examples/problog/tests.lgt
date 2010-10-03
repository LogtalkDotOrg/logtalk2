
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "problog" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	% explanation probability (and facts involved)
	test(problog_graph_1) :-
		graph::problog_max(path(1,4),Prob,FactsUsed),
		FactsUsed == [dir_edge(1,2),dir_edge(2,3),dir_edge(3,4)],
		Prob =~= 0.432.

	% success probability
	test(problog_graph_2) :-
		graph::problog_exact(path(1,4),Prob,Status),
		% 8 proofs
		Prob =~= 0.53864,
		Status == ok.

	% lower bound using 4 best proofs
	test(problog_graph_3) :-
		graph::problog_kbest(path(1,4),4,Prob,Status),
		% 4 proofs
		Prob =~= 0.517344,
		Status == ok.

	% approximation using monte carlo, to reach 95%-confidence interval width 0.01
	test(problog_graph_4) :-
		graph::problog_montecarlo(path(1,4),0.01,Prob),
		Prob =~= 0.536475.

	% upper and lower bound using iterative deepening, final interval width 0.01
	test(problog_graph_5) :-
		graph::problog_delta(path(1,4),0.01,Bound_low,Bound_up,Status),
		Bound_low =~= 0.5354096,
		Bound_up =~= 0.53864,
		Status == ok.

	% upper and lower bound obtained cutting the sld tree at probability 0.1 for each branch
	test(problog_graph_6) :-
		graph::problog_threshold(path(1,4),0.1,Bound_low,Bound_up,Status),
		% 4 proofs
		Bound_low =~= 0.517344,
		Bound_up =~= 0.563728,
		Status == ok.

	% lower bound obtained cutting the sld tree at probability 0.2 for each branch
	test(problog_graph_7) :-
		graph::problog_low(path(1,4),0.2,Bound_low,Status),
		% 1 proofs
		Bound_low =~= 0.432,
		Status == ok.

	test(problog_office_1) :-
		office::problog_exact(room_has_window, Prob, Status),
		Prob =~= 0.01517076,
		Status == ok.

:- end_object.
