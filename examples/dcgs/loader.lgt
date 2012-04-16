
:- initialization(
	logtalk_load([
		parsep,
		enigma,
		parsetree,
		sentences,
		tokenizer,
		morse,
		shell,
		walker,
		bom,
		faa,
		bypass,
		dcgtest
	])
). 

:- if(\+ current_logtalk_flag(prolog_dialect, lean)).

	% Lean Prolog doesn't support the 0'<char> used in these examples
	:- initialization(
		logtalk_load([
			calculator,
			macaddr,
			url,
			xml
		])
	). 

:- endif.
