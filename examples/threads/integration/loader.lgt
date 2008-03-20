
:- initialization((
	logtalk_load(coeficients, [reload(skip)]),	% allow for static binding
	logtalk_load(functions,   [reload(skip)]),	% allow for static binding
	logtalk_load(integration, [reload(skip)]))).
