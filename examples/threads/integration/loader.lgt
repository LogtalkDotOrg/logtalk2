
:- initialization((
	logtalk_load(functions,   [reload(skip)]),	% allow for static binding
	logtalk_load(areas,       [reload(skip)]),	% allow for static binding
	logtalk_load(integration, [reload(skip)]))).
