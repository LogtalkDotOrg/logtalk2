
:- initialization((
		logtalk_load(functions2d,   [reload(skip)]),	% allow for static binding
		logtalk_load(volumes2d,     [reload(skip)]),	% allow for static binding
		logtalk_load(integration2d, [])
	)).
