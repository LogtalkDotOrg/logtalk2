
:- initialization((
	logtalk_load([termp, term], [reload(skip)]),	% allow for static binding
	logtalk_load(lgtunit, [reload(skip)]))).
