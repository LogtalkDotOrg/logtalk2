
:- initialization((
	logtalk_load([types_loader, dates_loader]),
	logtalk_load(lgtunit, [reload(skip)]))).	% allow for static binding
