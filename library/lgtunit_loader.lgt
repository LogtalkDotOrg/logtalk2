
:- initialization((
	logtalk_load([library(types_loader),  library(dates_loader)]),
	logtalk_load(lgtunit, [reload(skip)]))).	% allow for static binding
