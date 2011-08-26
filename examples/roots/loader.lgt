
:- initialization((
	logtalk_load(
		[library(events_loader), library(types_loader), library(hierarchies_loader)],
		[reload(skip)]),		% allow for static binding
	logtalk_load(
		[(initialization)],
		[reload(skip)]),		% allow for static binding
	logtalk_load(
		[classes, prototypes, nil],
		[unknown(silent)]))).	% avoid warnings due to the use of a reflective design
