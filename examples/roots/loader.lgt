
:- initialization((
	logtalk_load(
		[library(events_loader), library(types_loader), library(hierarchies_loader)],
		% allow for static binding
		[reload(skip)]),
	logtalk_load(
		[(initialization)],
		% allow for static binding
		[reload(skip)]),
	logtalk_load(
		[classes, prototypes, nil],
		% allow for static binding and avoid warnings due to the use of a reflective design
		[reload(skip), unknown(silent)]))).
