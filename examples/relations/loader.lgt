
:- initialization((
	logtalk_load([library(events_loader), library(types_loader), library(hierarchies_loader)], [reload(skip)]),
	logtalk_load(roots(loader), [reload(skip)]),
	logtalk_load(relations))).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization((
	logtalk_load([library(events_loader), library(types_loader), library(hierarchies_loader)], [reload(skip)]),
	logtalk_load(roots(loader), [reload(skip)]),
	logtalk_load(relations, [xmlsref(standalone)]))).
*/
