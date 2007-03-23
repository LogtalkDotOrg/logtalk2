
:- initialization((
	logtalk_load([library(hierarchies_loader), library(types_loader)], [reload(skip)]),
	logtalk_load([ovals, polygons, sorting]))).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization((
	logtalk_load([library(hierarchies_loader), library(types_loader)], [reload(skip)]),
	logtalk_load([ovals, polygons, sorting], [xmlsref(standalone)]))).
*/
