
:- initialization((
	logtalk_load(library(all_loader), [reload(skip)]),
	logtalk_load(roots(loader), [reload(skip)]),
	logtalk_load(ch))).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization((
	logtalk_load(library(all_loader), [reload(skip)]),
	logtalk_load(roots(loader), [reload(skip)]),
	logtalk_load(ch, [xmlsref(standalone)]))).
*/
