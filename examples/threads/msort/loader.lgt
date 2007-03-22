
:- initialization((
	logtalk_load(library(random_loader), [reload(skip)]),
	logtalk_load([generator, msort]))).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization((
	logtalk_load(library(random_loader), [reload(skip)]),
	logtalk_load([generator, msort], [xmlsref(standalone)]))).
*/
