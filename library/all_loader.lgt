
:- initialization(
	logtalk_load([
		library(dates_loader),
		library(events_loader),
		library(debugging_loader),
		library(dependents_loader),
		library(hierarchies_loader),
		library(metapredicates_loader),
		library(random_loader),
		library(types_loader)])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load([
		library(dates_loader),
		library(events_loader),
		library(debugging_loader),
		library(dependents_loader),
		library(hierarchies_loader),
		library(metapredicates_loader),
		library(random_loader),
		library(types_loader)],
		[xmlsref(standalone)])).
*/
