
:- initialization(
	logtalk_load([
		initialization,
		classp, class,
		objectp, object,
		abstract_classp, abstract_class,
		nil])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load(
		[initialization,
		classp, class,
		objectp, object,
		abstract_classp, abstract_class,
		nil], [doctype(standalone)])).
*/
