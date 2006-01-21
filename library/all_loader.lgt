
:- initialization(
	logtalk_load([
		datep, date,									% dates
		timep, time,
		
		event_handlersp,								% events
		event_registryp, event_registry,
		before_event_registry, after_event_registry,
		monitorp, monitor,
		
		event_dbgp,										% debugging
		event_dbg, 

        subject,                                     	% dependents
        observer,

		hierarchyp,										% hierarchies
		proto_hierarchyp, proto_hierarchy,
		class_hierarchyp, class_hierarchy,

		metap, meta,									% metapredicates
		loopp, loop,

		randomp,										% random
		random,

		termp, term,									% types
		atomic,
		atom, callable,
		characterp, character,
		number, float, integer, natural,
		compound,
		listp, list, difflist,
		numberlistp, numberlist,
		varlist,
		queuep, queue,
		dictionaryp, bintree,
		setp, set,
		comparingp])).

/*
If you intend to use the FOP XSL:FO processor for generating PDF documenting
files, comment the directive above and uncomment the directive below

:- initialization(
	logtalk_load([
		datep, date,									% dates
		timep, time,
		
		event_handlersp,								% events
		event_registryp, event_registry,
		before_event_registry, after_event_registry,
		monitorp, monitor,
		
		event_dbgp,										% debugging
		event_dbg, 

        subject,                                     	% dependents
        observer,

		hierarchyp,										% hierarchies
		proto_hierarchyp, proto_hierarchy,
		class_hierarchyp, class_hierarchy,

		metap, meta,									% metapredicates
		loopp, loop,

		randomp,										% random
		random,

		termp, term,									% types
		atomic,
		atom, callable,
		characterp, character,
		number, float, integer, natural,
		compound,
		listp, list, difflist,
		numberlistp, numberlist,
		varlist,
		queuep, queue,
		dictionaryp, bintree,
		setp, set,
		comparingp],
		[xmlsref(standalone)])).
*/
