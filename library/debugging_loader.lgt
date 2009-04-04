
:- initialization((
	logtalk_load(events_loader),
	logtalk_load([event_dbgp, event_dbg], [events(allow), reload(skip)]))).	% allow for static binding
