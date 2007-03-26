
:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),
	logtalk_load([timer, message_counter, stop_watch], [events(on)]))).
