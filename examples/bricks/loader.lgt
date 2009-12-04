
:- initialization((
	logtalk_load(library(all_loader), [reload(skip)]),	% allow for static binding
	logtalk_load(roots(loader), [reload(skip)]),		% allow for static binding
	logtalk_load(relations(loader), [reload(skip)]),	% allow for static binding
	% compile messages with event support and turn event support on in order to 
	% both use the "stack_monitor" monitor for visualizing stack changes and to
	% allow the constrained relation "brick_stack" to perform its magic:
	logtalk_load(bricks, [events(allow)]),
	set_logtalk_flag(events, allow))).
