
:- initialization((
	logtalk_load(hook),
	logtalk_load(object, [hook(hook), xmldocs(on)]))). 
