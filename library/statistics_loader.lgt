
:- initialization((
	logtalk_load([statisticsp, statistics], [reload(skip)]),    % allow for static binding
	logtalk_load([population, sample], [reload(skip)]))).	    % allow for static binding
