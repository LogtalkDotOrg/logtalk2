
:- initialization(
	logtalk_load([
		termp, term,
		atomic,
		atom, callable,
		characterp, character,
		number, float, integer, natural,
		compound,
		pairs,
		listp, list, list1,
		difflist,
		numberlistp, numberlist,
		varlistp, varlist,
		queuep, queue,
		dictionaryp, bintree, rbtree,
		setp, set, set1,
		comparingp],
		[reload(skip)])).	% allow for static binding
