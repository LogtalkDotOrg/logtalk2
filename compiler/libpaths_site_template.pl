
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.22.0
%
%  Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- initialization(
	(assertz(logtalk_library_path(library, '$LOGTALKHOME/library/')),
	 assertz(logtalk_library_path(aliases, '$LOGTALKHOME/examples/aliases/')),
	 assertz(logtalk_library_path(benchmarks, '$LOGTALKHOME/examples/benchmarks/')),
	 assertz(logtalk_library_path(birds, '$LOGTALKHOME/examples/birds/')),
	 assertz(logtalk_library_path(bricks, '$LOGTALKHOME/examples/bricks/')),
	 assertz(logtalk_library_path(classvars, '$LOGTALKHOME/examples/classvars/')),
	 assertz(logtalk_library_path(dcgs, '$LOGTALKHOME/examples/dcgs/')),
	 assertz(logtalk_library_path(diamonds, '$LOGTALKHOME/examples/diamonds/')),
	 assertz(logtalk_library_path(dynpred, '$LOGTALKHOME/examples/dynpred/')),
	 assertz(logtalk_library_path(engines, '$LOGTALKHOME/examples/engines/')),
	 assertz(logtalk_library_path(errors, '$LOGTALKHOME/examples/errors/')),
	 assertz(logtalk_library_path(hello_world, '$LOGTALKHOME/examples/hello_world/')),
	 assertz(logtalk_library_path(inheritance, '$LOGTALKHOME/examples/inheritance/')),
	 assertz(logtalk_library_path(instmethods, '$LOGTALKHOME/examples/instmethods/')),
	 assertz(logtalk_library_path(planner, '$LOGTALKHOME/examples/lo/planner/')),
	 assertz(logtalk_library_path(travellers, '$LOGTALKHOME/examples/lo/travellers/')),
	 assertz(logtalk_library_path(logic, '$LOGTALKHOME/examples/logic/')),
	 assertz(logtalk_library_path(lpa, '$LOGTALKHOME/examples/lpa/')),
	 assertz(logtalk_library_path(metainterpreters, '$LOGTALKHOME/examples/metainterpreters/')),
	 assertz(logtalk_library_path(metapredicates, '$LOGTALKHOME/examples/metapredicates/')),
	 assertz(logtalk_library_path(mi, '$LOGTALKHOME/examples/mi/')),
	 assertz(logtalk_library_path(miscellaneous, '$LOGTALKHOME/examples/miscellaneous/')),
	 assertz(logtalk_library_path(msglog, '$LOGTALKHOME/examples/msglog/')),
	 assertz(logtalk_library_path(operators, '$LOGTALKHOME/examples/operators/')),
	 assertz(logtalk_library_path(parametric, '$LOGTALKHOME/examples/parametric/')),
	 assertz(logtalk_library_path(poem, '$LOGTALKHOME/examples/poem/')),
	 assertz(logtalk_library_path(points, '$LOGTALKHOME/examples/points/')),
	 assertz(logtalk_library_path(polygons, '$LOGTALKHOME/examples/polygons/')),
	 assertz(logtalk_library_path(profiling, '$LOGTALKHOME/examples/profiling/')),
	 assertz(logtalk_library_path(puzzles, '$LOGTALKHOME/examples/puzzles/')),
	 assertz(logtalk_library_path(reflection, '$LOGTALKHOME/examples/reflection/')),
	 assertz(logtalk_library_path(relations, '$LOGTALKHOME/examples/relations/')),
	 assertz(logtalk_library_path(roots, '$LOGTALKHOME/examples/roots/')),
	 assertz(logtalk_library_path(searching, '$LOGTALKHOME/examples/searching/')),
	 assertz(logtalk_library_path(shapes_ch, '$LOGTALKHOME/examples/shapes/ch/')),
	 assertz(logtalk_library_path(shapes_ph, '$LOGTALKHOME/examples/shapes/ph/')),
	 assertz(logtalk_library_path(sicstus, '$LOGTALKHOME/examples/sicstus/')),
	 assertz(logtalk_library_path(symdiff, '$LOGTALKHOME/examples/symdiff/')),
	 assertz(logtalk_library_path(viewpoints, '$LOGTALKHOME/examples/viewpoints/')))).
