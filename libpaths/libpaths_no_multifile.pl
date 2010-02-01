
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.38.3
%  
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% logtalk_library_path(Library, Path)
% paths must always end with a "/"
			
:- initialization((
	(	'$lgt_environment_variable'('HOME', _) ->	
		assertz(logtalk_library_path(home, '$HOME/'))	% this definition only works for POSIX systems
	;	true
	),
	assertz(logtalk_library_path(lgtuser, '$LOGTALKUSER/')),
	assertz(logtalk_library_path(contributions, lgtuser('contributions/'))),
	assertz(logtalk_library_path(examples, lgtuser('examples/'))),
	assertz(logtalk_library_path(library, lgtuser('library/'))),
	assertz(logtalk_library_path(ack, examples('ack/'))),
	assertz(logtalk_library_path(aliases, examples('aliases/'))),
	assertz(logtalk_library_path(assignvars, examples('assignvars/'))),
	assertz(logtalk_library_path(barriers, threads('barriers/'))),
	assertz(logtalk_library_path(benchmarks, examples('benchmarks/'))),
	assertz(logtalk_library_path(birds, examples('birds/'))),
	assertz(logtalk_library_path(birthdays, threads('birthdays/'))),
	assertz(logtalk_library_path(blackboard, threads('blackboard/'))),
	assertz(logtalk_library_path(bottles, examples('bottles/'))),
	assertz(logtalk_library_path(bricks, examples('bricks/'))),
	assertz(logtalk_library_path(buffer, threads('buffer/'))),
	assertz(logtalk_library_path(cc, examples('cc/'))),
	assertz(logtalk_library_path(classmethods, examples('classmethods/'))),
	assertz(logtalk_library_path(classvars, examples('classvars/'))),
	assertz(logtalk_library_path(clp_bp, constraints('bp/'))),
	assertz(logtalk_library_path(clp_eclipse, constraints('eclipse/'))),
	assertz(logtalk_library_path(clp_gprolog, constraints('gprolog/'))),
	assertz(logtalk_library_path(clp_swipl, constraints('swipl/'))),
	assertz(logtalk_library_path(clp_yap, constraints('yap/'))),
	assertz(logtalk_library_path(complements, examples('complements/'))),
	assertz(logtalk_library_path(constraints, examples('constraints/'))),
	assertz(logtalk_library_path(dcgs, examples('dcgs/'))),
	assertz(logtalk_library_path(debug_hooks, examples('debug_hooks/'))),
	assertz(logtalk_library_path(diamonds, examples('diamonds/'))),
	assertz(logtalk_library_path(dynpred, examples('dynpred/'))),
	assertz(logtalk_library_path(encodings, examples('encodings/'))),
	assertz(logtalk_library_path(engines, examples('engines/'))),
	assertz(logtalk_library_path(errors, examples('errors/'))),
	assertz(logtalk_library_path(expansion, examples('expansion/'))),
	assertz(logtalk_library_path(fft, threads('fft/'))),
	assertz(logtalk_library_path(fibonacci, threads('fibonacci/'))),
	assertz(logtalk_library_path(functions, threads('functions/'))),
	assertz(logtalk_library_path(hanoi, threads('hanoi/'))),
	assertz(logtalk_library_path(hello_world, examples('hello_world/'))),
	assertz(logtalk_library_path(hooks, examples('hooks/'))),
	assertz(logtalk_library_path(inheritance, examples('inheritance/'))),
	assertz(logtalk_library_path(instmethods, examples('instmethods/'))),
	assertz(logtalk_library_path(instvars, examples('instvars/'))),
	assertz(logtalk_library_path(integration, threads('integration/'))),
	assertz(logtalk_library_path(integration2d, threads('integration2d/'))),
	assertz(logtalk_library_path(lambdas, examples('lambdas/'))),
	assertz(logtalk_library_path(lo_planner, examples('lo/planner/'))),
	assertz(logtalk_library_path(lo_travellers, examples('lo/travellers/'))),
	assertz(logtalk_library_path(logging, examples('logging/'))),
	assertz(logtalk_library_path(logic, examples('logic/'))),
	assertz(logtalk_library_path(lpa_faults, examples('lpa/faults/'))),
	assertz(logtalk_library_path(lpa_timetables, examples('lpa/timetables/'))),
	assertz(logtalk_library_path(metainterpreters, examples('metainterpreters/'))),
	assertz(logtalk_library_path(metapredicates, examples('metapredicates/'))),
	assertz(logtalk_library_path(mi, examples('mi/'))),
	assertz(logtalk_library_path(miscellaneous, examples('miscellaneous/'))),
	assertz(logtalk_library_path(modules, examples('modules/'))),
	assertz(logtalk_library_path(msglog, examples('msglog/'))),
	assertz(logtalk_library_path(mtbatch, threads('mtbatch/'))),
	assertz(logtalk_library_path(multifile, examples('multifile/'))),
	assertz(logtalk_library_path(nondet, threads('nondet/'))),
	assertz(logtalk_library_path(operators, examples('operators/'))),
	assertz(logtalk_library_path(parametric, examples('parametric/'))),
	assertz(logtalk_library_path(people, examples('people/'))),
	assertz(logtalk_library_path(philosophers, threads('philosophers/'))),
	assertz(logtalk_library_path(poem, examples('poem/'))),
	assertz(logtalk_library_path(points, examples('points/'))),
	assertz(logtalk_library_path(polygons, examples('polygons/'))),
	assertz(logtalk_library_path(primes, threads('primes/'))),
	assertz(logtalk_library_path(profiling, examples('profiling/'))),
	assertz(logtalk_library_path(prototypes, examples('prototypes/'))),
	assertz(logtalk_library_path(proxies, examples('proxies/'))),
	assertz(logtalk_library_path(puzzles, examples('puzzles/'))),
	assertz(logtalk_library_path(reflection, examples('reflection/'))),
	assertz(logtalk_library_path(relations, examples('relations/'))),
	assertz(logtalk_library_path(roots, examples('roots/'))),
	assertz(logtalk_library_path(searching, examples('searching/'))),
	assertz(logtalk_library_path(securemp, examples('securemp/'))),
	assertz(logtalk_library_path(shapes_ch, examples('shapes/ch/'))),
	assertz(logtalk_library_path(shapes_ph, examples('shapes/ph/'))),
	assertz(logtalk_library_path(sicstus, examples('sicstus/'))),
	assertz(logtalk_library_path(sorting, threads('sorting/'))),
	assertz(logtalk_library_path(sync, threads('sync/'))),
	assertz(logtalk_library_path(symdiff, examples('symdiff/'))),
	assertz(logtalk_library_path(tabling, examples('tabling/'))),
	assertz(logtalk_library_path(tak, threads('tak/'))),
	assertz(logtalk_library_path(testing, examples('testing/'))),
	assertz(logtalk_library_path(threads, examples('threads/'))),
	assertz(logtalk_library_path(viewpoints, examples('viewpoints/'))),
	assertz(logtalk_library_path(xml_parser, contributions('xml_parser/')))
)).
