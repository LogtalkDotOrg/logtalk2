
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.40.2
%  
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- multifile(logtalk_library_path/2).		% logtalk_library_path(Library, Path)
:- dynamic(logtalk_library_path/2).			% paths must always end with a "/"

logtalk_library_path(home, '$HOME/') :-					% this definition only works for POSIX systems
	'$lgt_environment_variable'('HOME', _).
logtalk_library_path(home, '$HOMEDRIVE/$HOMEPATH/') :-	% this definition only works for Windows systems
	'$lgt_environment_variable'('HOMEDRIVE', _),		% for some back-end Prolog compilers
	'$lgt_environment_variable'('HOMEPATH', _).
logtalk_library_path(lgtuser, '$LOGTALKUSER/').
logtalk_library_path(contributions, lgtuser('contributions/')).
logtalk_library_path(examples, lgtuser('examples/')).
logtalk_library_path(library, lgtuser('library/')).
logtalk_library_path(ack, examples('ack/')).
logtalk_library_path(aliases, examples('aliases/')).
logtalk_library_path(assignvars, examples('assignvars/')).
logtalk_library_path(barriers, threads('barriers/')).
logtalk_library_path(benchmarks, examples('benchmarks/')).
logtalk_library_path(birds, examples('birds/')).
logtalk_library_path(birthdays, threads('birthdays/')).
logtalk_library_path(blackboard, threads('blackboard/')).
logtalk_library_path(bottles, examples('bottles/')).
logtalk_library_path(bricks, examples('bricks/')).
logtalk_library_path(buffer, threads('buffer/')).
logtalk_library_path(cc, examples('cc/')).
logtalk_library_path(classmethods, examples('classmethods/')).
logtalk_library_path(classvars, examples('classvars/')).
logtalk_library_path(clp_bp, constraints('bp/')).
logtalk_library_path(clp_eclipse, constraints('eclipse/')).
logtalk_library_path(clp_gprolog, constraints('gprolog/')).
logtalk_library_path(clp_swipl, constraints('swipl/')).
logtalk_library_path(clp_yap, constraints('yap/')).
logtalk_library_path(complements, examples('complements/')).
logtalk_library_path(constraints, examples('constraints/')).
logtalk_library_path(dcgs, examples('dcgs/')).
logtalk_library_path(debug_hooks, examples('debug_hooks/')).
logtalk_library_path(diamonds, examples('diamonds/')).
logtalk_library_path(dynpred, examples('dynpred/')).
logtalk_library_path(encodings, examples('encodings/')).
logtalk_library_path(engines, examples('engines/')).
logtalk_library_path(errors, examples('errors/')).
logtalk_library_path(expansion, examples('expansion/')).
logtalk_library_path(fft, threads('fft/')).
logtalk_library_path(fibonacci, threads('fibonacci/')).
logtalk_library_path(functions, threads('functions/')).
logtalk_library_path(hanoi, threads('hanoi/')).
logtalk_library_path(hello_world, examples('hello_world/')).
logtalk_library_path(help, examples('help/')).
logtalk_library_path(hooks, examples('hooks/')).
logtalk_library_path(inheritance, examples('inheritance/')).
logtalk_library_path(instmethods, examples('instmethods/')).
logtalk_library_path(instvars, examples('instvars/')).
logtalk_library_path(integration, threads('integration/')).
logtalk_library_path(integration2d, threads('integration2d/')).
logtalk_library_path(lambdas, examples('lambdas/')).
logtalk_library_path(lo_planner, examples('lo/planner/')).
logtalk_library_path(lo_travellers, examples('lo/travellers/')).
logtalk_library_path(logging, examples('logging/')).
logtalk_library_path(logic, examples('logic/')).
logtalk_library_path(lpa_faults, examples('lpa/faults/')).
logtalk_library_path(lpa_timetables, examples('lpa/timetables/')).
logtalk_library_path(metainterpreters, examples('metainterpreters/')).
logtalk_library_path(metapredicates, examples('metapredicates/')).
logtalk_library_path(mi, examples('mi/')).
logtalk_library_path(miscellaneous, examples('miscellaneous/')).
logtalk_library_path(modules, examples('modules/')).
logtalk_library_path(msglog, examples('msglog/')).
logtalk_library_path(mtbatch, threads('mtbatch/')).
logtalk_library_path(multifile, examples('multifile/')).
logtalk_library_path(nondet, threads('nondet/')).
logtalk_library_path(operators, examples('operators/')).
logtalk_library_path(parametric, examples('parametric/')).
logtalk_library_path(people, examples('people/')).
logtalk_library_path(philosophers, threads('philosophers/')).
logtalk_library_path(poem, examples('poem/')).
logtalk_library_path(points, examples('points/')).
logtalk_library_path(polygons, examples('polygons/')).
logtalk_library_path(primes, threads('primes/')).
logtalk_library_path(profiling, examples('profiling/')).
logtalk_library_path(prototypes, examples('prototypes/')).
logtalk_library_path(proxies, examples('proxies/')).
logtalk_library_path(puzzles, examples('puzzles/')).
logtalk_library_path(reflection, examples('reflection/')).
logtalk_library_path(relations, examples('relations/')).
logtalk_library_path(roots, examples('roots/')).
logtalk_library_path(searching, examples('searching/')).
logtalk_library_path(securemp, examples('securemp/')).
logtalk_library_path(shapes_ch, examples('shapes/ch/')).
logtalk_library_path(shapes_ph, examples('shapes/ph/')).
logtalk_library_path(sicstus, examples('sicstus/')).
logtalk_library_path(sorting, threads('sorting/')).
logtalk_library_path(sync, threads('sync/')).
logtalk_library_path(symdiff, examples('symdiff/')).
logtalk_library_path(tabling, examples('tabling/')).
logtalk_library_path(tak, threads('tak/')).
logtalk_library_path(testing, examples('testing/')).
logtalk_library_path(threads, examples('threads/')).
logtalk_library_path(verdi_neruda, contributions('verdi_neruda/')).
logtalk_library_path(viewpoints, examples('viewpoints/')).
logtalk_library_path(xml_parser, contributions('xml_parser/')).
