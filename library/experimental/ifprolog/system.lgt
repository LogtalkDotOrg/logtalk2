
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


	make_directory(Directory) :-
		{fail}.


	delete_directory(Directory) :-
		{fail}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{getcwd(Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{fail}.


	file_exists(File) :-
		{file_test(File, read)}.


	file_property(File, Property) :-
		{get_file_info(File, Attribute, Value), Property =.. [Attribute, Value]}.


	delete_file(File) :-
		{fail}.


	rename_file(Old, New) :-
		{fail}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{localtime(time, Year, Month, Day, _, _, Hours, Min, Secs)}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{current_host(Name)}.


:- end_object.
