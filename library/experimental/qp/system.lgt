
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), os(system(Command))}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), os(system(Command))}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{fail}.


	file_exists(File) :-
		{access(File, 4, 0)}.


	file_property(File, Property) :-
		{fail}.


	delete_file(File) :-
		{atom_concat('rm ', File, Command), os(system(Command))}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp), atom_concat(' ', New, Command), os(system(Command))}.


	getenv(Variable, Value) :-
		{fail}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{datime(datime(Year, Month, Day, Hour, Min, Sec))}.


	host(Name) :-
		{fail}.


:- end_object.
