
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), system(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), system(Command)}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{fail}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{dir(Directory, Files, _)}.


	file_exists(File) :-
		{fail}.


	file_property(File, Property) :-
		{fail}.


	delete_file(File) :-
		{atom_concat('rm ', File, Command), system(Command)}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp), atom_concat(Temp, ' ', Temp2), atom_concat(Temp2, New, Command), system(Command)}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{time(Secs, Mins, Hours, Day, Month, Year, _, _, _)}.


	cpu_time(Time) :-
		{statistics(_, _, _, _, Time, _, _)}.


	host_name(Name) :-
		{fail}.


:- end_object.
