
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for Qu-Prolog.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), os(system(Command))}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), os(system(Command))}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{fail}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{fail}.


	file_exists(File) :-
		{access(File, 4, 0)}.


	file_modtime(File, Time) :-
		{fail}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	file_size(File, Size) :-
		{fail}.


	file_type(File, Type) :-
		{fail}.


	file_permission(File, Permission) :-
		{fail}.
 

	delete_file(File) :-
		{atom_concat('rm ', File, Command), os(system(Command))}.


	rename_file(Old, New) :-
		{concat_atom([mv, Old, New], ' ', Command), os(system(Command))}.


	getenv(Variable, Value) :-
		{fail}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{realtime(RT), localtime(RT, LT), LT = local_time(Year, Month, Day, Hours, Mins, Secs)}.


	cpu_time(Time) :-
		{statistics(runtime, [Start,_]), Time is Start/1000}.


	host_name(Name) :-
		{fail}.


:- end_object.
