
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for IF/Prolog.']).


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


	file_modtime(File, Time) :-
		{get_file_info(File, mtime, Time)}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{get_file_info(File, mtime, Time),
		 localtime(Time, Year, Month, Day, _, _, Hours, Min, Secs)}.


	file_size(File, Size) :-
		{get_file_info(File, size, Size)}.


	file_type(File, Type) :-
		{get_file_info(File, mode, Mode)},
		file_mode_type(Mode, Type).

	file_mode_type(ifreg, regular).
	file_mode_type(ifdir, directory).
	file_mode_type(iflnk, symlink).


	file_permission(File, Permission) :-
		{file_test(File, Permission)}.
 

	delete_file(File) :-
		{fail}.


	rename_file(Old, New) :-
		{fail}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{localtime(time, Year, Month, Day, _, _, Hours, Min, Secs)}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{current_host(Name)}.


:- end_object.
