
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for B-Prolog.']).


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
		{fail}.


	absolute_file_name(File) :-
		{fail}.


	absolute_file_name(File, Full) :-
		{fail}.


	decompose_file_name(File, Directory, Base, Extension) :-
		{fail}.


	file_exists(File) :-
		{exists(File)}.


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
		{atom_concat('rm ', File, Command), system(Command)}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp), atom_concat(' ', New, Command), system(Command)}.


	symbolic_link(File, Target) :-
		{fail}.


	getenv(Variable, Value) :-
		{environ(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{date(Year, Month, Day), time(Hours, Mins, Secs)}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	cpu_time(Time) :-
		{cputime(Miliseconds), Time is Miliseconds/1000}.


	host_name(Name) :-
		{fail}.


:- end_object.
