
:- [library(files)].
:- [library(directory)].
:- [library(date)].


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), unix(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), unix(Command)}.


	change_directory(Directory) :-
		{unix(cd(Directory))}.


	working_directory(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{file_members_of_directory(Directory, '*', Files)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_property(File, Property) :-
		{file_property(File, Attribute, Value), Property =.. [Attribute, Value]}.


	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	getenv(Variable, Value) :-
		{fail}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{date(date(Day, Month, Year)), time(time(Hours, Mins, Secs))}.


	host(Name) :-
		{fail}.


:- end_object.
