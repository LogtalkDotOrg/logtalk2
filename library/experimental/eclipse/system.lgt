
:-  use_module(library(calendar)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


	make_directory(Directory) :-
		{mkdir(Directory)}.


	delete_directory(Directory) :-
		{delete(Directory)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{getcwd(Directory)}.


	directory_files(Directory, Files) :-
		{read_directory(Directory, "*", _, Files)}.


	file_exists(File) :-
		{exists(File)}.


	file_property(File, Property) :-
		{get_file_info(File, Attr, Value), Property =.. [Attr, Value]}.


	delete_file(File) :-
		{delete(File)}.


	rename_file(Old, New) :-
		{rename(Old, New)}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{mjd_now(MJD), mjd_to_date(MJD, Day/Month/Year), mjd_to_time(MJD, Hours:Mins:Secs)}.


	host(Name) :-
		{get_flag(hostname, String), atom_string(Name, String)}.


:- end_object.
