
:-  use_module(library(calendar)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for ECLiPSe.']).


	make_directory(Directory) :-
		{mkdir(Directory)}.


	delete_directory(Directory) :-
		{delete(Directory)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{getcwd(Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{read_directory(Directory, "*", _, Files)}.


	file_exists(File) :-
		{exists(File)}.


	file_modtime(File, Time) :-
		{get_file_info(File, mtime, Time)}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{get_file_info(File, mtime, Time), unix_to_mjd(Time, MJD), mjd_to_date(MJD, Day/Month/Year), mjd_to_time(MJD, Hours:Mins:Secs)}.


	file_size(File, Size) :-
		{get_file_info(File, size, Size)}.


	file_type(File, Type) :-
		{get_file_info(File, mode, Mode)},
		Mode /\ 8'170000 =:= Result,
		file_mode_type(Result, Type).

	file_mode_type(8'100000, regular) :- !.
	file_mode_type(8'040000, directory) :- !.
	file_mode_type(8'140000, socket) :- !.
	file_mode_type(8'120000, symlink) :- !.
	file_mode_type(_, unknown).


	delete_file(File) :-
		{delete(File)}.


	rename_file(Old, New) :-
		{rename(Old, New)}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{mjd_now(MJD), mjd_to_date(MJD, Day/Month/Year), mjd_to_time(MJD, Hours:Mins:Secs)}.


	cpu_time(Time) :-
		{cputime(Time)}.


	host_name(Name) :-
		{get_flag(hostname, String), atom_string(Name, String)}.


:- end_object.
