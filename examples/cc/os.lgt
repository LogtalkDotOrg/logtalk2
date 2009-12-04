
:- object(os,
	implements(osp)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/5/11,
		comment is 'Simple example of using conditional compilation to implement a portable operating-system interface for selected back-end Prolog compilers.']).

	:- if(current_logtalk_flag(prolog_dialect, swi)).

		make_directory(Directory) :-
			{make_directory(Directory)}.

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{working_directory(_, Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			{exists_directory(Directory)}.

		file_exists(File) :-
			{exists_file(File)}.

		file_modification_time(File, Time) :-
			{time_file(File, Time)}.

		file_size(File, Size) :-
			{size_file(File, Size)}.

		file_permission(File, Permission) :-
			{access_file(File, Permission)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		delete_file(File) :-
			{delete_file(File)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{get_time(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, Milisecs) :-
			{get_time(Time),
			 convert_time(Time, Year, Month, Day, Hours, Mins, Secs, Milisecs)}.

		cpu_time(Time) :-
			{Time is cputime}.

	:- elif(current_logtalk_flag(prolog_dialect, yap)).

		make_directory(Directory) :-
			{make_directory(Directory)}.

		delete_directory(Directory) :-
			{delete_file(Directory)}.

		change_directory(Directory) :-
			{cd(Directory)}.

		working_directory(Directory) :-
			{getcwd(Directory)}.

		directory_exists(Directory) :-
			{file_property(Directory, type(directory))}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, mod_time(Time))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		file_permission(File, Permission) :-
			{file_exists(File, Permission)}.
 
		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename(Old, New)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.

		cpu_time(Time) :-
			{Time is cputime}.

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

		make_directory(Directory) :-
			{standard:expand_atom(Directory, Expanded),
			 path_sysop(mkdir, Expanded)}.

		delete_directory(Directory) :-
			{standard:expand_atom(Directory, Expanded),
			 path_sysop(rmdir, Expanded)}.

		change_directory(Directory) :-
			{standard:expand_atom(Directory, Expanded),
			 path_sysop(chdir, Expanded)}.

		working_directory(Directory) :-
			{path_sysop(cwd, Directory)}.

		directory_exists(Directory) :-
			{standard:expand_atom(Directory, Expanded),
			 path_sysop(exists, Expanded),
			 path_sysop(isdir, Expanded)}.

		file_exists(File) :-
			{standard:expand_atom(File, Expanded),
			 path_sysop(exists, Expanded),
			 path_sysop(isplain, Expanded)}.

		file_modification_time(File, Time) :-
			{path_sysop(modtime, File, [High, Low]),
			 Time is Low + High * 2 ** 24}.

		file_size(File, Size) :-
			{path_sysop(size, File, Size)}.

		file_permission(File, read) :-
			{path_sysop(readable, File)}.

		file_permission(File, write) :-
			{path_sysop(writable, File)}.

		file_permission(File, execute) :-
			{path_sysop(executable, File)}.
 
		delete_file(File) :-
			{path_sysop(rm, File)}.

		rename_file(Old, New) :-
			{path_sysop(rename, Old, New)}.

		environment_variable(Variable, Value) :-
			{expand_atom(Variable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.

		cpu_time(Time) :-
			{cputime(Time)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		make_directory(Directory) :-
			{make_directory(Directory)}.

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{change_directory(Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory)}.

		directory_exists(Directory) :-
			{file_exists(Directory),
			 file_property(Directory, type(directory))}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, last_modification(Time))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		file_permission(File, Permission) :-
			{file_permission(File, Permission)}.
 
		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{date_time(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{date_time(dt(Year, Month, Day, Hours, Mins, Secs))}.

		cpu_time(Time) :-
			{cpu_time(Miliseconds),
			 Time is Miliseconds/1000}.

	:- elif(current_logtalk_flag(prolog_dialect, b)).

		make_directory(Directory) :-
			{make_directory(Directory)}.

		delete_directory(Directory) :-
			{delete_directory(Directory)}.

		change_directory(Directory) :-
			{chdir(Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory)}.

		directory_exists(Directory) :-
			{file_property(Directory, type(directory))}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, modification_time(Time))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		file_permission(File, Permission) :-
			{file_property(File, permission(Permission))}.
 
		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(ts(Year, Month, Day, Hours, Mins, Secs)) :-
			{date(Year, Month, Day), time(Hours, Mins, Secs)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{date(Year, Month, Day), time(Hours, Mins, Secs)}.

		cpu_time(Time) :-
			{cputime(Miliseconds), Time is Miliseconds/1000}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		make_directory(Directory) :-
			{make_directory(Directory)}.

		delete_directory(Directory) :-
			{delete_file(Directory)}.

		change_directory(Directory) :-
			{working_directory(_, Directory)}.

		working_directory(Directory) :-
			{working_directory(Directory, Directory)}.

		directory_exists(Directory) :-
			{directory_exists(Directory)}.

		file_exists(File) :-
			{file_exists(File)}.

		file_modification_time(File, Time) :-
			{file_property(File, mod_time(Size))}.

		file_size(File, Size) :-
			{file_property(File, size(Size))}.

		file_permission(File, Permission) :-
			{file_exists(File, Permission)}.
 
		delete_file(File) :-
			{delete_file(File)}.

		rename_file(Old, New) :-
			{rename_file(Old, New)}.

		environment_variable(Variable, Value) :-
			{environ(Variable, Value)}.

		time_stamp(Time) :-
			{datime(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.

		cpu_time(Time) :-
			{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		:- use_module(library(calendar)).

		make_directory(Directory) :-
			{mkdir(Directory)}.

		delete_directory(Directory) :-
			{delete(Directory)}.

		change_directory(Directory) :-
			{cd(Directory)}.

		working_directory(Directory) :-
			{getcwd(Directory)}.

		directory_exists(Directory) :-
			{exists(Directory)}.

		file_exists(File) :-
			{exists(File)}.

		file_modification_time(File, Time) :-
			{get_file_info(File, mtime, Time)}.

		file_size(File, Size) :-
			{get_file_info(File, size, Size)}.

		delete_file(File) :-
			{delete(File)}.

		rename_file(Old, New) :-
			{rename(Old, New)}.

		environment_variable(Variable, Value) :-
			{getenv(Variable, Value)}.

		time_stamp(Time) :-
			{mjd_now(Time)}.

		date_time(Year, Month, Day, Hours, Mins, Secs, 0) :-
			{mjd_now(MJD),
			 mjd_to_date(MJD, '/'('/'(Day, Month), Year)),
			 mjd_to_time(MJD, ':'(Hours, ':'(Mins, Secs)))}.

		cpu_time(Time) :-
			{cputime(Time)}.

	:- else.

		:- initialization((write('WARNING: back-end Prolog compiler not supported!'), nl)).

	:- endif.

:- end_object.
