
:- protocol(systemp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system protocol.']).

	:- public(make_directory/1).
	:- mode(make_directory(+atom), zero_or_one).
	:- info(make_directory/1, [
		comment is 'Makes a new directory.',
		argnames is ['Directory']]).

	:- public(delete_directory/1).
	:- mode(delete_directory(+atom), zero_or_one).
	:- info(delete_directory/1, [
		comment is 'Deletes a directory.',
		argnames is ['Directory']]).

	:- public(change_directory/1).
	:- mode(change_directory(+atom), zero_or_one).
	:- info(change_directory/1, [
		comment is 'Changes current working directory.',
		argnames is ['Directory']]).

	:- public(working_directory/1).
	:- mode(working_directory(?atom), zero_or_one).
	:- info(working_directory/1, [
		comment is 'Current working directory.',
		argnames is ['Directory']]).

	:- public(directory_exists/1).
	:- mode(directory_exists(+atom), zero_or_one).
	:- info(directory_exists/1, [
		comment is 'True if the specified directory exists.',
		argnames is ['Directory']]).

	:- public(directory_files/2).
	:- mode(directory_files(+atom, -list), zero_or_one).
	:- info(directory_files/2, [
		comment is 'List of all directory files. Fails if the directory does not exists or is not user readable.',
		argnames is ['Directory', 'Files']]).

	:- public(absolute_file_name/1).
	:- mode(absolute_file_name(+atom), zero_or_one).
	:- info(absolute_file_name/1, [
		comment is 'True if the argument is an absolute path name.',
		argnames is ['File']]).

	:- public(absolute_file_name/2).
	:- mode(absolute_file_name(+atom, ?atom), zero_or_one).
	:- info(absolute_file_name/2, [
		comment is 'Expands a file name into its absolute path.',
		argnames is ['File', 'Full']]).

	:- public(decompose_file_name/4).
	:- mode(decompose_file_name(+atom, ?atom, ?atom, ?atom), zero_or_one).
	:- info(decompose_file_name/4, [
		comment is 'Decomposes a file name into its parts.',
		argnames is ['File', 'Directory', 'Base', 'Extension']]).

	:- public(file_exists/1).
	:- mode(file_exists(+atom), zero_or_one).
	:- info(file_exists/1, [
		comment is 'True if the specified file exists.',
		argnames is ['File']]).

	:- public(file_modtime/2).
	:- mode(file_modtime(+atom, -number), zero_or_one).
	:- info(file_modtime/2, [
		comment is 'File modification time using a system-dependent timestamp.',
		argnames is ['File', 'Time']]).

	:- public(file_modtime/7).
	:- mode(file_modtime(+atom, -integer, -integer, -integer, -integer, -integer, -integer), zero_or_one).
	:- info(file_modtime/7, [
		comment is 'File modification time using calendar local date and time.',
		argnames is ['File', 'Year', 'Month', 'Day', 'Hours', 'Mins', 'Secs']]).

	:- public(file_size/2).
	:- mode(file_size(+atom, -integer), zero_or_one).
	:- info(file_size/2, [
		comment is 'File size in bytes.',
		argnames is ['File', 'Size']]).

	:- public(file_type/2).
	:- mode(file_type(+atom, ?atom), zero_or_one).
	:- info(file_type/2, [
		comment is 'File type (regular, directory, symlink, fifo, socket, and unknown).',
		argnames is ['File', 'Type']]).

	:- public(file_permission/2).
	:- mode(file_permission(+atom, ?atom), zero_or_more).
	:- info(file_permission/2, [
		comment is 'File permission (read, write, execute, and search).',
		argnames is ['File', 'Permission']]).

	:- public(delete_file/1).
	:- mode(delete_file(+atom), zero_or_one).
	:- info(delete_file/1, [
		comment is 'Deletes a file.',
		argnames is ['File']]).

	:- public(rename_file/2).
	:- mode(rename_file(+atom, +atom), zero_or_one).
	:- info(rename_file/2, [
		comment is 'Renames a file. Fails if the file does not exist or cannot be renamed.',
		argnames is ['Old', 'New']]).

	:- public(copy_file/2).
	:- mode(copy_file(+atom, +atom), zero_or_one).
	:- info(copy_file/2, [
		comment is 'Makes a copy of a file.',
		argnames is ['Original', 'Copy']]).

	:- public(symbolic_link/2).
	:- mode(symbolic_link(+atom, ?atom), zero_or_one).
	:- info(symbolic_link/2, [
		comment is 'Follows a symbolic link returning the target full path.',
		argnames is ['File', 'Target']]).

	:- public(getenv/2).
	:- mode(getenv(+atom, ?atom), zero_or_one).
	:- info(getenv/2, [
		comment is 'Gets environment variable value.',
		argnames is ['Variable', 'Value']]).

	:- public(setenv/2).
	:- mode(setenv(+atom, +atom), zero_or_one).
	:- info(setenv/2, [
		comment is 'Sets environment variable value.',
		argnames is ['Variable', 'Value']]).

	:- public(date_time/6).
	:- mode(date_time(?integer, ?integer, ?integer, ?integer, ?integer, ?integer), zero_or_one).
	:- info(date_time/6, [
		comment is 'Current calendar date and time.',
		argnames is ['Year', 'Month', 'Day', 'Hours', 'Mins', 'Secs']]).

	:- public(convert_time/7).
	:- mode(convert_time(+number, ?integer, ?integer, ?integer, ?integer, ?integer), zero_or_one).
	:- info(convert_time/7, [
		comment is 'Converts system dependent timestamp to calendar local date and time.',
		argnames is ['Time', 'Year', 'Month', 'Day', 'Hours', 'Mins', 'Secs']]).

	:- public(cpu_time/1).
	:- mode(cpu_time(-number), zero_or_one).
	:- info(cpu_time/1, [
		comment is 'System cpu time in seconds.',
		argnames is ['Time']]).

	:- public(host_name/1).
	:- mode(host_name(-atom), one).
	:- info(host_name/1, [
		comment is 'Host name.',
		argnames is ['Name']]).

	:- public(canonical_os_path/2).
	:- mode(canonical_os_path(+atom, -atom), one).
	:- mode(canonical_os_path(-atom, +atom), one).
	:- info(canonical_os_path/2, [
		comment is 'Converts between canonical and operating system dependent paths.',
		argnames is ['Canonical', 'OS']]).

	:- public(canonical_path/3).
	:- mode(canonical_path(+atom, -atom, -atom), zero_or_one).
	:- mode(canonical_path(-atom, +atom, -atom), zero_or_one).
	:- mode(canonical_path(-atom, -atom, +atom), zero_or_one).
	:- info(canonical_path/3, [
		comment is 'Converts between relative, absolute, and full canonical paths.',
		argnames is ['Relative', 'Absolute', 'Full'],
		exceptions is [
			'None of the arguments is instantiated' - instantiation_error,
			'Relative is neither a variable nor a relative path' - type_error(relative_path, 'Relative'),
			'Absolute is neither a variable nor a absolute path' - type_error(absolute_path, 'Absolute'),
			'Full is neither a variable nor a full path' - type_error(full_path, 'Full')]]).

	:- public(relative_path/1).
	:- mode(relative_path(+atom), zero_or_one).
	:- info(relative_path/1, [
		comment is 'True when the argument is a valid, relative canonical path.',
		argnames is ['Path'],
		exceptions is [
			'Path is not a valid path' - type_error(path, 'Path')]]).

	:- public(absolute_path/1).
	:- mode(absolute_path(+atom), zero_or_one).
	:- info(absolute_path/1, [
		comment is 'True when the argument is a valid, absolute canonical path.',
		argnames is ['Path'],
		exceptions is [
			'Path is not a valid path' - type_error(path, 'Path')]]).

	:- public(full_path/1).
	:- mode(full_path(+atom), zero_or_one).
	:- info(full_path/1, [
		comment is 'True when the argument is a valid, full canonical path.',
		argnames is ['Path'],
		exceptions is [
			'Path is not a valid path' - type_error(path, 'Path')]]).

	:- public(file_name_parts/9).
	:- mode(file_name_parts(+atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom), zero_or_one).
	:- mode(file_name_parts(-atom, +atom, +atom, +atom, +atom, +atom, +atom, +atom, +atom), zero_or_one).
	:- info(file_name_parts/9, [
		comment is 'Converts between a file name and its constituent parts.',
		argnames is ['File', 'Protocol', 'User', 'Password', 'Host', 'Port', 'Directory', 'Base', 'Extension']]).

:- end_protocol.
