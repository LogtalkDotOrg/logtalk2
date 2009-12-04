/*
On POSIX systems, the compilation of this example may generate invalid 
file names for the XML documenting files, wence the xmldocs(off) option
used below.
*/

:- initialization(
	logtalk_load(parametric, [xmldocs(off)])).
