// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.22.0
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

var WshShell = new ActiveXObject("WScript.Shell");

var WshProcessEnv = WshShell.Environment("PROCESS");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var logtalk_home;

if (WshProcessEnv.Item("LOGTALKHOME"))
	logtalk_home = WshProcessEnv.Item("LOGTALKHOME");
else if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

if (WScript.Arguments.Unnamed.Length > 0)
	usage_help();

if !(WshUserEnv.Item("LOGTALKUSER")) {
	var logtalk_user = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
	WScript.Echo("After the script completion, you must set the user environment variable");
	WScript.Echo("LOGTALKUSER pointing to MyDocuments\\logtalk");
	WScript.Echo("");
}
else {
	var logtalk_user = WshUserEnv.Item("LOGTALKUSER");
}

var fso = new ActiveXObject("Scripting.FileSystemObject");

if (fso.FolderExists(logtalk_user)) {
	WScript.Echo("Error! Logtalk directory already exists! Please rename it or delete it.");
	WScript.Echo("");
	usage_help();
}

WScript.Echo("Creating directory " + logtalk_user + "...");
fso.CreateFolder(logtalk_user);

WScript.Echo("Copying Logtalk files and directories...");
WScript.Echo("");
fso.CopyFolder(logtalk_home + "\\configs", logtalk_user + "\\configs");
fso.CopyFolder(logtalk_home + "\\examples", logtalk_user + "\\examples");
fso.CopyFolder(logtalk_home + "\\libpaths", logtalk_user + "\\libpaths");
fso.CopyFolder(logtalk_home + "\\library", logtalk_user + "\\library");
fso.CopyFolder(logtalk_home + "\\xml", logtalk_user + "\\xml");

WScript.Echo("Finished copying Logtalk files directories.");
WScript.Echo("");
WScript.Echo("You may need to edit the \%LOGTALKUSER\%\\libpaths\\libpaths.pl file to");
WScript.Echo("match your Prolog compiler and operating-system requirements or to add");
WScript.Echo("your own library paths.");
WScript.Echo("");
WScript.Echo("You may want to customize the default Logtalk compiler options by editing");
WScript.Echo("the configuration file for your Prolog compiler found in the directory");
WScript.Echo("\%LOGTALKUSER\%/configs.");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script copies the Logtalk user-modifiable files and directories");
	WScript.Echo("to the user home directory. The location can be set by the environment");
	WScript.Echo("variable \%LOGTALKUSER\% (defaults to MyDocuments\\logtalk when the");
	WScript.Echo("variable is not defined)");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
	WScript.Quit(1);
}
