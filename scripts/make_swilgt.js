// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.19.2
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

WScript.Echo("");
WScript.Echo("Making a shortcut named swilgt for running Logtalk with SWI-Prolog...");

var fso = new ActiveXObject("Scripting.FileSystemObject");

if (!fso.FolderExists(logtalk_home + "\\bin")) 
	fso.CreateFolder(logtalk_home + "\\bin"));

var f = fso.CreateTextFile(logtalk_home + "\\bin\\logtalkswi.pl", true);

f.WriteLine(":- system_module.");
f.WriteLine(":- include('" + logtalk_home + "\\compiler\\logtalk.pl').");
f.Close();

f = fso.CreateTextFile(logtalk_home + "\\bin\\logtalkswi.rc", true);

f.WriteLine(":- consult('" + logtalk_home + "\\configs\\swi.config').");
f.WriteLine(":- consult('" + logtalk_home + "\\configs\\swihook.pl').");
f.WriteLine(":- consult('" + logtalk_home + "\\bin\\logtalkswi.pl').");
f.Close();

f = fso.CreateTextFile(logtalk_home + "\\bin\\swilgt.bat", true).

f.WriteLine("plcon -f " +  logtalk_home + "\\bin\\logtalkswi.rc");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");
var link = WshShell.CreateShortcut(ProgramsPath + "\\swilgt.lnk");
link.Arguments = "";
link.Description = "Logtalk & SWI-Prolog";
link.IconLocation = "app.exe,1";
link.TargetPath = WshShell.RegRead("HKLM\\Software\\SWI\\Prolog\\");
link.WindowStyle = 3;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo("Done. A link to the script was been created in $prefix/bin.");
WScript.Echo("Users should define the environment variable LOGTALKHOME in");
WScript.Echo("order to use the script.");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script create a shortcut named swilgt ");
	WScript.Echo("for running Logtalk with SWI-Prolog.");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
	WScript.Quit(1);
}
