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
	WScript.Echo("Error! The system environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

logtalk_home = logtalk_home.replace(/\\/g, "\\\\");

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo("");
WScript.Echo("Creating a shortcut named Eclipselgt for running Logtalk with ECLiPSe...");

var fso = new ActiveXObject("Scripting.FileSystemObject");

if (!fso.FolderExists(logtalk_home + "\\bin")) 
	fso.CreateFolder(logtalk_home + "\\bin");

var f = fso.CreateTextFile(logtalk_home + "\\bin\\lgtceclipse.pl", true);

f.WriteLine(":- pragma(system).");
f.WriteLine(":- pragma(nodebug).");
f.Close();

WshShell.Run("cmd /c type " + logtalk_home + "\\compiler\\logtalk.pl" + " >> " + logtalk_home + "\\bin\\lgtceclipse.pl", true);

var lgthome_eclipse = logtalk_home.replace(/C:/, "C");
lgthome_eclipse = lgthome_eclipse.replace(/D:/, "D");
lgthome_eclipse = lgthome_eclipse.replace(/E:/, "E");
lgthome_eclipse = lgthome_eclipse.replace(/\\\\/g, "/");

f = fso.CreateTextFile(logtalk_home + "\\bin\\logtalkeclipse.pl", true);

f.WriteLine(":- ensure_loaded(library(toplevel)).");
f.WriteLine(":- compile('//" + lgthome_eclipse + "/configs/eclipseiso.config').");
f.WriteLine(":- compile('//" + lgthome_eclipse + "/bin/lgtceclipse.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");
var link = WshShell.CreateShortcut(ProgramsPath + "\\Eclipselgt.lnk");
link.Arguments = "-b %LOGTALKHOME%\\bin\\logtalkeclipse.pl";
link.Description = "Runs Logtalk with ECLiPSe";
link.IconLocation = "app.exe,1";
link.TargetPath = WshShell.RegRead("HKEY_LOCAL_MACHINE\\Software\\IC-Parc\\Eclipse\\5.7\\ECLIPSEDIR") + "\\lib\\i386_nt\\eclipse.exe";
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo("Done. The Eclipselgt shortcut was been added to the Start Menu Programs.");
WScript.Echo("Make sure that the LOGTALKHOME environment variable is defined for all");
WScript.Echo("users wishing to use the shortcut."
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script creates a shortcut named Eclipselgt for running Logtalk");
	WScript.Echo("with ECLiPSe. The script must be run by an user with administrative");
	WScript.Echo("rights. The LOGTALKHOME environment variable must be defined before");
	WScript.Echo("running this script.");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
}
