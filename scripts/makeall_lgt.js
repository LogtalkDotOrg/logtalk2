// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.25.0
//
// Copyright (c) 1998-2005 Paulo Moura.  All Rights Reserved.
// =================================================================

WScript.Echo('');
WScript.Echo('Creating scripts for running Logtalk with selected Prolog compilers...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

if (WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_ciaolgt.js", true))
	WScript.Echo('Logtalk - CIAO shortcut created');
else
	WScript.Echo('Logtalk - CIAO shortcut creation failed (CIAO Prolog not installed?)');

if (WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_eclipselgt.js", true))
	WScript.Echo('Logtalk - ECLiPSe shortcut created');
else
	WScript.Echo('Logtalk - ECLiPSe shortcut creation failed (ECLiPSe not installed?)');

if (WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_gplgt.js", true))
	WScript.Echo('Logtalk - GNU Prolog shortcut created');
else
	WScript.Echo('Logtalk - GNU Prolog shortcut creation failed (GNU Prolog not installed?)');

if (WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_plclgt.js", true))
	WScript.Echo('Logtalk - K-Prolog shortcut created');
else
	WScript.Echo('Logtalk - K-Prolog shortcut creation failed (K-Prolog not installed?)');

if (WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_sicstuslgt.js", true))
	WScript.Echo('Logtalk - SICStus shortcut created');
else
	WScript.Echo('Logtalk - SICStus shortcut creation failed (SICStus Prolog not installed?)');

if (WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_swilgt.js", true))
	WScript.Echo('Logtalk - SWI-Prolog shortcut created');
else
	WScript.Echo('Logtalk - SWI-Prolog shortcut creation failed (SWI-Prolog not installed?)');

if (WshShell.Run("cscript %LOGTALKHOME%\\scripts\\make_yaplgt.js", true))
	WScript.Echo('Logtalk - YAP shortcut created');
else
	WScript.Echo('Logtalk - YAP shortcut creation failed (YAP not installed?)');

WScript.Echo('Done. Done. Links to the created scripts was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the environment variables');
WScript.Echo('LOGTALKHOME and LOGTALKUSER are defined for all users wishing');
WScript.Echo('to use the shortcuts.');
WScript.Echo('');

WScript.Quit(0);
