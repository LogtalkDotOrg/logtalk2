// ================================================================
// Logtalk - Open source object-oriented logic programming language
// Release 2.38.1
//
// Copyright (c) 1998-2009 Paulo Moura. All Rights Reserved.
// 
// Logtalk is free software. You can redistribute it and/or modify
// it under the terms of the Artistic License 2.0 as published by 
// the The Perl Foundation.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// Artistic License 2.0 for more details. A copy of the license is 
// provided in the "LICENSE.txt" file.
// ================================================================

if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}

var WshShell = new ActiveXObject("WScript.Shell");

var directory = WshShell.CurrentDirectory;

var processor = "msxsl";
// var processor = "xsltproc";
// var processor = "xalan";
// var processor = "sabcmd";

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

var logtalk_home;
var logtalk_user;

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

if (WshSystemEnv.Item("LOGTALKUSER"))
	logtalk_user = WshSystemEnv.Item("LOGTALKUSER");
else if (WshUserEnv.Item("LOGTALKUSER"))
	logtalk_user = WshUserEnv.Item("LOGTALKUSER")
else {
	WScript.Echo("Error! The environment variable LOGTALKUSER must be defined first!");
	usage_help();
	WScript.Quit(1);
}

var xslt = logtalk_user + "\\xml\\lgttxt.xsl";

var d_arg = "";
var p_arg = "";

if (WScript.Arguments.Named.Exists("d"))
	d_arg = WScript.Arguments.Named.Item("d");

if (WScript.Arguments.Named.Exists("p"))
	p_arg = WScript.Arguments.Named.Item("p");

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (d_arg != "" && !FSObject.FolderExists(d_arg)) {
	WScript.Echo("Error! directory does not exists: " + d_arg);
	WScript.Echo("");
	usage_help();
} else if (d_arg != "")
	directory = d_arg;

if (p_arg != "" && p_arg != "msxsl" && p_arg != "xsltproc" && p_arg != "xalan" && p_arg != "sabcmd") {
	WScript.Echo("Error! Unsupported XSLT processor:" + p_arg);
	WScript.Echo("");
	usage_help();
} else if (p_arg != "")
	processor = p_arg;

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk.dtd")) {
	FSObject.CopyFile(logtalk_home + "\\xml\\logtalk.dtd", WshShell.CurrentDirectory + "\\logtalk.dtd");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\custom.ent")) {
	FSObject.CopyFile(logtalk_home + "\\xml\\custom.ent", WshShell.CurrentDirectory + "\\custom.ent");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk.xsd")) {
	FSObject.CopyFile(logtalk_home + "\\xml\\logtalk.xsd", WshShell.CurrentDirectory + "\\logtalk.xsd");
}

if (!FSObject.FileExists(directory + "\\logtalk.css")) {
	FSObject.CopyFile(logtalk_user + "\\xml\\logtalk.css", directory + "\\logtalk.css");
}

WScript.Echo("");
WScript.Echo("converting XML files...");

var files = new Enumerator(FSObject.GetFolder(WshShell.CurrentDirectory).Files);

for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
	var file = files.item().name;
	if (FSObject.GetExtensionName(file) == "xml") {
		WScript.Echo("  converting " + file);
		var txt_file = directory + "\\" + FSObject.GetBaseName(file) + ".txt";
		switch (processor) {
			case "msxsl" :
				WshShell.Run("msxsl -o \"" + txt_file + "\" \"" + file + "\" \"" + xslt + "\"", true);
				break;
			case "xsltproc" :
				WshShell.Run("xsltproc -o \"" + txt_file + "\" \"" + xslt + "\" \"" + file + "\"", true);
				break;
			case "xalan" :
				WshShell.Run("xalan -o \"" + txt_file + "\" \"" + file + "\" \"" + xslt + "\"", true);
				break;
			case "sabcmd" :
				WshShell.Run("sabcmd \"" + xslt + "\" \"" + file + "\" \"" + txt_file + "\"", true);
				break;
		}
	}
}

WScript.Echo("conversion done");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Logtalk XML files documenting files in the");
	WScript.Echo("current directory to text files");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " [/d:directory] [/p:processor]");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  d - output directory for the generated files (default is " + directory + ")");
	WScript.Echo("  p - XSLT processor (msxsl, xsltproc, xalan, or sabcmd; default is " + processor + ")");
	WScript.Echo("");
	WScript.Quit(1);
}
