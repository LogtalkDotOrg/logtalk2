// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.19.2
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

var WshShell = new ActiveXObject("WScript.Shell");

var WshSysEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

if (!WshSysEnv.Item("LOGTALKHOME") && !WshUserEnv.Item("LOGTALKHOME")) {
	WScript.Echo("The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

var html_xslt = WshShell.ExpandEnvironmentStrings("LOGTALKHOME") + "\\xml\\lgthtml.xsl";
var xhtml_xslt = WshShell.ExpandEnvironmentStrings("LOGTALKHOME") + "\\xml\\lgtxhtml.xsl";
var xslt;

var format = "xhtml";
// var format = "html";

var directory = WshShell.CurrentDirectory;

var index_file = "index.html";
var title = "Entity documentation index";

var processor = "xsltproc";
// var processor = "xalan";
// var processor = "sabcmd";

if (WScript.Arguments.Unnamed.Length > 0)
	usage_help();

if (WScript.Arguments.Named.Exists("f"))
	format = WScript.Arguments.Named.Item("f");

if (WScript.Arguments.Named.Exists("d"))
	directory = WScript.Arguments.Named.Item("d");

if (WScript.Arguments.Named.Exists("i"))
	index_file = WScript.Arguments.Named.Item("i");

if (WScript.Arguments.Named.Exists("t"))
	title = WScript.Arguments.Named.Item("t");

if (WScript.Arguments.Named.Exists("p"))
	processor = WScript.Arguments.Named.Item("p");

if (format = "xhtml")
	xslt = xhtml_xslt;
else if (format = "html")
	xslt = html_xslt;
else {
	WScript.Echo("unsupported output format: " + format);
	usage_help;
	WScript.Quit(1);
}

if (processor != "xsltproc" && processor != "xalan" && processor != "sabcmd") {
	WScript.Echo("unsupported XSLT processor:" + processor);
	WScript.Echo("");
	WScript.Quit(1);
}

var fso = new ActiveXObject("Scripting.FileSystemObject");

fso.CopyFile(WshShell.ExpandEnvironmentStrings("LOGTALKHOME") + "\\xml\\logtalk.dtd", WshShell.CurrentDirectory);
fso.CopyFile(WshShell.ExpandEnvironmentStrings("LOGTALKHOME") + "\\xml\\logtalk.xsd", WshShell.CurrentDirectory);
fso.CopyFile(WshShell.ExpandEnvironmentStrings("LOGTALKHOME") + "\\xml\\logtalk.css", directory);

WScript.Echo("");
WScript.Echo("converting XML files...");

var files = WshShell.CurrentDirectory.Files;

for (file in files) 
	if (file.Extension = ".xml") {
		WScript.Echo("  converting" + file.Name);
		switch (processor) {
			case "xsltproc" :
				WshShell.Run(xsltproc + " -o " + directory + "\\" + file.FileName + ".html" + xslt + " "+ file.Name, true);
			case "xalan" :
				WshShell.Run(xalan + " -o " +  + directory + "\\" + file.FileName + ".html" + " " + xslt, true);
			case "sabcmd" :
				WshShell.Run(sabcmd + " " + xslt + " " + file + " " + directory + "\\" + file.FileName + ".html", true);
		}
	}

WScript.Echo("conversion done");
WScript.Echo("");
WScript.Echo("generating index file...");

index_file = directory + "\\" + index_file;

switch (format) {
	case "xhtml" :	xhtml_index_file();
	case "html" :	html_index_file();
}

WScript.Echo("index file generated");
WScript.Echo("");

fso.DeleteFile("logtalk.dtd");
fso.DeleteFile("logtalk.xsd");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Logtalk XML files documenting files in the");
	WScript.Echo("current directory to XHTML or HTML files");
	WScript.Echo("");
	WScript.Echo("Usage: " + WScript.Name + " [help] [/f:format] [/o:directory] [/i:index] [/t:title] [/p:processor]");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  f - output file format (either xhtml or html; default is " + format + ")");
	WScript.Echo("  o - output directory for the generated files (default is " + directory + ")");
	WScript.Echo("  i - name of the index file (default is " + index_file + ")");
	WScript.Echo("  t - title to be used on the index file (default is " + title + ")");
	WScript.Echo("  p - XSLT processor (xsltproc, xalan, or sabcmd; default is " + processor + ")");
	WScript.Echo("  help - print usage help");
	WScript.Echo("");
	WScript.Quit(1);
}

function xhtml_index_file() {

	var f = fso.CreateTextFile(directory + "\\" + index_file, true);

	f.WriteLine("<?xml version=\"1.0\"?>");
	f.WriteLine("<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>");
	f.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">");
	f.WriteLine("<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">");
	f.WriteLine("<head>");
	f.WriteLine("    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>");
	f.WriteLine("    <title>" + title + "</title>");
	f.WriteLine("    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>");
	f.WriteLine("</head>");
	f.WriteLine("<body>");
	f.WriteLine("<h1>" + title + "</h1>");
	f.WriteLine("<ul>");

	var files = WshShell.CurrentDirectory.Files;
	var file;

	for (file in files) 
		if (file.Extension = ".xml") {
			WScript.Echo("  indexing" + file.FileName + ".html");
			f.WriteLine("    <li><a href=\"" + file.FileName + ".html" + "\">" + file.FileName + "</a></li>");
		}

	f.WriteLine("</ul>");

	var today = new Date();
	var year  = today.getFullYear();
	var month = today.getMonth() + 1;
	if (month < 10)
        month = "0" + month;
	day   = today.getDate();
	if (day < 10)
        day = "0" + day;
	strToday = year + "/" + month + "/" + day;
	f.WriteLine("<p>Generated on " + strToday + "</p>");

	f.WriteLine("</body>");
	f.WriteLine("</html>");

	f.Close();
}

function html_index_file() {

	var a = fso.CreateTextFile(directory + "\\" + index_file, true);

	f.WriteLine("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">");
	f.WriteLine("<html>");
	f.WriteLine("<head>");
	f.WriteLine("    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">");
	f.WriteLine("    <title>" + title + "</title>");
	f.WriteLine("    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\">");
	f.WriteLine("</head>");
	f.WriteLine("<body>");
	f.WriteLine("<h1>" + title + "</h1>");
	f.WriteLine("<ul>");

	var files = WshShell.CurrentDirectory.Files;
	var file;
	
	for (file in files) 
		if (file.Extension = ".xml") {
			WScript.Echo("  indexing" + file.FileName + ".html");
			f.WriteLine("    <li><a href=\"" + file.FileName + ".html" + "\">" + file.FileName + "</a></li>");
		}

	f.WriteLine("</ul>");

	var today = new Date();
	var year  = today.getFullYear();
	var month = today.getMonth() + 1;
	if (month < 10)
        month = "0" + month;
	var day   = today.getDate();
	if (day < 10)
        day = "0" + day;
	var strToday = year + "/" + month + "/" + day;
	f.WriteLine("<p>Generated on " + strToday + "</p>");

	f.WriteLine("</body>");
	f.WriteLine("</html>");

	f.Close();
}
