#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.37.5
## 
## Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
## Logtalk is free software.  You can redistribute it and/or modify
## it under the terms of the "Artistic License 2.0" as published by 
## The Perl Foundation. Consult the "LICENSE.txt" file for details.
## ================================================================

if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first, pointing"
	echo "to your Logtalk installation directory!"
	echo "Trying the default locations for the Logtalk installation..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "... using Logtalk installation found at /usr/local/share/logtalk"
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "... using Logtalk installation found at /usr/share/logtalk"
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "... using Logtalk installation found at /opt/local/share/logtalk"
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "... using Logtalk installation found at /opt/share/logtalk"
	else
		echo "... unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	echo
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi
export LOGTALKHOME

if ! [ "$LOGTALKUSER" ]; then
	echo "The environment variable LOGTALKUSER should be defined first, pointing"
	echo "to your Logtalk user directory!"
	echo "Trying the default location for the Logtalk user directory..."
	export LOGTALKUSER=$HOME/logtalk
	if [ -d "$LOGTALKUSER" ]; then		
		echo "... using Logtalk user directory found at $LOGTALKUSER"
	else
		echo "... Logtalk user directory not found at default location. Creating a"
		echo "new Logtalk user directory by running the \"cplgtdirs\" shell script:"
		cplgtdirs
	fi
elif ! [ -d "$LOGTALKUSER" ]; then
	echo "Cannot find \$LOGTALKUSER directory! Creating a new Logtalk user directory"
	echo "by running the \"cplgtdirs\" shell script:"
	cplgtdirs
fi
echo

format=xhtml
index_file=index.html
index_title="Entity documentation index"

usage_help()
{
	echo 
	echo "This script generates an index for all the Logtalk XML files"
	echo "documenting files in the current directory"
	echo
	echo "Usage:"
	echo "  $0 -f format -i index -t title"
	echo "  $0 -h"
	echo
	echo "Optional arguments:"
	echo "  -f format of the index file (either xhtml or html; default is $format)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used on the index file (default is $index_title)"
	echo "  -h help"
	echo
	exit 1
}

create_index_file()
{
	echo "" > "$index_file"

	case "$format" in
		xhtml)
			echo "<?xml version=\"1.0\" encoding=\"utf-8\"?>" >> "$index_file"
			echo "<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>" >> "$index_file"
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" >> "$index_file"
			echo "<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" >> "$index_file"
			;;
		html)
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" >> "$index_file"
			echo "<html>" >> "$index_file"
			;;
	esac

	echo "<head>" >> "$index_file"
	echo "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" >> "$index_file"
	echo "    <title>"$index_title"</title>" >> "$index_file"
	echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>" >> "$index_file"
	echo "</head>" >> "$index_file"
	echo "<body>" >> "$index_file"
	echo "<h1>"$index_title"</h1>" >> "$index_file"
	echo "<ul>" >> "$index_file"

	for file in `grep -l "<logtalk>" *.xml`; do
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		entity=${name%_*}
		pars=${name##*_}
		echo "  indexing $file"
		if [ $pars -gt 0 ]
		then
			echo "    <li><a href=\""$file"\">"$entity"/"$pars"</a></li>" >> "$index_file"
		else
			echo "    <li><a href=\""$file"\">"$entity"</a></li>" >> "$index_file"
		fi
	done

	echo "</ul>" >> "$index_file"

	date="`eval date`"

	echo "<p>Generated on "$date"</p>" >> "$index_file"
	echo "</body>" >> "$index_file"
	echo "</html>" >> "$index_file"
}

while getopts "f:i:t:h" Option
do
	case $Option in
		f) f_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [ "$f_arg" != "" ] && [ "$f_arg" != "xhtml" ] && [ "$f_arg" != "html" ] ; then
	echo "Error! Unsupported output format: $f_arg"
	usage_help
	exit 1
elif [ "$f_arg" != "" ] ; then
	format=$f_arg
fi

if [ "$i_arg" != "" ] ; then
	index_file=$i_arg
fi

if [ "$t_arg" != "" ] ; then
	index_title=$t_arg
fi

if ! [ -e "./logtalk.dtd" ] ; then
	cp "$LOGTALKHOME"/xml/logtalk.dtd .
fi

if ! [ -e "./custom.ent" ] ; then
	cp "$LOGTALKUSER"/xml/custom.ent .
fi

if ! [ -e "./logtalk.xsd" ] ; then
	cp "$LOGTALKHOME"/xml/logtalk.xsd .
fi

if ! [ -e "./logtalk.css" ] ; then
	cp "$LOGTALKUSER"/xml/logtalk.css .
fi

if ! [ -e "./lgtxml.xsl" ] ; then
	cp "$LOGTALKUSER"/xml/lgtxml.xsl .
fi

if [ `(ls *.xml | wc -l) 2> /dev/null` -gt 0 ] ; then
	echo
	echo "generating $index_file file..."
	create_index_file
	echo "$index_file file generated"
	echo
else
	echo
	echo "No XML files exist in the current directory!"
	echo
fi

exit 0
