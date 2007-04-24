#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.30.0
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

if [ -z "$1" ]; then
	case $( uname -s ) in
		Darwin	) prefix=/opt/local;;
		*		) prefix=/usr/local;;
	esac
	mkdir -p $prefix
else
	prefix="$1"
fi

if ! [ -d "$prefix" ]; then
	echo "Directory prefix does not exist!"
	echo
	exit 1
fi

echo
echo "Installing Logtalk on $prefix/share ..."
echo

mkdir -p $prefix/share

rm -rf $prefix/share/lgt2300
rm -f $prefix/share/logtalk

mkdir $prefix/share/lgt2300

cd ..
cp -R * $prefix/share/lgt2300

cd $prefix/share/lgt2300
scripts/cleandist.sh
find . -type f -print0 | xargs -0 chmod 644
find . -type d -print0 | xargs -0 chmod 755
chmod a+x integration/*.sh
chmod a+x manuals/userman/*.sh
chmod a+x manuals/refman/*.sh
chmod a+x scripts/*.sh
chmod a-x scripts/*.js
chmod a+x scripts/debian/postinst
chmod a+x scripts/debian/prerm
chmod a+x scripts/debian/postrm
chmod a+x scripts/linux/*.sh
chmod a+x scripts/macosx/postflight
chmod a+x xml/*.sh
chmod a-x xml/*.js

cd ..
ln -sf lgt2300 logtalk

mkdir -p $prefix/bin
cd $prefix/bin

ln -sf ../share/logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../share/logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../share/logtalk/xml/lgt2html.sh lgt2html
ln -sf ../share/logtalk/xml/lgt2xml.sh lgt2xml

echo
echo "Links to the cplgtdirs, lgt2pdf, lgt2html, and lgt2xml scripts have"
echo "been created on $prefix/bin; you may need to add this directory to"
echo "your execution path."
echo

ln -sf ../share/logtalk/integration/bplgt.sh bplgt
echo "bplgt script installed       (B-Prolog integration script)"

ln -sf ../share/logtalk/integration/ciaolgt.sh ciaolgt
echo "ciaolgt script installed     (Ciao Prolog integration script)"

ln -sf ../share/logtalk/integration/cxlgt.sh cxlgt
echo "cxlgt script installed       (CxProlog integration script)"

ln -sf ../share/logtalk/integration/eclipselgt.sh eclipselgt
echo "eclipselgt script installed  (ECLiPSe integration script)"

ln -sf ../share/logtalk/integration/gplgt.sh gplgt
echo "gplgt script installed       (GNU Prolog integration script)"

ln -sf ../share/logtalk/integration/plclgt.sh plclgt
echo "plclgt script installed      (K-Prolog integration script)"

#ln -sf ../share/logtalk/integration/qplgt.sh qplgt
#echo "qplgt script installed       (Qu-Prolog integration script)"

ln -sf ../share/logtalk/integration/sicstuslgt.sh sicstuslgt
echo "sicstuslgt script installed  (SICStus Prolog integration script)"

ln -sf ../share/logtalk/integration/swilgt.sh swilgt
echo "swilgt script installed      (SWI-Prolog integration script)"

ln -sf ../share/logtalk/integration/xsblgt.sh xsblgt
echo "xsblgt script installed      (XSB integration script)"

ln -sf ../share/logtalk/integration/yaplgt.sh yaplgt
echo "yaplgt script installed      (YAP integration script)"

echo
echo "The Prolog integration scripts can be found on $prefix/bin."
echo "Make sure that the Prolog compilers are also available on your execution"
echo "path."
echo
echo "If you got an unexpected failure when using one of the Prolog integration"
echo "scripts, make sure that the Prolog compiler is properly installed, consult"
echo "the NOTES file on the scripts directory, and try to run the corresponding"
echo "script individually."
echo
echo "Users should ensure that the environment variables LOGTALKHOME"
echo "and LOGTALKUSER are defined and then run the \"cplgtdirs\" script"
echo "once prior to using the Prolog integration scripts."
echo

echo "Logtalk basic installation completed. See the INSTALL and CUSTOMIZATION"
echo "files for details on customizing your working environment."
echo
echo "Users must define the environment variable LOGTALKHOME pointing to"
echo "$prefix/share/logtalk and then run the \"cplgtdirs\" shell script"
echo "in order to copy the Logtalk user-modifiable files to their home"
echo "directories."
echo
