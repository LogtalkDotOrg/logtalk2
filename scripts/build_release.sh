#!/bin/sh

## =================================================================
## Logtalk - Object oriented extension to Prolog
## Release 2.29.6
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## =================================================================

dir=`PWD`

cvs -d :pserver:anonymous@cvs.logtalk.org:/usr/local/cvsroot checkout logtalk

cd logtalk
scripts/cleandist.sh
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
cp -R logtalk/manuals man2296
tar -czf man2296.tgz man2296
mv logtalk lgt2296
tar -czf lgt2296.tgz lgt2296

mkdir -p debian/usr/local/share
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
cd lgt2296/scripts
./lgt_install.sh $dir/debian/usr/local
cp debian/logtalk.doc-base $dir/debian/usr/share/doc-base/logtalk-docs
cp debian/menu $dir/debian/usr/share/menu/logtalk
cd $dir/debian
ln -sf usr/local/share/lgt2296/scripts/debian DEBIAN
dpkg-deb -b . logtalk_2.29.6-1_all.deb
mv logtalk_2.29.6-1_all.deb ..
cd ..

md5="`md5 -q lgt2296.tgz`"
sudo mkdir -p /opt/local/var/db/dports/distfiles/logtalk
sudo cp -f lgt2296.tgz /opt/local/var/db/dports/distfiles/logtalk/lgt2296.tgz
cd /opt/local/var/db/dports/sources/rsync.rsync.darwinports.org_dpupdate_dports/lang/logtalk/
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 2.29.6/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo sed -e 's/^distname.*/distname lgt2296/' -i '' Portfile
sudo port clean --archive logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.29.6.pkg $dir
sudo port clean logtalk

cd $dir
mkdir manpdf2296
cd man2296/userman
./userman.sh
mv userman.pdf ../../manpdf2296
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2296
cd ../..
tar -czf manpdf2296.tgz manpdf2296
