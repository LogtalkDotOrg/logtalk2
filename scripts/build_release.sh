#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.32.1
##
## Copyright (c) 1998-2008 Paulo Moura. All Rights Reserved.
## 
## Logtalk is free software. You can redistribute it and/or modify
## it under the terms of the Artistic License 2.0 as published by 
## the The Perl Foundation.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
## Artistic License 2.0 for more details. A copy of the license is 
## provided in the "LICENSE.txt" file.
## ================================================================

dir=`PWD`

svn export http://svn.logtalk.org/logtalk/trunk logtalk

cd logtalk
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
cp -R logtalk/manuals man2321
tar -czf man2321.tgz man2321
mv logtalk lgt2321
tar -cjf lgt2321.tar.bz2 lgt2321

mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd lgt2321/scripts
./install.sh $dir/debian/usr
cp debian/logtalk.doc-base $dir/debian/usr/share/doc-base/logtalk-docs
cp debian/menu $dir/debian/usr/share/menu/logtalk
cp ../*.bib $dir/debian/usr/share/doc/logtalk
cp ../*.txt $dir/debian/usr/share/doc/logtalk
cp debian/copyright $dir/debian/usr/share/doc/logtalk
cp debian/changelog $dir/debian/usr/share/doc/logtalk
cp debian/changelog.Debian $dir/debian/usr/share/doc/logtalk
gzip --best $dir/debian/usr/share/doc/logtalk/*.bib 
gzip --best $dir/debian/usr/share/doc/logtalk/*.txt 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog 
gzip --best $dir/debian/usr/share/doc/logtalk/changelog.Debian
cp debian/control $dir/debian/DEBIAN
cp debian/postinst $dir/debian/DEBIAN
cp debian/prerm $dir/debian/DEBIAN
cp debian/postrm $dir/debian/DEBIAN
cd $dir
dpkg-deb --build debian logtalk_2.32.1-1_all.deb

md5="`md5 -q lgt2321.tar.bz2`"
sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
sudo cp -f lgt2321.tar.bz2 /opt/local/var/macports/distfiles/logtalk/lgt2321.tar.bz2
cd /opt/local/var/macports/sources/rsync.macports.org/release/ports/lang/logtalk/
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 2.32.1/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo port clean --archive logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.32.1.pkg $dir
sudo port clean logtalk

cd $dir
mkdir manpdf2321
cd man2321/userman
./userman.sh
mv userman.pdf ../../manpdf2321
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2321
cd ../..
tar -czf manpdf2321.tgz manpdf2321
