#!/bin/sh

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.0
##
## Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
## ================================================================

dir=`PWD`

svn export http://svn.logtalk.org/logtalk/trunk logtalk

cd logtalk
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
cp -R logtalk/manuals man2310
tar -czf man2310.tgz man2310
mv logtalk lgt2310
tar -cjf lgt2310.tar.bz2 lgt2310

mkdir -p debian/usr/bin
mkdir -p debian/usr/share/doc/logtalk
mkdir -p debian/usr/share/doc-base
mkdir -p debian/usr/share/menu
mkdir -p debian/DEBIAN
cd lgt2310/scripts
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
dpkg-deb --build debian logtalk_2.31.0-1_all.deb

md5="`md5 -q lgt2310.tar.bz2`"
sudo mkdir -p /opt/local/var/macports/distfiles/logtalk
sudo cp -f lgt2310.tar.bz2 /opt/local/var/macports/distfiles/logtalk/lgt2310.tar.bz2
cd /opt/local/var/macports/sources/rsync.macports.org/release/ports/lang/logtalk/
sudo cp -f Portfile Portfile.old
sudo sed -e 's/^version.*/version 2.31.0/' -i '' Portfile
sudo sed -e "s/^checksums.*/checksums md5 $md5/" -i '' Portfile
sudo sed -e 's/^distname.*/distname lgt2310/' -i '' Portfile
sudo sed -e 's/^extract\.suffix.*/extract.suffix .tar.bz2/' -i '' Portfile
sudo port clean --archive logtalk
sudo port destroot logtalk
sudo port pkg logtalk
cp -R work/logtalk-2.31.0.pkg $dir
sudo port clean logtalk

cd $dir
mkdir manpdf2310
cd man2310/userman
./userman.sh
mv userman.pdf ../../manpdf2310
cd ../refman
./refman.sh
mv refman.pdf ../../manpdf2310
cd ../..
tar -czf manpdf2310.tgz manpdf2310
