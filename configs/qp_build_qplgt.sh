#! /bin/sh

echo "Run from the configs directory"

cd ..
LOGTALKHOME=`pwd`
find . -name "*.lgt" -exec perl -pi -e "s/version is (\d)\.(\d)/version is '\1\.\2'/" {} \;

cd $LOGTALKHOME/configs; cp qu64.config qu64.ql

echo "fcompile('qu64.ql', [assemble_only(true)]), load(qu64). \
chdir('../compiler/'), fcompile('logtalk.pl', [assemble_only(true), string_table(256)]), load(logtalk)." | qp -s 2048 -d 1024 -h 2000


cd $LOGTALKHOME/configs; qc -c qphook.ql

cd ..
if ! [ -d bin ]
then
  mkdir bin
fi

cd bin
qc -s 2048 -d 1024 -h 2000 -o qplgt ../configs/qphook.qo ../configs/qu64.qo ../compiler/logtalk.qo
