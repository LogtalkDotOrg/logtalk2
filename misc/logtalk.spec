Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.19.2
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2192.tar.gz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2192
%build
%install
rm -rf /usr/local/lgt2192
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2192
cp -R * /usr/local/lgt2192
cd /usr/local
chmod -R go-w,a+r lgt2192
chmod a+x lgt2192
chmod a+x lgt2192/misc/*.sh
chmod a+x lgt2192/xml/*.sh
ln -sf lgt2192 logtalk
cd bin
ln -sf ../lgt2192/misc/cplgtdirs.sh cplgtdirs.sh
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2192/BIBLIOGRAPHY
%doc /usr/local/lgt2192/INSTALL
%doc /usr/local/lgt2192/LICENSE
%doc /usr/local/lgt2192/QUICK_START
%doc /usr/local/lgt2192/README
%doc /usr/local/lgt2192/RELEASE_NOTES
%doc /usr/local/lgt2192/UPGRADING
/usr/local/lgt2192/compiler
/usr/local/lgt2192/configs
/usr/local/lgt2192/examples
/usr/local/lgt2192/library
%docdir /usr/local/lgt2192/manuals
/usr/local/lgt2192/manuals
/usr/local/lgt2192/misc
/usr/local/lgt2192/wenv
/usr/local/lgt2192/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs.sh
