Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.17.3
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2173.tar.gz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2173
%build
%install
rm -rf /usr/local/lgt2173
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2173
cp -R * /usr/local/lgt2173
cd /usr/local
chmod -R go-w,a+r lgt2173
chmod a+x lgt2173
chmod a+x lgt2173/misc/*.sh
chmod a+x lgt2173/xml/*.sh
ln -sf lgt2173 logtalk
cd bin
ln -sf ../lgt2173/misc/cplgtdirs.sh cplgtdirs.sh
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2173/BIBLIOGRAPHY
%doc /usr/local/lgt2173/INSTALL
%doc /usr/local/lgt2173/LICENSE
%doc /usr/local/lgt2173/QUICK_START
%doc /usr/local/lgt2173/README
%doc /usr/local/lgt2173/RELEASE_NOTES
%doc /usr/local/lgt2173/UPGRADING
/usr/local/lgt2173/compiler
/usr/local/lgt2173/configs
/usr/local/lgt2173/examples
/usr/local/lgt2173/library
%docdir /usr/local/lgt2173/manuals
/usr/local/lgt2173/manuals
/usr/local/lgt2173/misc
/usr/local/lgt2173/wenv
/usr/local/lgt2173/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs.sh
