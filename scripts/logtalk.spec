Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.17.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
find /usr/local/lgt2170/ -name CVS -print | xargs rm -rf
find /usr/local/lgt2170/ -name .cvsignore -print | xargs rm -f
find /usr/local/lgt2170/ -name .#* -print | xargs rm -f
find /usr/local/lgt2170/ -name .DS_Store -print | xargs rm -f
%build
%install
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2170/BIBLIOGRAPHY
%doc /usr/local/lgt2170/INSTALL
%doc /usr/local/lgt2170/LICENSE
%doc /usr/local/lgt2170/QUICK_START
%doc /usr/local/lgt2170/README
%doc /usr/local/lgt2170/RELEASE_NOTES
%doc /usr/local/lgt2170/UPGRADING
/usr/local/lgt2170/compiler
/usr/local/lgt2170/configs
/usr/local/lgt2170/examples
/usr/local/lgt2170/library
%docdir /usr/local/lgt2170/manuals
/usr/local/lgt2170/manuals
/usr/local/lgt2170/misc
/usr/local/lgt2170/wenv
/usr/local/lgt2170/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs.sh
