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
find /usr/local/lib/lgt2170/ -name CVS -print | xargs rm -rf
find /usr/local/lib/lgt2170/ -name .cvsignore -print | xargs rm -f
find /usr/local/lib/lgt2170/ -name .#* -print | xargs rm -f
find /usr/local/lib/lgt2170/ -name .DS_Store -print | xargs rm -f
find /usr/local/share/lgt2170/ -name CVS -print | xargs rm -rf
find /usr/local/share/lgt2170/ -name .cvsignore -print | xargs rm -f
find /usr/local/share/lgt2170/ -name .#* -print | xargs rm -f
find /usr/local/share/lgt2170/ -name .DS_Store -print | xargs rm -f
%build
%install
%clean
%files
%defattr(-,root,users)
/usr/local/lib/lgt2170
/usr/local/lib/logtalk
/usr/local/share/lgt2170
/usr/local/share/logtalk
