Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.20.2
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2202.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2202
%build
%install
rm -rf /usr/local/lgt2202
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2202
cp -R * /usr/local/lgt2202
cd /usr/local
chmod -R go-w,a+r lgt2202
chmod a+x lgt2202
chmod a+x lgt2202/misc/*.sh
chmod a+x lgt2202/xml/*.sh
ln -sf lgt2202 logtalk
cd bin
ln -sf ../lgt2202/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2202/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2202/xml/lgt2html.sh lgt2html
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2202/BIBLIOGRAPHY
%doc /usr/local/lgt2202/INSTALL
%doc /usr/local/lgt2202/LICENSE
%doc /usr/local/lgt2202/QUICK_START
%doc /usr/local/lgt2202/README
%doc /usr/local/lgt2202/RELEASE_NOTES
%doc /usr/local/lgt2202/UPGRADING
/usr/local/lgt2202/compiler
/usr/local/lgt2202/configs
/usr/local/lgt2202/examples
/usr/local/lgt2202/library
%docdir /usr/local/lgt2202/manuals
/usr/local/lgt2202/manuals
/usr/local/lgt2202/misc
/usr/local/lgt2202/wenv
/usr/local/lgt2202/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
