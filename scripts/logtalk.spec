Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.26.1
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2261.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2261
%build
%install
rm -rf /usr/local/lgt2261
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2261
cp -R * /usr/local/lgt2261
cd /usr/local
chmod -R go-w,a+r lgt2261
chmod a+x lgt2261
chmod a+x lgt2261/scripts/*.sh
chmod a+x lgt2261/xml/*.sh
ln -sf lgt2261 logtalk
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2261/BIBLIOGRAPHY
%doc /usr/local/lgt2261/INSTALL
%doc /usr/local/lgt2261/LICENSE
%doc /usr/local/lgt2261/QUICK_START
%doc /usr/local/lgt2261/README
%doc /usr/local/lgt2261/RELEASE_NOTES
%doc /usr/local/lgt2261/UPGRADING
/usr/local/lgt2261/compiler
/usr/local/lgt2261/configs
/usr/local/lgt2261/contributions
/usr/local/lgt2261/examples
/usr/local/lgt2261/libpaths
/usr/local/lgt2261/library
%docdir /usr/local/lgt2261/manuals
/usr/local/lgt2261/manuals
/usr/local/lgt2261/scripts
/usr/local/lgt2261/wenv
/usr/local/lgt2261/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
