Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.28.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2280.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2280
%build
%install
rm -rf /usr/local/lgt2280
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2280
cp -R * /usr/local/lgt2280
cd /usr/local
chmod -R go-w,a+r lgt2280
chmod a+x lgt2280
chmod a+x lgt2280/scripts/*.sh
chmod a+x lgt2280/xml/*.sh
ln -sf lgt2280 logtalk
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2280/BIBLIOGRAPHY.bib
%doc /usr/local/lgt2280/CUSTOMIZE.txt
%doc /usr/local/lgt2280/INSTALL.txt
%doc /usr/local/lgt2280/LICENSE.txt
%doc /usr/local/lgt2280/QUICK_START.txt
%doc /usr/local/lgt2280/README.txt
%doc /usr/local/lgt2280/RELEASE_NOTES.txt
%doc /usr/local/lgt2280/UPGRADING.txt
/usr/local/lgt2280/compiler
/usr/local/lgt2280/configs
/usr/local/lgt2280/contributions
/usr/local/lgt2280/examples
/usr/local/lgt2280/libpaths
/usr/local/lgt2280/library
%docdir /usr/local/lgt2280/manuals
/usr/local/lgt2280/manuals
/usr/local/lgt2280/scripts
/usr/local/lgt2280/wenv
/usr/local/lgt2280/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
%post
echo "export LOGTALKHOME=$RPM_INSTALL_PREFIX/logtalk" > /etc/profile.d/logtalk.sh
echo "setenv LOGTALKHOME $RPM_INSTALL_PREFIX/logtalk" > /etc/profile.d/logtalk.csh
eval export LOGTALKHOME=$RPM_INSTALL_PREFIX/logtalk; cd $LOGTALKHOME/scripts; ./makeall_lgt.sh $RPM_INSTALL_PREFIX
%postun
rm -f /etc/profile.d/logtalk.sh
rm -f /etc/profile.d/logtalk.csh
