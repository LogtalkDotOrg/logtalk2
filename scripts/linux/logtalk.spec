Name: logtalk
Summary: Logtalk - Open source object-oriented logic programming language
Version: 2.30.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://logtalk.org/files/lgt2300.tgz
BuildArchitectures: noarch
URL: http://logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented logic programming language that can use most Prolog implementations as a back-end compiler. As a multi-paradigm language, Logtalk includes support for both prototypes and classes, protocols, component-based programming through category-based composition, event-driven programming, and multi-threading programming.
%prep
%setup -n lgt2300
%build
%install
mkdir -p /usr/local/share
rm -rf /usr/local/share/lgt2300
rm -f /usr/local/share/logtalk
mkdir /usr/local/share/lgt2300
cp -R * /usr/local/share/lgt2300
cd /usr/local/share
chmod -R go-w,a+r lgt2300
chmod a+x lgt2300
chmod a+x lgt2300/scripts/*.sh
chmod a-x lgt2300/scripts/*.js
chmod a+x lgt2300/scripts/linux/*.sh
chmod a+x lgt2300/scripts/macosx/postflight
chmod a+x lgt2300/xml/*.sh
chmod a-x lgt2300/xml/*.js
ln -sf lgt2300 logtalk
cd ..
mkdir -p bin 
cd bin
ln -sf ../share/logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../share/logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../share/logtalk/xml/lgt2html.sh lgt2html
ln -sf ../share/logtalk/xml/lgt2xml.sh lgt2xml
%clean
%files
%defattr(-,root,users)
%doc /usr/local/share/lgt2300/BIBLIOGRAPHY.bib
%doc /usr/local/share/lgt2300/CUSTOMIZE.txt
%doc /usr/local/share/lgt2300/INSTALL.txt
%doc /usr/local/share/lgt2300/LICENSE.txt
%doc /usr/local/share/lgt2300/QUICK_START.txt
%doc /usr/local/share/lgt2300/README.txt
%doc /usr/local/share/lgt2300/RELEASE_NOTES.txt
%doc /usr/local/share/lgt2300/UPGRADING.txt
/usr/local/share/lgt2300/compiler
/usr/local/share/lgt2300/configs
/usr/local/share/lgt2300/contributions
/usr/local/share/lgt2300/examples
/usr/local/share/lgt2300/libpaths
/usr/local/share/lgt2300/library
%docdir /usr/local/share/lgt2300/manuals
/usr/local/share/lgt2300/manuals
/usr/local/share/lgt2300/scripts
/usr/local/share/lgt2300/wenv
/usr/local/share/lgt2300/xml
/usr/local/share/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
%post
mkdir -p /etc/profile.d
echo "# Logtalk environment setup" > /etc/profile.d/logtalk.sh
echo "" >> /etc/profile.d/logtalk.sh
echo "# Logtalk installation directory:" >> /etc/profile.d/logtalk.sh
echo "export LOGTALKHOME=$RPM_INSTALL_PREFIX/share/logtalk" >> /etc/profile.d/logtalk.sh
echo "" >> /etc/profile.d/logtalk.sh
echo "# Default location for Logtalk end-user files:" >> /etc/profile.d/logtalk.sh
echo "export LOGTALKUSER=\$HOME/logtalk" >> /etc/profile.d/logtalk.sh
chmod a+x /etc/profile.d/logtalk.sh
echo "# Logtalk environment setup" > /etc/profile.d/logtalk.csh
echo "" >> /etc/profile.d/logtalk.csh
echo "# Logtalk installation directory:" >> /etc/profile.d/logtalk.csh
echo "setenv LOGTALKHOME $RPM_INSTALL_PREFIX/share/logtalk" >> /etc/profile.d/logtalk.csh
echo "" >> /etc/profile.d/logtalk.csh
echo "# Default location for Logtalk end-user files:" >> /etc/profile.d/logtalk.csh
echo "setenv LOGTALKUSER \$HOME/logtalk" >> /etc/profile.d/logtalk.csh
chmod a+x /etc/profile.d/logtalk.csh
eval export LOGTALKHOME=$RPM_INSTALL_PREFIX/share/logtalk; cd $LOGTALKHOME/scripts; ./makeall_lgt.sh $RPM_INSTALL_PREFIX
echo ""
echo "Defined the following environment variables for all users:"
echo ""
echo "  Logtalk installation directory: LOGTALKHOME = $RPM_INSTALL_PREFIX/share/logtalk"
echo "  Default Logtalk user files directory: LOGTALKUSER = \$HOME/logtalk"
echo ""
echo "You may need to logout and login again or start a new shell in order to"
echo "use the new environment variables."
echo ""
echo "You may change the default value of the LOGTALKUSER environment variable"
echo "in your shell configuration files if you already use, or want to use, a "
echo "different location for the Logtalk user files directory."
echo ""
%postun
rm -f /etc/profile.d/logtalk.sh 2> /dev/null
rm -f /etc/profile.d/logtalk.csh 2> /dev/null
