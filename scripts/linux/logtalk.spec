Name: logtalk
Summary: Logtalk - Open source object-oriented logic programming language
Version: 2.37.5
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://logtalk.org/files/lgt2375.tar.bz2
BuildArchitectures: noarch
URL: http://logtalk.org/
AutoReqProv: no
%description
Logtalk is an object-oriented logic programming language that can use most Prolog implementations as a back-end compiler. As a multi-paradigm language, it includes support for both prototypes and classes, protocols (interfaces), component-based programming through category-based composition, event-driven programming, and high-level multi-threading programming.

%prep
%setup -q -c lgt2375 -n lgt2375

%build

%install
rm -rf $RPM_BUILD_ROOT
cd scripts
mkdir -p $RPM_BUILD_ROOT/usr/local
./install.sh $RPM_BUILD_ROOT/usr/local

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc /usr/local/share/lgt2375/BIBLIOGRAPHY.bib
%doc /usr/local/share/lgt2375/CUSTOMIZE.txt
%doc /usr/local/share/lgt2375/INSTALL.txt
%doc /usr/local/share/lgt2375/LICENSE.txt
%doc /usr/local/share/lgt2375/QUICK_START.txt
%doc /usr/local/share/lgt2375/README.txt
%doc /usr/local/share/lgt2375/RELEASE_NOTES.txt
%doc /usr/local/share/lgt2375/UPGRADING.txt
/usr/local/share/lgt2375/VERSION.txt
/usr/local/share/lgt2375/compiler
/usr/local/share/lgt2375/configs
/usr/local/share/lgt2375/contributions
/usr/local/share/lgt2375/examples
/usr/local/share/lgt2375/integration
/usr/local/share/lgt2375/libpaths
/usr/local/share/lgt2375/library
%doc /usr/local/share/lgt2375/manuals
/usr/local/share/lgt2375/scripts
/usr/local/share/lgt2375/settings.lgt
/usr/local/share/lgt2375/wenv
/usr/local/share/lgt2375/xml
/usr/local/share/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
/usr/local/bin/lgt2txt
/usr/local/bin/logtalk_select
/usr/local/bin/bplgt
/usr/local/bin/ciaolgt
/usr/local/bin/cxlgt
/usr/local/bin/eclipselgt
/usr/local/bin/gplgt
/usr/local/bin/plclgt
/usr/local/bin/qplgt
/usr/local/bin/quintuslgt
/usr/local/bin/sicstuslgt
/usr/local/bin/swilgt
/usr/local/bin/xsblgt
/usr/local/bin/xsb64lgt
/usr/local/bin/xsbmtlgt
/usr/local/bin/xsbmt64lgt
/usr/local/bin/yaplgt

%post
echo
echo "Installed Logtalk on \"/usr/local/share\"."
echo
echo "Links to the \"cplgtdirs\", \"lgt2pdf\", \"lgt2html\", \"lgt2xml\", and"
echo "\"lgt2txt\" scripts have been created on \"/usr/local/bin\";"
echo " you may need to add this directoryto your execution path."
echo
echo "The following integration scripts are installed for running Logtalk"
echo "with selected back-end Prolog compilers:"
echo
echo "* B-Prolog (version 7.1 or later):         bplgt"
echo "* Ciao (version 1.10):                     ciaolgt    (first run must use sudo)"
echo "* CxProlog (version 0.97.2 or later):      cxlgt"
echo "* ECLiPSe (versions 5.10, 6.0):            eclipselgt"
echo "* GNU Prolog (version 1.3.1 or later):     gplgt"
echo "* K-Prolog (versions 5.1.5, 6.0.4):        plclgt"
echo "* Qu-Prolog (version 8.9 or later):        qplgt"
echo "* Quintus Prolog (version 3.5):            quintuslgt  (requires patching Logtalk)"
echo "* SICStus Prolog (versions 3.12.x, 4.0.x): sicstuslgt"
echo "* SWI-Prolog (version 5.6.44 or later):    swilgt"
echo "* XSB (version 3.2 or later):              xsblgt     (first run must use sudo)"
echo "* XSB 64 bits (version 3.2 or later):      xsb64lgt   (first run must use sudo)"
echo "* XSB MT (version 3.2 or later):           xsbmtlgt   (first run must use sudo)"
echo "* XSB MT 64 bits (version 3.2 or later):   xsbmt64lgt (first run must use sudo)"
echo "* YAP (version 5.1.3 or later):            yaplgt"
echo
echo "The Prolog integration scripts can be found on \"/usr/local/bin\"."
echo "Make sure that the Prolog compilers are properly installed and available"
echo "on your execution path."
echo
echo "Integration with Quintus Prolog requires manual patches that render"
echo "Logtalk incompatible with all the other compilers."
echo
echo "If you get an unexpected failure when using one of the Prolog integration"
echo "scripts, consult the \"/usr/local/share/logtalk/configs/NOTES.txt\" file"
echo "for compatibility notes."
echo
mkdir -p /etc/profile.d
echo "# Logtalk environment setup" > /etc/profile.d/logtalk.sh
echo "" >> /etc/profile.d/logtalk.sh
echo "# Logtalk installation directory:" >> /etc/profile.d/logtalk.sh
echo "export LOGTALKHOME=/usr/local/share/logtalk" >> /etc/profile.d/logtalk.sh
echo "" >> /etc/profile.d/logtalk.sh
echo "# Default location for Logtalk end-user files:" >> /etc/profile.d/logtalk.sh
echo "export LOGTALKUSER=\$HOME/logtalk" >> /etc/profile.d/logtalk.sh
chmod a+x /etc/profile.d/logtalk.sh
echo "# Logtalk environment setup" > /etc/profile.d/logtalk.csh
echo "" >> /etc/profile.d/logtalk.csh
echo "# Logtalk installation directory:" >> /etc/profile.d/logtalk.csh
echo "setenv LOGTALKHOME /usr/local/share/logtalk" >> /etc/profile.d/logtalk.csh
echo "" >> /etc/profile.d/logtalk.csh
echo "# Default location for Logtalk end-user files:" >> /etc/profile.d/logtalk.csh
echo "setenv LOGTALKUSER \$HOME/logtalk" >> /etc/profile.d/logtalk.csh
chmod a+x /etc/profile.d/logtalk.csh
echo "Defined the following environment variables for all users:"
echo
echo "  Logtalk installation directory: LOGTALKHOME = /usr/local/share/logtalk"
echo "  Default Logtalk user files directory: LOGTALKUSER = \$HOME/logtalk"
echo
echo "You may need to logout and login again or start a new shell in order to"
echo "use the new environment variables."
echo
echo "Users may change the default value of the LOGTALKUSER environment variable"
echo "in their shell configuration files if they already use, or want to use, a "
echo "different location for the Logtalk user files directory. This directory "
echo "is created by running the \"cplgtdirs\" shell script, which must be run "
echo "once by each user before using the integration scripts."
echo
if [ -x /usr/bin/update-mime-database ]; then
	mkdir -p /usr/share/mime/packages
	rm -f /usr/share/mime/packages/logtalk.xml
	cp /usr/local/share/lgt2375/scripts/freedesktop/logtalk.xml /usr/share/mime/packages/logtalk.xml
	update-mime-database /usr/share/mime
	echo "Added the Logtalk mime-type to the Shared MIME-info Database."
	echo
fi
echo "Logtalk basic installation completed."
echo

%postun
rm -f /etc/profile.d/logtalk.sh 2> /dev/null
rm -f /etc/profile.d/logtalk.csh 2> /dev/null
