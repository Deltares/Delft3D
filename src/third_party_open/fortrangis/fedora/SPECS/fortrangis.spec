%global releaseno 1
# Note: define srcarchivename in CI build only.
%{!?srcarchivename: %global srcarchivename %{name}-%{version}-%{releaseno}}

Summary: FortranGIS Fortran interfaces Open Source GIS libraries 
Name: fortrangis
Version: 3.0
Release: %{releaseno}%{dist}
License: LGPL
Group: Applications/GIS
URL: http://fortrangis.berlios.de/
Packager: Davide Cesari <dcesari69@gmail.com>
Source: https://github.com/arpa-simc/%{name}/archive/v%{version}-%{releaseno}.tar.gz#/%{srcarchivename}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildRequires: libtool
BuildRequires: doxygen
BuildRequires: gcc-gfortran
BuildRequires: shapelib-devel
BuildRequires: proj-devel
Buildrequires: gdal-devel

%if 0%{?fedora} < 9 || 0%{?rhel}
%define _fmoddir       %{_libdir}/gfortran/modules
%endif

%package -n fortrangis-devel
Summary:  FortranGIS development files
Group: Applications/GIS
Requires: %{name}

%if 0%{?fedora} >= 9
%package -n fortrangis-doc
Summary:  FortranGIS documentation
Group: Applications/GIS
%endif

%description -n fortrangis-devel
Development files, necessary for building applications using
FortranGIS

%if 0%{?fedora} >= 9
%description -n fortrangis-doc
Doxygen documentation for FortranGIS package
%endif

%description
FortranGIS is a collection of Fortran interfaces to the most common
Open Source GIS libraries, plus some more Fortran-specific tools.

The libraries interfaced at the moment are Shapelib, GDAL and Proj.

%prep
%setup -q -n %{srcarchivename}

%build

autoreconf -ifv

%configure %{?fedora:CPPFLAGS=-I/usr/include/libshp} %{?rhel:--disable-doxydoc}
make 

%install
make DESTDIR=%{buildroot} install
%if 0%{?fedora} >= 9  || 0%{?rhel}
mkdir -p $RPM_BUILD_ROOT%{_fmoddir}
mv $RPM_BUILD_ROOT%{_includedir}/*.mod $RPM_BUILD_ROOT%{_fmoddir}
%endif

%clean
[ "%{buildroot}" != / ] && rm -rf "%{buildroot}"

%files
%defattr(-, root, root)
%{_libdir}/*.so.*
%doc %{_datadir}/doc/%{name}-%{version}/COPYING
%doc %{_datadir}/doc/%{name}-%{version}/README.md

%files -n fortrangis-devel
%{_libdir}/*.a
%exclude %{_libdir}/*.la
%{_libdir}/*.so
%if 0%{?fedora} < 9  && !0%{?rhel}
%{_includedir}/*
%else
%{_fmoddir}/*.mod
%endif

%if 0%{?fedora} >= 9
%files -n fortrangis-doc
%defattr(-,root,root,-)
%doc %{_datadir}/doc/%{name}-%{version}/html
%endif

%pre

%post

%preun

%postun

%changelog
* Fri Sep  2 2022 Daniele Branchini <dbranchini@arpae.it> - 3.0-1
- Added support for proj6 (#15)
- Removed .la files
- cmake bugfix (#16)

* Fri Feb 18 2022 Daniele Branchini <dbranchini@arpae.it> - 2.7-1
- Fixed spec for new CI
- Fix interfacing with proj C API (#10)
- Improvements for cmake build system (#11,#12)
- Fixed segmentation fault of `pj_transform_f`
- Fixed issues in makefiles (#4)
- Added autogen.sh (#6)

* Fri Jun  5 2020 Daniele Branchini <dbranchini@arpae.it> - 2.6-6
- Bogus release to rebuild on new official gdal CentOS 8 package

* Thu Jan 23 2020 Daniele Branchini <dbranchini@arpae.it> - 2.6-5
- Restored gdal dependency for CentOS 8 build

* Mon Oct 15 2018 Daniele Branchini <dbranchini@arpae.it> - 2.6-4
- added dependencies for example programs (hotfix for f29 builds)

* Wed Apr 18 2018 Daniele Branchini <dbranchini@arpae.it> - 2.6-3
- enabling integration with SIMC copr repo

* Wed Mar 14 2018 Daniele Branchini <dbranchini@arpae.it> - 2.6-2
- enabling CentOs build and travis integration

* Wed Jul 13 2016 Daniele Branchini <dbranchini@arpae.it> - 2.6-1
- upstream patch for https://gcc.gnu.org/bugzilla/show_bug.cgi?id=68040

* Tue Jul 12 2016 Daniele Branchini <dbranchini@arpae.it> - 2.4-2
- automatizing rpmbuild

* Tue Mar 22 2016 Dario Mastropasqua,,, <dariomas@users.noreply.github.com> 2.4-2
- disabled fortrangis-doc package in RHEL (CentOS) because doxygen crashes
- corrected Fortran mod dir for RHEL (CentOS)

