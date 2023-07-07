# predefined constant values

!IFDEF CONFIG_OPT
!INCLUDE $(CONFIG_OPT)
!ENDIF

!IFNDEF WIN64
!IF [cl 2>&1|find "x64" >NUL ] == 0
WIN64=YES
!ENDIF
!ENDIF

# identify compiler version
!IFNDEF MSVC_VER
!IF "$(_NMAKE_VER)" == "6.00.8168.0"
MSVC_VER = 1200
CMAKE_GENERATOR = "Visual Studio 6"
CMAKE_BUILDDIR = vc6
!ELSEIF "$(_NMAKE_VER)" == "7.00.9466"
MSVC_VER = 1300
CMAKE_GENERATOR = "Visual Studio 7"
CMAKE_BUILDDIR = vc7
!ELSEIF "$(_NMAKE_VER)" == "7.10.3077"
MSVC_VER = 1310
CMAKE_GENERATOR = "Visual Studio 7 .NET 2003"
CMAKE_BUILDDIR = vc71
!ELSEIF "$(_NMAKE_VER)" == "8.00.50727.42"
MSVC_VER = 1400
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 8 2005 Win64"
CMAKE_BUILDDIR = vc8x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 8 2005"
CMAKE_BUILDDIR = vc8
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "8.00.50727.762"
MSVC_VER = 1400
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 8 2005 Win64"
CMAKE_BUILDDIR = vc8x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 8 2005"
CMAKE_BUILDDIR = vc8
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "9.00.21022.08"
MSVC_VER = 1500
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 9 2008 Win64"
CMAKE_BUILDDIR = vc9x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 9 2008"
CMAKE_BUILDDIR = vc9
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "9.00.30729.01"
MSVC_VER = 1500
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 9 2008 Win64"
CMAKE_BUILDDIR = vc9x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 9 2008"
CMAKE_BUILDDIR = vc9
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "10.00.30128.01"
MSVC_VER = 1600
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 10 Win64"
CMAKE_BUILDDIR = vc10x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 10"
CMAKE_BUILDDIR = vc10
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "10.00.30319.01"
MSVC_VER = 1600
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 10 Win64"
CMAKE_BUILDDIR = vc10x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 10"
CMAKE_BUILDDIR = vc10
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "11.00.50727.1"
MSVC_VER = 1700
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 11 Win64"
CMAKE_BUILDDIR = vc11x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 11"
CMAKE_BUILDDIR = vc11
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "11.00.61030.0"
MSVC_VER = 1700
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 11 Win64"
CMAKE_BUILDDIR = vc11x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 11"
CMAKE_BUILDDIR = vc11
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "12.00.21005.1"
MSVC_VER = 1800
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 12 Win64"
CMAKE_BUILDDIR = vc12x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 12"
CMAKE_BUILDDIR = vc12
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.00.23026.0"
MSVC_VER = 1900
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 14 Win64"
CMAKE_BUILDDIR = vc14x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 14"
CMAKE_BUILDDIR = vc14
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.00.23506.0"
MSVC_VER = 1900
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 14 Win64"
CMAKE_BUILDDIR = vc14x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 14"
CMAKE_BUILDDIR = vc14
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.00.24210.0"
MSVC_VER = 1900
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 14 Win64"
CMAKE_BUILDDIR = vc14x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 14"
CMAKE_BUILDDIR = vc14
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.11.25507.1"
MSVC_VER = 1911
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 15 Win64"
CMAKE_BUILDDIR = vc15x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 15"
CMAKE_BUILDDIR = vc15
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.10.25019.0"
MSVC_VER = 1911
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 15 2017 Win64"
CMAKE_BUILDDIR = vc16x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 15 2017"
CMAKE_BUILDDIR = vc16
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.15.26726.0"
MSVC_VER = 1911
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 15 2017 Win64"
CMAKE_BUILDDIR = vc16x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 15 2017"
CMAKE_BUILDDIR = vc16
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.16.27034.0"
MSVC_VER = 1911
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 15 2017 Win64"
CMAKE_BUILDDIR = vc16x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 15 2017"
CMAKE_BUILDDIR = vc16
!ENDIF
!ELSEIF "$(_NMAKE_VER)" == "14.22.27905.0"
MSVC_VER = 1922
!IFDEF WIN64
CMAKE_GENERATOR = "Visual Studio 16 2019" -A x64
CMAKE_BUILDDIR = vc17x64
!ELSE
CMAKE_GENERATOR = "Visual Studio 16 2019" -A x86
CMAKE_BUILDDIR = vc17
!ENDIF
!ELSE
!ERROR This compiler version $(_NMAKE_VER) is not supported or must be enumerated in the makefile
!ENDIF
!ENDIF

!IFNDEF SERVER_URL
SERVER_URL=http://www.gisinternals.com/sdk
!ENDIF

# common feature configuration flags
#DEBUG = YES
#NO_CLEAN = YES
#NO_BUILD = YES
#NO_COPY = YES
#LEAK_CHECK = YES

# buildbot related settings
!IF "$(OUTPUT_DIR)" == "E:\buildbot\osgeo\slaves\szekerest-msvc80-gdal\release-1400"
DEBUG = YES
!ENDIF

!IF "$(OUTPUT_DIR)" == "E:\buildbot\osgeo\slaves\szekerest-msvc90-gdal\release-1500"
DEBUG = YES
!ENDIF

!IFNDEF OSGEO4W_DIR
!IFDEF WIN64
OSGEO4W_DIR = E:\OSGeo4W64
OSGEO4W_ARCH = x86_64
!ELSE
OSGEO4W_DIR = E:\OSGeo4W
OSGEO4W_ARCH = x86
!ENDIF
!ENDIF

!IFNDEF RSYNC_DIR
RSYNC_DIR = E:\builds\rsync
!ENDIF

!IFDEF DEBUG
MS_PROJECT_DIR=RelWithDebInfo
!IFDEF WIN64
MS_PROJECT_CONFIG = "RelWithDebInfo|x64"
!ELSE
MS_PROJECT_CONFIG = "RelWithDebInfo|Win32"
!ENDIF
!ELSE
MS_PROJECT_DIR=Release
!IFDEF WIN64
MS_PROJECT_CONFIG = "Release|x64"
!ELSE
MS_PROJECT_CONFIG = "Release|Win32"
!ENDIF
!ENDIF

!IFNDEF MS_REVISION
MS_REVISION=HEAD
!ENDIF

!IFNDEF GDAL_REVISION
GDAL_REVISION=HEAD
!ENDIF

!IFNDEF MAPMANAGER_REVISION
MAPMANAGER_REVISION=HEAD
!ENDIF

!IFNDEF SPECIFY_FEATURES
GDAL_GEOS = YES
GDAL_POSTGIS = YES
GDAL_ECW = YES
!IF $(MSVC_VER) >= 1911
#VC2017 is not yet supported by the ECW5 SDK
GDAL_ECW3 = YES
!ENDIF
GDAL_CURL = YES
#GDAL_SQLITE = YES
GDAL_SPATIALITE = YES
GDAL_CSHARP = YES
GDAL_PYTHON = YES
GDAL_JAVA = YES
GDAL_XERCES = YES
GDAL_EXPAT = YES
GDAL_MYSQL = YES
GDAL_MSSQL = YES
GDAL_OPENJPEG = YES
GDAL_AMIGOCLOUD = YES
GDAL_GNM = YES
GDAL_KMLSUPEROVERLAY = YES
#GDAL_DEBUG_VSIMALLOC = YES
GDAL_PDF = YES
#GDAL_TIFF = YES
#GDAL_SDE = YES
GDAL_LIBKML = YES

MS_PROJ = YES
MS_FREETYPE = YES
MS_GEOS = YES
MS_PNG = YES
MS_GDAL = YES
MS_WXS = YES
MS_AGG = YES
MS_POSTGIS = YES
MS_ICONV = YES
#MS_MING = YES
MS_SOS = YES
MS_CSHARP = YES
MS_JAVA = YES
MS_PYTHON = YES
MS_FCGI = YES
MS_PDF = YES
MS_FRIBIDI = YES
#MS_OCI = YES
MS_CAIRO = YES
MS_GIFLIB = YES
#MS_OGL = YES
#MS_SDE = YES
#MS_PHP = YES
!ENDIF

# directory layout

!IFNDEF BASE_DIR
BASE_DIR = $(MAKEDIR)
!ENDIF

!IFNDEF INSTALL_DIR
INSTALL_DIR = C:\Inetpub\wwwroot\sdk
!ENDIF

!IFNDEF MAPMANAGER_DIR
MAPMANAGER_DIR = MapManager-$(MS_VERSION)
!ENDIF

!IFNDEF CMAKE_DIR
CMAKE_DIR = E:\builds\cmake-3.9.1-win32-x86
#CMAKE_DIR = E:\builds\cmake-3.14.5-win32-x86
!ENDIF

!IF $(MSVC_VER) >= 1400
#MS_PHP = YES
!ENDIF

!IFNDEF JAVA_HOME
!IFDEF WIN64
JAVA_HOME = C:\Java\jdk1.8.0_191x64
!ELSE
JAVA_HOME = C:\Java\jdk1.8.0_191
!ENDIF
!ENDIF

!IF EXIST("$(JAVA_HOME)")
MS_CMAKE_JAVA=-DWITH_JAVA=1 
!ENDIF

!IFNDEF ANT_HOME
ANT_HOME = C:\Programs\apache-ant-1.7.0
!ENDIF

!IFNDEF WIX_DIR
WIX_DIR = wix
!ENDIF

!IFNDEF GDAL_DIR
GDAL_DIR = gdal
!ENDIF

!IFNDEF PHPSDK_DIR
PHPSDK_DIR = E:\php-sdk
!ENDIF

!IFNDEF GDAL_PATH
GDAL_PATH = $(BASE_DIR)\$(GDAL_DIR)
!ENDIF

!IF EXIST("detect_mrsid.opt")
!INCLUDE detect_mrsid.opt
!ENDIF

!IFNDEF GDALTEST_DIR
GDALTEST_DIR = gdal-autotest
!ENDIF

!IFNDEF GDALTEST_SCRIPT
GDALTEST_SCRIPT =run_all.py
!ENDIF

!IFNDEF GDALTEST_DIR_SRC
GDALTEST_DIR_SRC = E:\sdk\vc7\gdal\autotest
!ENDIF

!IFNDEF MSAUTOTEST_DIR
MSAUTOTEST_DIR = msautotest-5-0
!ENDIF

!IFNDEF SQLITE_DIR
SQLITE_DIR = sqlite-3.24.0
!ENDIF

!IFNDEF GIFLIB_DIR
GIFLIB_DIR = giflib-4.1.6
!ENDIF

!IFNDEF SPATIALITE_DIR
SPATIALITE_DIR = libspatialite-4.3.0a
!ENDIF

!IFNDEF FREEXL_DIR
FREEXL_DIR = freexl-1.0.2
!ENDIF

!IFNDEF SQLNCLI_VERSION
SQLNCLI_VERSION = 11
!ENDIF

!IFNDEF SQLNCLI_DIR
SQLNCLI_DIR = C:\Program Files (x86)\Microsoft SQL Server\$(SQLNCLI_VERSION)0\SDK
!ENDIF

!IFNDEF MSODBCSQL_VERSION
MSODBCSQL_VERSION = 17
!ENDIF

!IFNDEF MSODBCSQL_DIR
MSODBCSQL_DIR = C:\Program Files\Microsoft SQL Server\Client SDK\ODBC\$(MSODBCSQL_VERSION)0\SDK
!ENDIF

!IFNDEF FILEGDB_API_DIR
!IF $(MSVC_VER) == 1911
FILEGDB_API_DIR = FileGDB_API_1_5_1-VS2017
!IFDEF WIN64
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin64
!ELSE
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1900
FILEGDB_API_DIR = FileGDB_API_1_5_1-VS2015
!IFDEF WIN64
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin64
!ELSE
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1800
FILEGDB_API_DIR = FileGDB_API_VS2013_1_4
!IFDEF WIN64
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin64
!ELSE
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1700
FILEGDB_API_DIR = FileGDB_API_VS2012_1_4
!IFDEF WIN64
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin64
!ELSE
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
FILEGDB_API_DIR = FileGDB_API_VS2010_1_4
!IFDEF WIN64
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin64
!ELSE
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
FILEGDB_API_DIR = FileGDB_API_VS2008_1_3
!IFDEF WIN64
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin64
!ELSE
FILEGDB_BINPATH = $(BASE_DIR)\$(FILEGDB_API_DIR)\bin
!ENDIF
!ENDIF
!ENDIF

!IFNDEF PHP_VERSION
PHP_VERSION = php-5.3.7
!ENDIF

!IFNDEF PHP_DIR
!IFDEF WIN64
PHP_DIR = $(PHPSDK_DIR)\php53dev\vc9\x64\$(PHP_VERSION)
PHP_BINDIR = $(PHP_DIR)\x64\Release_TS
!ELSE
PHP_DIR = $(PHPSDK_DIR)\php53dev\vc9\x86\$(PHP_VERSION)
PHP_BINDIR = $(PHP_DIR)\Release_TS
!ENDIF
!ENDIF

!IFNDEF EXPAT_DIR
!IF $(MSVC_VER) == 1600
EXPAT_DIR = expat-2.0.1-VS10
!ELSEIF $(MSVC_VER) == 1500
EXPAT_DIR = expat-2.0.1-VS9
!ELSEIF $(MSVC_VER) == 1400
EXPAT_DIR = expat-2.0.1-VS8
!ELSE
EXPAT_DIR = expat-2.0.1
!ENDIF
!ENDIF

!IFNDEF EXPAT2_DIR
EXPAT2_DIR = expat-2.1.0
!ENDIF

!IFNDEF LIBKML_DIR
!IF $(MSVC_VER) == 1911
LIBKML_DIR=libkml-svn-VC15
!ELSEIF $(MSVC_VER) == 1900
LIBKML_DIR=libkml-svn-VC14
!ELSEIF $(MSVC_VER) == 1800
LIBKML_DIR=libkml-svn-VC12
!ELSEIF $(MSVC_VER) == 1700
LIBKML_DIR=libkml-svn-VC11
!ELSEIF $(MSVC_VER) == 1600
LIBKML_DIR=libkml-svn-VC10
!ELSEIF $(MSVC_VER) == 1500
LIBKML_DIR=libkml-svn-VC9
!ELSEIF $(MSVC_VER) == 1400
LIBKML_DIR=libkml-svn
!ENDIF
!ENDIF

!IFNDEF REGEX_DIR
REGEX_DIR = regex-0.12
!ENDIF

!IFNDEF REGEX_PATH
REGEX_PATH=$(BASE_DIR)\$(REGEX_DIR)
!ENDIF

!IFNDEF PROJ_DIR
PROJ_DIR = proj-6.1
!ENDIF

!IFNDEF PROJ_DATUMGRID_DIR
PROJ_DATUMGRID_DIR = proj-datumgrid-1.8
!ENDIF

!IFNDEF GEOS_DIR
GEOS_DIR = geos-3.4
!ENDIF

!IFNDEF ZLIB_DIR
ZLIB_DIR = zlib-1.2.3
!ENDIF

!IFNDEF MINIZIP_DIR
MINIZIP_DIR = unzip11
!ENDIF

!IFNDEF LIBPNG_DIR
LIBPNG_DIR = libpng-1.2.35
!ENDIF

!IFNDEF JPEG_DIR
JPEG_DIR = jpeg-6b
!ENDIF

!IFNDEF GD_DIR
GD_DIR = gd-2.0.35
!ENDIF

!IFNDEF MS_DIR
MS_DIR = mapserver-7-0
!ENDIF

!IFNDEF MS_PATH
MS_PATH = $(BASE_DIR)\$(MS_DIR)
!ENDIF

!IFNDEF MAPCACHE_DIR
MAPCACHE_DIR = mapcache-dev
!ENDIF

!IFNDEF PCRE_DIR
PCRE_DIR = pcre-8.35
!ENDIF

!IFNDEF APR_DIR
APR_DIR = apr-1.5.1
!ENDIF

!IFNDEF APR_UTIL_DIR
APR_UTIL_DIR = apr-util-1.5.3
!ENDIF

!IFNDEF APR_ICONV_DIR
APR_ICONV_DIR = apr-iconv-1.2.1
!ENDIF

!IFNDEF MS_CMAKE_BUILD
!IF EXIST ($(MS_PATH)\CMakeLists.txt)
MS_CMAKE_BUILD = 1 
!ENDIF
!ENDIF

!IFNDEF SWIG_DIR
SWIG_DIR = SWIG-1.3.39
!ENDIF

!IFNDEF CURL_DIR
CURL_DIR = curl-7.37.1
!ENDIF

!IFNDEF OPENSSL_DIR
OPENSSL_DIR = openssl-1.1.1c
!ENDIF

!IFNDEF PDF_DIR
PDF_DIR = PDFlib-Lite-7.0.4p1
!ENDIF

!IFNDEF FRIBIDI_DIR
FRIBIDI_DIR = fribidi-0.19.2
!ENDIF

!IFNDEF MING_DIR
MING_DIR= ming-0.2a
!ENDIF

!IFNDEF JBIG_DIR
JBIG_DIR= jbigkit-2.0
!ENDIF

!IFNDEF LIBTIFF_DIR
LIBTIFF_DIR=tiff-4.0.9
!ENDIF

!IFNDEF LIBGEOTIFF_DIR
LIBGEOTIFF_DIR=libgeotiff-1.4.2
!ENDIF

!IFNDEF LASZIP_DIR
LASZIP_DIR=laszip-src-3.2.2
!ENDIF

!IFNDEF FCGI_DIR
FCGI_DIR= fcgi-2.4.0
!ENDIF

!IFNDEF ICONV_DIR
ICONV_DIR = libiconv-1.11.1
!ENDIF

!IFNDEF LIBXML_DIR
LIBXML_DIR = libxml2-2.9.8
!ENDIF

!IFNDEF GETTEXT_DIR
GETTEXT_DIR = gettext-0.13
!ENDIF

!IFNDEF PGSQL_DIR
PGSQL_DIR = postgresql-9.6.8
!ENDIF

!IFNDEF POPPLER_DIR
POPPLER_DIR = poppler-0.30.0
!ENDIF

!IFNDEF VLD_DIR
VLD_DIR = vld-source\vld
!ENDIF

!IFNDEF PYTHON_DIR
!IFDEF WIN64
PYTHON_DIR = Python27-AMD64
!ELSE
PYTHON_DIR = Python27
!ENDIF
!IF $(MSVC_VER) == 1310
PYTHON_DIR = Python27
!ENDIF
!ENDIF

!IF "$(PYTHON_DIR)" == "Python25"
PYTHON_OUTDIR = python\build\lib.win32-2.5
PYTHON_BDIST_OPTS = --formats=wininst
!ELSEIF "$(PYTHON_DIR)" == "Python26"
PYTHON_OUTDIR = python\build\lib.win32-2.6
PYTHON_BDIST_OPTS = --formats=wininst
!ELSEIF "$(PYTHON_DIR)" == "Python26-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-2.6
PYTHON_BDIST_OPTS = --formats=wininst
!ELSEIF "$(PYTHON_DIR)" == "Python27"
PYTHON_OUTDIR = python\build\lib.win32-2.7
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python27-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-2.7
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python31"
PYTHON_OUTDIR = python\build\lib.win32-3.1
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python31-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-3.1
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python32"
PYTHON_OUTDIR = python\build\lib.win32-3.2
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python32-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-3.2
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python33"
PYTHON_OUTDIR = python\build\lib.win32-3.3
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python33-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-3.3
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python34"
PYTHON_OUTDIR = python\build\lib.win32-3.4
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python34-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-3.4
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python35"
PYTHON_OUTDIR = python\build\lib.win32-3.5
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python35-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-3.5
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python36"
PYTHON_OUTDIR = python\build\lib.win32-3.6
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python36-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-3.6
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python37"
PYTHON_OUTDIR = python\build\lib.win32-3.7
PYTHON_BDIST_OPTS = --formats=msi
!ELSEIF "$(PYTHON_DIR)" == "Python37-AMD64"
PYTHON_OUTDIR = python\build\lib.win-amd64-3.7
PYTHON_BDIST_OPTS = --formats=msi
!ENDIF

!IFNDEF PYDIR
PYDIR = $(BASE_DIR)\$(PYTHON_DIR)
!ENDIF

!IF EXIST ($(BASE_DIR)\$(PYTHON_DIR))
MS_CMAKE_PYTHON=-DWITH_PYTHON=1 "-DPYTHON_LIBRARY=$(BASE_DIR:\=/)/$(PYTHON_DIR:\=/)/libs/python27.lib" "-DPYTHON_INCLUDE_DIR=$(BASE_DIR:\=/)/$(PYTHON_DIR:\=/)/include" "-DPYTHON_EXECUTABLE=$(BASE_DIR:\=/)/$(PYTHON_DIR:\=/)/python.exe"
!ENDIF

!IFNDEF AGG_DIR
AGG_DIR = agg-2.4
!ENDIF

!IFNDEF CAIRO_DIR
CAIRO_DIR = cairo-1.14.10
!ENDIF

!IFNDEF LIBSVG_DIR
LIBSVG_DIR = libsvg-0.1.4
!ENDIF

!IFNDEF LIBSVG_CAIRO_DIR
LIBSVG_CAIRO_DIR = libsvg-cairo-0.1.6
!ENDIF

!IFNDEF PIXMAN_DIR
PIXMAN_DIR = pixman-0.34.0
!ENDIF

!IFNDEF FONTCONFIG_DIR
FONTCONFIG_DIR = fontconfig-2.12.4
!ENDIF

!IFNDEF FTGL_DIR
FTGL_DIR = ftgl-2.1.3~rc5
!ENDIF

!IFNDEF XERCES_DIR
XERCES_DIR=xerces-c-3.2.1
!ENDIF

!IFNDEF KEA_DIR
KEA_DIR=kealib-1.4.4
!ENDIF

!IFNDEF HDF5_DIR
#HDF5_DIR=hdf5-1.8.2
HDF5_DIR=hdf5-1.8.14
SZIP_DIR=szip-2.1
!ENDIF

!IFNDEF HDF4_DIR
HDF4_DIR=hdf-4.2.9
SZIP_DIR=szip-2.1
!ENDIF

!IFNDEF NETCDF_DIR
#NETCDF_DIR=netcdf-3.6.1
NETCDF_DIR=netcdf-4.3.2
!ENDIF

!IFNDEF FITS_DIR
FITS_DIR=fits-3.14
!ENDIF

!IF $(MSVC_VER) == 1911
!IFDEF WIN64
!IF EXIST("$(VCINSTALLDIR)Tools\MSVC\14.11.25503\lib\x64\setargv.obj")
SETARGV = "$(VCINSTALLDIR)Tools\MSVC\14.11.25503\lib\x64\setargv.obj"
!ENDIF
!ELSE
!IF EXIST("$(VCINSTALLDIR)Tools\MSVC\14.11.25503\lib\x86\setargv.obj")
SETARGV = "$(VCINSTALLDIR)Tools\MSVC\14.11.25503\lib\x86\setargv.obj"
!ENDIF
!ENDIF
!ELSE
!IF $(MSVC_VER) == 1310
VCDIR=$(VCINSTALLDIR)\vc7
!ELSE
VCDIR=$(VCINSTALLDIR)
!ENDIF

!IFDEF WIN64
SETARGV = "$(VCDIR)\lib\amd64\setargv.obj"
!ELSE
SETARGV = "$(VCDIR)\lib\setargv.obj"
!ENDIF
!ENDIF

!IFNDEF HARFBUZZ_DIR
HARFBUZZ_DIR=harfbuzz-1.8.0
!ENDIF

!IFNDEF FT_DIR
FT_DIR=freetype-2.9.1
FT_LIB=freetype.lib
!IF $(MSVC_VER) == 1400
FT_SRCPATH=$(FT_DIR)\builds\win32\vc2005
!IFDEF WIN64
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2005\x64
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC8x64
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc8
!ELSE
FT_LIBPATH=$(FT_DIR)\objs\win32\vc2005
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC8
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc8
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
FT_SRCPATH=$(FT_DIR)\builds\win32\vc2008
!IFDEF WIN64
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2008\x64

ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC9x64
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc9
!ELSE
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2008
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC9
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc9
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
FT_SRCPATH=$(FT_DIR)\builds\win32\VC2010
!IFDEF WIN64
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2010\x64
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC10x64
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc10
!ELSE
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2010
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC10
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc10
!ENDIF
!ELSEIF $(MSVC_VER) == 1700
FT_SRCPATH=$(FT_DIR)\builds\win32\VC2012
!IFDEF WIN64
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2012\x64
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC11x64
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc11
!ELSE
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2012
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC11
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc11
!ENDIF
!ELSEIF $(MSVC_VER) == 1800
FT_SRCPATH=$(FT_DIR)\builds\win32\VC2013
!IFDEF WIN64
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2013\x64
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC12x64
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc12
!ELSE
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2013
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC12
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc12
!ENDIF
!ELSEIF $(MSVC_VER) == 1900
FT_SRCPATH=$(FT_DIR)\builds\win32\VC2015
!IFDEF WIN64
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2015\x64
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC14x64
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc14
!ELSE
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2015
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC14
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc14
!ENDIF
!ELSEIF $(MSVC_VER) == 1911
FT_SRCPATH=$(FT_DIR)\builds\win32\VC2017
!IFDEF WIN64
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2017\x64
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC15x64
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc15
!ELSE
FT_LIBPATH=$(FT_DIR)\objs\Win32\vc2017
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC15
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc15
!ENDIF
!ELSE
FT_LIBPATH=$(FT_DIR)\objs
FT_SRCPATH=$(FT_DIR)\builds\win32\visualc
ODBC_DIR="C:\Program Files (x86)\Microsoft Visual Studio 8\VC\PlatformSDK"
FTGL_LIBPATH=$(FTGL_DIR)\msvc\build-VC7
FTGL_SRCPATH=$(FTGL_DIR)\msvc\vc71
!ENDIF
!ENDIF

!IFNDEF MYSQL_DIR
!IF $(MSVC_VER) == 1400
!IFDEF WIN64
MYSQL_DIR=mysql-6.0.2\mysql-6.0.2-VC8x64
!ELSE
MYSQL_DIR=mysql-6.0.2\mysql-6.0.2-VC8
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
MYSQL_DIR=mysql-6.1.3
!ELSEIF $(MSVC_VER) >= 1900
MYSQL_DIR=mysql-6.1.11
!ELSE
MYSQL_DIR=mysql-6.1.5
!ENDIF
!ENDIF

!IFNDEF OPENJPEG_DIR
!IF $(MSVC_VER) == 1400
!IFDEF WIN64
OPENJPEG_DIR=openjpegv2\vc8x64
!ELSE
OPENJPEG_DIR=openjpegv2\vc8
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
OPENJPEG_DIR=openjpegv2\vc9x64
!ELSE
OPENJPEG_DIR=openjpegv2\vc9
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
!IFDEF WIN64
OPENJPEG_DIR=openjpegv2\vc10x64
!ELSE
OPENJPEG_DIR=openjpegv2\vc10
!ENDIF
!ELSE
OPENJPEG_DIR=openjpegv2\vc7
!ENDIF
!ENDIF

!IFNDEF OPENJPEG2_DIR
OPENJPEG2_DIR=openjpeg-2.3.0
!ENDIF

!IFNDEF SDE_DIR
SDE_DIR = ArcSDE92sp6
SDE_VERSION=92
!ENDIF

!IFNDEF OCI_DIR
!IF $(MSVC_VER) == 1900
!IFDEF WIN64
OCI_DIR = Oracle\instantclient_12_2-x64
!ELSE
OCI_DIR = Oracle\instantclient_12_2
!ENDIF
OCI_VERSION = 12
!ELSEIF $(MSVC_VER) >= 1600
!IFDEF WIN64
OCI_DIR = Oracle\instantclient_12_1-x64
!ELSE
OCI_DIR = Oracle\instantclient_12_1
!ENDIF
OCI_VERSION = 12
!ELSE
!IFDEF WIN64
OCI_DIR = Oracle\instantclient_11_1_7_0-x64
!ELSE
OCI_DIR = Oracle\instantclient_11_1_7_0
!ENDIF
OCI_VERSION = 11
!ENDIF
!ENDIF

!IF EXIST($(BASE_DIR)\$(OCI_DIR))
MS_CMAKE_ORACLE=-DWITH_ORACLE_PLUGIN=1 "-DORACLE_INCLUDE_DIR=$(BASE_DIR:\=/)/$(OCI_DIR)/sdk/include"
!ENDIF

# dependent constants

!IFDEF WIN64
COMPILER_VER=$(MSVC_VER)-x64
!ELSE
COMPILER_VER=$(MSVC_VER)
!ENDIF

!IFNDEF OUTPUT_DIR
OUTPUT_DIR = $(BASE_DIR)\release-$(COMPILER_VER)
!ENDIF

!IFNDEF ECW_DIR
!IF $(MSVC_VER) == 1911
!IFNDEF GDAL_ECW3
#ECW_DIR = ECWSDK51
#ECWDIR = $(BASE_DIR)\$(ECW_DIR)
#ECWSDK_VERSION=51
!IFDEF WIN64
#ECWLIB = $(ECWDIR)\lib\vc120\x64\NCSEcw.lib
!ELSE
#ECWLIB = $(ECWDIR)\lib\vc120\win32\NCSEcw.lib
!ENDIF
#ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(ECWDIR)\include -I$(ECWDIR)\include\NCSECW\api -I$(ECWDIR)\include\NCSECW\jp2 -I$(ECWDIR)\include\NCSECW\ecw
!ELSE
ECW_DIR = libecwj2-3.3-VC15
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ENDIF
!ELSEIF $(MSVC_VER) == 1900
!IFNDEF GDAL_ECW3
ECW_DIR = ECWSDK53
ECWDIR = $(BASE_DIR)\$(ECW_DIR)
ECWSDK_VERSION=53
!IFDEF WIN64
ECWLIB = $(ECWDIR)\lib\vc140\x64\NCSEcw.lib
!ELSE
ECWLIB = $(ECWDIR)\lib\vc140\win32\NCSEcw.lib
!ENDIF
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(ECWDIR)\include -I$(ECWDIR)\include\NCSECW\api -I$(ECWDIR)\include\NCSECW\jp2 -I$(ECWDIR)\include\NCSECW\ecw
!ELSE
ECW_DIR = libecwj2-3.3-VC14
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ENDIF
!ELSEIF $(MSVC_VER) == 1800
!IFNDEF GDAL_ECW3
ECW_DIR = ECWSDK53
ECWDIR = $(BASE_DIR)\$(ECW_DIR)
ECWSDK_VERSION=53
!IFDEF WIN64
ECWLIB = $(ECWDIR)\lib\vc120\x64\NCSEcw.lib
!ELSE
ECWLIB = $(ECWDIR)\lib\vc120\win32\NCSEcw.lib
!ENDIF
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(ECWDIR)\include -I$(ECWDIR)\include\NCSECW\api -I$(ECWDIR)\include\NCSECW\jp2 -I$(ECWDIR)\include\NCSECW\ecw
!ELSE
ECW_DIR = libecwj2-3.3-VC12
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ENDIF
!ELSEIF $(MSVC_VER) == 1700
!IFNDEF GDAL_ECW3
ECW_DIR = ECWSDK53
ECWDIR = $(BASE_DIR)\$(ECW_DIR)
ECWSDK_VERSION=53
!IFDEF WIN64
ECWLIB = $(ECWDIR)\lib\vc110\x64\NCSEcw.lib
!ELSE
ECWLIB = $(ECWDIR)\lib\vc110\win32\NCSEcw.lib
!ENDIF
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(ECWDIR)\include -I$(ECWDIR)\include\NCSECW\api -I$(ECWDIR)\include\NCSECW\jp2 -I$(ECWDIR)\include\NCSECW\ecw
!ELSE
ECW_DIR = libecwj2-3.3-VC11
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
!IFNDEF GDAL_ECW3
ECW_DIR = ECWSDK51
ECWDIR = $(BASE_DIR)\$(ECW_DIR)
ECWSDK_VERSION=51
!IFDEF WIN64
ECWLIB = $(ECWDIR)\lib\vc100\x64\NCSEcw.lib
!ELSE
ECWLIB = $(ECWDIR)\lib\vc100\win32\NCSEcw.lib
!ENDIF
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(ECWDIR)\include -I$(ECWDIR)\include\NCSECW\api -I$(ECWDIR)\include\NCSECW\jp2 -I$(ECWDIR)\include\NCSECW\ecw
!ELSE
ECW_DIR = libecwj2-3.3-VC10
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFNDEF GDAL_ECW3
ECW_DIR = ECWSDK51
ECWDIR = $(BASE_DIR)\$(ECW_DIR)
ECWSDK_VERSION=51
!IFDEF WIN64
ECWLIB = $(ECWDIR)\lib\vc90\x64\NCSEcw.lib
!ELSE
ECWLIB = $(ECWDIR)\lib\vc90\win32\NCSEcw.lib
!ENDIF
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(ECWDIR)\include -I$(ECWDIR)\include\NCSECW\api -I$(ECWDIR)\include\NCSECW\jp2 -I$(ECWDIR)\include\NCSECW\ecw
!ELSE
ECW_DIR = libecwj2-3.3-VC9
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
ECW_DIR = libecwj2-3.3-VC8
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ELSE
ECW_DIR = libecwj2-3.3
ECWSDK_VERSION=33
ECWLIB = $(OUTPUT_DIR)\lib\libecwj2.lib
ECWDIR = $(OUTPUT_DIR)
ECWFLAGS= /DECWSDK_VERSION=$(ECWSDK_VERSION) -I$(OUTPUT_DIR)\include /D_MBCS /D_UNICODE /DUNICODE /D_WINDOWS /DLIBECWJ2 /DWIN32 /D_WINDLL -DNO_X86_MMI
!ENDIF
!ENDIF

!IFNDEF PKG_VERSION
PKG_VERSION = $(GDAL_DIR)-$(MS_DIR)
!ENDIF

!IF ([type $(GDAL_PATH)\VERSION|find "1.10." > NUL] == 0)
GDAL_VER = 1.10
USE_OPENJP2 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "1.11." > NUL] == 0)
GDAL_VER = 1.11
USE_OPENJP2 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "1.12." > NUL] == 0)
GDAL_VER = 1.12
USE_OPENJP2 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "2.0." > NUL] == 0)
GDAL_VER = 2.0
USE_OPENJP2 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "2.1." > NUL] == 0)
GDAL_VER = 2.1
USE_OPENJP2 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "2.2." > NUL] == 0)
GDAL_VER = 2.2
USE_OPENJP2 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "2.3." > NUL] == 0)
GDAL_VER = 2.3
USE_OPENJP2_NEW_METHOD = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "2.4." > NUL] == 0)
GDAL_VER = 2.4
USE_OPENJP2_NEW_METHOD = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "2.5." > NUL] == 0)
GDAL_VER = 2.5
USE_OPENJP2_NEW_METHOD = 1
USE_PROJ6 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "2.6." > NUL] == 0)
GDAL_VER = 2.6
USE_OPENJP2_NEW_METHOD = 1
USE_PROJ6 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "3.0." > NUL] == 0)
GDAL_VER = 3.0
USE_OPENJP2_NEW_METHOD = 1
USE_PROJ6 = 1
!ELSE IF ([type $(GDAL_PATH)\VERSION|find "3.1." > NUL] == 0)
GDAL_VER = 3.1
USE_OPENJP2_NEW_METHOD = 1
USE_PROJ6 = 1
!ENDIF

!IFNDEF GDAL_VERSIONTAG
!IF EXIST ($(OUTPUT_DIR)\bin\gdal17.dll)
GDAL_VERSIONTAG = 17
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal18.dll)
GDAL_VERSIONTAG = 18
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal19.dll)
GDAL_VERSIONTAG = 19
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal20.dll)
GDAL_VERSIONTAG = 20
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal18dev.dll)
GDAL_VERSIONTAG = 18dev
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal19dev.dll)
GDAL_VERSIONTAG = 19dev
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal110dev.dll)
GDAL_VERSIONTAG = 110dev
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal111.dll)
GDAL_VERSIONTAG = 111
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal111dev.dll)
GDAL_VERSIONTAG = 111dev
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal110.dll)
GDAL_VERSIONTAG = 110
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal20dev.dll)
GDAL_VERSIONTAG = 20dev
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal200.dll)
GDAL_VERSIONTAG = 200
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal201.dll)
GDAL_VERSIONTAG = 201
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
GDAL_MSSQL_DEF = -dogr_MSSQLSpatial=ogr_MSSQLSpatial
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal202.dll)
GDAL_VERSIONTAG = 202
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
GDAL_MSSQL_DEF = -dogr_MSSQLSpatial=ogr_MSSQLSpatial
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal203.dll)
GDAL_VERSIONTAG = 203
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
GDAL_MSSQL_DEF = -dogr_MSSQLSpatial=ogr_MSSQLSpatial
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal204.dll)
GDAL_VERSIONTAG = 204
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
GDAL_MSSQL_DEF = -dogr_MSSQLSpatial=ogr_MSSQLSpatial
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal205.dll)
GDAL_VERSIONTAG = 205
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
GDAL_MSSQL_DEF = -dogr_MSSQLSpatial=ogr_MSSQLSpatial
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal206.dll)
GDAL_VERSIONTAG = 206
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
GDAL_MSSQL_DEF = -dogr_MSSQLSpatial=ogr_MSSQLSpatial
!ELSE IF EXIST ($(OUTPUT_DIR)\bin\gdal300.dll)
GDAL_VERSIONTAG = 300
GDAL_KEA_DEF = -dgdal_KEA=gdal_KEA.dll
GDAL_MSSQL_DEF = -dogr_MSSQLSpatial=ogr_MSSQLSpatial
!ENDIF
!ENDIF

!IFDEF MRSID_DLL
MRSID_SETUP_FLAGS = $(MRSID_SETUP_FLAGS) "-dlti_dsdk=$(MRSID_DLL)" "-dlti_dsdk_name=$(MRSID_DLL_NAME)"
!ENDIF

!IF EXIST ($(MRSID_RASTER_DIR)\lib\tbb.dll)
MRSID_SETUP_FLAGS = $(MRSID_SETUP_FLAGS) "-dmrsid_tbb=$(MRSID_RASTER_DIR)\lib\tbb.dll"
!ENDIF

!IFDEF LIDAR_DLL
MRSID_SETUP_FLAGS = $(MRSID_SETUP_FLAGS) "-dlti_lidar_dsdk=$(LIDAR_DLL)" "-dlti_lidar_dsdk_name=$(LIDAR_DLL_NAME)"
!ENDIF

EXTRAFLAGS =

default: update platform gdal gdalplugins gdal-csharp

gdalpluginlibs: gdal-sde gdal-oci gdal-mrsid gdal-mrsid-lidar gdal-hdf4 gdal-hdf5 gdal-netcdf gdal-fits gdal-ecw gdal-ecw3 ogr-pg gdal-filegdb gdal-kea gdal-amigocloud ogr-mssql ogr-mssql-msodbc
	
gdalplugins: gdalpluginlibs gdalversion

msplugins:
!IFNDEF NO_BUILD
!IFDEF WIN64
    nmake ms-sql2008 WIN64=$(WIN64)
    nmake ms-sde WIN64=$(WIN64)
    nmake ms-sde WIN64=$(WIN64) SDE_DIR=ArcSDE91 SDE_VERSION=91
    nmake ms-oci WIN64=$(WIN64)
!ELSE
    nmake ms-sql2008
    nmake ms-sde
    nmake ms-sde SDE_DIR=ArcSDE91 SDE_VERSION=91
    nmake ms-oci
!ENDIF
!ENDIF

gdalbindings: gdal-csharp gdal-python

graph: zlib libpng jpeg gd

platform: zlib libpng jpeg geos proj iconv freexl spatialite openssl curl freetype pgsql gd libxml minizip fits agg ming fastcgi expat2 pdf fribidi libjbig libtiff libecw mrsidlib fontconfig pixman cairo ftgl xerces szip hdf5lib netcdf kea hdf4lib mysql libopenjpeg poppler giflib visual-leak-detector msvcr libsvg libsvg-cairo harfbuzz

rebuild: remove-output gdal-optfile gdal gdalpluginlibs gdal-csharp gdal-java ms ms-csharp msplugins gdalversion gdal-python package package-dev

full-rebuild: clean platform

rebuild-svn: remove-output gdal-optfile gdal gdalpluginlibs gdal-csharp gdal-java ms ms-csharp msplugins gdalversion gdal-python package

rebuild-gdal: remove-output gdal-optfile gdal

remove-output:
    del $(OUTPUT_DIR)\bin\gdal*.dll
    del $(OUTPUT_DIR)\bin\libmap.dll
    if exist $(OUTPUT_DIR)\install del $(OUTPUT_DIR)\install\*.exe $(OUTPUT_DIR)\install\*.msi $(OUTPUT_DIR)\install\*.wxs $(OUTPUT_DIR)\install\*.wixobj $(OUTPUT_DIR)\install\*.wixpdb
    if exist $(OUTPUT_DIR)\bin\gdal rd /Q /S $(OUTPUT_DIR)\bin\gdal
    if exist $(OUTPUT_DIR)\bin\gdal-data rd /Q /S $(OUTPUT_DIR)\bin\gdal-data
    if exist $(OUTPUT_DIR)\bin\ms rd /Q /S $(OUTPUT_DIR)\bin\ms
clean:
	if exist $(OUTPUT_DIR) rd /Q /S $(OUTPUT_DIR)
	mkdir $(OUTPUT_DIR)
	mkdir $(OUTPUT_DIR)\bin
	mkdir $(OUTPUT_DIR)\lib
	mkdir $(OUTPUT_DIR)\include
	mkdir $(OUTPUT_DIR)\doc
	
setargv:
    echo "$(SETARGV)"

update: get_ca_bundle
    set TERM=msys
	set "PATH=E:\Git\bin;%PATH%"
	cd $(MS_DIR)
	git reset --hard HEAD
	git pull origin
	git reset --hard $(MS_REVISION)
	git log --pretty=format:%H -n 1 > $(OUTPUT_DIR)\doc\ms_revision.txt
	type $(OUTPUT_DIR)\doc\ms_revision.txt
	cd $(BASE_DIR)
	cd $(GDAL_DIR)
	git reset --hard HEAD
	git pull origin
	git reset --hard $(GDAL_REVISION)
	git log --pretty=format:%H -n 1 > $(OUTPUT_DIR)\doc\gdal_revision.txt
	type $(OUTPUT_DIR)\doc\gdal_revision.txt
	cd $(BASE_DIR)
	
update2:
    set TERM=msys
	set PATH=E:\Git\bin;%PATH%
    cd E:\sdk\vc7\mapserver-git
    git pull origin
	git log --pretty=format:%H -n 1
	cd $(BASE_DIR)

update-gdal:
	cd $(GDAL_PATH)
	svn update --revision $(GDAL_REVISION) --non-interactive
	cd $(BASE_DIR)
	
update-gdal-autotest:
	cd $(BASE_DIR)\$(GDALTEST_DIR)
	svn update --revision $(GDAL_REVISION) --non-interactive
	cd $(BASE_DIR)
	
update-ms-autotest:
    set TERM=msys
	set PATH=E:\Git\bin;%PATH%
	cd $(BASE_DIR)\$(MSAUTOTEST_DIR)
	rem svn update --revision $(MS_REVISION) --non-interactive
	git pull origin
	git log --pretty=format:%H -n 1
	cd $(BASE_DIR)
	
updatecmd:
    echo @echo off>update-$(COMPILER_VER).bat
    echo set PATH=^%CD^%\release-$(COMPILER_VER);^%CD^%\release-$(COMPILER_VER)\bin;^%CD^%\release-$(COMPILER_VER)\bin\curl;^%PATH^%>>update-$(COMPILER_VER).bat
    echo if not exist update mkdir update>>update-$(COMPILER_VER).bat
    echo if exist update\config.bat call update\config.bat>>update-$(COMPILER_VER).bat
    echo echo downloading update for release-$(COMPILER_VER)-$(PKG_VERSION)>>update-$(COMPILER_VER).bat
    echo curl -# -o update\update.bin "$(SERVER_URL)/GetUpdater.ashx?package=release-$(COMPILER_VER)-$(PKG_VERSION)&time=%lastupdate%">>update-$(COMPILER_VER).bat
    echo inflate c -1 -1 update\update.bin update\update.bat>>update-$(COMPILER_VER).bat
    echo if exist update\update.bat cmd /c update\update.bat>>update-$(COMPILER_VER).bat
    echo del update\update.bat>>update-$(COMPILER_VER).bat
    echo del update\update.bin>>update-$(COMPILER_VER).bat
    echo pause>>update-$(COMPILER_VER).bat
	
package:
    if exist $(OUTPUT_DIR)\install $(BASE_DIR)\cert\signdir $(OUTPUT_DIR)\install
    xcopy /Y $(BASE_DIR)\SDKShell.bat $(OUTPUT_DIR)
    xcopy /Y $(BASE_DIR)\read-me.txt $(OUTPUT_DIR)
    if exist $(OUTPUT_DIR)-$(PKG_VERSION).zip del $(OUTPUT_DIR)-$(PKG_VERSION).zip
    7z a -tzip $(OUTPUT_DIR)-$(PKG_VERSION).zip $(OUTPUT_DIR)\bin $(OUTPUT_DIR)\doc changelog.txt SDKShell.bat read-me.txt license.txt *.rtf
    xcopy /Y $(OUTPUT_DIR)-$(PKG_VERSION).zip $(INSTALL_DIR)
    if not exist $(INSTALL_DIR)\doc mkdir $(INSTALL_DIR)\doc
    if not exist $(INSTALL_DIR)\doc\release-$(COMPILER_VER)-$(PKG_VERSION) mkdir $(INSTALL_DIR)\doc\release-$(COMPILER_VER)-$(PKG_VERSION)
    xcopy /Y $(OUTPUT_DIR)\doc $(INSTALL_DIR)\doc\release-$(COMPILER_VER)-$(PKG_VERSION)
    if not exist $(INSTALL_DIR)\release-$(COMPILER_VER)-$(PKG_VERSION) mkdir $(INSTALL_DIR)\release-$(COMPILER_VER)-$(PKG_VERSION)
    if exist $(OUTPUT_DIR)\install xcopy /Y $(OUTPUT_DIR)\install\*.msi $(INSTALL_DIR)\release-$(COMPILER_VER)-$(PKG_VERSION)
    if exist $(OUTPUT_DIR)\install xcopy /Y $(OUTPUT_DIR)\install\*.exe $(INSTALL_DIR)\release-$(COMPILER_VER)-$(PKG_VERSION)
	uploadftp "$(OUTPUT_DIR)-$(PKG_VERSION).zip" downloads
    if exist $(OUTPUT_DIR)\install uploadftpdir $(OUTPUT_DIR)\install\*.msi downloads/release-$(COMPILER_VER)-$(PKG_VERSION)
	if exist $(OUTPUT_DIR)\install uploadftpdir $(OUTPUT_DIR)\install\*.exe downloads/release-$(COMPILER_VER)-$(PKG_VERSION)
	uploadftpdir $(OUTPUT_DIR)\doc\*.txt downloads/doc/release-$(COMPILER_VER)-$(PKG_VERSION)	

package-libs:
    if exist $(OUTPUT_DIR)-$(PKG_VERSION)-libs.zip del $(OUTPUT_DIR)-$(PKG_VERSION)-libs.zip
    7z a -tzip $(OUTPUT_DIR)-$(PKG_VERSION)-libs.zip $(OUTPUT_DIR)\lib $(OUTPUT_DIR)\include
    xcopy /Y $(OUTPUT_DIR)-$(PKG_VERSION)-libs.zip $(INSTALL_DIR)
	uploadftp "$(OUTPUT_DIR)-$(PKG_VERSION)-libs.zip" downloads
	
package-src:
    if exist $(OUTPUT_DIR)-$(PKG_VERSION)-src.zip del $(OUTPUT_DIR)-$(PKG_VERSION)-src.zip
    rem cd $(MS_DIR)
    rem nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
    rem cd $(BASE_DIR)
    rem cd $(GDAL_DIR)
    rem nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	nmake gdal NO_BUILD=YES NO_COPY=YES GDAL_DIR=$(GDAL_DIR)
	nmake gdalplugins NO_BUILD=YES NO_COPY=YES GDAL_DIR=$(GDAL_DIR)
	nmake ms NO_BUILD=YES NO_COPY=YES MS_DIR=$(MS_DIR)
	cd $(BASE_DIR)
    7z a -tzip -xr!?git\ -xr!?svn\ $(OUTPUT_DIR)-$(PKG_VERSION)-src.zip $(MS_DIR) $(GDAL_DIR)
    xcopy /Y $(OUTPUT_DIR)-$(PKG_VERSION)-src.zip $(INSTALL_DIR)
	uploadftp "$(OUTPUT_DIR)-$(PKG_VERSION)-src.zip" downloads
    
package-dev:
    if exist $(OUTPUT_DIR)-dev.zip del $(OUTPUT_DIR)-dev.zip
    if exist $(OUTPUT_DIR)\install del $(OUTPUT_DIR)\install\*.exe $(OUTPUT_DIR)\install\*.msi $(OUTPUT_DIR)\install\*.bz2
    7z a -tzip $(OUTPUT_DIR)-dev.zip $(OUTPUT_DIR) $(REGEX_DIR) $(SWIG_DIR)\swig.exe $(SWIG_DIR)\Lib Makefile readme.txt changelog.txt license.txt *.rtf
    xcopy /Y $(OUTPUT_DIR)-dev.zip $(INSTALL_DIR)
	uploadftp "$(OUTPUT_DIR)-dev.zip" downloads
    
package-dev-x64:
    if exist $(OUTPUT_DIR)-dev.zip del $(OUTPUT_DIR)-dev.zip
    cd $(MS_DIR)
    nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
    cd $(BASE_DIR)
    cd $(GDAL_DIR)
    nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
    cd $(BASE_DIR)
    7z a -tzip $(OUTPUT_DIR)-dev.zip $(OUTPUT_DIR) $(OUTPUT_DIR)-x64 $(MS_DIR) $(GDAL_DIR) $(REGEX_DIR) $(SWIG_DIR)\swig.exe $(SWIG_DIR)\Lib Makefile readme.txt changelog.txt license.txt *.rtf
    
install: package package-dev
    xcopy /Y $(OUTPUT_DIR).zip $(INSTALL_DIR)
    xcopy /Y $(OUTPUT_DIR)-dev.zip $(INSTALL_DIR)
	uploadftp "$(OUTPUT_DIR).zip" downloads
	uploadftp "$(OUTPUT_DIR)-dev.zip" downloads
    
mkgdalinst: mkgdalinst-core mkgdalinst-oci mkgdalinst-ecw mkgdalinst-ecw3 mkgdalinst-mrsid mkgdalinst-filegdb
    
mkgdalinst-core:
!IFDEF WIX_DIR
    set PATH=$(BASE_DIR)\$(WIX_DIR);$(PATH)
    if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
    -del $(OUTPUT_DIR)\install\GDAL.wixobj
    -del $(OUTPUT_DIR)\install\GDAL.wixpdb
    -del $(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-core.msi
    -heat.exe dir $(OUTPUT_DIR)\bin\gdal-data -out $(OUTPUT_DIR)\install\gdal-data.wxs -var var.SourceDir -cg GdalData -dr gdaldataDir -g1 -ag -ke -srd -scom -sreg
    -candle.exe "-dSourceDir=$(OUTPUT_DIR)\bin\gdal-data" -out "$(OUTPUT_DIR)\install\gdal-data.wixobj" $(OUTPUT_DIR)\install\gdal-data.wxs
!IFDEF WIN64
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "$(GDAL_KEA_DEF)" "$(GDAL_MSSQL_DEF)" "-dgdal_dll=gdal$(GDAL_VERSIONTAG).dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-core.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x64 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ELSE
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "$(GDAL_KEA_DEF)" "$(GDAL_MSSQL_DEF)" "-dgdal_dll=gdal$(GDAL_VERSIONTAG).dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-core.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x86 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ENDIF
    -Light.exe $(WIX_LIGHT_OPTS) -sice:ICE82 -sice:ICE03 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -out "$(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-core.msi" -pdbout "$(OUTPUT_DIR)\install\GDAL.wixpdb" "$(OUTPUT_DIR)\install\GDAL.wixobj" "$(OUTPUT_DIR)\install\gdal-data.wixobj"
    cd $(BASE_DIR)
!ENDIF 

mkgdalinst-oci:
!IFDEF WIX_DIR
    set PATH=$(BASE_DIR)\$(WIX_DIR);$(PATH)
    if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
    -del $(OUTPUT_DIR)\install\GDAL.wixobj
    -del $(OUTPUT_DIR)\install\GDAL.wixpdb
    -del $(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-oracle.msi
!IF EXIST ($(OUTPUT_DIR)\bin\gdal\plugins\ogr_OCI.dll)
!IFDEF WIN64
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dogr_OCI=ogr_OCI.dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-oracle.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x64 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ELSE
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dogr_OCI=ogr_OCI.dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-oracle.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x86 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ENDIF
    -Light.exe $(WIX_LIGHT_OPTS) -sice:ICE82 -sice:ICE03 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -out "$(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-oracle.msi" -pdbout "$(OUTPUT_DIR)\install\GDAL.wixpdb" "$(OUTPUT_DIR)\install\GDAL.wixobj"
    cd $(BASE_DIR)
!ENDIF
!ENDIF 

mkgdalinst-ecw:
!IFDEF WIX_DIR
    set PATH=$(BASE_DIR)\$(WIX_DIR);$(PATH)
    if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
    -del $(OUTPUT_DIR)\install\GDAL.wixobj
    -del $(OUTPUT_DIR)\install\GDAL.wixpdb
    -del $(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-ecw-$(ECWSDK_VERSION).msi
!IF EXIST ($(OUTPUT_DIR)\bin\gdal\plugins\gdal_ECW_JP2ECW.dll) || EXIST ($(OUTPUT_DIR)\bin\gdal\plugins-optional\gdal_ECW_JP2ECW.dll)  
!IFDEF WIN64
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dgdal_ECW_JP2ECW=gdal_ECW_JP2ECW.dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" "-dECWSDKVersion=$(ECWSDK_VERSION)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-ecw-$(ECWSDK_VERSION).msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x64 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ELSE
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dgdal_ECW_JP2ECW=gdal_ECW_JP2ECW.dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" "-dECWSDKVersion=$(ECWSDK_VERSION)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-ecw-$(ECWSDK_VERSION).msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x86 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ENDIF
    -Light.exe $(WIX_LIGHT_OPTS) -sice:ICE82 -sice:ICE03 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -out "$(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-ecw-$(ECWSDK_VERSION).msi" -pdbout "$(OUTPUT_DIR)\install\GDAL.wixpdb" "$(OUTPUT_DIR)\install\GDAL.wixobj"
    cd $(BASE_DIR)
!ENDIF
!ENDIF 

mkgdalinst-ecw3:
	nmake mkgdalinst-ecw GDAL_ECW3=1

mkgdalinst-mrsid:
!IFDEF WIX_DIR
    set PATH=$(BASE_DIR)\$(WIX_DIR);$(PATH)
    if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
    -del $(OUTPUT_DIR)\install\GDAL.wixobj
    -del $(OUTPUT_DIR)\install\GDAL.wixpdb
    -del $(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-mrsid.msi
!IF EXIST ($(OUTPUT_DIR)\bin\gdal\plugins\gdal_MrSID.dll)    
!IFDEF WIN64
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dgdal_MrSID=gdal_MrSID.dll" $(MRSID_SETUP_FLAGS) "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-mrsid.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x64 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ELSE
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dgdal_MrSID=gdal_MrSID.dll" $(MRSID_SETUP_FLAGS) "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-mrsid.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x86 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ENDIF
    -Light.exe $(WIX_LIGHT_OPTS) -sice:ICE82 -sice:ICE03 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -out "$(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-mrsid.msi" -pdbout "$(OUTPUT_DIR)\install\GDAL.wixpdb" "$(OUTPUT_DIR)\install\GDAL.wixobj"
    cd $(BASE_DIR)
!ENDIF
!ENDIF 

mkgdalinst-filegdb:
!IFDEF FILEGDB_API_DIR
!IFDEF WIX_DIR
    set PATH=$(BASE_DIR)\$(WIX_DIR);$(PATH)
    if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
    -del $(OUTPUT_DIR)\install\GDAL.wixobj
    -del $(OUTPUT_DIR)\install\GDAL.wixpdb
    -del $(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-filegdb.msi
!IF EXIST ($(OUTPUT_DIR)\bin\gdal\plugins-external\ogr_FileGDB.dll)    
!IFDEF WIN64
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dogr_FileGDB=ogr_FileGDB.dll" "-dFileGDBBINDIR=$(FILEGDB_BINPATH)" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-filegdb.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x64 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ELSE
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dogr_FileGDB=ogr_FileGDB.dll" "-dFileGDBBINDIR=$(FILEGDB_BINPATH)" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-filegdb.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x86 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ENDIF
    -Light.exe $(WIX_LIGHT_OPTS) -sice:ICE82 -sice:ICE03 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -out "$(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-filegdb.msi" -pdbout "$(OUTPUT_DIR)\install\GDAL.wixpdb" "$(OUTPUT_DIR)\install\GDAL.wixobj"
    cd $(BASE_DIR)
!ENDIF
!ENDIF
!ENDIF

mkgdalinst-netcdf:
!IFDEF WIX_DIR
    set PATH=$(BASE_DIR)\$(WIX_DIR);$(PATH)
    if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
    -del $(OUTPUT_DIR)\install\GDAL.wixobj
    -del $(OUTPUT_DIR)\install\GDAL.wixpdb
    -del $(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-netcdf.msi
!IF EXIST ($(OUTPUT_DIR)\bin\gdal\plugins-external\ogr_FileGDB.dll)    
!IFDEF WIN64
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dogr_FileGDB=ogr_FileGDB.dll" "-dFileGDBBINDIR=$(FILEGDB_BINPATH)" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-filegdb.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x64 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ELSE
    -candle.exe "-dVersionTag=$(GDAL_VERSIONTAG)" "-dogr_FileGDB=ogr_FileGDB.dll" "-dFileGDBBINDIR=$(FILEGDB_BINPATH)" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-filegdb.msi" -out "$(OUTPUT_DIR)\install\GDAL.wixobj" -arch x86 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" "$(BASE_DIR)\GDAL.wxs"
!ENDIF
    -Light.exe $(WIX_LIGHT_OPTS) -sice:ICE82 -sice:ICE03 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -out "$(OUTPUT_DIR)\install\gdal-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-netcdf.msi" -pdbout "$(OUTPUT_DIR)\install\GDAL.wixpdb" "$(OUTPUT_DIR)\install\GDAL.wixobj"
    cd $(BASE_DIR)
!ENDIF
!ENDIF

gdal-nuget:
    cd nuget
    nuget pack GDAL.nuspec -version $(GDAL_RELEASE_VER)
    nuget pack GDAL.Native.nuspec -version $(GDAL_RELEASE_VER)
    nuget pack GDAL.Plugins.nuspec -version $(GDAL_RELEASE_VER)
    nuget pack GDAL.Linux.nuspec -version $(GDAL_RELEASE_VER)
    sign GDAL.$(GDAL_RELEASE_VER).nupkg
    sign GDAL.Native.$(GDAL_RELEASE_VER).nupkg
    sign GDAL.Plugins.$(GDAL_RELEASE_VER).nupkg
    sign GDAL.Linux.$(GDAL_RELEASE_VER).nupkg
    cd $(BASE_DIR)
    
gdal-nuget-push:
    cd nuget
    nuget setApiKey $(NUGET_GDAL_API_KEY)
    nuget push GDAL.Native.$(GDAL_RELEASE_VER).nupkg -Source https://api.nuget.org/v3/index.json
    nuget push GDAL.Plugins.$(GDAL_RELEASE_VER).nupkg -Source https://api.nuget.org/v3/index.json
    nuget push GDAL.$(GDAL_RELEASE_VER).nupkg -Source https://api.nuget.org/v3/index.json

mkmapserverinst:
    set PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\ms\apps;$(PATH)
	for /F "tokens=1-3" %%i IN ('mapserv -v'); do ( if "%%i %%j"=="MapServer version" nmake mkmapserverinst-core MS_VERSION=%%k )

mkmapserverinst-core:
!IFDEF WIX_DIR
    set PATH=$(BASE_DIR)\$(WIX_DIR);$(PATH)
    if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
    -del $(OUTPUT_DIR)\install\MapServer.wixobj
    -del $(OUTPUT_DIR)\install\MapServer.wixpdb
    -del $(OUTPUT_DIR)\install\mapserver-$(GDAL_VERSIONTAG)-$(COMPILER_VER)-core.msi
!IFDEF WIN64
    -candle.exe "-dMSVersionTag=$(MS_VERSION)" "-dVersionTag=$(GDAL_VERSIONTAG)" "-dgdal_dll=gdal$(GDAL_VERSIONTAG).dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=mapserver-$(MS_VERSION)-$(COMPILER_VER)-core.msi" -out "$(OUTPUT_DIR)\install\MapServer.wixobj" -arch x64 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixIIsExtension.dll" "$(BASE_DIR)\wixprojects\mapserver\MapServer.wxs"
!ELSE
    -candle.exe "-dMSVersionTag=$(MS_VERSION)" "-dVersionTag=$(GDAL_VERSIONTAG)" "-dgdal_dll=gdal$(GDAL_VERSIONTAG).dll" "-dCompiler=$(MSVC_VER)" "-dTargetDir=$(OUTPUT_DIR)\bin" "-dBaseDir=$(BASE_DIR)" -dTargetExt=.msi "-dTargetFileName=mapserver-$(MS_VERSION)-$(COMPILER_VER)-core.msi" -out "$(OUTPUT_DIR)\install\MapServer.wixobj" -arch x86 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixIIsExtension.dll" "$(BASE_DIR)\wixprojects\mapserver\MapServer.wxs"
!ENDIF
    -Light.exe $(WIX_LIGHT_OPTS) -sice:ICE82 -sice:ICE03 -ext "$(BASE_DIR)\$(WIX_DIR)\WixUtilExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixNetFxExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixUIExtension.dll" -ext "$(BASE_DIR)\$(WIX_DIR)\WixIIsExtension.dll" -loc "$(BASE_DIR)\wixprojects\mapserver\MapServer.wxl" -out "$(OUTPUT_DIR)\install\mapserver-$(MS_VERSION)-$(COMPILER_VER)-core.msi" -pdbout "$(OUTPUT_DIR)\install\MapServer.wixpdb" "$(OUTPUT_DIR)\install\MapServer.wixobj" "$(OUTPUT_DIR)\install\gdal-data.wixobj"
    cd $(BASE_DIR)
!ENDIF

harfbuzz:
!IFDEF HARFBUZZ_DIR
    cd $(BASE_DIR)\$(HARFBUZZ_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" "-DHB_HAVE_FREETYPE=ON" "-DCMAKE_CXX_STANDARD_LIBRARIES=$(OUTPUT_DIR)\lib\libpng.lib $(OUTPUT_DIR)\lib\zdll.lib"
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(HARFBUZZ_DIR)\$(CMAKE_BUILDDIR)\$(MS_PROJECT_DIR)\harfbuzz.lib $(OUTPUT_DIR)\lib
	if exist $(OUTPUT_DIR)\include\harfbuzz rmdir /s /q $(OUTPUT_DIR)\include\harfbuzz
	mkdir $(OUTPUT_DIR)\include\harfbuzz
    xcopy /Y /S $(BASE_DIR)\$(HARFBUZZ_DIR)\src\hb*.h $(OUTPUT_DIR)\include\harfbuzz
!ENDIF
	cd $(BASE_DIR)
!ENDIF

harfbuzz-old:
!IFDEF HARFBUZZ_DIR
    cd $(HARFBUZZ_DIR)\src
!IFNDEF NO_CLEAN
    nmake -f Makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake -f Makefile.vc OUTPUT_DIR=$(OUTPUT_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(HARFBUZZ_DIR)\src\harfbuzz.lib $(OUTPUT_DIR)\lib
	if not exist $(OUTPUT_DIR)\include\harfbuzz mkdir $(OUTPUT_DIR)\include\harfbuzz
    xcopy /Y /S $(BASE_DIR)\$(HARFBUZZ_DIR)\src\hb*.h $(OUTPUT_DIR)\include\harfbuzz
!ENDIF
    cd $(BASE_DIR)
!ENDIF

freetype:
!IFDEF FT_DIR
    cd $(BASE_DIR)\$(FT_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" "-DZLIB_LIBRARY_RELEASE=$(OUTPUT_DIR)\lib\zdll.lib"
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(FT_DIR)\$(CMAKE_BUILDDIR)\$(MS_PROJECT_DIR)\freetype.lib $(OUTPUT_DIR)\lib
	if exist $(OUTPUT_DIR)\include\freetype rmdir /s /q $(OUTPUT_DIR)\include\freetype
    xcopy /Y /S $(BASE_DIR)\$(FT_DIR)\include\*.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
!ENDIF
	
freetype-old:
!IFDEF FT_DIR
!IFNDEF NO_BUILD
    cd $(FT_SRCPATH)
!IFDEF WIN64
!IF $(MSVC_VER) >= 1600
    devenv /rebuild "Release" freetype.sln /Project freetype /ProjectConfig "Release|x64"
!ELSE	
    devenv /rebuild "LIB Release" freetype.sln /Project freetype /ProjectConfig "Release|x64"
!ENDIF
!ELSE
!IF $(MSVC_VER) == 1310
    devenv /rebuild Release freetype.sln /Project freetype
!ELSEIF $(MSVC_VER) >= 1600
    devenv /rebuild "Release" freetype.sln /Project freetype /ProjectConfig "Release|Win32"
!ELSE
    devenv /rebuild "LIB Release" freetype.sln /Project freetype /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
    cd $(BASE_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(FT_LIBPATH)\$(FT_LIB) $(OUTPUT_DIR)\lib
    xcopy /Y /S $(FT_DIR)\include\*.h $(OUTPUT_DIR)\include
!ENDIF
!ENDIF

geos:
!IFDEF GEOS_DIR
    cd $(BASE_DIR)\$(GEOS_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\install" "-DBUILD_SHARED_LIBS=ON"
!IFDEF WIN64	
    devenv /rebuild Release geos.sln /Project geos_c /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release geos.sln /Project geos_c /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    cd $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\lib\Release\geos_c.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\lib\Release\geos.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\bin\Release\geos.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\bin\Release\geos_c.dll $(OUTPUT_DIR)\bin
	if not exist $(OUTPUT_DIR)\include\geos mkdir $(OUTPUT_DIR)\include\geos
	xcopy /Y /S $(BASE_DIR)\$(GEOS_DIR)\include\geos\*.h $(OUTPUT_DIR)\include\geos
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\include\geos.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\include\geos\platform.h $(OUTPUT_DIR)\include\geos
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\include\geos\version.h $(OUTPUT_DIR)\include\geos
	xcopy /Y $(BASE_DIR)\$(GEOS_DIR)\$(CMAKE_BUILDDIR)\capi\geos_c.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
!ENDIF

geos2:
	cd $(GEOS_DIR)\src
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
	del *.manifest *.pdb *.obj *.ilk *.exp
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc MSVC_VER=$(MSVC_VER)
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y geos_c.dll $(OUTPUT_DIR)\bin
	xcopy /Y geos_c_i.lib $(OUTPUT_DIR)\lib
!ENDIF
	cd ..
!IFNDEF NO_COPY
    cd include
    xcopy /Y geos.h $(OUTPUT_DIR)\include
    if not exist $(OUTPUT_DIR)\include\geos mkdir $(OUTPUT_DIR)\include\geos
    xcopy /Y /S geos\*.h $(OUTPUT_DIR)\include\geos
!ENDIF
	cd ..
!IFNDEF NO_COPY
	xcopy /Y capi\*.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
	
gdal-optfile:
    echo SWIG=$(BASE_DIR)\$(SWIG_DIR)\swig.exe > $(OUTPUT_DIR)\gdal.opt
	echo $(SWIG_DIR) > $(OUTPUT_DIR)\doc\gdal_deps.txt
	echo PYDIR=$(PYDIR) >> $(OUTPUT_DIR)\gdal.opt
	echo $(PYTHON_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
	echo GDAL_ROOT=$(GDAL_PATH) >> $(OUTPUT_DIR)\gdal.opt
!IFDEF GDAL_GEOS
    echo GEOS_DIR=$(BASE_DIR)\$(GEOS_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo GEOS_CFLAGS=-I$(OUTPUT_DIR)\include -DHAVE_GEOS >> $(OUTPUT_DIR)\gdal.opt
    echo GEOS_LIB=$(OUTPUT_DIR)\lib\geos_c.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(GEOS_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF    
    echo MSVC_VER=$(MSVC_VER) >> $(OUTPUT_DIR)\gdal.opt
!IFDEF WIN64
	echo WIN64=$(WIN64) >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFDEF DEBUG
    echo DEBUG=$(DEBUG) >> $(OUTPUT_DIR)\gdal.opt
!IFDEF LEAK_CHECK
    echo MSVC_VLD_FLAGS=-DMSVC_USE_VLD=1 -D_DEBUG -I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo MSVC_VLD_LIB=$(OUTPUT_DIR)\lib\vld.lib >> $(OUTPUT_DIR)\gdal.opt
    echo OPTFLAGS= /nologo /MDd /EHsc /Zi /W4 /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE /Fd$(GDAL_ROOT)\gdal.pdb >> $(OUTPUT_DIR)\gdal.opt
!ELSE
    echo OPTFLAGS= /nologo /MP /MD /Zi /Od /EHsc /FC /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE /DDEBUG  >> $(OUTPUT_DIR)\gdal.opt
!ENDIF    
!ENDIF
    echo SETARGV=$(SETARGV) >> $(OUTPUT_DIR)\gdal.opt
!IFDEF GDAL_DEBUG_VSIMALLOC
    echo DEBUG_VSIMALLOC = YES >> $(OUTPUT_DIR)\gdal.opt
    echo DEBUG_VSIMALLOC_STATS = YES >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFDEF GDAL_POSTGIS
    echo PG_INC_DIR = $(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo PG_LIB = $(OUTPUT_DIR)\lib\libpqdll.lib wsock32.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(PGSQL_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_JAVA
    echo JAVA_HOME=$(JAVA_HOME) >> $(OUTPUT_DIR)\gdal.opt
    echo ANT_HOME=$(ANT_HOME) >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFDEF GDAL_CURL
    echo CURL_DIR= $(BASE_DIR)\$(CURL_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo CURL_INC = -I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo CURL_LIB = $(OUTPUT_DIR)\lib\libcurl_imp.lib wsock32.lib wldap32.lib winmm.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(CURL_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
    echo $(OPENSSL_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_SQLITE
    echo SQLITE_INC=-I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo SQLITE_LIB=$(OUTPUT_DIR)\lib\sqlite3_i.lib >> $(OUTPUT_DIR)\gdal.opt
    echo SQLITE_HAS_COLUMN_METADATA=yes >> $(OUTPUT_DIR)\gdal.opt
    echo $(SQLITE_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_SPATIALITE
    echo SQLITE_INC=-I$(OUTPUT_DIR)\include -DHAVE_SPATIALITE -DSPATIALITE_AMALGAMATION >> $(OUTPUT_DIR)\gdal.opt
    echo SQLITE_LIB=$(OUTPUT_DIR)\lib\sqlite3_i.lib $(OUTPUT_DIR)\lib\spatialite_i.lib >> $(OUTPUT_DIR)\gdal.opt
    echo SQLITE_HAS_COLUMN_METADATA=yes >> $(OUTPUT_DIR)\gdal.opt
	echo $(SQLITE_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
    echo $(SPATIALITE_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_KMLSUPEROVERLAY
!IF EXIST ($(GDAL_PATH)\frmts\kmlsuperoverlay)
    echo KMLSUPEROVERLAY_SUPPORTED = YES >> $(OUTPUT_DIR)\gdal.opt
    echo MINIZIP_INCLUDE = -I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo MINIZIP_LIBRARY = $(OUTPUT_DIR)\lib\minizip.lib >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
!ENDIF
!IFDEF GDAL_MYSQL
    echo MYSQL_INC_DIR=$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo MYSQL_LIB=$(OUTPUT_DIR)\lib\libmysql.lib advapi32.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(MYSQL_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_XERCES
    echo XERCES_DIR=$(BASE_DIR)\$(XERCES_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo XERCES_INCLUDE=-I$(OUTPUT_DIR)\include -I$(OUTPUT_DIR)\include\xercesc >> $(OUTPUT_DIR)\gdal.opt
    echo XERCES_LIB=$(OUTPUT_DIR)\lib\xerces-c_3.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(XERCES_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
    echo ILI_ENABLED = YES >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFDEF GDAL_EXPAT
    echo EXPAT_DIR=$(BASE_DIR)\$(EXPAT_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo EXPAT_INCLUDE=-I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo EXPAT_LIB=$(OUTPUT_DIR)\lib\expat.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(EXPAT_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_LIBKML
!IFDEF LIBKML_DIR
    echo LIBKML_DIR=$(BASE_DIR)\$(LIBKML_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo LIBKML_INCLUDE=-I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo LIBKML_LIBS=$(OUTPUT_DIR)\lib\libkmlbase.lib $(OUTPUT_DIR)\lib\libkmlconvenience.lib $(OUTPUT_DIR)\lib\libkmldom.lib $(OUTPUT_DIR)\lib\libkmlengine.lib $(OUTPUT_DIR)\lib\libkmlregionator.lib $(OUTPUT_DIR)\lib\libkmlxsd.lib $(OUTPUT_DIR)\lib\expat.lib $(OUTPUT_DIR)\lib\zdll.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(LIBKML_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!ENDIF
!IFDEF GDAL_PDF
    echo POPPLER_ENABLED = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_CFLAGS = -I$(OUTPUT_DIR)\include -I$(OUTPUT_DIR)\include\poppler >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_HAS_OPTCONTENT = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_0_20_OR_LATER = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_0_23_OR_LATER = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_BASE_STREAM_HAS_TWO_ARGS = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_LIBS = $(OUTPUT_DIR)\lib\poppler.lib $(OUTPUT_DIR)\lib\$(FT_LIB) $(OUTPUT_DIR)\lib\harfbuzz.lib advapi32.lib gdi32.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(POPPLER_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_OPENJPEG
    echo OPENJPEG_ENABLED = YES >> $(OUTPUT_DIR)\gdal.opt
!IFDEF USE_OPENJP2_NEW_METHOD
    echo OPENJPEG_CFLAGS = -I$(OUTPUT_DIR)\include\openjpeg-2.1 >> $(OUTPUT_DIR)\gdal.opt
    echo OPENJPEG_LIB = $(OUTPUT_DIR)\lib\openjp2.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(OPENJPEG2_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ELSEIFDEF USE_OPENJP2
    echo OPENJPEG_CFLAGS = -I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo OPENJPEG_LIB = $(OUTPUT_DIR)\lib\openjp2.lib >> $(OUTPUT_DIR)\gdal.opt
    echo OPENJPEG_VERSION = 20100 >> $(OUTPUT_DIR)\gdal.opt
    echo $(OPENJPEG2_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ELSE
    echo OPENJPEG_LIB = $(OUTPUT_DIR)\lib\openjpeg.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(OPENJPEG_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!ENDIF
!IFDEF GDAL_TIFF
    echo TIFF_INC=-I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo TIFF_LIB=$(OUTPUT_DIR)\lib\libtiff_i.lib >> $(OUTPUT_DIR)\gdal.opt
    echo TIFF_OPTS=-DBIGTIFF_SUPPORT
    echo $(JBIG_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
    echo $(LIBTIFF_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
	echo $(LIBGEOTIFF_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF GDAL_AMIGOCLOUD
    echo AMIGOCLOUD_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFDEF GDAL_GNM
    echo INCLUDE_GNM_FRMTS=YES >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFDEF SZIP_DIR
    echo $(SZIP_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF FITS_DIR
    echo $(FITS_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF HDF5_DIR
    echo $(HDF5_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF HDF5_DIR
    echo $(KEA_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF HDF5_DIR
    echo $(HDF4_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF ECW_DIR
    echo $(ECW_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF MRSID_DIR
    echo $(MRSID_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF OCI_DIR
    echo $(OCI_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF NETCDF_DIR
    echo $(NETCDF_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt
!ENDIF
!IFDEF USE_PROJ6
    echo PROJ_INCLUDE = -I$(OUTPUT_DIR)\include\proj6 >> $(OUTPUT_DIR)\gdal.opt
    echo PROJ_LIBRARY = $(OUTPUT_DIR)\lib\proj_6_1.lib >> $(OUTPUT_DIR)\gdal.opt
!ENDIF

gdal-clean:
!IFNDEF NO_CLEAN
	cd $(GDAL_PATH)
	nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	-del gdal*.dll
	cd swig
	nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	cd $(BASE_DIR)
!ENDIF

gdal-build:
	cd $(GDAL_PATH)
!IFNDEF NO_BUILD
	nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y *.dll $(OUTPUT_DIR)\bin
	xcopy /Y port\*.h $(OUTPUT_DIR)\include
	xcopy /Y gcore\*.h $(OUTPUT_DIR)\include
	xcopy /Y alg\*.h $(OUTPUT_DIR)\include
	xcopy /Y ogr\*.h $(OUTPUT_DIR)\include
	xcopy /Y apps\*.h $(OUTPUT_DIR)\include
	xcopy /Y frmts\mem\memdataset.h $(OUTPUT_DIR)\include
	if exist frmts\raw\rawdataset.h xcopy /Y frmts\raw\rawdataset.h $(OUTPUT_DIR)\include
    if exist gcore\rawdataset.h xcopy /Y gcore\rawdataset.h $(OUTPUT_DIR)\include
	xcopy /Y frmts\gtiff\libgeotiff\*.h $(OUTPUT_DIR)\include
	xcopy /Y frmts\gtiff\libgeotiff\*.inc $(OUTPUT_DIR)\include
	xcopy /Y frmts\vrt\*.h $(OUTPUT_DIR)\include
	xcopy /Y ogr\ogrsf_frmts\*.h $(OUTPUT_DIR)\include
	xcopy /Y gdal_i.lib $(OUTPUT_DIR)\lib
	if not exist $(OUTPUT_DIR)\bin\gdal mkdir $(OUTPUT_DIR)\bin\gdal
	if not exist $(OUTPUT_DIR)\bin\gdal\apps mkdir $(OUTPUT_DIR)\bin\gdal\apps
	xcopy /Y apps\*.exe $(OUTPUT_DIR)\bin\gdal\apps
	if not exist $(OUTPUT_DIR)\bin\gdal-data mkdir $(OUTPUT_DIR)\bin\gdal-data
	xcopy /Y data\*.* $(OUTPUT_DIR)\bin\gdal-data
	if exist $(OUTPUT_DIR)\bin\gdal\plugins del $(OUTPUT_DIR)\bin\gdal\plugins\*.dll
    if exist $(OUTPUT_DIR)\bin\gdal\plugins-optional del $(OUTPUT_DIR)\bin\gdal\plugins-optional\*.dll
	if exist $(OUTPUT_DIR)\bin\gdal\plugins-external del $(OUTPUT_DIR)\bin\gdal\plugins-external\*.dll
!ENDIF
	cd $(BASE_DIR)

gdal: gdal-optfile gdal-clean gdal-build
	
gdalversion2:
!IFNDEF NO_COPY
    SET GDAL_DRIVER_PATH=$(OUTPUT_DIR)\bin\gdal\plugins;$(OUTPUT_DIR)\bin\gdal\plugins-external
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(BASE_DIR)\$(SDE_DIR);$(BASE_DIR)\$(OCI_DIR);$(FILEGDB_BINPATH)
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
    cd $(OUTPUT_DIR)\bin
	gdal\apps\gdalinfo --version > $(OUTPUT_DIR)\doc\gdal_version.txt
	gdal\apps\gdalinfo --formats > $(OUTPUT_DIR)\doc\gdal_formats.txt
	gdal\apps\ogrinfo --formats > $(OUTPUT_DIR)\doc\ogr_formats.txt
	cd $(BASE_DIR)
!ENDIF
	
gdalversion:
!IFNDEF NO_COPY
!IFDEF WIN64
    nmake gdalversion2 WIN64=$(WIN64)
!ELSE
    nmake gdalversion2
!ENDIF
!ENDIF
	
gdal-csharp:	
!IFDEF GDAL_CSHARP
	cd $(GDAL_PATH)\swig\csharp
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_BUILD
!IFDEF DEBUG
	nmake /f makefile.vc interface EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt DEBUG=1
!ELSE
	nmake /f makefile.vc interface EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF	
	nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\csharp mkdir $(OUTPUT_DIR)\bin\gdal\csharp
	xcopy /Y *.dll $(OUTPUT_DIR)\bin\gdal\csharp
	xcopy /Y *.exe $(OUTPUT_DIR)\bin\gdal\csharp
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-csharp-test:	
!IFDEF GDAL_CSHARP
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\gdal\csharp;$(PATH)
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj6\SHARE
	cd $(GDAL_PATH)\swig\csharp
	nmake /f makefile.vc test EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	cd $(BASE_DIR)
!ENDIF

gdal-java: gdal-optfile
!IFDEF GDAL_JAVA
	cd $(GDAL_PATH)\swig\java
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
	cd $(GDAL_PATH)\swig
!IFNDEF NO_BUILD
	nmake /f makefile.vc java EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\java mkdir $(OUTPUT_DIR)\bin\gdal\java
	cd $(GDAL_PATH)\swig\java
	xcopy /Y *.dll $(OUTPUT_DIR)\bin\gdal\java
	xcopy /Y *.jar $(OUTPUT_DIR)\bin\gdal\java
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-java-test:	
!IFDEF GDAL_JAVA
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\gdal\java;$(PATH)
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
	cd $(GDAL_PATH)\swig\java
	nmake /f makefile.vc test EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	cd $(BASE_DIR)
!ENDIF

gdal-python-all:
!IFNDEF NO_BUILD
!IFDEF WIN64
    rem nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python31-AMD64 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python31-AMD64 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python32-AMD64 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python32-AMD64 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python33-AMD64 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python33-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python34-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python34-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python35-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python35-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python36-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python36-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python37-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python37-AMD64 SWIG_DIR=SWIG-2.0.4
	nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python27-AMD64 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python27-AMD64 SWIG_DIR=SWIG-2.0.4
!ELSE
    rem nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python31 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python31 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python32 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python32 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python33 SWIG_DIR=SWIG-2.0.4
    rem nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python33 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python34 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python34 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python35 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python35 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python36 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python36 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python37 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python37 SWIG_DIR=SWIG-2.0.4
	nmake gdal-python GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python27 SWIG_DIR=SWIG-2.0.4
    nmake gdal-python-bdist GDAL_DIR=$(GDAL_DIR) PYTHON_DIR=Python27 SWIG_DIR=SWIG-2.0.4
!ENDIF
!ENDIF

gdal-python: gdal-optfile
!IFDEF GDAL_PYTHON
    SET DISTUTILS_USE_SDK=1
    SET MSSdk=1
	cd $(GDAL_PATH)\swig
	echo [build_ext] > python\setup.cfg
    echo include_dirs = ../../port;../../gcore;../../alg;../../ogr/;../../ogr/ogrsf_frmts;../../gnm;../../apps >> python\setup.cfg
    echo library_dirs = ../../ >> python\setup.cfg
    echo libraries = gdal_i >> python\setup.cfg
    cd python
!IFNDEF NO_CLEAN    
    if exist build\nul rmdir /S /Q build
!ENDIF    
    cd ..
!IFNDEF NO_BUILD
	nmake /f makefile.vc python EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
	cd $(PYTHON_OUTDIR)
!IFNDEF NO_COPY	
	if not exist $(OUTPUT_DIR)\bin\gdal\python mkdir $(OUTPUT_DIR)\bin\gdal\python
	xcopy /Y *.py $(OUTPUT_DIR)\bin\gdal\python
	cd osgeo
	if not exist $(OUTPUT_DIR)\bin\gdal\python\osgeo mkdir $(OUTPUT_DIR)\bin\gdal\python\osgeo
	if exist _gdal.pyd.manifest mt -manifest _gdal.pyd.manifest -outputresource:_gdal.pyd;2
	if exist _gdalconst.pyd.manifest mt -manifest _gdalconst.pyd.manifest -outputresource:_gdalconst.pyd;2
	if exist _ogr.pyd.manifest mt -manifest _ogr.pyd.manifest -outputresource:_ogr.pyd;2
	if exist _osr.pyd.manifest mt -manifest _osr.pyd.manifest -outputresource:_osr.pyd;2
	xcopy /Y *.py $(OUTPUT_DIR)\bin\gdal\python\osgeo
	xcopy /Y *.pyd $(OUTPUT_DIR)\bin\gdal\python\osgeo
	cd $(GDAL_PATH)\swig\python\scripts
	if not exist $(OUTPUT_DIR)\bin\gdal\python\scripts mkdir $(OUTPUT_DIR)\bin\gdal\python\scripts
	xcopy /Y *.py $(OUTPUT_DIR)\bin\gdal\python\scripts
!ENDIF
	cd $(BASE_DIR)  
!ENDIF

gdal-python-bdist:
!IFDEF GDAL_PYTHON
    SET DISTUTILS_USE_SDK=1
    SET MSSdk=1
	cd $(GDAL_PATH)\swig
	echo [build_ext] > python\setup.cfg
    echo include_dirs = ../../port;../../gcore;../../alg;../../ogr/ >> python\setup.cfg
    echo library_dirs = ../../ >> python\setup.cfg
    echo libraries = gdal_i >> python\setup.cfg
    cd python
!IFNDEF NO_CLEAN    
    if exist dist\nul rmdir /S /Q dist
!ENDIF    
!IFNDEF NO_BUILD
	$(BASE_DIR)\$(PYTHON_DIR)\python.exe setup.py bdist $(PYTHON_BDIST_OPTS)
!ENDIF
!IFNDEF NO_COPY	
	if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
	xcopy /Y dist\*.exe $(OUTPUT_DIR)\install
	xcopy /Y dist\*.msi $(OUTPUT_DIR)\install
!ENDIF
	cd $(BASE_DIR)  
!ENDIF

gdal-autotest: update-gdal-autotest
    SET GDAL_DOWNLOAD_TEST_DATA=YES
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
    SET GDAL_DRIVER_PATH=$(OUTPUT_DIR)\bin\gdal\plugins;$(OUTPUT_DIR)\bin\gdal\plugins-external
    SET GDAL_DATA=$(GDAL_PATH)\data
    SET PYTHONPATH=$(OUTPUT_DIR)\bin\gdal\python
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\gdal\python\osgeo;$(BASE_DIR)\$(SDE_DIR);$(BASE_DIR)\$(OCI_DIR);$(FILEGDB_BINPATH);$(PATH)   
    cd $(GDALTEST_DIR)
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe $(GDALTEST_SCRIPT)
    cd $(BASE_DIR)
    
gdal-autotest3:
    SET GDAL_DOWNLOAD_TEST_DATA=YES
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
    SET GDAL_DRIVER_PATH=$(OUTPUT_DIR)\bin\gdal\plugins;$(OUTPUT_DIR)\bin\gdal\plugins-external
    SET GDAL_DATA=$(GDAL_PATH)\data
    SET PYTHONPATH=$(OUTPUT_DIR)\bin\gdal\python
    SET CPL_DEBUG=ON
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\gdal\python\osgeo;$(BASE_DIR)\$(SDE_DIR);$(BASE_DIR)\$(OCI_DIR);$(FILEGDB_BINPATH);$(PATH)   
    cd $(GDALTEST_DIR)\gdrivers
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe mrsid.py CPL_DEBUG=YES
        
    
    cd $(BASE_DIR)
    
gdal-autotest2:
    SET GDAL_DOWNLOAD_TEST_DATA=YES
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
    SET GDAL_DRIVER_PATH=$(OUTPUT_DIR)\bin\gdal\plugins;$(OUTPUT_DIR)\bin\gdal\plugins-external
    SET GDAL_DATA=$(GDAL_PATH)\data
    SET PYTHONPATH=$(OUTPUT_DIR)\bin\gdal\python
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\gdal\python\osgeo;$(BASE_DIR)\$(SDE_DIR);$(BASE_DIR)\$(OCI_DIR);$(FILEGDB_BINPATH);$(PATH)
!IFDEF GDALTEST_DIR_SRC
    if not exist $(GDALTEST_DIR)\gdrivers\tmp\cache mkdir $(GDALTEST_DIR)\gdrivers\tmp\cache
    if exist $(GDALTEST_DIR_SRC)\gdrivers\tmp\cache xcopy /D /Y /S $(GDALTEST_DIR_SRC)\gdrivers\tmp\cache $(GDALTEST_DIR)\gdrivers\tmp\cache
    if not exist $(GDALTEST_DIR)\gcore\tmp\cache mkdir $(GDALTEST_DIR)\gcore\tmp\cache
    if exist $(GDALTEST_DIR_SRC)\gcore\tmp\cache xcopy /D /Y /S $(GDALTEST_DIR_SRC)\gcore\tmp\cache $(GDALTEST_DIR)\gcore\tmp\cache
!ENDIF 
    cd $(GDALTEST_DIR)
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_all.py
    cd $(BASE_DIR)

gdal-sde:
!IFDEF GDAL_SDE
!IFDEF SDE_DIR
!IFNDEF WIN64
    echo SDE_ENABLED=YES >> $(OUTPUT_DIR)\gdal.opt
    echo SDE_VERSION=$(SDE_VERSION) >> $(OUTPUT_DIR)\gdal.opt
    echo SDE_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo SDE_SDK = $(BASE_DIR)\$(SDE_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo SDE_INC = $(BASE_DIR)\$(SDE_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
!IF $(SDE_VERSION) > 91
    echo SDE_LIB = $(BASE_DIR)\$(SDE_DIR)\lib\pe.lib $(BASE_DIR)\$(SDE_DIR)\lib\sde.lib $(BASE_DIR)\$(SDE_DIR)\lib\sg.lib >> $(OUTPUT_DIR)\gdal.opt
!ELSE
    echo SDE_LIB = $(BASE_DIR)\$(SDE_DIR)\lib\pe$(SDE_VERSION).lib $(BASE_DIR)\$(SDE_DIR)\lib\sde$(SDE_VERSION).lib $(BASE_DIR)\$(SDE_DIR)\lib\sg$(SDE_VERSION).lib >> $(OUTPUT_DIR)\gdal.opt 
!ENDIF
	cd $(GDAL_PATH)\ogr\ogrsf_frmts\sde
!IFNDEF NO_CLEAN	
	nmake /f makefile.vc clean
	del *.manifest
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc ogr_SDE.dll EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if exist ogr_SDE.dll.manifest mt -manifest ogr_SDE.dll.manifest -outputresource:ogr_SDE.dll;2
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
	xcopy /Y ogr_SDE.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(GDAL_PATH)\frmts\sde
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if exist ogr_SDE.dll.manifest mt -manifest ogr_SDE.dll.manifest -outputresource:ogr_SDE.dll;2
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y gdal_SDE.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF
!ENDIF

gdal-oci:
!IFDEF OCI_DIR
    echo OCI_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo OCI_LIB =	$(BASE_DIR)\$(OCI_DIR)\sdk\lib\msvc\oci.lib >> $(OUTPUT_DIR)\gdal.opt
    echo OCI_INCLUDE =	-I$(BASE_DIR)\$(OCI_DIR)\sdk\include >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\ogr\ogrsf_frmts\oci
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
	del *.manifest
!ENDIF
!IFNDEF NO_BUILD	
	nmake /f makefile.vc ogr_OCI.dll EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if exist ogr_OCI.dll.manifest mt -manifest ogr_OCI.dll.manifest -outputresource:ogr_OCI.dll;2
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
	xcopy /Y ogr_OCI.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(GDAL_PATH)\frmts\georaster
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
	del *.manifest
!ENDIF
!IFNDEF NO_BUILD	
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
	xcopy /Y gdal_GEOR.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-mrsid: gdal-optfile
!IFDEF MRSID_DIR
    echo MRSID_DIR=$(BASE_DIR)\$(MRSID_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_RDLLBUILD = YES >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_LDLLBUILD = YES >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_INCLUDE=$(MRSID_INCLUDE) >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_LIB=$(MRSID_LIB) advapi32.lib user32.lib >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_PLUGIN = YES >> $(OUTPUT_DIR)\gdal.opt
    
    echo MRSID_RASTER_DIR = $(MRSID_RASTER_DIR)
    echo MRSID_RVER       = $(MRSID_RVER)
    echo MRSID_JP2        = $(MRSID_JP2)
    echo MRSID_ESDK       = $(MRSID_ESDK)
    echo MRSID_RDLLBUILD  = $(MRSID_RDLLBUILD)
    echo MRSID_LIDAR_DIR  = $(MRSID_LIDAR_DIR)
    echo MRSID_LVER       = $(MRSID_LVER)
    echo MRSID_LDLLBUILD  = $(MRSID_LDLLBUILD)
    echo MRSID_CONFIG     = $(MRSID_CONFIG)
    echo MRSID_FLAGS      = $(MRSID_FLAGS)
    echo MRSID_INCLUDE    = $(MRSID_INCLUDE)
    echo MRSID_LIB        = $(MRSID_LIB)
    echo MRSID_DLL        = $(MRSID_DLL)
    echo LIDAR_DLL        = $(LIDAR_DLL)
    
    cd $(GDAL_PATH)\frmts\mrsid
!IFNDEF NO_CLEAN	
	nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt 
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
!IFNDEF NO_COPY
    if exist $(OUTPUT_DIR)\bin\lti_dsdk_dll.dll del $(OUTPUT_DIR)\bin\lti_dsdk_dll.dll
    if exist $(OUTPUT_DIR)\bin\lti_dsdk.dll del $(OUTPUT_DIR)\bin\lti_dsdk.dll
    if exist $(OUTPUT_DIR)\bin\lti_lidar_dsdk.dll del $(OUTPUT_DIR)\bin\lti_lidar_dsdk.dll
	if exist $(OUTPUT_DIR)\bin\lti_dsdk_9.0.dll del $(OUTPUT_DIR)\bin\lti_dsdk_9.0.dll
	if exist $(OUTPUT_DIR)\bin\lti_dsdk_9.1.dll del $(OUTPUT_DIR)\bin\lti_dsdk_9.1.dll
    if exist $(OUTPUT_DIR)\bin\lti_lidar_dsdk_1.1.dll del $(OUTPUT_DIR)\bin\lti_lidar_dsdk_1.1.dll
	if exist $(OUTPUT_DIR)\bin\tbb.dll del $(OUTPUT_DIR)\bin\tbb.dll
	xcopy /Y gdal_MrSID.dll $(OUTPUT_DIR)\bin\gdal\plugins
	if exist $(MRSID_DLL) xcopy /Y $(MRSID_DLL) $(OUTPUT_DIR)\bin
	if exist $(LIDAR_DLL) xcopy /Y $(LIDAR_DLL) $(OUTPUT_DIR)\bin
	if exist $(MRSID_RASTER_DIR)\lib\tbb.dll xcopy /Y $(MRSID_RASTER_DIR)\lib\tbb.dll $(OUTPUT_DIR)\bin
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-mrsid-lidar: gdal-optfile
!IFDEF MRSID_DIR
    echo MRSID_DIR=$(BASE_DIR)\$(MRSID_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_RDLLBUILD = YES >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_LDLLBUILD = YES >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_INCLUDE=$(MRSID_INCLUDE) >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_LIB=$(MRSID_LIB) advapi32.lib user32.lib >> $(OUTPUT_DIR)\gdal.opt
    echo MRSID_PLUGIN = YES >> $(OUTPUT_DIR)\gdal.opt
    
    echo MRSID_RASTER_DIR = $(MRSID_RASTER_DIR)
    echo MRSID_RVER       = $(MRSID_RVER)
    echo MRSID_JP2        = $(MRSID_JP2)
    echo MRSID_ESDK       = $(MRSID_ESDK)
    echo MRSID_RDLLBUILD  = $(MRSID_RDLLBUILD)
    echo MRSID_LIDAR_DIR  = $(MRSID_LIDAR_DIR)
    echo MRSID_LVER       = $(MRSID_LVER)
    echo MRSID_LDLLBUILD  = $(MRSID_LDLLBUILD)
    echo MRSID_CONFIG     = $(MRSID_CONFIG)
    echo MRSID_FLAGS      = $(MRSID_FLAGS)
    echo MRSID_INCLUDE    = $(MRSID_INCLUDE)
    echo MRSID_LIB        = $(MRSID_LIB)
    echo MRSID_DLL        = $(MRSID_DLL)
    echo LIDAR_DLL        = $(LIDAR_DLL)
    
    cd $(GDAL_PATH)\frmts\mrsid_lidar
!IFNDEF NO_CLEAN	
	nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt 
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y gdal_MG4Lidar.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-hdf4: gdal-optfile
!IFDEF HDF4_DIR
    echo HDF4_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo HDF4_DIR =	$(BASE_DIR)\$(HDF4_DIR)\$(CMAKE_BUILDDIR)\install >> $(OUTPUT_DIR)\gdal.opt
    echo HDF4_LIB =	$(OUTPUT_DIR)\lib\hdfdll.lib $(OUTPUT_DIR)\lib\mfhdfdll.lib  >> $(OUTPUT_DIR)\gdal.opt
    echo HDF4_INCLUDE = $(OUTPUT_DIR)\include\hdf4 >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\frmts\hdf4
!IFNDEF NO_CLEAN	
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
    nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
    if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y gdal_HDF4.dll $(OUTPUT_DIR)\bin\gdal\plugins
	cd $(OUTPUT_DIR)\bin\gdal\plugins
	copy /Y gdal_HDF4.dll gdal_HDF4Image.dll
!ENDIF
	cd $(BASE_DIR)
!ENDIF
	
gdal-hdf5: gdal-optfile
!IFDEF HDF5_DIR
    echo HDF5_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo HDF5_DIR =	$(OUTPUT_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo HDF5_LIB =	$(OUTPUT_DIR)\lib\hdf5.lib >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\frmts\hdf5
!IFNDEF NO_CLEAN	
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y gdal_HDF5.dll $(OUTPUT_DIR)\bin\gdal\plugins
	cd $(OUTPUT_DIR)\bin\gdal\plugins
	copy /Y gdal_HDF5.dll gdal_HDF5Image.dll
	copy /Y gdal_HDF5.dll gdal_BAG.dll
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-kea: gdal-optfile
!IFDEF KEA_DIR
!IF EXIST ($(GDAL_PATH)\frmts\kea)
    echo KEA_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo KEA_LIB =	$(OUTPUT_DIR)\lib\libkea.lib $(OUTPUT_DIR)\lib\hdf5.lib $(OUTPUT_DIR)\lib\hdf5_cpp.lib >> $(OUTPUT_DIR)\gdal.opt
	echo KEA_CFLAGS = -I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\frmts\kea
!IFNDEF NO_CLEAN	
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y gdal_KEA.dll $(OUTPUT_DIR)\bin\gdal\plugins
	cd $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF

gdal-amigocloud: gdal-optfile
!IF EXIST ($(GDAL_PATH)\ogr\ogrsf_frmts\amigocloud)
    echo AMIGOCLOUD_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\ogr\ogrsf_frmts\amigocloud
!IFNDEF NO_CLEAN	
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y ogr_AmigoCloud.dll $(OUTPUT_DIR)\bin\gdal\plugins
	cd $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-netcdf: gdal-optfile
!IFDEF NETCDF_DIR
    echo NETCDF_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo NETCDF_SETTING=yes >> $(OUTPUT_DIR)\gdal.opt
    echo NETCDF_LIB=$(OUTPUT_DIR)\lib\netcdf.lib >> $(OUTPUT_DIR)\gdal.opt
    echo NETCDF_INC_DIR=$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\frmts\netcdf
!IFNDEF NO_CLEAN		
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y gdal_netCDF.dll $(OUTPUT_DIR)\bin\gdal\plugins
	cd $(OUTPUT_DIR)\bin\gdal\plugins
	copy /Y gdal_netCDF.dll gdal_GMT.dll
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-fits: gdal-optfile
!IFDEF FITS_DIR
    echo FITS_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo FITS_INC_DIR=$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo FITS_LIB=$(OUTPUT_DIR)\lib\cfitsio.lib >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\frmts\fits
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
	xcopy /Y gdal_FITS.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-pdf: gdal-optfile
!IFDEF GDAL_PDF
    echo PDF_PLUGIN=YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_ENABLED = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_CFLAGS = -I$(OUTPUT_DIR)\include -I$(OUTPUT_DIR)\include\poppler >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_HAS_OPTCONTENT = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_0_20_OR_LATER = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_0_23_OR_LATER = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_BASE_STREAM_HAS_TWO_ARGS = YES >> $(OUTPUT_DIR)\gdal.opt
    echo POPPLER_LIBS = $(OUTPUT_DIR)\lib\poppler.lib $(OUTPUT_DIR)\lib\$(FT_LIB) $(OUTPUT_DIR)\lib\libtiff_i.lib $(OUTPUT_DIR)\lib\libpng.lib $(OUTPUT_DIR)\lib\libjpeg.lib $(OUTPUT_DIR)\lib\zdll.lib advapi32.lib gdi32.lib >> $(OUTPUT_DIR)\gdal.opt
    echo $(POPPLER_DIR) >> $(OUTPUT_DIR)\doc\gdal_deps.txt

	cd $(GDAL_PATH)\frmts\pdf
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
	xcopy /Y gdal_PDF.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF

gdal-kmlsuperoverlay: gdal-optfile
!IFDEF GDAL_KMLSUPEROVERLAY
!IF EXIST ($(GDAL_PATH)\frmts\kmlsuperoverlay)
    echo KMLSUPEROVERLAY_SUPPORTED = YES >> $(OUTPUT_DIR)\gdal.opt
    echo MINIZIP_INCLUDE = -I$(OUTPUT_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
    echo MINIZIP_LIBRARY = $(OUTPUT_DIR)\lib\minizip.lib >> $(OUTPUT_DIR)\gdal.opt
    echo KMLSUPEROVERLAY_PLUGIN = YES >> $(OUTPUT_DIR)\gdal.opt
	cd $(GDAL_PATH)\frmts\kmlsuperoverlay
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
	xcopy /Y gdal_KMLSUPEROVERLAY.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF

gdal-filegdb: gdal-optfile
!IFDEF FILEGDB_API_DIR
!IF EXIST ($(GDAL_PATH)\ogr\ogrsf_frmts\filegdb)
    echo FGDB_ENABLED = YES >> $(OUTPUT_DIR)\gdal.opt
    echo FGDB_PLUGIN = YES >> $(OUTPUT_DIR)\gdal.opt
    echo FGDB_SDK = $(BASE_DIR)\$(FILEGDB_API_DIR) >> $(OUTPUT_DIR)\gdal.opt
    echo FGDB_INC = $(BASE_DIR)\$(FILEGDB_API_DIR)\include >> $(OUTPUT_DIR)\gdal.opt
!IFDEF WIN64
    echo FGDB_LIB = $(BASE_DIR)\$(FILEGDB_API_DIR)\lib64\FileGDBAPI.lib >> $(OUTPUT_DIR)\gdal.opt
!ELSE
    echo FGDB_LIB = $(BASE_DIR)\$(FILEGDB_API_DIR)\lib\FileGDBAPI.lib >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
    cd $(GDAL_PATH)\ogr\ogrsf_frmts\filegdb
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins-external mkdir $(OUTPUT_DIR)\bin\gdal\plugins-external
	xcopy /Y ogr_FileGDB.dll $(OUTPUT_DIR)\bin\gdal\plugins-external
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF

ogr-pg: gdal-optfile
!IFDEF GDAL_POSTGIS
    cd $(GDAL_PATH)\ogr\ogrsf_frmts\pg
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc ogr_PG.dll EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins-optional mkdir $(OUTPUT_DIR)\bin\gdal\plugins-optional
	xcopy /Y ogr_PG.dll $(OUTPUT_DIR)\bin\gdal\plugins-optional
!ENDIF
	cd $(BASE_DIR)
!ENDIF

ogr-mssql: gdal-optfile
!IFDEF GDAL_MSSQL_DEF
!IFDEF GDAL_MSSQL
    echo SQLNCLI_VERSION = $(SQLNCLI_VERSION) >> $(OUTPUT_DIR)\gdal.opt
    echo SQLNCLI_DIR = $(SQLNCLI_DIR) >> $(OUTPUT_DIR)\gdal.opt
!IFDEF WIN64
	echo SQLNCLI_LIB = "$(SQLNCLI_DIR)\Lib\x64\sqlncli$(SQLNCLI_VERSION).lib" >> $(OUTPUT_DIR)\gdal.opt
!ELSE
	echo SQLNCLI_LIB = "$(SQLNCLI_DIR)\Lib\x86\sqlncli$(SQLNCLI_VERSION).lib" >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
    echo SQLNCLI_INCLUDE = "-I$(SQLNCLI_DIR)\Include" -DSQLNCLI_VERSION=$(SQLNCLI_VERSION) -DMSSQL_BCP_SUPPORTED=1 >> $(OUTPUT_DIR)\gdal.opt
    cd $(GDAL_PATH)\ogr\ogrsf_frmts\mssqlspatial
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins-optional mkdir $(OUTPUT_DIR)\bin\gdal\plugins-optional
	xcopy /Y ogr_MSSQLSpatial.dll $(OUTPUT_DIR)\bin\gdal\plugins-optional
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF

ogr-mssql-msodbc: gdal-optfile
!IFDEF GDAL_MSSQL_DEF
!IFDEF GDAL_MSSQL
    echo MSODBCSQL_VERSION = $(MSODBCSQL_VERSION) >> $(OUTPUT_DIR)\gdal.opt
    echo MSODBCSQL_DIR = $(MSODBCSQL_DIR) >> $(OUTPUT_DIR)\gdal.opt
!IFDEF WIN64
	echo MSODBCSQL_LIB = "$(MSODBCSQL_DIR)\Lib\x64\msodbcsql$(MSODBCSQL_VERSION).lib" >> $(OUTPUT_DIR)\gdal.opt
!ELSE
	echo MSODBCSQL_LIB = "$(MSODBCSQL_DIR)\Lib\x86\msodbcsql$(MSODBCSQL_VERSION).lib" >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
    echo MSODBCSQL_INCLUDE = "-I$(MSODBCSQL_DIR)\Include" -DMSODBCSQL_VERSION=$(MSODBCSQL_VERSION) -DMSSQL_BCP_SUPPORTED=1 >> $(OUTPUT_DIR)\gdal.opt
    cd $(GDAL_PATH)\ogr\ogrsf_frmts\mssqlspatial
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins-optional mkdir $(OUTPUT_DIR)\bin\gdal\plugins-optional
    if not exist $(OUTPUT_DIR)\bin\gdal\plugins-optional\msodbc mkdir $(OUTPUT_DIR)\bin\gdal\plugins-optional\msodbc
	xcopy /Y ogr_MSSQLSpatial.dll $(OUTPUT_DIR)\bin\gdal\plugins-optional\msodbc
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF

gdal-ecw: gdal-optfile
!IFDEF ECW_DIR
    echo ECW_PLUGIN = YES >> $(OUTPUT_DIR)\gdal.opt
    echo ECWDIR=$(ECWDIR) >> $(OUTPUT_DIR)\gdal.opt
    echo ECWLIB=$(ECWLIB) >> $(OUTPUT_DIR)\gdal.opt
    echo ECWFLAGS= $(ECWFLAGS) >> $(OUTPUT_DIR)\gdal.opt
!IF $(MSVC_VER) == 1500   
    echo _WIN32_WINNT= 0x0500 >> $(OUTPUT_DIR)\gdal.opt
!ENDIF
    cd $(GDAL_PATH)\frmts\ecw
!IFNDEF NO_CLEAN
    nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.vc plugin EXT_NMAKE_OPT=$(OUTPUT_DIR)\gdal.opt _WIN32_WINNT=0x0500
!ENDIF
!IFNDEF NO_COPY
!IFNDEF GDAL_ECW3
    if not exist $(OUTPUT_DIR)\bin\gdal\plugins mkdir $(OUTPUT_DIR)\bin\gdal\plugins
    xcopy /Y gdal_ECW_JP2ECW.dll $(OUTPUT_DIR)\bin\gdal\plugins
!ELSE
	if not exist $(OUTPUT_DIR)\bin\gdal\plugins-optional mkdir $(OUTPUT_DIR)\bin\gdal\plugins-optional
    xcopy /Y gdal_ECW_JP2ECW.dll $(OUTPUT_DIR)\bin\gdal\plugins-optional
!ENDIF
!ENDIF    
    cd $(BASE_DIR)
!ENDIF

gdal-ecw3:
	nmake gdal-ecw GDAL_ECW3=1

proj-optfile:
!IFDEF DEBUG
    echo OPTFLAGS=/nologo /MD /EHsc /Zi /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE /Fdproj.pdb > proj.opt
!ELSE
    echo OPTFLAGS=/nologo /MD /EHsc /Ox /D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE /DNDEBUG > proj.opt
!ENDIF
    echo MSVC_VER=$(MSVC_VER) >> proj.opt
    echo INSTDIR=$(OUTPUT_DIR)\bin\proj >> proj.opt
    echo PROJ_LIB_DIR=$(OUTPUT_DIR)\bin\proj\SHARE >> proj.opt
    
proj:
!IFDEF PROJ_DIR
    cd $(BASE_DIR)\$(PROJ_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
    if exist data\proj.db del data\proj.db
    if exist data\all.sql.in del data\all.sql.in
!ENDIF
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
!IFNDEF NO_BUILD
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" -DPROJ_TESTS=OFF -DCMAKE_BUILD_TYPE=$(MS_PROJECT_DIR) -DBUILD_LIBPROJ_SHARED=ON
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR) --target install
    xcopy /Y lib\$(MS_PROJECT_DIR)\proj_6_1.lib $(OUTPUT_DIR)\lib
    xcopy /Y bin\$(MS_PROJECT_DIR)\proj_6_1.dll $(OUTPUT_DIR)\bin
    if not exist $(OUTPUT_DIR)\bin\proj6 mkdir $(OUTPUT_DIR)\bin\proj6
	if not exist $(OUTPUT_DIR)\bin\proj6\apps mkdir $(OUTPUT_DIR)\bin\proj6\apps
    xcopy /Y bin\$(MS_PROJECT_DIR)\*.exe $(OUTPUT_DIR)\bin\proj6\apps
    if not exist $(OUTPUT_DIR)\bin\proj6\share mkdir $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\world $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\other.extra $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\nad27 $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\GL27 $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\nad83 $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\nad.lst $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\CH $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\ITRF2000 $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\ITRF2008 $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\ITRF2014 $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S data\proj.db $(OUTPUT_DIR)\bin\proj6\share
    xcopy /Y /S $(BASE_DIR)\$(PROJ_DATUMGRID_DIR)\* $(OUTPUT_DIR)\bin\proj6\share
    if exist $(OUTPUT_DIR)\include\proj6 rmdir /s /q $(OUTPUT_DIR)\include\proj6
	mkdir $(OUTPUT_DIR)\include\proj6
    mkdir $(OUTPUT_DIR)\include\proj6\proj
    xcopy /Y $(BASE_DIR)\$(PROJ_DIR)\include\proj\*.hpp $(OUTPUT_DIR)\include\proj6\proj
    xcopy /Y /S $(BASE_DIR)\$(PROJ_DIR)\src\proj.h $(OUTPUT_DIR)\include\proj6
    xcopy /Y /S $(BASE_DIR)\$(PROJ_DIR)\src\proj_api.h $(OUTPUT_DIR)\include\proj6
    xcopy /Y /S $(BASE_DIR)\$(PROJ_DIR)\src\proj_experimental.h $(OUTPUT_DIR)\include\proj6
    xcopy /Y /S $(BASE_DIR)\$(PROJ_DIR)\src\proj_constants.h $(OUTPUT_DIR)\include\proj6
    xcopy /Y /S $(BASE_DIR)\$(PROJ_DIR)\src\geodesic.h $(OUTPUT_DIR)\include\proj6
!ENDIF
	cd $(BASE_DIR)
!ENDIF

proj-old: proj-optfile
	cd $(PROJ_DIR)
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.vc EXT_NMAKE_OPT=$(BASE_DIR)\proj.opt
!ENDIF
!IFNDEF NO_COPY
    cd src
	xcopy /Y *.dll $(OUTPUT_DIR)\bin
	if not exist $(OUTPUT_DIR)\bin\proj mkdir $(OUTPUT_DIR)\bin\proj
	if not exist $(OUTPUT_DIR)\bin\proj\apps mkdir $(OUTPUT_DIR)\bin\proj\apps
	xcopy /Y *.exe $(OUTPUT_DIR)\bin\proj\apps
	xcopy /Y *.lib $(OUTPUT_DIR)\lib
    xcopy /Y *.h $(OUTPUT_DIR)\include
    cd ..
    cd nad
    nmake /f makefile.vc install-all EXT_NMAKE_OPT=$(BASE_DIR)\proj.opt
!ENDIF
	cd $(BASE_DIR)
	


zlib:
	cd $(ZLIB_DIR)
!IFNDEF NO_CLEAN
	nmake /f win32/Makefile.msc clean
	del *.manifest
!ENDIF
!IFNDEF NO_BUILD
!IF $(MSVC_VER) >= 1400
	nmake /f win32/Makefile.msc "LOC=/D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE"
!ELSE
	nmake /f win32/Makefile.msc
!ENDIF
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y *.dll $(OUTPUT_DIR)\bin
	xcopy /Y *.lib $(OUTPUT_DIR)\lib
    xcopy /Y *.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
	
	
minizip:
!IFDEF MINIZIP_DIR
	cd $(MINIZIP_DIR)
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	nmake /f Makefile.vc OUTPUT_DIR=$(OUTPUT_DIR)
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y *.lib $(OUTPUT_DIR)\lib
    xcopy /Y zip.h $(OUTPUT_DIR)\include
    xcopy /Y unzip.h $(OUTPUT_DIR)\include
    xcopy /Y ioapi.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
!ENDIF

	
libpng:
	cd $(LIBPNG_DIR)
!IFNDEF NO_CLEAN
	nmake /f scripts\makefile.vcwin32 clean
	del *.manifest
!ENDIF
!IFNDEF NO_BUILD
!IF $(MSVC_VER) >= 1400
	nmake /f scripts\makefile.vcwin32 "EXTRAFLAGS=/D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE"
!ELSE
	nmake /f scripts\makefile.vcwin32
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
    xcopy /Y *.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)

jpeg:
	cd $(JPEG_DIR)
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
	del *.manifest
!ENDIF
!IFNDEF NO_BUILD
!IF $(MSVC_VER) >= 1400
	nmake /f makefile.vc "EXTRAFLAGS=/D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE"
!ELSE
	nmake /f makefile.vc
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
    xcopy /Y *.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)

gd:
	cd $(GD_DIR)
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
!IFDEF WIN64
        nmake /f makefile.vc gd.lib MSVC_VER=$(MSVC_VER) WIN64=YES SDK_DIR=$(OUTPUT_DIR)
!ELSE
	nmake /f makefile.vc gd.lib MSVC_VER=$(MSVC_VER) SDK_DIR=$(OUTPUT_DIR)
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
    xcopy /Y *.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
    
    
openssl:
!IFDEF OPENSSL_DIR
!IFDEF WIN64
    cd $(OPENSSL_DIR)\x64
    perl Configure VC-WIN64A no-asm
!ELSE
    cd $(OPENSSL_DIR)\Win32
    perl Configure VC-WIN32 no-asm
!ENDIF
!IFNDEF NO_CLEAN
	nmake clean
!ENDIF
!IFNDEF NO_BUILD
    nmake
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\include\openssl mkdir $(OUTPUT_DIR)\include\openssl
	xcopy /Y inc32\openssl\*.h $(OUTPUT_DIR)\include\openssl
    xcopy /Y out32dll\libeay32.dll $(OUTPUT_DIR)\bin
    xcopy /Y out32dll\libeay32.lib $(OUTPUT_DIR)\lib
    xcopy /Y out32dll\ssleay32.dll $(OUTPUT_DIR)\bin
    xcopy /Y out32dll\ssleay32.lib $(OUTPUT_DIR)\lib
    if not exist $(OUTPUT_DIR)\bin\curl mkdir $(OUTPUT_DIR)\bin\curl
    xcopy /Y out32dll\openssl.exe $(OUTPUT_DIR)\bin\curl
!ENDIF 
    cd $(BASE_DIR)
!ENDIF    

	
openssl2:
!IFDEF OPENSSL_DIR
!IFDEF WIN64
    cd $(OPENSSL_DIR)\x64
    -del ms\*.obj
    perl Configure VC-WIN64A no-asm
    ms\do_win64a
!ELSE
    cd $(OPENSSL_DIR)\Win32
    -del ms\*.obj 
    perl Configure VC-WIN32 no-asm
    ms\do_ms
!ENDIF
!IFNDEF NO_CLEAN
	nmake /f ms\ntdll.mak vclean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f ms\ntdll.mak
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\include\openssl mkdir $(OUTPUT_DIR)\include\openssl
	xcopy /Y inc32\openssl\*.h $(OUTPUT_DIR)\include\openssl
    xcopy /Y out32dll\libeay32.dll $(OUTPUT_DIR)\bin
    xcopy /Y out32dll\libeay32.lib $(OUTPUT_DIR)\lib
    xcopy /Y out32dll\ssleay32.dll $(OUTPUT_DIR)\bin
    xcopy /Y out32dll\ssleay32.lib $(OUTPUT_DIR)\lib
    if not exist $(OUTPUT_DIR)\bin\curl mkdir $(OUTPUT_DIR)\bin\curl
    xcopy /Y out32dll\openssl.exe $(OUTPUT_DIR)\bin\curl
!ENDIF 
    cd $(BASE_DIR)
!ENDIF

get_ca_bundle:
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\curl;$(PATH)
    -if exist $(OUTPUT_DIR)\bin\curl\curl.exe $(OUTPUT_DIR)\bin\curl\curl.exe -o $(OUTPUT_DIR)\bin\curl\curl-ca-bundle.crt "https://curl.haxx.se/ca/cacert.pem"

curl2:
!IFDEF CURL_DIR
!IF $(MSVC_VER) == 1600
!IFDEF WIN64
    cd $(BASE_DIR)\$(CURL_DIR)\vc10x64
!IFNDEF NO_BUILD
    devenv /rebuild Release curl.sln /ProjectConfig "Release|x64"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(CURL_DIR)\vc10
!IFNDEF NO_BUILD
    devenv /rebuild Release curl.sln /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    cd $(BASE_DIR)\$(CURL_DIR)\vc9x64
!IFNDEF NO_BUILD
    devenv /rebuild Release curl.sln /ProjectConfig "Release|x64"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(CURL_DIR)\vc9
!IFNDEF NO_BUILD
    devenv /rebuild Release curl.sln /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    cd $(BASE_DIR)\$(CURL_DIR)\vc8x64
!IFNDEF NO_BUILD
    devenv /rebuild Release curl.sln /ProjectConfig "Release|x64"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(CURL_DIR)\vc8
!IFNDEF NO_BUILD
    devenv /rebuild Release curl.sln /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(CURL_DIR)\vc7
!IFNDEF NO_BUILD
    devenv /rebuild Release curl.sln
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    rem xcopy /Y lib\libcurl_imp.lib $(OUTPUT_DIR)\lib
    rem if exist lib\libcurl.dll xcopy /Y lib\libcurl.dll $(OUTPUT_DIR)\bin
    rem if exist lib\Release\libcurl.dll xcopy /Y lib\Release\libcurl.dll $(OUTPUT_DIR)\bin
    rem if not exist $(OUTPUT_DIR)\include\curl mkdir $(OUTPUT_DIR)\include\curl
    rem xcopy /Y /S include\*.h $(OUTPUT_DIR)\include\curl
    rem if not exist $(OUTPUT_DIR)\bin\curl mkdir $(OUTPUT_DIR)\bin\curl
    rem if exist src\curl.exe xcopy /Y src\curl.exe $(OUTPUT_DIR)\bin\curl
    rem if exist src\Release\curl.exe xcopy /Y src\Release\curl.exe $(OUTPUT_DIR)\bin\curl
!ENDIF
    cd $(BASE_DIR)
!ENDIF

curl:
!IFDEF CURL_DIR
    cd $(BASE_DIR)\$(CURL_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
	if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
!IFNDEF NO_BUILD
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(CURL_DIR)\$(CMAKE_BUILDDIR)\install" -DZLIB_LIBRARY=$(OUTPUT_DIR)\lib\zdll.lib -DZLIB_INCLUDE_DIR=$(OUTPUT_DIR)\include -DHAVE_INET_PTON=0 -DCURL_DISABLE_LDAPS=OFF -DCURL_LDAP_WIN=OFF
!IFDEF WIN64	
    devenv /rebuild Release curl.sln /Project curl /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release curl.sln /Project curl /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y lib\Release\libcurl_imp.lib $(OUTPUT_DIR)\lib
    if exist lib\libcurl.dll xcopy /Y lib\libcurl.dll $(OUTPUT_DIR)\bin
    if exist lib\Release\libcurl.dll xcopy /Y lib\Release\libcurl.dll $(OUTPUT_DIR)\bin
    if not exist $(OUTPUT_DIR)\include\curl mkdir $(OUTPUT_DIR)\include\curl
    xcopy /Y /S ..\include\curl\*.h $(OUTPUT_DIR)\include\curl
    if not exist $(OUTPUT_DIR)\bin\curl mkdir $(OUTPUT_DIR)\bin\curl
    if exist src\curl.exe xcopy /Y src\curl.exe $(OUTPUT_DIR)\bin\curl
    if exist src\Release\curl.exe xcopy /Y src\Release\curl.exe $(OUTPUT_DIR)\bin\curl
!ENDIF
    cd $(BASE_DIR)
!ENDIF

curl3:
!IFDEF CURL_DIR
!IFNDEF NO_BUILD
    cd $(BASE_DIR)\$(CURL_DIR)\winbuild
!IFNDEF NO_CLEAN
    rem nmake /f Makefile.vc clean MODE=dll
!ENDIF
!IFDEF WIN64	
    nmake /f Makefile.vc "MACHINE=x64" "MODE=dll" "VC=10" "WITH_SSL=dll" "WITH_ZLIB=dll" WITH_DEVEL=$(OUTPUT_DIR)
!ELSE
    nmake /f Makefile.vc "MACHINE=x86" "MODE=dll" "VC=10" "WITH_SSL=dll" "WITH_ZLIB=dll" WITH_DEVEL=$(OUTPUT_DIR)
!ENDIF
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y lib\libcurl_imp.lib $(OUTPUT_DIR)\lib
    if exist lib\libcurl.dll xcopy /Y lib\libcurl.dll $(OUTPUT_DIR)\bin
    if exist lib\Release\libcurl.dll xcopy /Y lib\Release\libcurl.dll $(OUTPUT_DIR)\bin
    if not exist $(OUTPUT_DIR)\include\curl mkdir $(OUTPUT_DIR)\include\curl
    xcopy /Y /S include\*.h $(OUTPUT_DIR)\include\curl
    if not exist $(OUTPUT_DIR)\bin\curl mkdir $(OUTPUT_DIR)\bin\curl
    if exist src\curl.exe xcopy /Y src\curl.exe $(OUTPUT_DIR)\bin\curl
    if exist src\Release\curl.exe xcopy /Y src\Release\curl.exe $(OUTPUT_DIR)\bin\curl
!ENDIF
    cd $(BASE_DIR)
!ENDIF

agg:
    cd $(AGG_DIR)\src
!IFNDEF NO_CLEAN
    nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
!IF $(MSVC_VER) >= 1400
    nmake /f makefile.vc agg.lib FT_DIR=$(BASE_DIR)\$(FT_DIR) "EXTRAFLAGS=/D_CRT_SECURE_NO_DEPRECATE /D_CRT_NONSTDC_NO_DEPRECATE"
!ELSE
    nmake /f makefile.vc agg.lib FT_DIR=$(BASE_DIR)\$(FT_DIR)
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
!ENDIF
    cd ..\..
!IFNDEF NO_COPY
    xcopy /Y $(AGG_DIR)\include\*.h $(OUTPUT_DIR)\include
    xcopy /Y $(AGG_DIR)\include\platform\win32\*.h $(OUTPUT_DIR)\include
    xcopy /Y $(AGG_DIR)\font_freetype\*.h $(OUTPUT_DIR)\include
!ENDIF
	
ms-optfile:
	echo MS_BASE=$(MS_PATH) > $(OUTPUT_DIR)\mapserver.opt
	echo $(SWIG_DIR) > $(OUTPUT_DIR)\doc\ms_deps.txt
	echo THREADS=-DUSE_THREAD >> $(OUTPUT_DIR)\mapserver.opt
	echo MSVC_VER=$(MSVC_VER) >> $(OUTPUT_DIR)\mapserver.opt
!IFDEF WIN64
	echo WIN64=$(WIN64) >> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF DEBUG
    echo MS_DEBUG = YES >> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
	echo SWIG=$(BASE_DIR)\$(SWIG_DIR)\swig.exe >> $(OUTPUT_DIR)\mapserver.opt
	echo GD_DIR=$(BASE_DIR)\$(GD_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo GD_LIB=$(BASE_DIR)\$(GD_DIR)\gd.lib >> $(OUTPUT_DIR)\mapserver.opt
	echo $(GD_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
	echo $(ZLIB_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
	echo $(JPEG_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!IFDEF MS_PROJ
	echo PROJ=-DUSE_PROJ -DUSE_PROJ_API_H >> $(OUTPUT_DIR)\mapserver.opt
    echo $(PROJ_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_FREETYPE
	echo FT_DIR=$(BASE_DIR)\$(FT_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo FT_LIB=$(BASE_DIR)\$(FT_LIB) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(FT_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_GEOS
	echo GEOS=-DUSE_GEOS >> $(OUTPUT_DIR)\mapserver.opt
	echo GEOS_DIR=$(BASE_DIR)\$(GEOS_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(GEOS_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_PHP
	echo PHP_HOME=$(PHP_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(PHP_VERSION) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
	echo DOT_NET=YES >> $(OUTPUT_DIR)\mapserver.opt
!IFDEF MS_PNG
	echo PNG_DIR=$(BASE_DIR)\$(LIBPNG_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo PNG_LIB=$(BASE_DIR)\$(LIBPNG_DIR)\libpng.lib $(BASE_DIR)\$(ZLIB_DIR)\zdll.lib >> $(OUTPUT_DIR)\mapserver.opt
	echo RGBA_PNG_ENABLED=-DUSE_RGBA_PNG >> $(OUTPUT_DIR)\mapserver.opt
	echo $(LIBPNG_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_GDAL
	echo GDAL=-DUSE_GDAL >> $(OUTPUT_DIR)\mapserver.opt
	echo OGR=-DUSE_OGR >> $(OUTPUT_DIR)\mapserver.opt
	echo GDAL_DIR=$(GDAL_PATH) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(GDAL_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_FCGI
        echo FCGIDEF=-DUSE_FASTCGI >> $(OUTPUT_DIR)\mapserver.opt
        echo FCGIDIR=$(BASE_DIR)\$(FCGI_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(FCGI_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_PDF
    echo PDF=-DUSE_PDF >> $(OUTPUT_DIR)\mapserver.opt
    echo PDF_DIR=$(BASE_DIR)\$(PDF_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(PDF_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_FRIBIDI
    echo FRIBIDI=-DUSE_FRIBIDI >> $(OUTPUT_DIR)\mapserver.opt
    echo FRIBIDI_DIR=$(BASE_DIR)\$(FRIBIDI_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(FRIBIDI_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_GIFLIB
    echo GIF=-DUSE_GIF >> $(OUTPUT_DIR)\mapserver.opt
    echo GIFLIB_DIR=$(BASE_DIR)\$(GIFLIB_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(GIFLIB_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_WXS
	echo WMS=-DUSE_WMS_SVR >> $(OUTPUT_DIR)\mapserver.opt
	echo CURL=-DUSE_CURL -DUSE_CURLOPT_PROXYAUTH >> $(OUTPUT_DIR)\mapserver.opt
	echo WMSCLIENT=-DUSE_WMS_LYR >> $(OUTPUT_DIR)\mapserver.opt
	echo CURL_DIR=$(BASE_DIR)\$(CURL_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo CURL_INC=-I$(BASE_DIR)\$(CURL_DIR)/include >> $(OUTPUT_DIR)\mapserver.opt
    echo CURL_LIB=$(BASE_DIR)\$(CURL_DIR)\lib\libcurl_imp.lib >> $(OUTPUT_DIR)\mapserver.opt
    echo $(CURL_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
	echo WINSOCK_LIB =WS2_32.Lib >> $(OUTPUT_DIR)\mapserver.opt
	echo WFSCLIENT= -DUSE_WFS_LYR >> $(OUTPUT_DIR)\mapserver.opt
	echo WFS=-DUSE_WFS_SVR >> $(OUTPUT_DIR)\mapserver.opt
	echo WCS=-DUSE_WCS_SVR >> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_JAVA
    echo JAVA_HOME=$(JAVA_HOME) >> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF REGEX_PATH
    echo REGEX_DIR=$(REGEX_PATH) >> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_AGG
	echo AGG=-DUSE_AGG >> $(OUTPUT_DIR)\mapserver.opt
	echo AGG_DIR=$(BASE_DIR)\$(AGG_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(AGG_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_CAIRO
	echo CAIRO=-DUSE_CAIRO -DUSE_SVG_CAIRO >> $(OUTPUT_DIR)\mapserver.opt
	echo CAIRO_DIR=$(BASE_DIR)\$(CAIRO_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo FONTCONFIG_DIR=$(BASE_DIR)\$(FONTCONFIG_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(CAIRO_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
	echo $(LIBSVG_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
	echo $(LIBSVG_CAIRO_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
	echo $(PIXMAN_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
	echo $(FONTCONFIG_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_OGL
	echo OGL=-DUSE_OGL >> $(OUTPUT_DIR)\mapserver.opt
	echo FTGL_DIR=$(BASE_DIR)\$(FTGL_DIR) >> $(OUTPUT_DIR)\mapserver.opt
	echo $(FTGL_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_POSTGIS
	echo POSTGIS=-DUSE_POSTGIS >> $(OUTPUT_DIR)\mapserver.opt
    echo POSTGIS_DIR=$(BASE_DIR)\$(PGSQL_DIR) >> $(OUTPUT_DIR)\mapserver.opt
    echo $(PGSQL_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_ICONV
    echo ICONV=-DUSE_ICONV >> $(OUTPUT_DIR)\mapserver.opt
    echo ICONV_DIR=$(OUTPUT_DIR) >> $(OUTPUT_DIR)\mapserver.opt
    echo $(ICONV_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
    echo $(GETTEXT_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_SOS
    echo SOS=-DUSE_SOS_SVR >> $(OUTPUT_DIR)\mapserver.opt
    echo XML2_ENABLED=-DUSE_LIBXML2 >> $(OUTPUT_DIR)\mapserver.opt
    echo LIBXML_DIR=$(OUTPUT_DIR) >> $(OUTPUT_DIR)\mapserver.opt
    echo $(LIBXML_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_MING
    echo MING=-DUSE_MING_FLASH >> $(OUTPUT_DIR)\mapserver.opt
    echo $(MING_DIR) >> $(OUTPUT_DIR)\doc\ms_deps.txt
!ENDIF
!IFDEF MS_OCI
    echo ORACLE_DIR = $(BASE_DIR)\$(OCI_DIR)\sdk  >> $(OUTPUT_DIR)\mapserver.opt
    echo ORACLE=-DUSE_ORACLESPATIAL  >> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
    echo INCLUDES = -Irenderers\agg\include -I$(OUTPUT_DIR)\include  \>> $(OUTPUT_DIR)\mapserver.opt
!IFDEF MS_OCI
    echo -I$(BASE_DIR)\$(OCI_DIR)\sdk\include \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF    
    echo -I$(REGEX_PATH) >> $(OUTPUT_DIR)\mapserver.opt
    echo EXTERNAL_LIBS = /LIBPATH:$(OUTPUT_DIR)\lib gd.lib libpng.lib libjpeg.lib \>> $(OUTPUT_DIR)\mapserver.opt
!IFDEF MS_PROJ
	echo proj_i.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_SOS
    echo libxml2.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_ICONV
    echo iconv.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_FCGI
    echo libfcgi.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_OCI
    echo $(BASE_DIR)\$(OCI_DIR)\sdk\lib\msvc\oci.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_PDF
    echo pdflib.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_FRIBIDI
    echo fribidi.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_GIFLIB
    echo giflib.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_POSTGIS
    echo libpqdll.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_AGG
    if not exist $(MS_PATH)\renderers\agg echo agg.lib \>> $(OUTPUT_DIR)\mapserver.opt
	echo expat.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_CAIRO
	echo cairo.lib libsvg.lib libsvg-cairo.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_OGL
	echo ftgl.lib opengl32.lib glu32.lib user32.lib gdi32.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_GDAL
	echo gdal_i.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_WXS
	echo libcurl_imp.lib WS2_32.Lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_FREETYPE
	echo $(FT_LIB) \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_GEOS
	echo geos_c.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFDEF MS_MING
    echo libming.lib \>> $(OUTPUT_DIR)\mapserver.opt
!ENDIF
    echo libjpeg.lib zdll.lib >> $(OUTPUT_DIR)\mapserver.opt
	
ms-osgeo:
!IFDEF MS_CMAKE_BUILD
    SET PATH=$(OSGEO4W_DIR)\apps\msys\bin;$(PATH)
    cd $(MS_PATH)
	xcopy /Y cgiutil.h $(OSGEO4W_DIR)\include
	xcopy /Y hittest.h $(OSGEO4W_DIR)\include
	xcopy /Y dxfcolor.h $(OSGEO4W_DIR)\include
	xcopy /Y map*.h $(OSGEO4W_DIR)\include
    cd $(CMAKE_BUILDDIR)
    xcopy /Y $(MS_PROJECT_DIR)\mapserver.dll $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\msplugin_mssql2008.dll $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\msplugin_oracle.dll $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\mapserv.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\msencrypt.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\legend.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\scalebar.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\shp2img.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\shptree.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\shptreevis.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\shptreetst.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\sortshp.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\tile4ms.exe $(OSGEO4W_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\mapserver.lib $(OSGEO4W_DIR)\lib
	xcopy /Y $(MS_PROJECT_DIR)\msplugin_mssql2008.lib $(OSGEO4W_DIR)\lib
	xcopy /Y $(MS_PROJECT_DIR)\msplugin_oracle.lib $(OSGEO4W_DIR)\lib
	xcopy /Y mapserver-config.h $(OSGEO4W_DIR)\include
	xcopy /Y mapserver-version.h $(OSGEO4W_DIR)\include
	$(OSGEO4W_DIR)\bin\mapserv -v
	cd $(OSGEO4W_DIR)
	tar -cvf mapserver$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar lib/mapserver.lib lib/msplugin_mssql2008.lib lib/msplugin_oracle.lib include/map*.h include/cgiutil.h include/hittest.h include/dxfcolor.h bin/mapserver.dll bin/msplugin_mssql2008.dll bin/msplugin_oracle.dll bin/mapserv.exe bin/msencrypt.exe bin/legend.exe bin/scalebar.exe bin/shp2img.exe bin/shptree.exe bin/shptreevis.exe bin/shptreetst.exe bin/sortshp.exe  bin/tile4ms.exe
	bzip2 -f mapserver$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar
	if not exist $(RSYNC_DIR)\$(OSGEO4W_ARCH) mkdir $(RSYNC_DIR)\$(OSGEO4W_ARCH)
	if not exist $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG) mkdir $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)
	xcopy /Y $(OSGEO4W_DIR)\mapserver$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar.bz2 $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)
	echo rsync -avz --chmod 744 -e "./ssh -i ./id_rsa" ./$(OSGEO4W_ARCH)/mapserver$(MS_OSGEO4W_PKG)/mapserver$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar.bz2 tamas@upload.osgeo.org:/osgeo/download/osgeo4w/$(OSGEO4W_ARCH)/release/mapserver/mapserver$(MS_OSGEO4W_PKG)/ >>$(RSYNC_DIR)\upload$(OSGEO4W_ARCH).bat
	
	cd $(BASE_DIR)
!ENDIF

ms-python-osgeo:
!IFDEF MS_CMAKE_BUILD
    SET PATH=$(OSGEO4W_DIR)\apps\msys\bin;$(PATH)
    cd $(MS_PATH)
	xcopy /Y $(OUTPUT_DIR)\bin\ms\python\mapscript.py $(OSGEO4W_DIR)\apps\Python27\Lib\site-packages
	xcopy /Y $(OUTPUT_DIR)\bin\ms\python\_mapscript.pyd $(OSGEO4W_DIR)\apps\Python27\Lib\site-packages
	xcopy /Y $(OUTPUT_DIR)\lib\ms-python\_mapscript.lib $(OSGEO4W_DIR)\apps\Python27\libs
	cd $(OSGEO4W_DIR)
	tar -cvf mapscript-python$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar apps/Python27/Lib/site-packages/mapscript.py apps/Python27/Lib/site-packages/_mapscript.pyd apps/Python27/libs/_mapscript.lib
	bzip2 -f mapscript-python$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar
	if not exist $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)\mapscript-python$(MS_OSGEO4W_PKG) mkdir $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)\mapscript-python$(MS_OSGEO4W_PKG)
	xcopy /Y $(OSGEO4W_DIR)\mapscript-python$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar.bz2  $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)\mapscript-python$(MS_OSGEO4W_PKG)
	echo rsync -avz --chmod 744 -e "./ssh -i ./id_rsa" ./$(OSGEO4W_ARCH)/mapserver$(MS_OSGEO4W_PKG)/mapscript-python$(MS_OSGEO4W_PKG)/mapscript-python$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar.bz2 tamas@upload.osgeo.org:/osgeo/download/osgeo4w/$(OSGEO4W_ARCH)/release/mapserver/mapserver$(MS_OSGEO4W_PKG)/mapscript-python$(MS_OSGEO4W_PKG)/ >>$(RSYNC_DIR)\upload$(OSGEO4W_ARCH).bat
	cd $(BASE_DIR)
!ENDIF

ms-java-osgeo:
!IFDEF MS_CMAKE_BUILD
    SET PATH=$(OSGEO4W_DIR)\apps\msys\bin;$(PATH)
    cd $(MS_PATH)
	xcopy /Y $(OUTPUT_DIR)\bin\ms\java\mapscript.jar $(OSGEO4W_DIR)\lib
	xcopy /Y $(OUTPUT_DIR)\bin\ms\java\javamapscript.dll $(OSGEO4W_DIR)\bin
	if not exist $(OSGEO4W_DIR)\apps\mapserver mkdir $(OSGEO4W_DIR)\apps\mapserver
	if not exist $(OSGEO4W_DIR)\apps\mapserver\java mkdir $(OSGEO4W_DIR)\apps\mapserver\java
	xcopy /Y $(MS_PATH)\mapscript\java\examples\*.java $(OSGEO4W_DIR)\apps\mapserver\java
	cd $(OSGEO4W_DIR)
	tar -cvf mapscript-java$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar apps/mapserver/java/*.java bin/javamapscript.dll lib/mapscript.jar
	bzip2 -f mapscript-java$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar
	if not exist $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)\mapscript-java$(MS_OSGEO4W_PKG) mkdir $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)\mapscript-java$(MS_OSGEO4W_PKG)
	xcopy /Y $(OSGEO4W_DIR)\mapscript-java$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar.bz2  $(RSYNC_DIR)\$(OSGEO4W_ARCH)\mapserver$(MS_OSGEO4W_PKG)\mapscript-java$(MS_OSGEO4W_PKG)
	echo rsync -avz --chmod 744 -e "./ssh -i ./id_rsa" ./$(OSGEO4W_ARCH)/mapserver$(MS_OSGEO4W_PKG)/mapscript-java$(MS_OSGEO4W_PKG)/mapscript-java$(MS_OSGEO4W_PKG)-$(MS_OSGEO4W_VER)-$(MS_OSGEO4W_BUILD).tar.bz2 tamas@upload.osgeo.org:/osgeo/download/osgeo4w/$(OSGEO4W_ARCH)/release/mapserver/mapserver$(MS_OSGEO4W_PKG)/mapscript-java$(MS_OSGEO4W_PKG)/ >>$(RSYNC_DIR)\upload$(OSGEO4W_ARCH).bat
	
	cd $(BASE_DIR)
!ENDIF

upload-osgeo:
	cd $(RSYNC_DIR)
	upload$(OSGEO4W_ARCH).bat
	SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\curl;$(PATH)
    -if exist $(OUTPUT_DIR)\bin\curl\curl.exe $(OUTPUT_DIR)\bin\curl\curl.exe "http://upload.osgeo.org/cgi-bin/osgeo4w-regen.sh"
	-if exist $(OUTPUT_DIR)\bin\curl\curl.exe $(OUTPUT_DIR)\bin\curl\curl.exe "http://upload.osgeo.org/cgi-bin/osgeo4w-promote.sh"
	cd $(BASE_DIR)

fribidi-osgeo:
    SET PATH=$(OSGEO4W_DIR)\apps\msys\bin;$(PATH)
    xcopy /Y $(OUTPUT_DIR)\lib\fribidi.lib $(OSGEO4W_DIR)\lib
	xcopy /Y $(OUTPUT_DIR)\bin\fribidi.dll $(OSGEO4W_DIR)\bin
	xcopy /Y $(OUTPUT_DIR)\include\fribidi*.h $(OSGEO4W_DIR)\include
	cd $(OSGEO4W_DIR)
	tar -cvf $(FRIBIDI_DIR)-1.tar lib/fribidi.lib include/fribidi*.h bin/fribidi.dll
	bzip2 -f $(FRIBIDI_DIR)-1.tar
	if exist $(OUTPUT_DIR)\install xcopy /Y $(OSGEO4W_DIR)\$(FRIBIDI_DIR)-1.tar.bz2 $(INSTALL_DIR)\release-$(COMPILER_VER)-$(PKG_VERSION)
	cd $(BASE_DIR)
	
cairo-osgeo:
    SET PATH=$(OSGEO4W_DIR)\apps\msys\bin;$(PATH)
    xcopy /Y $(OUTPUT_DIR)\lib\cairo.lib $(OSGEO4W_DIR)\lib
	xcopy /Y $(OUTPUT_DIR)\bin\cairo.dll $(OSGEO4W_DIR)\bin
	xcopy /Y $(OUTPUT_DIR)\include\cairo*.h $(OSGEO4W_DIR)\include
	cd $(OSGEO4W_DIR)
	tar -cvf $(CAIRO_DIR)-1.tar lib/cairo.lib include/cairo*.h bin/cairo.dll
	bzip2 -f $(CAIRO_DIR)-1.tar
	if exist $(OUTPUT_DIR)\install xcopy /Y $(OSGEO4W_DIR)\$(CAIRO_DIR)-1.tar.bz2 $(INSTALL_DIR)\release-$(COMPILER_VER)-$(PKG_VERSION)
	cd $(BASE_DIR)
	
curl:
!IFDEF CURL_DIR
    cd $(BASE_DIR)\$(CURL_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
	if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
!IFNDEF NO_BUILD
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(CURL_DIR)\$(CMAKE_BUILDDIR)\install" -DZLIB_LIBRARY=$(OUTPUT_DIR)\lib\zdll.lib -DZLIB_INCLUDE_DIR=$(OUTPUT_DIR)\include -DHAVE_INET_PTON=0
!IFDEF WIN64	
    devenv /rebuild Release curl.sln /Project curl /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release curl.sln /Project curl /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y lib\Release\libcurl_imp.lib $(OUTPUT_DIR)\lib
    if exist lib\libcurl.dll xcopy /Y lib\libcurl.dll $(OUTPUT_DIR)\bin
    if exist lib\Release\libcurl.dll xcopy /Y lib\Release\libcurl.dll $(OUTPUT_DIR)\bin
    if not exist $(OUTPUT_DIR)\include\curl mkdir $(OUTPUT_DIR)\include\curl
    xcopy /Y /S ..\include\curl\*.h $(OUTPUT_DIR)\include\curl
    if not exist $(OUTPUT_DIR)\bin\curl mkdir $(OUTPUT_DIR)\bin\curl
    if exist src\curl.exe xcopy /Y src\curl.exe $(OUTPUT_DIR)\bin\curl
    if exist src\Release\curl.exe xcopy /Y src\Release\curl.exe $(OUTPUT_DIR)\bin\curl
!ENDIF
    cd $(BASE_DIR)
!ENDIF

mapcache-build:
!IFDEF MAPCACHE_DIR
    cd $(BASE_DIR)\$(MAPCACHE_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=release-$(COMPILER_VER)" "-DJPEG_LIBRARY=$(OUTPUT_DIR)\lib\libjpeg.lib" "-DFCGI_LIBRARY=$(OUTPUT_DIR)\lib\libfcgi.lib" "-DAPR_INCLUDE_DIR=$(OUTPUT_DIR)\include" "-DAPR_LIBRARY=$(OUTPUT_DIR)\lib\libapr-1.lib" "-DAPU_LIBRARY=$(OUTPUT_DIR)\lib\libaprutil-1.lib" "-DZLIB_LIBRARY=$(OUTPUT_DIR)\lib\zdll.lib" "-DWITH_APACHE=OFF" "-DWITH_MAPSERVER=ON" "-DMAPSERVER_INCLUDE_DIR=$(OUTPUT_DIR)\include" "-DMAPSERVER_LIBRARY=$(OUTPUT_DIR)\lib\mapserver.lib" "-DBUILD_SHARED_LIBS=OFF" "-DSQLITE_LIBRARY=$(OUTPUT_DIR)\lib\spatialite_i.lib" "-DWITH_FCGI=OFF"
!IFDEF WIN64	
    devenv /rebuild Release mapcache.sln /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release mapcache.sln /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(MAPCACHE_DIR)\$(CMAKE_BUILDDIR)\Release\*.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(MAPCACHE_DIR)\$(CMAKE_BUILDDIR)\cgi\Release\*.exe $(OUTPUT_DIR)\bin\ms\apps
	xcopy /Y $(BASE_DIR)\$(MAPCACHE_DIR)\$(CMAKE_BUILDDIR)\util\Release\*.exe $(OUTPUT_DIR)\bin\ms\apps
!ENDIF
    cd $(BASE_DIR)
!ENDIF

tiff-build:
!IFDEF POPPLER_DIR
    cd $(BASE_DIR)\$(LIBTIFF_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)"
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR) --target install
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\$(CMAKE_BUILDDIR)\libtiff\$(MS_PROJECT_DIR)\tiff.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\$(CMAKE_BUILDDIR)\libtiff\$(MS_PROJECT_DIR)\tiff.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\$(CMAKE_BUILDDIR)\libtiff\$(MS_PROJECT_DIR)\tiffxx.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\libtiff\tiff.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\libtiff\tiffio.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\libtiff\tiffvers.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\$(CMAKE_BUILDDIR)\libtiff\tiffconf.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
!ENDIF

geotiff-build:
!IFDEF POPPLER_DIR
    cd $(BASE_DIR)\$(LIBGEOTIFF_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)"
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR) --target install
	xcopy /Y $(BASE_DIR)\$(LIBGEOTIFF_DIR)\$(CMAKE_BUILDDIR)\lib\$(MS_PROJECT_DIR)\geotiff.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(LIBGEOTIFF_DIR)\$(CMAKE_BUILDDIR)\lib\$(MS_PROJECT_DIR)\xtiff.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(LIBGEOTIFF_DIR)\*.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(LIBGEOTIFF_DIR)\*.inc $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(LIBGEOTIFF_DIR)\$(CMAKE_BUILDDIR)\*.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
!ENDIF

laszip:
!IFDEF LASZIP_DIR
    cd $(BASE_DIR)\$(LASZIP_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)"
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR) --target install
!ENDIF
	cd $(BASE_DIR)
!ENDIF

pdal-build:
!IFDEF POPPLER_DIR
    cd $(BASE_DIR)\pdal
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    set ORACLE_HOME=
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" "-DGEOS_LIBRARY=$(OUTPUT_DIR:\=/)/lib/geos_c.lib" "-DGEOS_INCLUDE_DIR=$(OUTPUT_DIR:\=/)/include" "-DPOSTGRESQL_LIBRARIES=$(OUTPUT_DIR:\=/)/lib/libpqdll.lib"
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR) --target install
!ENDIF
	cd $(BASE_DIR)
!ENDIF

pcre-build:
!IFDEF MAPCACHE_DIR
    cd $(BASE_DIR)\$(PCRE_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" 
!IFDEF WIN64	
    devenv /rebuild Release PCRE.sln /Project pcre /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release PCRE.sln /Project pcre /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(PCRE_DIR)\$(CMAKE_BUILDDIR)\Release\pcre.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(PCRE_DIR)\$(CMAKE_BUILDDIR)\pcre.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

apr-build:
!IFDEF MAPCACHE_DIR
    cd $(BASE_DIR)\$(APR_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" 
!IFDEF WIN64	
    devenv /rebuild Release apr.sln /Project libapr-1 /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release apr.sln /Project libapr-1 /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(APR_DIR)\$(CMAKE_BUILDDIR)\Release\libapr-1.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(APR_DIR)\$(CMAKE_BUILDDIR)\Release\libapr-1.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(APR_DIR)\$(CMAKE_BUILDDIR)\apr.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(APR_DIR)\include\apr*.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

apr-util-build:
!IFDEF MAPCACHE_DIR
    cd $(BASE_DIR)\$(APR_UTIL_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" "-DAPU_HAVE_CRYPTO=ON"
!IFDEF WIN64	
    devenv /rebuild Release APR-Util.sln /Project libaprutil-1 /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release APR-Util.sln /Project libaprutil-1 /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(APR_UTIL_DIR)\$(CMAKE_BUILDDIR)\Release\libaprutil-1.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(APR_UTIL_DIR)\$(CMAKE_BUILDDIR)\Release\expat.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(APR_UTIL_DIR)\$(CMAKE_BUILDDIR)\Release\libaprutil-1.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(APR_UTIL_DIR)\$(CMAKE_BUILDDIR)\*.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(APR_UTIL_DIR)\include\*.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF
	
ms-cmake:
!IFDEF MS_CMAKE_BUILD
    cd $(MS_PATH)
	set JAVA_HOME=$(JAVA_HOME)
	set ORACLE_HOME=$(OCI_DIR)
	set PYTHONPATH=$(BASE_DIR)\$(PYTHON_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
	if exist build rd /Q /S build
!ENDIF
	if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR);$(BASE_DIR)\$(OCI_DIR)\sdk\lib\msvc" "-DJPEG_LIBRARY=$(OUTPUT_DIR:\=/)/lib/libjpeg.lib" "-DZLIB_LIBRARY=$(OUTPUT_DIR:\=/)/lib/zdll.lib" -DWITH_THREADS=1 $(MS_CMAKE_PYTHON) $(MS_CMAKE_JAVA) -DWITH_CSHARP=1 -DWITH_PHP=0 -DWITH_ORACLESPATIAL=0 $(MS_CMAKE_ORACLE) -DWITH_MSSQL2008=1 -DWITH_KML=1 -DWITH_SVGCAIRO=1 "-DSVG_LIBRARY=$(OUTPUT_DIR)\lib\libsvg.lib" "-DSVGCAIRO_LIBRARY=$(OUTPUT_DIR:\=/)/lib/libsvg-cairo.lib" "-DSWIG_EXECUTABLE=$(BASE_DIR)\SWIG-1.3.39\swig.exe" "-DHARFBUZZ_INCLUDE_DIR=$(OUTPUT_DIR:\=/)/include/harfbuzz" -DWITH_GD=1 "-DGD_LIBRARY=$(OUTPUT_DIR:\=/)/lib/gd.lib" "-DPOSTGRESQL_LIBRARY=$(OUTPUT_DIR:\=/)/lib/libpqdll.lib" "-DFREETYPE_LIBRARY=$(OUTPUT_DIR:\=/)/lib/freetype.lib" "-DPROTOBUFC_COMPILER=$(OUTPUT_DIR:\=/)/bin/protoc.exe" "-DPROTOBUFC_INCLUDE_DIR=$(OUTPUT_DIR:\=/)/include/protobuf-c" "-DPROJ_LIBRARY=$(OUTPUT_DIR:\=/)/lib/proj_i.lib" -DWITH_CLIENT_WMS=1 -DWITH_CLIENT_WFS=1 -DWITH_SOS=1 -DREGEX_DIR=$(REGEX_PATH:\=/) -DMS_EXTERNAL_LIBS=WS2_32.Lib "-DCMAKE_CXX_FLAGS_RELEASE=/MD /Oi /Ob2 /D NDEBUG" "-DCMAKE_C_FLAGS_RELEASE=/MD /Oi /Ob2 /D NDEBUG" "-DWITH_THREAD_SAFETY=1" "-DPKG_CONFIG_EXECUTABLE=C:\Program Files (x86)\Mono\bin\pkg-config.exe" "-DICONV_DLL=$(OUTPUT_DIR:\=/)/bin/iconv.dll" -DCMAKE_BUILD_TYPE=$(MS_PROJECT_DIR)
	cd $(BASE_DIR)
!ENDIF

ms: ms-optfile ms-cmake
	cd $(MS_PATH)
!IFDEF MS_CMAKE_BUILD
    cd $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_CLEAN
!IFDEF MS_CMAKE_BUILD
    devenv /clean $(MS_PROJECT_DIR) MapServer.sln /ProjectConfig $(MS_PROJECT_CONFIG)
!ELSE
	del mapscriptvars
	nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!ENDIF
!IFNDEF NO_BUILD
!IFDEF MS_CMAKE_BUILD
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ELSE
	nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\ms mkdir $(OUTPUT_DIR)\bin\ms
	if not exist $(OUTPUT_DIR)\bin\ms\apps mkdir $(OUTPUT_DIR)\bin\ms\apps
!IFDEF MS_CMAKE_BUILD
    xcopy /Y $(MS_PROJECT_DIR)\*.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(MS_PROJECT_DIR)\*.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(MS_PROJECT_DIR)\*.exe $(OUTPUT_DIR)\bin\ms\apps
	xcopy /Y $(MS_PATH)\$(CMAKE_BUILDDIR)\mapserver*.h $(OUTPUT_DIR)\include
	xcopy /Y $(MS_PATH)\map*.h $(OUTPUT_DIR)\include
	xcopy /Y $(MS_PATH)\cgiutil.h $(OUTPUT_DIR)\include
	xcopy /Y $(MS_PATH)\hittest.h $(OUTPUT_DIR)\include
!ELSE
	xcopy /Y *.dll $(OUTPUT_DIR)\bin
	xcopy /Y *.exe $(OUTPUT_DIR)\bin\ms\apps
!ENDIF
!ENDIF
	cd ..
!IFNDEF NO_COPY
	cd $(OUTPUT_DIR)\bin
	ms\apps\mapserv -v > $(OUTPUT_DIR)\doc\ms_version.txt
!ENDIF
	cd $(BASE_DIR)
	
ms-csharp:
!IFDEF MS_CSHARP
!IFDEF MS_CMAKE_BUILD
    cd $(MS_PATH)\$(CMAKE_BUILDDIR)
!ELSE
	cd $(MS_PATH)\mapscript\csharp
!ENDIF
!IFNDEF NO_CLEAN
!IFNDEF MS_CMAKE_BUILD
    nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ELSE
    del *.cs
!ENDIF
!ENDIF
!IFNDEF NO_BUILD
!IFDEF MS_CMAKE_BUILD
    rem devenv /build $(MS_PROJECT_DIR) MapServer.sln /Project csharpmapscript /ProjectConfig $(MS_PROJECT_CONFIG)
!ELSE
	nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\ms\csharp mkdir $(OUTPUT_DIR)\bin\ms\csharp
	
!IFDEF MS_CMAKE_BUILD
    xcopy /Y mapscript\csharp\$(MS_PROJECT_DIR)\*.dll $(OUTPUT_DIR)\bin\ms\csharp
	xcopy /Y mapscript\csharp\*.dll $(OUTPUT_DIR)\bin\ms\csharp
	xcopy /Y mapscript\csharp\*.exe $(OUTPUT_DIR)\bin\ms\csharp
!ELSE
	xcopy /Y *.dll $(OUTPUT_DIR)\bin\ms\csharp
	xcopy /Y *.exe $(OUTPUT_DIR)\bin\ms\csharp
!ENDIF
!ENDIF
	cd $(BASE_DIR)
!ENDIF

ms-csharp-test:	
!IFDEF MS_CSHARP
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\ms\csharp;$(PATH)
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
	cd $(MS_PATH)\mapscript\csharp
	nmake /f makefile.vc test EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
	cd $(BASE_DIR)
!ENDIF

ms-java: ms-optfile
!IFDEF MS_JAVA
!IFDEF MS_CMAKE_BUILD
    cd $(MS_PATH)\$(CMAKE_BUILDDIR)
!ELSE
	cd $(MS_PATH)\mapscript\java
!ENDIF
!IFNDEF NO_CLEAN
!IFNDEF MS_CMAKE_BUILD
    nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ELSE
    -rmdir /s /q edu
!ENDIF
!ENDIF
!IFNDEF NO_BUILD
!IFDEF MS_CMAKE_BUILD
    rem devenv /build $(MS_PROJECT_DIR) MapServer.sln /Project javamapscript /ProjectConfig $(MS_PROJECT_CONFIG)
!ELSE
    nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\ms\java mkdir $(OUTPUT_DIR)\bin\ms\java
!IFDEF MS_CMAKE_BUILD
    xcopy /Y mapscript\java\$(MS_PROJECT_DIR)\*.dll $(OUTPUT_DIR)\bin\ms\java
	xcopy /Y mapscript\java\*.jar $(OUTPUT_DIR)\bin\ms\java
!ELSE
    xcopy /Y *.dll $(OUTPUT_DIR)\bin\ms\java
	xcopy /Y *.jar $(OUTPUT_DIR)\bin\ms\java
!ENDIF
!ENDIF
	cd $(BASE_DIR)
!ENDIF

ms-java-test:
!IFNDEF MS_CMAKE_BUILD	
!IFDEF MS_JAVA
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\ms\java;$(PATH)
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
	cd $(MS_PATH)\mapscript\java
	nmake /f makefile.vc test EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
	cd $(BASE_DIR)
!ENDIF
!ENDIF

ms-python:
!IFDEF MS_PYTHON
    SET DISTUTILS_USE_SDK=1
    SET MSSdk=1
    SET LIB=%LIB%;$(OUTPUT_DIR)\lib
!IFDEF MS_CMAKE_BUILD
    cd $(MS_PATH)\$(CMAKE_BUILDDIR)
!ELSE
	cd $(MS_PATH)\mapscript\python
!ENDIF
!IFNDEF NO_CLEAN
!IFNDEF MS_CMAKE_BUILD
	-del mapscript_wrap.c
    -rmdir /s /q build
!ENDIF
!ENDIF
!IFNDEF NO_BUILD
!IFDEF MS_CMAKE_BUILD
    rem devenv /build $(MS_PROJECT_DIR) MapServer.sln /Project _pythonmapscript /ProjectConfig $(MS_PROJECT_CONFIG)
!ELSE
	$(BASE_DIR)\$(SWIG_DIR)\swig.exe -python -shadow -o mapscript_wrap.c ../mapscript.i
!ENDIF
!ENDIF
!IFNDEF MS_CMAKE_BUILD
!IFNDEF NO_CLEAN
	$(BASE_DIR)\$(PYTHON_DIR)\python.exe setup.py clean
!ENDIF
!IFNDEF NO_BUILD
	$(BASE_DIR)\$(PYTHON_DIR)\python.exe setup.py build
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\ms\python mkdir $(OUTPUT_DIR)\bin\ms\python
	if not exist $(OUTPUT_DIR)\lib\ms-python mkdir $(OUTPUT_DIR)\lib\ms-python
!IFDEF MS_CMAKE_BUILD
    xcopy /Y mapscript\python\mapscript.py $(OUTPUT_DIR)\bin\ms\python
	xcopy /Y mapscript\python\$(MS_PROJECT_DIR)\*.pyd $(OUTPUT_DIR)\bin\ms\python
	xcopy /Y mapscript\python\$(MS_PROJECT_DIR)\*.lib $(OUTPUT_DIR)\lib\ms-python
!ELSE
    cd ..\$(PYTHON_OUTDIR)
	xcopy /Y *.py $(OUTPUT_DIR)\bin\ms\python
	xcopy /Y *.pyd $(OUTPUT_DIR)\bin\ms\python
	xcopy /Y *.lib $(OUTPUT_DIR)\lib\ms-python
!ENDIF
!ENDIF
	cd $(BASE_DIR)
!ENDIF

ms-python-bdist:
!IFNDEF MS_CMAKE_BUILD
!IFDEF MS_PYTHON
    SET DISTUTILS_USE_SDK=1
    SET MSSdk=1
    SET LIB=%LIB%;$(OUTPUT_DIR)\lib
	cd $(MS_PATH)\mapscript\python
!IFNDEF NO_CLEAN
	-rmdir /s /q dist
!ENDIF
!IFNDEF NO_BUILD
	$(BASE_DIR)\$(PYTHON_DIR)\python.exe setup.py bdist --formats=wininst
!ENDIF
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\install mkdir $(OUTPUT_DIR)\install
	xcopy /Y dist\*.exe $(OUTPUT_DIR)\install
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF

ms-php: ms-optfile
!IFNDEF MS_CMAKE_BUILD
!IFDEF MS_PHP
    cd $(MS_PATH)\mapscript\php
!IFNDEF NO_CLEAN
	nmake /f makefile.vc clean EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!IFNDEF NO_BUILD
	nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
	if exist php_mapscript.dll.manifest mt -manifest php_mapscript.dll.manifest -outputresource:php_mapscript.dll;2
	rem nmake /f makefile.vc proj EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF	
!IFNDEF NO_COPY
	if not exist $(OUTPUT_DIR)\bin\ms\php mkdir $(OUTPUT_DIR)\bin\ms\php
	xcopy /Y *.dll $(OUTPUT_DIR)\bin\ms\php
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF

ms-php-test:	
!IFDEF MS_PHP
    SET PATH=$(OUTPUT_DIR)\bin;$(PHP_BINDIR);$(PATH)
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
    cd $(MS_PATH)\tests
    del /Q *_0.png
    del /Q *_1.png
    del /Q *_2.png
    copy /Y test.map demo.map
	php.exe -d extension_dir=$(OUTPUT_DIR)\bin\ms\php -f $(MS_PATH)\mapscript\php\examples\test_draw_map.phtml >$(MS_PATH)\tests\phptest.html
	cd $(BASE_DIR)
!ENDIF

ms-cairo: ms-optfile
!IFNDEF WIN64
    echo INCLUDES = -I$(OUTPUT_DIR)\include -I$(REGEX_PATH) -I$(BASE_DIR)\$(CAIRO_DIR)\src -I$(BASE_DIR)\$(FONTCONFIG_DIR) >> $(OUTPUT_DIR)\mapserver.opt
    cd $(MS_PATH)
    nmake /f makefile.vc EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
	cd $(BASE_DIR)
!ENDIF

ms-autotest: update-ms-autotest
    SET MS_ERRORFILE=stderr
    SET MS_DEBUGLEVEL=1
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\ms\apps;$(OUTPUT_DIR)\bin\ms\python;$(BASE_DIR)\pdiff\bin;$(BASE_DIR)\$(SDE_DIR);$(BASE_DIR)\$(OCI_DIR);$(FILEGDB_BINPATH);$(PATH)
  
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
    SET GDAL_DRIVER_PATH=$(OUTPUT_DIR)\bin\gdal\plugins;$(OUTPUT_DIR)\bin\gdal\plugins-external
    SET GDAL_DATA=$(GDAL_PATH)\data
    cd $(MSAUTOTEST_DIR)
    cd gdal
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd..
    cd misc
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd..
    cd wxs
    rem $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd..
!IF EXIST ($(MSAUTOTEST_DIR)\renderers)    
    cd renderers
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd..
!ENDIF
!IF EXIST ($(MSAUTOTEST_DIR)\query)    
    cd query
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd..
!ENDIF
    cd $(BASE_DIR)
    
ms-autotest2:
    SET MS_ERRORFILE=stderr
    SET MS_DEBUGLEVEL=1
    SET PATH=$(OUTPUT_DIR)\bin;$(OUTPUT_DIR)\bin\debug;$(OUTPUT_DIR)\bin\ms\apps;$(OUTPUT_DIR)\bin\ms\python;$(BASE_DIR)\pdiff\bin;$(PATH)
    SET PROJ_LIB=$(OUTPUT_DIR)\bin\proj\SHARE
    cd $(MSAUTOTEST_DIR)
    cd gdal
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd..
    cd misc
    $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd..
    cd wxs
    rem $(BASE_DIR)\$(PYTHON_DIR)\python.exe run_test.py
    cd $(BASE_DIR)
	
ms-sde: ms-optfile
!IFDEF MS_SDE
!IFDEF SDE_DIR
!IFNDEF WIN64
    echo SDE_OPT=-DUSE_SDE_PLUGIN >> $(OUTPUT_DIR)\mapserver.opt
    echo SDE_VERSION=$(SDE_VERSION)  >> $(OUTPUT_DIR)\mapserver.opt
    echo SDE_DIR=$(BASE_DIR)\$(SDE_DIR)  >> $(OUTPUT_DIR)\mapserver.opt
    echo SDE_INC=-I$(BASE_DIR)\$(SDE_DIR)\include  >> $(OUTPUT_DIR)\mapserver.opt
    echo INCLUDES = -I$(OUTPUT_DIR)\include -I$(REGEX_PATH) -I$(BASE_DIR)\$(SDE_DIR)\include >> $(OUTPUT_DIR)\mapserver.opt
    if not exist $(OUTPUT_DIR)\bin\ms\plugins mkdir $(OUTPUT_DIR)\bin\ms\plugins
    cd $(MS_PATH)
!IFDEF MS_CMAKE_BUILD
    cd $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_COPY
!IFNDEF MS_CMAKE_BUILD
    del msplugin_sde_*
    -del mapsde.obj
!ENDIF
!ENDIF
!IFNDEF NO_BUILD
!IFDEF MS_CMAKE_BUILD
    rem devenv /build $(MS_PROJECT_DIR) MapServer.sln /Project msplugin_sde91 /ProjectConfig $(MS_PROJECT_CONFIG)
!ELSE
    nmake /f makefile.vc plugins EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\ms\plugins\sde mkdir $(OUTPUT_DIR)\bin\ms\plugins\sde
!IFDEF MS_CMAKE_BUILD
    xcopy /Y $(MS_PROJECT_DIR)\msplugin_sde_*.dll $(OUTPUT_DIR)\bin\ms\plugins\sde
!ELSE
    xcopy /Y msplugin_sde_*.dll $(OUTPUT_DIR)\bin\ms\plugins\sde
!ENDIF
!ENDIF
	cd $(BASE_DIR)
!ENDIF
!ENDIF
!ENDIF

ms-oci: ms-optfile
!IFDEF OCI_DIR
    echo ORACLE_DIR = $(BASE_DIR)\$(OCI_DIR)\sdk  >> $(OUTPUT_DIR)\mapserver.opt
    echo ORACLE=-DUSE_ORACLE_PLUGIN  >> $(OUTPUT_DIR)\mapserver.opt
    echo INCLUDES = -I$(OUTPUT_DIR)\include -I$(REGEX_PATH) -I$(BASE_DIR)\$(OCI_DIR)\sdk\include >> $(OUTPUT_DIR)\mapserver.opt
    echo ORACLE_LIB=$(BASE_DIR)\$(OCI_DIR)\sdk\lib\msvc\oci.lib >> $(OUTPUT_DIR)\mapserver.opt
    if not exist $(OUTPUT_DIR)\bin\ms\plugins mkdir $(OUTPUT_DIR)\bin\ms\plugins
    cd $(MS_PATH)
!IFDEF MS_CMAKE_BUILD
    cd $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_CLEAN
!IFNDEF MS_CMAKE_BUILD
    del msplugin_oracle*
    -del maporaclespatial.obj
!ENDIF
!ENDIF
!IFNDEF NO_BUILD
!IFDEF MS_CMAKE_BUILD
    rem devenv /build $(MS_PROJECT_DIR) MapServer.sln /Project msplugin_oracle /ProjectConfig $(MS_PROJECT_CONFIG)
!ELSE
    nmake /f makefile.vc plugins EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\ms\plugins\oci mkdir $(OUTPUT_DIR)\bin\ms\plugins\oci
!IFDEF MS_CMAKE_BUILD
    xcopy /Y $(MS_PROJECT_DIR)\msplugin_oracle.dll $(OUTPUT_DIR)\bin\ms\plugins\oci
!ELSE
    xcopy /Y msplugin_oracle.dll $(OUTPUT_DIR)\bin\ms\plugins\oci
!ENDIF
!ENDIF
	cd $(BASE_DIR)
!ENDIF

ms-sql2008: ms-optfile
    echo MSSQL2008=YES  >> $(OUTPUT_DIR)\mapserver.opt
    echo ODBC_LIBS=odbc32.lib odbccp32.lib user32.lib  >> $(OUTPUT_DIR)\mapserver.opt
    if not exist $(OUTPUT_DIR)\bin\ms\plugins mkdir $(OUTPUT_DIR)\bin\ms\plugins
    cd $(MS_PATH)
!IFDEF MS_CMAKE_BUILD
    cd $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_CLEAN
!IFNDEF MS_CMAKE_BUILD
    del msplugin_mssql2008*
!ENDIF
!ENDIF
!IFNDEF NO_BUILD
!IFDEF MS_CMAKE_BUILD
    rem devenv /build $(MS_PROJECT_DIR) MapServer.sln /Project msplugin_mssql2008 /ProjectConfig $(MS_PROJECT_CONFIG)
!ELSE
    nmake /f makefile.vc plugins EXT_NMAKE_OPT=$(OUTPUT_DIR)\mapserver.opt
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\ms\plugins\mssql2008 mkdir $(OUTPUT_DIR)\bin\ms\plugins\mssql2008
!IFDEF MS_CMAKE_BUILD
    xcopy /Y $(MS_PROJECT_DIR)\msplugin_mssql2008.dll $(OUTPUT_DIR)\bin\ms\plugins\mssql2008
!ELSE
	xcopy /Y msplugin_mssql2008.dll $(OUTPUT_DIR)\bin\ms\plugins\mssql2008
!ENDIF
!ENDIF
	cd $(BASE_DIR) 

pgsql:
    cd $(PGSQL_DIR)\src\interfaces\libpq
!IFNDEF NO_CLEAN
    nmake /f win32.mak clean
!ENDIF
!IFNDEF NO_BUILD
!IFDEF WIN64
    nmake /f win32.mak CPU=AMD64 MSVC_VER=$(MSVC_VER) USE_SSL=1 SSL_INC=$(OUTPUT_DIR)\include SSL_LIB_PATH=$(OUTPUT_DIR)\lib
!ELSE
    nmake /f win32.mak MSVC_VER=$(MSVC_VER) USE_SSL=1 SSL_INC=$(OUTPUT_DIR)\include SSL_LIB_PATH=$(OUTPUT_DIR)\lib
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y libpq-fe.h $(OUTPUT_DIR)\include
    cd Release
    xcopy /Y *.dll $(OUTPUT_DIR)\bin
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
!ENDIF
    cd $(BASE_DIR)
!IFNDEF NO_COPY
    xcopy /Y $(PGSQL_DIR)\src\include\postgres_ext.h $(OUTPUT_DIR)\include
	xcopy /Y $(PGSQL_DIR)\src\include\pg_config_ext.h $(OUTPUT_DIR)\include
!ENDIF

iconv:
    cd $(ICONV_DIR)
!IFNDEF NO_CLEAN
    if exist Release rd /Q /S Release
    mkdir Release
    nmake /f makefile.msvc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.msvc NO_NLS=1 DLL=1 MFLAGS=-MD
    nmake -f Makefile.msvc NO_NLS=1 DLL=1 MFLAGS=-MD install PREFIX=$(BASE_DIR)\$(ICONV_DIR)\Release
!ENDIF
    cd ..
    cd $(GETTEXT_DIR)
!IFNDEF NO_CLEAN
    nmake /f makefile.msvc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake -f Makefile.msvc DLL=1 MFLAGS=-MD PREFIX=$(BASE_DIR)\$(ICONV_DIR)\Release
    nmake -f Makefile.msvc DLL=1 MFLAGS=-MD install PREFIX=$(BASE_DIR)\$(ICONV_DIR)\Release
!ENDIF
    cd ..
    cd $(ICONV_DIR)
!IFNDEF NO_CLEAN
    nmake /f makefile.msvc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.msvc DLL=1 MFLAGS=-MD PREFIX=$(BASE_DIR)\$(ICONV_DIR)\Release
    cd src
    if exist iconv.dll.manifest mt -manifest iconv.dll.manifest -outputresource:iconv.dll;2
    cd .. 
    nmake /f makefile.msvc DLL=1 MFLAGS=-MD install PREFIX=$(BASE_DIR)\$(ICONV_DIR)\Release
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(ICONV_DIR)\Release\bin\iconv.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(ICONV_DIR)\Release\lib\iconv.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(ICONV_DIR)\Release\include\iconv.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)

libxml:
    cd $(LIBXML_DIR)
!IFNDEF NO_CLEAN
    if exist Release rmdir /s /q Release
    mkdir Release
!ENDIF
    cd win32
!IFNDEF NO_CLEAN
    nmake /f makefile.msvc clean
!ENDIF
!IFNDEF NO_BUILD
    cscript configure.js compiler=msvc vcmanifest=yes include=$(OUTPUT_DIR)\include lib=$(OUTPUT_DIR)\lib
    nmake /f makefile.msvc
    nmake /f makefile.msvc install PREFIX=$(BASE_DIR)\$(LIBXML_DIR)\Release
!ENDIF
    cd ..\..
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(LIBXML_DIR)\Release\bin\libxml2.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(LIBXML_DIR)\Release\lib\libxml2.lib $(OUTPUT_DIR)\lib
    xcopy /Y /S $(BASE_DIR)\$(LIBXML_DIR)\Release\include\* $(OUTPUT_DIR)\include
!ENDIF 

giflib:
    cd $(GIFLIB_DIR)\lib
!IFNDEF NO_CLEAN
    nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.vc
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y giflib.lib $(OUTPUT_DIR)\lib
    xcopy /Y gif_lib.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)  
    
sqlite:
    cd $(SQLITE_DIR)
!IFNDEF NO_CLEAN
    nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.vc
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y *.dll $(OUTPUT_DIR)\bin
    xcopy /Y *.exe $(OUTPUT_DIR)\bin
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
    xcopy /Y sqlite3.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
    
spatialite:
    cd $(SPATIALITE_DIR)
!IFNDEF NO_CLEAN
    nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.vc
    xcopy /Y src\headers\*.h $(OUTPUT_DIR)\include
	if exist $(OUTPUT_DIR)\include\spatialite rmdir /s /q $(OUTPUT_DIR)\include\spatialite
    mkdir $(OUTPUT_DIR)\include\spatialite
    xcopy /Y src\headers\spatialite\*.h $(OUTPUT_DIR)\include\spatialite
	xcopy /Y src\headers\spatialite\sqlite3.h $(OUTPUT_DIR)\include
!IFDEF WIN64
    nmake /f makefile.vc OUTPUT_DIR=$(OUTPUT_DIR) WIN64=YES
!ELSE
    nmake /f makefile.vc OUTPUT_DIR=$(OUTPUT_DIR)
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y *.dll $(OUTPUT_DIR)\bin
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
!ENDIF
    cd $(BASE_DIR)
	
freexl:
    cd $(FREEXL_DIR)
!IFNDEF NO_CLEAN
    nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
	xcopy /Y headers\*.h $(OUTPUT_DIR)\include
    nmake /f makefile.vc OUTPUT_DIR=$(OUTPUT_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y *.dll $(OUTPUT_DIR)\bin
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
!ENDIF
    cd $(BASE_DIR)
	
mysql:
!IFDEF MYSQL_DIR
!IFNDEF NO_BUILD
    cd $(BASE_DIR)\$(MYSQL_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
	if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(MYSQL_DIR)\$(CMAKE_BUILDDIR)\install"
!IFDEF WIN64	
    devenv /rebuild Release libmysql.sln /Project libmysql /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release libmysql.sln /Project libmysql /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y $(BASE_DIR)\$(MYSQL_DIR)\$(CMAKE_BUILDDIR)\libmysql\Release\*.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(MYSQL_DIR)\$(CMAKE_BUILDDIR)\libmysql\Release\*.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(MYSQL_DIR)\include\*.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(MYSQL_DIR)\$(CMAKE_BUILDDIR)\include\my*.h $(OUTPUT_DIR)\include
	if not exist $(OUTPUT_DIR)\include\mysql mkdir $(OUTPUT_DIR)\include\mysql
    xcopy /Y $(BASE_DIR)\$(MYSQL_DIR)\include\mysql\*.h $(OUTPUT_DIR)\include\mysql
	if not exist $(OUTPUT_DIR)\include\mysql\psi mkdir $(OUTPUT_DIR)\include\mysql\psi
    xcopy /Y $(BASE_DIR)\$(MYSQL_DIR)\include\mysql\psi\*.h $(OUTPUT_DIR)\include\mysql\psi
    if not exist $(OUTPUT_DIR)\include\atomic mkdir $(OUTPUT_DIR)\include\atomic
    xcopy /Y $(BASE_DIR)\$(MYSQL_DIR)\include\atomic\*.h $(OUTPUT_DIR)\include\atomic
!ENDIF
    cd $(BASE_DIR)
!ENDIF

mysql2:
!IFDEF MYSQL_DIR
!IFNDEF NO_BUILD
    cd $(MYSQL_DIR)
!IFDEF WIN64    
    devenv /rebuild Release libmysql.sln /Project libmysql /ProjectConfig "Release|x64"
!ELSE
!IF $(MSVC_VER) == 1310
    devenv /rebuild Release libmysql.sln /Project libmysql
!ELSE
    devenv /rebuild Release libmysql.sln /Project libmysql /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
    cd $(BASE_DIR)
!ENDIF
    cd $(MYSQL_DIR)\libmysql\Release
!IFNDEF NO_COPY
    xcopy /Y *.dll $(OUTPUT_DIR)\bin
    xcopy /Y *.lib $(OUTPUT_DIR)\lib
    cd ..\..\include
    xcopy /Y *.h $(OUTPUT_DIR)\include
    if not exist $(OUTPUT_DIR)\include\mysys mkdir $(OUTPUT_DIR)\include\mysys
    xcopy /Y mysys\*.h $(OUTPUT_DIR)\include\mysys
    if not exist $(OUTPUT_DIR)\include\atomic mkdir $(OUTPUT_DIR)\include\atomic
    xcopy /Y atomic\*.h $(OUTPUT_DIR)\include\atomic
!ENDIF
    cd $(BASE_DIR)
!ENDIF

fits:
!IFDEF FITS_DIR
    cd $(FITS_DIR)
!IFNDEF NO_CLEAN
    nmake -f makefile.vcc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake -f makefile.vcc
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y Release\cfitsio.dll $(OUTPUT_DIR)\bin
    xcopy /Y Release\cfitsio.lib $(OUTPUT_DIR)\lib
    xcopy /Y fitsio.h $(OUTPUT_DIR)\include
    xcopy /Y longnam.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

ming:
!IFDEF MS_MING
    cd $(MING_DIR)\src
!IFNDEF NO_CLEAN
    nmake /f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f makefile.vc
!ENDIF
    cd ..\..
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(MING_DIR)\src\libming.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(MING_DIR)\*.h $(OUTPUT_DIR)\include
!ENDIF
!ENDIF

fastcgi:
!IFDEF FCGI_DIR
    cd $(FCGI_DIR)\libfcgi
!IFNDEF NO_CLEAN
    nmake /f libfcgi.mak clean
!ENDIF
!IFNDEF NO_BUILD
    nmake /f libfcgi.mak
!ENDIF
    cd ..\..
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(FCGI_DIR)\libfcgi\Release\libfcgi.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(FCGI_DIR)\libfcgi\Release\libfcgi.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(FCGI_DIR)\include\*.h $(OUTPUT_DIR)\include
!ENDIF
!ENDIF

xerces:
!IFDEF XERCES_DIR
    cd $(BASE_DIR)\$(XERCES_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)"
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\$(CMAKE_BUILDDIR)\src\$(MS_PROJECT_DIR)\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\$(CMAKE_BUILDDIR)\src\$(MS_PROJECT_DIR)\xerces-c_3_2.dll $(OUTPUT_DIR)\bin
    if exist $(OUTPUT_DIR)\include\xercesc rmdir /s /q $(OUTPUT_DIR)\include\xercesc
	mkdir $(OUTPUT_DIR)\include\xercesc
    cd $(BASE_DIR)\$(XERCES_DIR)\src
    xcopy  /Y /S xercesc\*.h $(OUTPUT_DIR)\include\xercesc
    xcopy  /Y /S xercesc\*.hpp $(OUTPUT_DIR)\include\xercesc
    xcopy  /Y /S xercesc\*.c $(OUTPUT_DIR)\include\xercesc
	cd $(BASE_DIR)\$(XERCES_DIR)\$(CMAKE_BUILDDIR)\src
	xcopy  /Y /S xercesc\*.hpp $(OUTPUT_DIR)\include\xercesc
!ENDIF
	cd $(BASE_DIR)
!ENDIF

xerces2:
!IFDEF XERCES_DIR
!IFNDEF NO_BUILD
!IF $(MSVC_VER) == 1911
!IFDEF WIN64
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC15\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC15\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1900
!IFDEF WIN64
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC14\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC14\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1800
!IFDEF WIN64
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC12\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC12\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1700
!IFDEF WIN64
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC11\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC11\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
!IFDEF WIN64
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC10\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC10\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC9\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC9\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC8\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC8\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib /ProjectConfig "Release|Win32"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(XERCES_DIR)\Projects\Win32\VC7.1\xerces-all
    devenv /rebuild Release xerces-all.sln /Project XercesLib
!ENDIF
    cd $(BASE_DIR)
!ENDIF
!IFNDEF NO_COPY
!IF $(MSVC_VER) == 1911
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC15\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC15\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC15\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC15\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1900
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC14\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC14\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC14\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC14\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1800
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC12\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC12\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC12\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC12\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1700
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC11\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC11\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC11\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC11\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC10\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC10\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC10\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC10\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC9\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC9\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC9\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC9\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC8\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win64\VC8\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC8\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC8\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSE
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC7.1\Release\xerces-c_3.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(XERCES_DIR)\Build\Win32\VC7.1\Release\xerces-c_3_1.dll $(OUTPUT_DIR)\bin	
!ENDIF
    if not exist $(OUTPUT_DIR)\include\xercesc mkdir $(OUTPUT_DIR)\include\xercesc
    cd $(BASE_DIR)\$(XERCES_DIR)\src
    xcopy  /Y /S xercesc\*.h $(OUTPUT_DIR)\include\xercesc
    xcopy  /Y /S xercesc\*.hpp $(OUTPUT_DIR)\include\xercesc
    xcopy  /Y /S xercesc\*.c $(OUTPUT_DIR)\include\xercesc
!ENDIF
	cd $(BASE_DIR)
!ENDIF

libkml:
!IFDEF LIBKML_DIR
!IFNDEF NO_COPY
    cd $(LIBKML_DIR)\src
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(LIBKML_DIR)\x64\release\*.lib $(OUTPUT_DIR)\lib
!ELSE
    xcopy /Y $(BASE_DIR)\$(LIBKML_DIR)\release\*.lib $(OUTPUT_DIR)\lib
!ENDIF
	if not exist $(OUTPUT_DIR)\include\kml mkdir $(OUTPUT_DIR)\include\kml
    xcopy /Y /S kml\*.h $(OUTPUT_DIR)\include\kml
	cd ..\third_party\boost_1_34_1
	if not exist $(OUTPUT_DIR)\include\boost mkdir $(OUTPUT_DIR)\include\boost
    xcopy /Y /S boost\*.hpp $(OUTPUT_DIR)\include\boost
!ENDIF
    cd $(BASE_DIR)
!ENDIF

expat2:
!IFDEF EXPAT2_DIR
    cd $(BASE_DIR)\$(EXPAT2_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" 
!IFDEF WIN64	
    devenv /rebuild Release expat.sln /Project expat /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release expat.sln /Project expat /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(EXPAT2_DIR)\$(CMAKE_BUILDDIR)\Release\expat.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(EXPAT2_DIR)\$(CMAKE_BUILDDIR)\Release\expat.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(EXPAT2_DIR)\lib\expat.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(EXPAT2_DIR)\lib\expat_external.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

expat:
!IFDEF EXPAT_DIR
!IFNDEF NO_BUILD
    cd $(EXPAT_DIR)\Source
!IFDEF WIN64    
    devenv /rebuild Release expat.sln /Project expat /ProjectConfig "Release|x64"
!ELSE
!IF $(MSVC_VER) == 1310
    devenv /rebuild Release expat.sln /Project expat
!ELSE
    devenv /rebuild Release expat.sln /Project expat /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
    cd $(BASE_DIR)
!ENDIF
!IFNDEF NO_COPY
!IF $(MSVC_VER) >= 1400
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\x64\bin\release\libexpat.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\x64\bin\release\libexpat.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\win32\bin\release\libexpat.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\win32\bin\release\libexpat.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSE
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\win32\bin\release\libexpat.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\win32\bin\release\libexpat.dll $(OUTPUT_DIR)\bin	
!ENDIF
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\lib\expat.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(EXPAT_DIR)\Source\lib\expat_external.h $(OUTPUT_DIR)\include
    cd $(BASE_DIR)
!ENDIF
!ENDIF

pdf:
!IFDEF PDF_DIR
!IFDEF WIN64
    cd $(PDF_DIR)
!IFNDEF NO_CLEAN
    nmake clean TARGET=Release_DLL CPU=X64
!ENDIF
!IFNDEF NO_BUILD
    nmake TARGET=Release_DLL CPU=X64
!ENDIF
!ELSE
    cd $(PDF_DIR)
!IFNDEF NO_CLEAN
    nmake clean TARGET=Release_DLL CPU=X86
!ENDIF
!IFNDEF NO_BUILD
    nmake TARGET=Release_DLL CPU=X86
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(PDF_DIR)\libs\pdflib\Release_DLL\pdflib.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(PDF_DIR)\libs\pdflib\Release_DLL\pdflib.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(PDF_DIR)\libs\pdflib\pdflib.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

fribidi:
!IFDEF FRIBIDI_DIR
    cd $(FRIBIDI_DIR)
!IFNDEF NO_CLEAN
    nmake -f makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake -f makefile.vc
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(FRIBIDI_DIR)\fribidi.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(FRIBIDI_DIR)\fribidi.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(FRIBIDI_DIR)\lib\fribidi*.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(FRIBIDI_DIR)\charset\fribidi*.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

version:
    echo MSVC_VER=$(MSVC_VER)  _NMAKE_VER=$(_NMAKE_VER)  WIN64=$(WIN64)  BASE_DIR=$(BASE_DIR)
	
hdfsuite: szip hdf5lib kea netcdf hdf4lib

szip:
!IFDEF SZIP_DIR
    cd $(BASE_DIR)\$(SZIP_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=release-$(COMPILER_VER)" "-DBUILD_SHARED_LIBS=ON"
!IFDEF WIN64	
    devenv /rebuild Release szip.sln /Project szip /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release szip.sln /Project szip /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(SZIP_DIR)\$(CMAKE_BUILDDIR)\bin\Release\*.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(SZIP_DIR)\$(CMAKE_BUILDDIR)\bin\Release\*.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(SZIP_DIR)\src\*.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(SZIP_DIR)\$(CMAKE_BUILDDIR)\SZconfig.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

hdf4lib:
!IFDEF HDF4_DIR
!IFNDEF NO_BUILD
    cd $(BASE_DIR)\$(HDF4_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
	if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(HDF4_DIR)\$(CMAKE_BUILDDIR)\install" "-DHDF4_ENABLE_Z_LIB_SUPPORT=ON" "-DHDF4_ENABLE_SZIP_SUPPORT=ON" "-DHDF4_BUILD_FORTRAN=OFF" "-DJPEG_LIBRARY=$(OUTPUT_DIR)\lib\libjpeg.lib" "-DSZIP_DIR=$(BASE_DIR)\$(SZIP_DIR)\$(CMAKE_BUILDDIR)" "-DBUILD_SHARED_LIBS=ON"
!IFDEF WIN64	
    devenv /rebuild Release hdf4.sln /Project INSTALL /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release hdf4.sln /Project INSTALL /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
	xcopy /Y $(BASE_DIR)\$(HDF4_DIR)\$(CMAKE_BUILDDIR)\install\lib\*.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(HDF4_DIR)\$(CMAKE_BUILDDIR)\bin\Release\*.dll $(OUTPUT_DIR)\bin
	if not exist $(OUTPUT_DIR)\include\hdf4 mkdir $(OUTPUT_DIR)\include\hdf4
    xcopy  /Y /S $(BASE_DIR)\$(HDF4_DIR)\$(CMAKE_BUILDDIR)\install\include\*.h $(OUTPUT_DIR)\include\hdf4
!ENDIF
    cd $(BASE_DIR)
!ENDIF

kea:
!IFDEF KEA_DIR
    cd $(BASE_DIR)\$(KEA_DIR)\trunk
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(KEA_DIR)\trunk\$(CMAKE_BUILDDIR)\install" "-DHDF5_LIB_PATH=$(OUTPUT_DIR)\lib" "-DHDF5_INCLUDE_DIR=$(OUTPUT_DIR)\include"
	xcopy /Y $(BASE_DIR)\$(KEA_DIR)\trunk\$(CMAKE_BUILDDIR)\include\libkea\kea-config.h $(BASE_DIR)\$(KEA_DIR)\trunk\include\libkea
!IFDEF WIN64	
    devenv /rebuild Release LIBKEA.sln /Project ALL_BUILD /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release LIBKEA.sln /Project ALL_BUILD /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(KEA_DIR)\trunk\$(CMAKE_BUILDDIR)\src\Release\*.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(KEA_DIR)\trunk\$(CMAKE_BUILDDIR)\src\Release\*.dll $(OUTPUT_DIR)\bin
	if not exist $(OUTPUT_DIR)\include\libkea mkdir $(OUTPUT_DIR)\include\libkea
	xcopy /Y $(BASE_DIR)\$(KEA_DIR)\trunk\include\libkea\*.h $(OUTPUT_DIR)\include\libkea
!ENDIF
    cd $(BASE_DIR)
!ENDIF

hdf5lib:
!IFDEF HDF5_DIR
    cd $(BASE_DIR)\$(HDF5_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(HDF5_DIR)\$(CMAKE_BUILDDIR)\install" "-DHDF5_ENABLE_Z_LIB_SUPPORT=ON" "-DHDF5_ENABLE_SZIP_SUPPORT=ON" "-DSZIP_DIR=$(BASE_DIR)\$(SZIP_DIR)\$(CMAKE_BUILDDIR)" "-DBUILD_SHARED_LIBS=ON" "-DHDF5_BUILD_HL_LIB=ON" "-DHDF5_BUILD_CPP_LIB=ON"
!IFDEF WIN64	
    devenv /rebuild Release hdf5.sln /Project ALL_BUILD /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release hdf5.sln /Project ALL_BUILD /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\$(CMAKE_BUILDDIR)\bin\Release\*.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\$(CMAKE_BUILDDIR)\bin\Release\*.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\c++\src\*.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\src\*.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\hl\src\*.h $(OUTPUT_DIR)\include
	xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\$(CMAKE_BUILDDIR)\*.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

    
hdf5lib2:
!IFDEF HDF5_DIR
!IFNDEF NO_BUILD
!IF $(MSVC_VER) == 1600
!IFDEF WIN64
    cd $(BASE_DIR)\$(HDF5_DIR)\windows_vc10\proj\all
    devenv /rebuild Release all.sln /Project hdf5dll /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(HDF5_DIR)\windows_vc10\proj\all
    devenv /rebuild Release all.sln /Project hdf5dll /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    cd $(BASE_DIR)\$(HDF5_DIR)\windows_vc9\proj\all
    devenv /rebuild Release all.sln /Project hdf5dll /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(HDF5_DIR)\windows_vc9\proj\all
    devenv /rebuild Release all.sln /Project hdf5dll /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    cd $(BASE_DIR)\$(HDF5_DIR)\windows\proj\all
    devenv /rebuild Release all.sln /Project hdf5dll /ProjectConfig "Release|x64"
!ELSE
    cd $(BASE_DIR)\$(HDF5_DIR)\windows\proj\all
    devenv /rebuild Release all.sln /Project hdf5dll /ProjectConfig "Release|Win32"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(HDF5_DIR)\windows_vnet\proj\all
    devenv /rebuild Release all.sln /Project hdf5dll
!ENDIF
    cd $(BASE_DIR)
!ENDIF
!IFNDEF NO_COPY
!IF $(MSVC_VER) == 1600
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC10-x64\hdf5dll.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC10-x64\hdf5dll.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC10\hdf5dll.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC10\hdf5dll.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC9-x64\hdf5dll.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC9-x64\hdf5dll.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC9\hdf5dll.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC9\hdf5dll.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-x64\hdf5dll.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-x64\hdf5dll.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release\hdf5dll.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release\hdf5dll.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSE
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC7\hdf5dll.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(HDF5_DIR)\proj\hdf5dll\Release-VC7\hdf5dll.dll $(OUTPUT_DIR)\bin	
!ENDIF
    xcopy  /Y /S $(BASE_DIR)\$(HDF5_DIR)\src\*.h $(OUTPUT_DIR)\include
    cd $(BASE_DIR)
!ENDIF
!ENDIF
    
mrsidlib:
!IFDEF MRSID_DIR
!IFNDEF NO_COPY
!IF EXIST("$(BASE_DIR)\$(MRSID_DIR)\lib\Release_md\lti_dsdk_dll.dll")
    xcopy /Y $(BASE_DIR)\$(MRSID_DIR)\lib\Release_md\lti_dsdk_dll.dll $(OUTPUT_DIR)\bin
!ENDIF
!IF EXIST("$(BASE_DIR)\$(MRSID_DIR)\Raster_DSDK\lib\lti_dsdk_dll.dll")
    xcopy /Y $(BASE_DIR)\$(MRSID_DIR)\Raster_DSDK\lib\lti_dsdk_dll.dll $(OUTPUT_DIR)\bin
!ENDIF
!IF EXIST($(MRSID_DLL))
    xcopy /Y $(MRSID_DLL) $(OUTPUT_DIR)\bin
!ENDIF
!IF EXIST($(LIDAR_DLL))
    xcopy /Y $(LIDAR_DLL) $(OUTPUT_DIR)\bin
!ENDIF
!ENDIF
!ENDIF

netcdf:
!IFDEF NETCDF_DIR
    cd $(BASE_DIR)\$(NETCDF_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if exist $(OUTPUT_DIR)\include\netcdf.h del $(OUTPUT_DIR)\include\netcdf.h
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(NETCDF_DIR)\$(CMAKE_BUILDDIR)\install" "-DHDF5_DIR=$(BASE_DIR)\$(HDF5_DIR)\$(CMAKE_BUILDDIR)" "-DUSE_HDF4=OFF" "-DBUILD_SHARED_LIBS=ON"
	xcopy /Y $(OUTPUT_DIR)\lib\szip.lib $(BASE_DIR)\$(NETCDF_DIR)\$(CMAKE_BUILDDIR)\liblib
!IFDEF WIN64	
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    cd $(BASE_DIR)\$(NETCDF_DIR)\$(CMAKE_BUILDDIR)
	xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)\$(CMAKE_BUILDDIR)\liblib\Release\netcdf.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)\$(CMAKE_BUILDDIR)\liblib\Release\netcdf.dll $(OUTPUT_DIR)\bin
	xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)\include\netcdf.h $(OUTPUT_DIR)\include
!ENDIF
	cd $(BASE_DIR)
!ENDIF

netcdf2:
!IFDEF NETCDF_DIR
!IFNDEF NO_BUILD
!IF $(MSVC_VER) == 1600
!IFDEF WIN64
    cd $(NETCDF_DIR)-VC10\win32\NET
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|x64"
!ELSE
    cd $(NETCDF_DIR)-VC10\win32\NET
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    cd $(NETCDF_DIR)-VC9\win32\NET
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|x64"
!ELSE
    cd $(NETCDF_DIR)-VC9\win32\NET
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|Win32"
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    cd $(NETCDF_DIR)-VC8\win32\NET
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|x64"
!ELSE
    cd $(NETCDF_DIR)-VC8\win32\NET
    devenv /rebuild Release netcdf.sln /Project netcdf /ProjectConfig "Release|Win32"
!ENDIF
!ELSE
    cd $(NETCDF_DIR)\win32\NET
    devenv /rebuild Release netcdf.sln /Project netcdf
!ENDIF
    cd $(BASE_DIR)
!ENDIF
!IFNDEF NO_COPY
!IF $(MSVC_VER) == 1600
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC10\win32\NET\x64\release\netcdf.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC10\win32\NET\x64\release\netcdf.lib $(OUTPUT_DIR)\lib
!ELSE
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC10\win32\NET\release\netcdf.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC10\win32\NET\release\netcdf.lib $(OUTPUT_DIR)\lib
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC9\win32\NET\x64\release\netcdf.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC9\win32\NET\x64\release\netcdf.lib $(OUTPUT_DIR)\lib
!ELSE
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC9\win32\NET\release\netcdf.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC9\win32\NET\release\netcdf.lib $(OUTPUT_DIR)\lib
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC8\win32\NET\x64\release\netcdf.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC8\win32\NET\x64\release\netcdf.lib $(OUTPUT_DIR)\lib
!ELSE
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC8\win32\NET\release\netcdf.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)-VC8\win32\NET\release\netcdf.lib $(OUTPUT_DIR)\lib
!ENDIF
!ELSE
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)\win32\NET\release\netcdf.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)\win32\NET\release\netcdf.lib $(OUTPUT_DIR)\lib	
!ENDIF
    xcopy /Y $(BASE_DIR)\$(NETCDF_DIR)\libsrc\netcdf.h $(OUTPUT_DIR)\include
    cd $(BASE_DIR)
!ENDIF
!ENDIF

libecw:
!IFDEF ECW_DIR
!IF $(ECWSDK_VERSION) >= 50
!IFNDEF NO_COPY
!IF $(MSVC_VER) == 1900
!IFDEF WIN64
    xcopy /Y $(ECWDIR)\redistributable\vc140\x64\NCSEcw.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(ECWDIR)\redistributable\vc140\Win32\NCSEcw.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1800
!IFDEF WIN64
    xcopy /Y $(ECWDIR)\redistributable\vc120\x64\NCSEcw.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(ECWDIR)\redistributable\vc120\Win32\NCSEcw.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1700
!IFDEF WIN64
    xcopy /Y $(ECWDIR)\redistributable\vc110\x64\NCSEcw.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(ECWDIR)\redistributable\vc110\Win32\NCSEcw.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
!IFDEF WIN64
    xcopy /Y $(ECWDIR)\redistributable\vc100\x64\NCSEcw.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(ECWDIR)\redistributable\vc100\Win32\NCSEcw.dll $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    xcopy /Y $(ECWDIR)\redistributable\vc90\x64\NCSEcw.dll $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y $(ECWDIR)\redistributable\vc90\Win32\NCSEcw.dll $(OUTPUT_DIR)\bin
!ENDIF
!ENDIF
!ENDIF
!ELSE
!IFNDEF NO_BUILD
    cd $(ECW_DIR)\Source\NCSBuildQmake
!IFDEF WIN64    
    devenv /rebuild Release libecwj2-win32-shared.sln /Project libecwj2 /ProjectConfig "Release|x64"
!ELSE
!IF $(MSVC_VER) == 1310
    devenv /rebuild Release libecwj2-win32-shared.sln /Project libecwj2
!ELSE
    devenv /rebuild Release libecwj2-win32-shared.sln /Project libecwj2 /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
    cd $(BASE_DIR)
!ENDIF
!IFNDEF NO_COPY
!IFNDEF WIN64
    xcopy /Y $(BASE_DIR)\$(ECW_DIR)\lib\libecwj2.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(ECW_DIR)\lib\*.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(ECW_DIR)\Source\include\NCS*.h $(OUTPUT_DIR)\include
!ELSE
    xcopy /Y $(BASE_DIR)\$(ECW_DIR)\lib64\libecwj2.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(ECW_DIR)\lib64\*.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(ECW_DIR)\Source\include\NCS*.h $(OUTPUT_DIR)\include
!ENDIF
!ENDIF
!ENDIF
!ENDIF

libjbig:
!IFDEF JBIG_DIR
    cd $(JBIG_DIR)
!IFNDEF NO_CLEAN
    nmake -f Makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake -f Makefile.vc
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(JBIG_DIR)\libjbig\libjbig.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(JBIG_DIR)\libjbig\jbig*.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

php:
!IFDEF PHPSDK_DIR
    SET PATH=$(PATH);$(PHPSDK_DIR)\bin;$(PHPSDK_DIR)\script
    SET BISON_SIMPLE=$(PHPSDK_DIR)\bin\bison.simple
    cd $(PHP_DIR)
!IFNDEF NO_CLEAN
    nmake clean
!ENDIF
!IFNDEF NO_BUILD
    buildconf
    configure --with-php-build=$(OUTPUT_DIR) --with-iconv=shared --with-libxml=no
    nmake
    nmake snap
!ENDIF
!IFNDEF NO_COPY
!ENDIF
    cd $(BASE_DIR)
!ENDIF

libtiff-old:
!IFDEF LIBTIFF_DIR
    cd $(LIBTIFF_DIR)
!IFNDEF NO_CLEAN
    nmake -f Makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake -f Makefile.vc
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\libtiff\libtiff.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\libtiff\libtiff_i.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(LIBTIFF_DIR)\libtiff\tif*.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

fontconfig:
!IFDEF FONTCONFIG_DIR
    cd $(FONTCONFIG_DIR)\src
!IFNDEF NO_CLEAN
    nmake -f Makefile.vc clean
!ENDIF
!IFNDEF NO_BUILD
    nmake -f Makefile.vc OUTPUT_DIR=$(OUTPUT_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(FONTCONFIG_DIR)\src\fontconfig.lib $(OUTPUT_DIR)\lib
    if not exist $(OUTPUT_DIR)\include\fontconfig mkdir $(OUTPUT_DIR)\include\fontconfig
    xcopy /Y /S $(BASE_DIR)\$(FONTCONFIG_DIR)\fontconfig\*.h $(OUTPUT_DIR)\include\fontconfig
!ENDIF
    cd $(BASE_DIR)
!ENDIF

pixman:
!IFDEF PIXMAN_DIR
    cd $(PIXMAN_DIR)\pixman
!IFNDEF NO_CLEAN
    -del /Q /S release
!ENDIF
!IFNDEF NO_BUILD
!IFDEF WIN64
    make -f Makefile.win32 CFG=release MMX=off SSE2=on SSSE3=off
!ELSE
    make -f Makefile.win32 CFG=release MMX=on SSE2=on SSSE3=off
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(PIXMAN_DIR)\pixman\release\pixman-1.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(PIXMAN_DIR)\pixman\pixman.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(PIXMAN_DIR)\pixman\pixman-version.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

cairo:
!IFDEF CAIRO_DIR
    cd $(CAIRO_DIR)\src
!IFNDEF NO_CLEAN
    -del /Q /S release
!ENDIF
!IFNDEF NO_BUILD
    -del $(OUTPUT_DIR)\include\cairo*.h
    make -f Makefile.win32 CFG=release OUTPUT_DIR=$(OUTPUT_DIR)
    cd release
	if exist cairo.dll.manifest mt -manifest cairo.dll.manifest -outputresource:cairo.dll;2
	cd ..
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\release\cairo.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\release\cairo.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\cairo.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\cairo-version.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\cairo-features.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\cairo-deprecated.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\cairo-pdf.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\cairo-svg.h $(OUTPUT_DIR)\include
    xcopy /Y $(BASE_DIR)\$(CAIRO_DIR)\src\cairo-ft.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

libsvg:
!IFDEF LIBSVG_DIR
    cd $(LIBSVG_DIR)\src
!IFNDEF NO_CLEAN
    nmake -f makefile.vc clean SDK_DIR=$(OUTPUT_DIR)
!ENDIF
!IFNDEF NO_BUILD
    nmake -f makefile.vc SDK_DIR=$(OUTPUT_DIR)
	cd ..
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(LIBSVG_DIR)\src\libsvg.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(LIBSVG_DIR)\src\svg.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

libsvg-cairo:
!IFDEF LIBSVG_DIR
    cd $(LIBSVG_CAIRO_DIR)\src
!IFNDEF NO_CLEAN
    nmake -f makefile.vc clean SDK_DIR=$(OUTPUT_DIR)
!ENDIF
!IFNDEF NO_BUILD
    nmake -f makefile.vc SDK_DIR=$(OUTPUT_DIR)
	cd ..
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(LIBSVG_CAIRO_DIR)\src\libsvg-cairo.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(LIBSVG_CAIRO_DIR)\src\svg-cairo.h $(OUTPUT_DIR)\include
!ENDIF
    cd $(BASE_DIR)
!ENDIF

ftgl:
!IFDEF FTGL_DIR
!IFNDEF NO_BUILD
    cd $(FTGL_SRCPATH)
!IFDEF WIN64    
    devenv /rebuild Release ftgl.sln /Project ftgl_dll /ProjectConfig "Release|x64"
!ELSE
!IF $(MSVC_VER) == 1310
    devenv /rebuild Release ftgl.sln /Project ftgl_dll
!ELSE
    devenv /rebuild Release ftgl.sln /Project ftgl_dll /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
    cd $(BASE_DIR)
!ENDIF
!IFNDEF NO_COPY
    xcopy  /Y $(BASE_DIR)\$(FTGL_DIR)\msvc\Build\ftgl.lib $(OUTPUT_DIR)\lib
    xcopy  /Y $(BASE_DIR)\$(FTGL_DIR)\msvc\Build\ftgl.dll $(OUTPUT_DIR)\bin
    if not exist $(OUTPUT_DIR)\include\FTGL mkdir $(OUTPUT_DIR)\include\FTGL
    xcopy /Y /S $(BASE_DIR)\$(FTGL_DIR)\src\FTGL\*.h $(OUTPUT_DIR)\include\FTGL
!ENDIF
!ENDIF


visual-leak-detector:
!IFDEF VLD_DIR
!IFNDEF NO_COPY
    if not exist $(OUTPUT_DIR)\bin\debug mkdir $(OUTPUT_DIR)\bin\debug
    xcopy  /Y $(BASE_DIR)\$(VLD_DIR)\include\*.h $(OUTPUT_DIR)\include
!IFDEF WIN64
    xcopy  /Y $(BASE_DIR)\$(VLD_DIR)\x64\Release\*.dll $(OUTPUT_DIR)\bin\debug
    xcopy  /Y $(BASE_DIR)\$(VLD_DIR)\x64\Release\vld.lib $(OUTPUT_DIR)\lib
!ELSE
    xcopy  /Y $(BASE_DIR)\$(VLD_DIR)\Win32\Release\*.dll $(OUTPUT_DIR)\bin\debug
    xcopy  /Y $(BASE_DIR)\$(VLD_DIR)\Win32\Release\vld.lib $(OUTPUT_DIR)\lib
!ENDIF
!ENDIF
!ENDIF

protobuf-build:
!IFDEF POPPLER_DIR
    cd $(BASE_DIR)\protobuf\cmake
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" "-Dprotobuf_BUILD_TESTS=OFF" 
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR) --target install
!ENDIF
	cd $(BASE_DIR)
!ENDIF

protobuf-c-build:
!IFDEF POPPLER_DIR
    cd $(BASE_DIR)\protobuf-c\build-cmake
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(OUTPUT_DIR)" "-DMSVC_STATIC_BUILD=ON"
    rem $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR)
!ENDIF
!IFNDEF NO_COPY
    $(CMAKE_DIR)\bin\cmake --build . --config $(MS_PROJECT_DIR) --target install
!ENDIF
	cd $(BASE_DIR)
!ENDIF

poppler:
!IFDEF POPPLER_DIR
    cd $(BASE_DIR)\$(POPPLER_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(POPPLER_DIR)\$(CMAKE_BUILDDIR)\install" "-DFREETYPE_LIBRARY=$(OUTPUT_DIR)\lib\freetype.lib" "-DJPEG_LIBRARY=$(OUTPUT_DIR)\lib\libjpeg.lib" "-DZLIB_LIBRARY=$(OUTPUT_DIR)\lib\zdll.lib" "-DENABLE_LIBOPENJPEG=OFF" "-DENABLE_RELOCATABLE=OFF" "-DTIFF_LIBRARY=$(OUTPUT_DIR)\lib\libtiff_i.lib" "-DBUILD_SHARED_LIBS=ON"
!IFDEF WIN64	
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    cd $(BASE_DIR)\$(POPPLER_DIR)\$(CMAKE_BUILDDIR)
	xcopy /Y Release\poppler.lib $(OUTPUT_DIR)\lib
	if not exist $(OUTPUT_DIR)\include\poppler mkdir $(OUTPUT_DIR)\include\poppler
	xcopy /Y poppler\poppler-config.h $(OUTPUT_DIR)\include\poppler
	cd $(BASE_DIR)\$(POPPLER_DIR)
    xcopy /Y /S poppler\*.h $(OUTPUT_DIR)\include\poppler
    if not exist $(OUTPUT_DIR)\include\poppler\goo mkdir $(OUTPUT_DIR)\include\poppler\goo
    xcopy /Y /S goo\*.h $(OUTPUT_DIR)\include\poppler\goo
    if not exist $(OUTPUT_DIR)\include\poppler\splash mkdir $(OUTPUT_DIR)\include\poppler\splash
    xcopy /Y /S splash\*.h $(OUTPUT_DIR)\include\poppler\splash
!ENDIF
	cd $(BASE_DIR)
!ENDIF

poppler2:
!IFDEF POPPLER_DIR
!IF $(MSVC_VER) == 1600
!IFDEF WIN64
    cd $(BASE_DIR)\$(POPPLER_DIR)\vc10x64
!IFNDEF NO_BUILD
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|x64"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(POPPLER_DIR)\vc10
!IFNDEF NO_BUILD
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    cd $(BASE_DIR)\$(POPPLER_DIR)\vc9x64
!IFNDEF NO_BUILD
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|x64"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(POPPLER_DIR)\vc9
!IFNDEF NO_BUILD
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!ELSEIF $(MSVC_VER) == 1400
!IFDEF WIN64
    cd $(BASE_DIR)\$(POPPLER_DIR)\vc8x64
!IFNDEF NO_BUILD
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|x64"
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(POPPLER_DIR)\vc8
!IFNDEF NO_BUILD
    devenv /rebuild Release poppler.sln /Project poppler /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!ELSE
    cd $(BASE_DIR)\$(POPPLER_DIR)\vc7
!IFNDEF NO_BUILD
    devenv /rebuild Release poppler.sln /Project poppler
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y Release\poppler.lib $(OUTPUT_DIR)\lib
    if not exist $(OUTPUT_DIR)\include\poppler mkdir $(OUTPUT_DIR)\include\poppler
    xcopy /Y /S poppler\*.h $(OUTPUT_DIR)\include\poppler
    if not exist $(OUTPUT_DIR)\include\poppler\goo mkdir $(OUTPUT_DIR)\include\poppler\goo
    xcopy /Y /S goo\*.h $(OUTPUT_DIR)\include\poppler\goo
    if not exist $(OUTPUT_DIR)\include\poppler\splash mkdir $(OUTPUT_DIR)\include\poppler\splash
    xcopy /Y /S splash\*.h $(OUTPUT_DIR)\include\poppler\splash
!ENDIF
    cd $(BASE_DIR)
!ENDIF

libopenjpeg_save:
!IFDEF OPENJPEG_DIR
    cd $(OPENJPEG_DIR)
!IFNDEF NO_BUILD
    devenv /rebuild Release openjpeg.sln /Project openjpeg
!ENDIF 
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(OPENJPEG_DIR)\bin\Release\openjpeg.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(OPENJPEG_DIR)\bin\Release\openjpeg.dll $(OUTPUT_DIR)\bin
    xcopy /Y $(BASE_DIR)\$(OPENJPEG_DIR)\libopenjpeg\openjpeg.h $(OUTPUT_DIR)\include  
!ENDIF
    cd $(BASE_DIR)
!ENDIF

libopenjpeg:
!IFDEF OPENJPEG2_DIR
    cd $(BASE_DIR)\$(OPENJPEG2_DIR)
!IFNDEF NO_CLEAN
    if exist $(CMAKE_BUILDDIR) rd /Q /S $(CMAKE_BUILDDIR)
!ENDIF
!IFNDEF NO_BUILD
    if not exist $(CMAKE_BUILDDIR) mkdir $(CMAKE_BUILDDIR)
	cd $(CMAKE_BUILDDIR)
    $(CMAKE_DIR)\bin\cmake ..\ -G $(CMAKE_GENERATOR) "-DCMAKE_PREFIX_PATH=$(OUTPUT_DIR)" "-DCMAKE_INSTALL_PREFIX=$(BASE_DIR)\$(OPENJPEG2_DIR)\trunk\$(CMAKE_BUILDDIR)\install" "-DTIFF_LIBRARY=$(OUTPUT_DIR)\lib\libtiff_i.lib" "-DZLIB_LIBRARY=$(OUTPUT_DIR)\lib\zdll.lib"
!IFDEF WIN64	
    devenv /rebuild Release openjpeg.sln /Project openjp2 /ProjectConfig "Release|x64"
!ELSE
    devenv /rebuild Release openjpeg.sln /Project openjp2 /ProjectConfig "Release|Win32"
!ENDIF
!ENDIF
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\$(CMAKE_BUILDDIR)\bin\Release\openjp2.lib $(OUTPUT_DIR)\lib
	xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\$(CMAKE_BUILDDIR)\bin\Release\openjp2.dll $(OUTPUT_DIR)\bin
	if not exist $(OUTPUT_DIR)\include\openjpeg-2.1 mkdir $(OUTPUT_DIR)\include\openjpeg-2.1
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\src\lib\openjp2\openjpeg.h $(OUTPUT_DIR)\include\openjpeg-2.1
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\src\lib\openjp2\opj_stdint.h $(OUTPUT_DIR)\include\openjpeg-2.1
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\$(CMAKE_BUILDDIR)\src\lib\openjp2\opj*.h $(OUTPUT_DIR)\include\openjpeg-2.1	
!ENDIF
    cd $(BASE_DIR)
!ENDIF
	
libopenjpeg2:
!IFDEF OPENJPEG2_DIR
    cd $(OPENJPEG2_DIR)
!IFNDEF NO_BUILD
    devenv /rebuild Release openjpeg.sln /Project openjp2
!ENDIF 
!IFNDEF NO_COPY
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\bin\Release\openjp2.lib $(OUTPUT_DIR)\lib
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\bin\Release\openjp2.dll $(OUTPUT_DIR)\bin
	if not exist $(OUTPUT_DIR)\include\openjpeg-2.0 mkdir $(OUTPUT_DIR)\include\openjpeg-2.0
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\src\lib\openjp2\openjpeg.h $(OUTPUT_DIR)\include\openjpeg-2.0
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\src\lib\openjp2\opj_stdint.h $(OUTPUT_DIR)\include\openjpeg-2.0
    xcopy /Y $(BASE_DIR)\$(OPENJPEG2_DIR)\src\lib\openjp2\opj_config.h $(OUTPUT_DIR)\include\openjpeg-2.0	
!ENDIF
    cd $(BASE_DIR)
!ENDIF

msvcr:
!IF $(MSVC_VER) == 1911
!IFDEF WIN64
    xcopy /Y "%VCToolsRedistDir%x64\Microsoft.VC141.CRT\vcruntime140.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCToolsRedistDir%x64\Microsoft.VC141.CRT\msvcp140.dll" $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y "%VCToolsRedistDir%x86\Microsoft.VC141.CRT\vcruntime140.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCToolsRedistDir%x86\Microsoft.VC141.CRT\msvcp140.dll" $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1900
!IFDEF WIN64
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC140.CRT\vcruntime140.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC140.CRT\msvcp140.dll" $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC140.CRT\vcruntime140.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC140.CRT\msvcp140.dll" $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1800
!IFDEF WIN64
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC120.CRT\msvcr120.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC120.CRT\msvcp120.dll" $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC120.CRT\msvcr120.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC120.CRT\msvcp120.dll" $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1700
!IFDEF WIN64
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC110.CRT\msvcr110.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC110.CRT\msvcp110.dll" $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC110.CRT\msvcr110.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC110.CRT\msvcp110.dll" $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1600
!IFDEF WIN64
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC100.CRT\msvcr100.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x64\Microsoft.VC100.CRT\msvcp100.dll" $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC100.CRT\msvcr100.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%redist\x86\Microsoft.VC100.CRT\msvcp100.dll" $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1500
!IFDEF WIN64
    xcopy /Y "%VCInstallDir%\redist\amd64\Microsoft.VC90.CRT\msvcr90.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%\redist\amd64\Microsoft.VC90.CRT\msvcp90.dll" $(OUTPUT_DIR)\bin
!ELSE
    xcopy /Y "%VCInstallDir%\redist\x86\Microsoft.VC90.CRT\msvcr90.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "%VCInstallDir%\redist\x86\Microsoft.VC90.CRT\msvcp90.dll" $(OUTPUT_DIR)\bin
!ENDIF
!ELSEIF $(MSVC_VER) == 1310
    xcopy /Y "C:\Program Files (x86)\Microsoft Visual Studio .NET 2003\SDK\v1.1\Bin\msvcr71.dll" $(OUTPUT_DIR)\bin
    xcopy /Y "C:\Program Files (x86)\Microsoft Visual Studio .NET 2003\SDK\v1.1\Bin\msvcp71.dll" $(OUTPUT_DIR)\bin
!ENDIF

mapmanager-build:
!IF $(MSVC_VER) >= 1600
	cd $(BASE_DIR)\$(MAPMANAGER_DIR)
!IFNDEF NO_BUILD
!IF $(MSVC_VER) >= 1900
    devenv /rebuild Release MapManager2015.sln /Project MapManager
!ELSE
	devenv /rebuild Release MapManager.sln /Project MapManager
!ENDIF
!ENDIF
	cd $(BASE_DIR)
!ENDIF

mapmanager-update:
!IF $(MSVC_VER) >= 1600
	set TERM=msys
	set "PATH=E:\Git\bin;%PATH%"
	cd $(BASE_DIR)\$(MAPMANAGER_DIR)
	git pull origin ms-$(MS_VERSION)
	rem git reset --hard $(MAPMANAGER_REVISION)
	cd $(BASE_DIR)
!ENDIF

mapmanager-installer:
!IF $(MSVC_VER) >= 1600
	cd $(BASE_DIR)\$(MAPMANAGER_DIR)
!IFNDEF NO_BUILD
!IF $(MSVC_VER) >= 1900
    devenv /rebuild Release MapManager2015.sln /Project Installer
!ELSE
	devenv /rebuild Release MapManager.sln /Project Installer
!ENDIF
	$(BASE_DIR)\cert\sign "$(BASE_DIR)\$(MAPMANAGER_DIR)\Installer\bin\Release\MapManager.msi"
!ENDIF
!IFNDEF NO_COPY
	$(BASE_DIR)\uploadftp $(BASE_DIR)\$(MAPMANAGER_DIR)\Installer\bin\Release\MapManager.msi downloads/release-$(COMPILER_VER)-$(PKG_VERSION)
!ENDIF
	cd $(BASE_DIR)
!ENDIF

op-disable:
    echo This operation is disabled!
