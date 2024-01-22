# Microsoft Developer Studio Project File - Name="diofDll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=diofDll - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "diofDll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "diofDll.mak" CFG="diofDll - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "diofDll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "diofDll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/diof90/diofDll", ITBAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "diofDll - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /dll /nologo /warn:nofileopt
# ADD F90 /compile_only /define:"WIN32" /define:"USE_DIOF_DLL" /fpp /include:"Release/" /include:"../include" /dll /nologo /warn:nofileopt
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "DLL_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "DLL_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /machine:I386
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=Copy DLL to testdir
PostBuild_Cmds=call copyDll.bat Release
# End Special Build Tool

!ELSEIF  "$(CFG)" == "diofDll - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /debug:full /dll /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /debug:full /define:"WIN32" /define:"USE_DIOF_DLL" /fpp /include:"Debug/" /include:"../include" /dll /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "DLL_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "../include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "DLL_EXPORTS" /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /debug /machine:I386 /pdbtype:sept
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Desc=Copy DLL to testdir
PostBuild_Cmds=call copyDll.bat Debug
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "diofDll - Win32 Release"
# Name "diofDll - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE="..\libsrc\diof90\dio-2dfield-rw.F90"
DEP_F90_DIO_2=\
	"..\libsrc\diof90\dio-sync.inc"\
	
NODEP_F90_DIO_2=\
	".\Debug\Dio_3d_block.mod"\
	".\Debug\dio_ds.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-2dfield-shm.F90"
NODEP_F90_DIO_2D=\
	".\Debug\dio_2dfield_rw.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-3d-block.F90"
NODEP_F90_DIO_3=\
	".\Debug\dio_shm.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-ds-config.F90"
NODEP_F90_DIO_D=\
	".\Debug\Dio_Prop.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-ds.F90"
DEP_F90_DIO_DS=\
	"..\libsrc\diof90\dio-time-support.inc"\
	
NODEP_F90_DIO_DS=\
	".\Debug\dio_shm.mod"\
	".\Debug\dio_streams.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-ini.F90"
NODEP_F90_DIO_I=\
	".\Debug\Dio_Prop.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-plt-f77.F90"
DEP_F90_DIO_P=\
	"..\libsrc\diof90\dio-time-support.inc"\
	
NODEP_F90_DIO_P=\
	".\Debug\dio_ds.mod"\
	".\Debug\dio_plt_rw.mod"\
	".\Debug\dio_streams.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-plt-rw.F90"
DEP_F90_DIO_PL=\
	"..\libsrc\diof90\dio-sync.inc"\
	"..\libsrc\diof90\dio-time-support.inc"\
	
NODEP_F90_DIO_PL=\
	".\Debug\Dio_3d_block.mod"\
	".\Debug\dio_ds.mod"\
	".\Debug\Dio_ini.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-plt-shm.F90"
NODEP_F90_DIO_PLT=\
	".\Debug\dio_plt_rw.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-prop.F90"
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-sem-support.F90"
NODEP_F90_DIO_S=\
	".\Debug\dio_streams.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-streams.F90"
DEP_F90_DIO_ST=\
	"..\include\nefis.inc"\
	"..\libsrc\diof90\dio-sync.inc"\
	"..\libsrc\diof90\dio-time-support.inc"\
	
NODEP_F90_DIO_ST=\
	".\Debug\dio_ds_config.mod"\
	".\Debug\dio_shm.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-sync-support.F90"
NODEP_F90_DIO_SY=\
	".\Debug\dio_streams.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-sync.F90"
DEP_F90_DIO_SYN=\
	"..\libsrc\diof90\dio-sync-support.inc"\
	
NODEP_F90_DIO_SYN=\
	".\Debug\dio_ds.mod"\
	".\Debug\dio_shm.mod"\
	".\Debug\dio_streams.mod"\
	
# End Source File
# Begin Source File

SOURCE="..\libsrc\diof90\dio-time-support.F90"
NODEP_F90_DIO_T=\
	".\Debug\dio_ds.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\libsrc\diof90\dio_shm.cpp
# End Source File
# Begin Source File

SOURCE=..\libsrc\diof90\dio_shm_datablock.cpp
# End Source File
# Begin Source File

SOURCE=..\libsrc\diof90\dio_shm_f2c_c.cpp
# End Source File
# Begin Source File

SOURCE=..\libsrc\diof90\dio_shm_f2c_c.h
# End Source File
# Begin Source File

SOURCE=..\libsrc\diof90\dio_shm_f2c_f.F90
# End Source File
# Begin Source File

SOURCE=..\libsrc\diof90\dio_shm_handle.cpp
# End Source File
# Begin Source File

SOURCE=..\libsrc\diof90\dio_shm_sync.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\DiofDllVersion.rc
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# End Group
# End Target
# End Project
