# Microsoft Developer Studio Project File - Name="Nefis5" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=Nefis5 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "nefis5.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "nefis5.mak" CFG="Nefis5 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Nefis5 - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "Nefis5 - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Nefis5 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "libsrc/win32"
# PROP Intermediate_Dir "libsrc/win32"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /include:"Release/" /nologo /traceback /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I ".\include" /D "NDEBUG" /D "_WIN64" /D "_WINDOWS" /D "WIN64" /D "PTR8" /D "STDCALL" /D "MIXED_STR_LEN_ARG" /YX /FD /c
# ADD BASE RSC /l 0x413
# ADD RSC /l 0x413
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\lib\dev_studio\release\static\nefis.lib"

!ELSEIF  "$(CFG)" == "Nefis5 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "libsrc/win32_debug"
# PROP Intermediate_Dir "libsrc/win32_debug"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /nologo /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /traceback /warn:argument_checking /warn:nofileopt /warn:unused
# SUBTRACT F90 /warn:truncated_source
# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /W4 /GX /ZI /Od /I "./include" /D "_DEBUG" /D "_WIN64" /D "_WINDOWS" /D "WIN64" /D "PTR8" /D "STDCALL" /D "MIXED_STR_LEN_ARG" /FR /FD /c
# ADD BASE RSC /l 0x413
# ADD RSC /l 0x413
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\lib\dev_studio\debug\static\nefis.lib"

!ENDIF 

# Begin Target

# Name "Nefis5 - Win32 Release"
# Name "Nefis5 - Win32 Debug"
# Begin Source File

SOURCE=.\src\c2c.c
# End Source File
# Begin Source File

SOURCE=.\src\df.c
# End Source File
# Begin Source File

SOURCE=.\src\er.c
# End Source File
# Begin Source File

SOURCE=.\src\f2c.c
# End Source File
# Begin Source File

SOURCE=.\src\gp.c
# End Source File
# Begin Source File

SOURCE=.\src\gt.c
# End Source File
# Begin Source File

SOURCE=.\src\hs.c
# End Source File
# Begin Source File

SOURCE=.\src\oc.c
# End Source File
# Begin Source File

SOURCE=.\src\pt.c
# End Source File
# Begin Source File

SOURCE=.\src\rt.c
# End Source File
# Begin Source File

SOURCE=".\src\wl-xdr.c"
# End Source File
# End Target
# End Project
