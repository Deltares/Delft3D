This SDK provides the necessary files to compile mapserver and the gdal libraries for x86 and x64 platforms with MSVC

1. Required common steps for the compilation
-------------------------------------------

Open Visual Studio Command Prompt of the supported compiler version and architecture


2. gdal compilation
-------------------

nmake gdal
nmake gdal-csharp

3. mapserver compilation
------------------------

nmake ms
nmake ms-csharp


the resulting binaries are copied into the output directory like:
release-[compiler version][-x64]


4. x64 builds
-------------
(Requires to use the Visual Studio 2005 or 2008 x64 Win64 Command Prompt)
The makefile supports to auto-detect the target architecture and define WIN64=YES accordingly



5. Debug builds
---------------

By using the DEBUG compiler option the result contains the required debug information to step into the code by using the Visual Studio debugger;

nmake gdal DEBUG=YES



