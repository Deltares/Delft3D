This package contains precompiled binaries of CodeSynthesis XSD, a
W3C XML Schema to C++ Data Binding compiler, built for Microsoft
Windows. For more information about XSD visit

http://www.codesynthesis.com/products/xsd/

This README file describes how to start using XSD in the Microsoft
Windows environment with Visual Studio .NET 2003 (7.1), Visual Studio 
2005 (8.0), Visual Studio 2008 (9.0), and Visual Studio 2010 (10.0).


Prerequisites
-------------

The XSD runtime library and the generated code depend on the Xerces-C++
XML parser which you can obtain from http://xerces.apache.org/xerces-c/.
You can also download precompiled Xerces-C++ libraries for Windows from
http://xerces.apache.org/xerces-c/download.cgi


Environment
-----------

Before you can start building examples or your applications that use XSD
you need to set include, library and executable search paths in the Visual
Studio IDE and the System Environment.

1. Setting up Xerces-C++

  First you need to set up Xerces-C++ include and library search paths.
  If you already have Xerces-C++ set up in your development environment,
  you can skip to the next step. Here we assume that your Xerces-C++ path
  is C:\projects\xerces-c-x.y.z. If you have Xerces-C++ in a different
  place, you will need to adjust the paths below accordingly.


  a) For Visual Studio .NET 2003 (7.1):

       In the Visual Studio IDE, select "Tools"/"Options"/"Projects"/"VC++
       Directories".

       Then, in the "Show directories for" field, select "Include files" and
       create a new entry with the value "C:\projects\xerces-c-x.y.z\include".

       After that, in the "Show directories for" field, select "Library files"
       and create a new entry with the value "C:\projects\xerces-c-x.y.z\lib".
   
       After that, in the "Show directories for" field, select "Executable files"
       and create a new entry with the value "C:\projects\xerces-c-x.y.z\bin".

     For Visual Studio 2005 (8.0) and Visual Studio 2008 (9.0):
     
       In the Visual Studio IDE, select "Tools"/"Options"/"Projects and
       Solution"/"VC++ Directories".

       Then, in the "Show directories for" field, select "Include files" and
       create a new entry with the value "C:\projects\xerces-c-x.y.z\include".

       After that, in the "Show directories for" field, select "Library files"
       and create a new entry with the value "C:\projects\xerces-c-x.y.z\lib".

       After that, in the "Show directories for" field, select "Executable files"
       and create a new entry with the value "C:\projects\xerces-c-x.y.z\bin".

       If you are building the 64-bit version of your application, repeat the 
       above steps for the 64-bit version of Xerces-C++ while selecting x64 
       in the "Platform" drop-down list in the VC++ Directories dialog (Visual
       Studio keeps a separate set of paths for each platform). 

     For Visual Studio 2010 (10.0):

       1. Open an existing or create a new C++ project (you can open one of
          the example solutions)  

       2. Open the Property Manager view by selecting "View"->"Property 
          Manager" (or "View"->"Other Windows"->"Property Manager") menu 
          action

       3. Expand the property hierarchy for the project and find the 
          Microsoft.Cpp.Win32.user property sheet

       4. Right click on Microsoft.Cpp.Win32.user and select the "Properties"
          menu action

       5. Select the VC++ Directories tab

       6. Add the "C:\projects\xerces-c-x.y.z\include" path to the "Include 
          Directories" field (the paths are separated by a semicolon)

       7. Add the "C:\projects\xerces-c-x.y.z\lib" path to the "Library 
          Directories" field

       8. Add the "C:\projects\xerces-c-x.y.z\bin" path to the "Executable 
          Directories" field

       9. Click Ok to close the dialog and then click the Save button at the 
          top of the Property Manager view to save Microsoft.Cpp.Win32.user          

       If you are building the 64-bit version of your application, repeat
       the above steps for the 64-bit version of Xerces-C++ but using the 
       Microsoft.Cpp.x64.user property sheet (Visual Studio keeps a separate
       set of paths for each platform). 


  b) In the Control Panel, choose "System" and select the "Advanced" tab.
     Click on the "Environment Variables" button. In the "System Variables"
     list, select "Path" and add (via "Edit" button) the
     ";C:\projects\xerces-c-x.y.z\bin" path at the end.


2. Setting up XSD

  Now you need to set up XSD executable and include search paths. Here we
  assume that your XSD path is C:\projects\xsd-x.y.z. If you have XSD in 
  a different place, you will need to adjust the paths below accordingly.

  For Visual Studio .NET 2003 (7.1):

    In the Visual Studio IDE, select "Tools"/"Options"/"Projects"/"VC++
    Directories".

    Then, in the "Show directories for" field, select "Include files" and
    create a new entry with the value "C:\projects\xsd-x.y.z\libxsd".

    After that, in the "Show directories for" field, select "Executable
    files" and create a new entry with the value "C:\projects\xsd-x.y.z\bin".
    Make sure it is the first line in the list of directories (use the
    "Up" button to move the new entry up, if necessary).

  For Visual Studio 2005 (8.0) and Visual Studio 2008 (9.0):
     
    In the Visual Studio IDE, select "Tools"/"Options"/"Projects and
    Solution"/"VC++ Directories".

    Then, in the "Show directories for" field, select "Include files" and
    create a new entry with the value "C:\projects\xsd-x.y.z\libxsd".

    After that, in the "Show directories for" field, select "Executable
    files" and create a new entry with the value "C:\projects\xsd-x.y.z\bin".
    Make sure it is the first line in the list of directories (use the
    "Up" button to move the new entry up, if necessary).

    If you are building the 64-bit version of your application, repeat the 
    above steps using the same paths while selecting x64 in the "Platform"
    drop-down list in the VC++ Directories dialog (Visual Studio keeps a 
    separate set of paths for each platform). 

  For Visual Studio 2010 (10.0):

    1. Open an existing or create a new C++ project (you can open one of
       the example solutions)  

    2. Open the Property Manager view by selecting "View"->"Property 
       Manager" (or "View"->"Other Windows"->"Property Manager") menu
       action

    3. Expand the property hierarchy for the project and find the 
       Microsoft.Cpp.Win32.user property sheet

    4. Right click on Microsoft.Cpp.Win32.user and select the "Properties"
       menu action

    5. Select the VC++ Directories tab

    6. Add the "C:\projects\xsd-x.y.z\libxsd" path to the "Include 
       Directories" field (the paths are separated by a semicolon)

    7. Add the "C:\projects\xsd-x.y.z\bin" path to the "Executable 
       Directories" field and make sure it is the first path in the       
       the list of directories

    8. Click Ok to close the dialog and then click the Save button at the 
       top of the Property Manager view to save Microsoft.Cpp.Win32.user

    If you are building the 64-bit version of your application, repeat the 
    above steps using the same paths but using the Microsoft.Cpp.x64.user 
    property sheet (Visual Studio keeps a separate set of paths for each 
    platform). 


3. Restart the Visual Studio IDE.


Building Examples
-----------------

Now you are ready to build examples. Simply open the solution file
found in the examples\cxx\tree and examples\cxx\parser directories.

Some of the examples depend on additional third-party libraries or
show a specific feature of XSD and are not included in the solutions
above. They come with their individual solution files:

examples/cxx/tree/embedded         - example of schema embedding
examples/cxx/tree/custom           - examples of type customization
examples/cxx/tree/custom/calendar  - depends on the Boost date_time library
examples/cxx/tree/compression      - depends on the zlib library
examples/cxx/tree/binary/boost     - depends on the Boost serialization library
examples/cxx/tree/binary/cdr       - depends on the ACE library
examples/cxx/tree/binary/xdr       - requires a third-party XDR library
examples/cxx/tree/xpath            - depends on the XQilla library (XPath 2)
examples/cxx/tree/dbxml            - depends on the Berkeley DB XML library


Using XSD in Your Projects
--------------------------

For various ways to integrate the XSD compiler with the Visual Studio IDE 
as well as other Visual Studio-specific topics, refer to the Using XSD with
Microsoft Visual Studio Wiki page:

http://wiki.codesynthesis.com/Using_XSD_with_Microsoft_Visual_Studio
