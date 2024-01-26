===================================
=== Delft3D-INIFILES README.TXT ===
===================================
About this file: last change date   : Feb 01, 2005
                 last change author : Adri.Mourits@deltares.nl
                 previous authors   : Arjen.Markus@deltares.nl

This file descibes how to use the Delft3D-INIFILES library.

This file contains the following three chapters:
1) Getting the inifiles library
2) Interface
3) Example


==============================
1 GETTING THE INIFILES LIBRARY
============================== 

On Windows
----------
Name of library      : inifiles.lib
Name of module files : properties.mod
                       tree_data_types.mod
                       tree_structures.mod
To be found in       : VSS, $/delft3d/libraries/ini_files/ini_files/lib/win32
                       VSS, $/delft3d/libraries/ini_files/ini_files/modules/win32
Compiler used        : Compaq Visual Fortran 6.6.C

On Linux
--------
Name of library      : libinifiles.a
Name of module files : properties.mod
                       tree_data_types.mod
                       tree_structures.mod
To be found in       : VSS, $/delft3d/libraries/ini_files/ini_files/lib/wlinux
                       VSS, $/delft3d/libraries/ini_files/ini_files/modules/wlinux
Compiler used        : Intel Fortran Compiler 8.0


===========
2 INTERFACE
===========

The library contains the following seven subroutines:
1) prop_file(filename ,error)
      character(*), intent(in)  :: filename
      integer     , intent(out) :: error
      !
      !   Purpose:    Read the props from file
      !   Context:    Called in an application before calls to prop_get_***
      !   Summary:
      !               Read the props file, store the lines with
      !               chapters and key-value pairs.
      !   Arguments:
      !   filename    Name of the file to read
      !   error       0: no error occured
      !               1: file does not exist
      !               2: unable to open file
      !               3: no properties found in file
      !               4: more than max_properties found
      !   Restrictions:
      !               - One file at a time can be handled
      !               - Maximum number of properties in a file: 200
      !               - Maximum length of a line in a file    : 256
      !   Comment lines:
      !               Chapters are recognised by the character "[" in the first column of a line.
      !               Keywords are recognised by the character "=" somewhere in the line.
      !               All other lines are assumed to be comments.

2) prop_get_double(chapter   ,key       ,value     )
      double      ,intent (out) :: value
      character(*),intent (in)  :: chapter
      character(*),intent (in)  :: key
      !
      !   Purpose:    Get the double value for a property
      !   Context:    Used by applications
      !   Summary:
      !               Use prop_get_string to get the string value.
      !               Convert it to double.
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Value of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Comments on this line:
      !               Value is set with the first real found behind the character "=".
      !               The following example is allowed:
      !               DoubleIn = Gravity 9.8000000000, m/s*2

3) prop_get_doubles(chapter ,key ,value ,valuelength )
      integer             , intent (in)  :: valuelength
      double, dimension(*), intent (out) :: value
      character(*)        , intent (in)  :: chapter
      character(*)        , intent (in)  :: key
      !
      !   Purpose:    Get the array of real doubles for a property
      !   Context:    Used by applications
      !   Summary:
      !               Use prop_get_string to get the string value.
      !               Convert it to doubles.
      !               If the string contains less doubles than valuelength,
      !               only the doubles found are set in value.
      !               If the string contains more doubles than valuelength,
      !               only valuelength reals are set in value
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Values of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Comments on this line:
      !               Everywhere behind the character "=".
      !               The following example is allowed:
      !               DoublesIn = (x,y): 4.500000,5.9000000 Start point

4) prop_get_integer(chapter   ,key       ,value     )
      integer     ,intent (out) :: value
      character(*),intent (in)  :: chapter
      character(*),intent (in)  :: key
      !
      !   Purpose:    Get the integer value for a property
      !   Context:    Used by applications
      !   Summary:
      !               Use prop_get_string to get the string value.
      !               Convert it to integer.
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Value of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Comments on this line:
      !               Value is set with the first integer found behind the character "=".
      !               The following example is allowed:
      !               IntegerIn = Index 8, denoting the startpoint for searches

5) prop_get_integers(chapter   ,key       ,value     ,valuelength          )
      integer              ,intent (in)  :: valuelength
      integer, dimension(*),intent (out) :: value
      character(*)         ,intent (in)  :: chapter
      character(*)         ,intent (in)  :: key
      !
      !   Purpose:    Get the array of integer values for a property
      !   Context:    Used by applications
      !   Summary:
      !               Use prop_get_string to get the string value.
      !               Convert it to integers.
      !               If the string contains less integers than valuelength,
      !               only the integers found are set in value.
      !               If the string contains more integers than valuelength,
      !               only valuelength integers are set in value
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Values of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Comments on this line:
      !               Everywhere behind the character "=".
      !               The following example is allowed:
      !               IntegersIn = (n,m): 4,5

6) prop_get_logical(chapter   ,key       ,value     )
      character(*),intent (in)  :: chapter
      character(*),intent (in)  :: key
      logical     ,intent (out) :: value
      !
      !   Purpose:    Get the logical value for a property
      !   Context:    Used by applications
      !   Summary:
      !               Use prop_get_string to get the string value.
      !               Convert it to logical.
      !               Allowed strings to detect the value true:
      !               Y|YES|yes|Yes|T|TRUE|true|True|J|JA|Ja|ja|W|WAAR|Waar|waar
      !               Allowed strings to detect the value false:
      !               N|NO|no|No|F|FALSE|false|False|N|NEE|Nee|nee|O|ONWAAR|Onwaar|onwaar
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Value of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Comments on this line:
      !               Not allowed

7) prop_get_real(chapter   ,key       ,value     )
      real        ,intent (out) :: value
      character(*),intent (in)  :: chapter
      character(*),intent (in)  :: key
      !
      !   Purpose:    Get the real value for a property
      !   Context:    Used by applications
      !   Summary:
      !               Use prop_get_string to get the string value.
      !               Convert it to real.
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Value of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Comments on this line:
      !               Value is set with the first real found behind the character "=".
      !               The following example is allowed:
      !               RealIn = Gravity 9.8, m/s*2

8) prop_get_reals(chapter ,key ,value ,valuelength )
      integer           , intent (in)  :: valuelength
      real, dimension(*), intent (out) :: value
      character(*)      , intent (in)  :: chapter
      character(*)      , intent (in)  :: key
      !
      !   Purpose:    Get the array of real values for a property
      !   Context:    Used by applications
      !   Summary:
      !               Use prop_get_string to get the string value.
      !               Convert it to reals.
      !               If the string contains less reals than valuelength,
      !               only the reals found are set in value.
      !               If the string contains more reals than valuelength,
      !               only valuelength reals are set in value
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Values of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Comments on this line:
      !               Everywhere behind the character "=".
      !               The following example is allowed:
      !               RealsIn = (x,y): 4.5,5.9 Start point

9) prop_get_string(chapterin ,keyin     ,value     )
      character(*),intent(in) :: chapterin
      character(*),intent(in) :: keyin
      character(*)            :: value
      !
      !   Purpose:    Get the string value for a property
      !   Context:    Used by applications
      !   Summary:
      !               Go through the list of props to check the
      !               chapter. When the right chapter is found, check
      !               for the key.
      !               Only set the value if the key matches
      !   Arguments:
      !   chapter     Name of the chapter (case-insensitive) or "*" to get any key
      !   key         Name of the key (case-insensitive)
      !   value       Value of the key (not set if the key is not found,
      !               so you can set a default value)
      !   Delimiters:
      !               If the value starts with the character "#", this character is removed.
      !               If a second character "#" is found , this character and everything behind
      !               this character is removed.
      !   Comments on this line:
      !               Use the delimiters "#". Example:
      !               StringIn = # AFileName # Comments are allowed behind the second "#"


=========
3 EXAMPLE
=========
Example ini-file
----------------
DredgeInputFileVersion = 0.2            * 0.2 is the only value allowed
Areas = 1
[Area 1]
Depth = 0.60
DredgePolygonPoints = 4
DredgePoint1 = 14.165256  8.105211E-01
DredgePoint2 = 14.165256 -3.402965E-01
DredgePoint3 = 17.768812 -3.751698E-01
DredgePoint4 = 17.710691  8.221455E-01

Example code in application to read this inifile
------------------------------------------------
       call prop_file(dredgefile, istat)
       if (istat /= 0) then
          select case (istat)
          case(1)
             call errmsg('file not found')
          case(3)
             call errmsg('no properties found in file')
          case default
             call errmsg('reading error')
          endselect
          call d3stop(1, gdp)
       endif
       !
       ! Check version number of dredge input file
       !
       call prop_get_real('*','DredgeInputFileVersion',versionnrinput)
       if (comparereal(versionnr, versionnrinput, eps) /= 0) then
          call errmsg ('Dredge input file must have version number ',versionnr)
          call d3stop(1, gdp)
       endif
       !
       ! Read dimensions from input file
       !
       call prop_get_integer('*','areas',nadred)
       do ia = 1, nadred
          write (chapter,'(a5,i0)')'area ',ia
          call prop_get_integer(chapter,'dredgepolygonpoints',ip)
          totnpdr = totnpdr + ip
          call prop_get_integer(chapter,'dumppolygonpoints',ip)
          totnpdu = totnpdu + ip
       enddo
       !
       ! read from input file for each dredge area
       !
       do ia = 1, nadred
          !
          ! read dredge depth
          !
          ddred(ia) = misvalue
          write (chapter,'(a5,i0)')'area ',ia
          call prop_get_real(chapter,'depth',ddred(ia))
          if (comparereal(ddred(ia),misvalue,eps) == 0) then
             call errmsg ('Unable to read dredge depth of area ',ia)
             call d3stop(1, gdp)
          endif
          !
          ! read # dredge polygon points
          !
          npdr(ia) = -1
          call prop_get_integer(chapter,'dredgepolygonpoints',npdr(ia))
          if (npdr(ia) == -1) then
             call errmsg ('Unable to read the number of points in dredge polygone of area',ia)
             call d3stop(1, gdp)
          endif
          !
          ! read the dredge polygon points
          !
          do ip = 1, npdr(ia)
             inputvals = misvalue
             write (parname,'(a11,i0)')'dredgepoint',ip
             call prop_get_reals(chapter,parname,inputvals,2)
             if (comparereal(inputvals(1),misvalue,eps) == 0 .or. &
               & comparereal(inputvals(2),misvalue,eps) == 0        ) then
                call errmsg ('Unable to read dredge polygone point ',ip,' of area ',ia)
                call d3stop(1, gdp)
             endif
             xdr(icdr) = inputvals(1)
             ydr(icdr) = inputvals(2)
          enddo

==============================
=== END OF FILE README.TXT ===
============================== 