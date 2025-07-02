!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

 ! DOC
!
!  propert.f - Subroutines for handling properties (key-value pairs)
!
!  Copyright (C) 2000 Arjen Markus
!
!  Arjen Markus
!
!
!  General information:
!  This module contains a number of auxilary subroutines that can be
!  used to handle key-value pairs, as found in Windows INI-files.
!  Public functions include:
!  - prop_file:           Set the file containing the properties
!  - prop_get:            Get the value of a property (this may be
!                         a string, an integer, a real or a logical
!                         value)
!
!
!  Limitations:
!  - The total length of a line can be no more than 256 characters
!    (this includes any leading blanks)
!  - The total number of key-value pairs can be no more than 200
!  - There can only be one property file at a time.
!
!  Property files have a similar format as Windows INI-files, the
!  only exceptions being that they do not need to have chapters,
!  chapters can be ignored (by specifying '*' as the chapter) and
!  property files can contain comment lines. That is, any line not
!  recognised as a chapter or as a key value pair (the key name
!  must begin with a letter) is ignored.
!
!  There is no guarantee that the behaviour in case of multiple
!  matches (in other words: the keyword appears more than once)
!  will remain the same in future versions. Furthermore, other
!  libraries that read such files may use a different matching
!  strategy. So, avoid these situations.
!
! ENDDOC
!
!  $Author$
!  $Date$
!  $Source$
!
! --------------------------------------------------------------------
!   Module:    PROPERTIES
!   Author:    Arjen Markus
!   Purpose:   Handle props (key-value pairs) in Fortran 90
!   Context:   To be used by applications
! --------------------------------------------------------------------
!
       module RR_PROPERTIES

       implicit none

       integer, parameter, private                   :: max_props  = 300
       integer, parameter, private                   :: max_length = 256

       integer, private                                  :: no_props
       character(len=max_length), dimension(1:max_props) :: props

       interface prop_get
          module procedure prop_get_string
          module procedure prop_get_integer
          module procedure prop_get_real
          module procedure prop_get_logical
       end interface

       contains

! --------------------------------------------------------------------
!   Subroutine: prop_file
!   Author:     Arjen Markus
!   Purpose:    Read the props from file
!   Context:    Called before calls to prop_get
!   Summary:
!               Read the props file, store the lines with
!               chapters and key-value pairs.
!   Arguments:
!   filename    Name of the file to read
! --------------------------------------------------------------------
!
      subroutine prop_file( filename )

      character(len=*) filename

      integer lu, k, eof
      character(len=max_length) line

      no_props = 0
      
      open(newunit=lu, file = filename, status = 'old' )
!
! -------- To do:
!          Get rid of leading blanks
      do
         read( lu, '(a)', iostat = eof ) line
         if ( eof .ne. 0 ) exit
!
! -------- Remove carriage returns
!
         k = index( line, char(13) )
         if ( k .gt. 0 ) then
            line(k:k) = ' '
         endif
!
! -------- Chapters
!
         if ( line(1:1) .eq. '[' ) then
            k = index( line, ']' )
            if ( k .le. 0 ) cycle

            no_props = no_props + 1
            props(no_props) = line(1:k-1)
         else
!
! -------- Key-value pairs
!
            k = index( line, '=' )
            if ( k .le. 0 ) cycle
            k = index( 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz', line(1:1) )
            if ( k .le. 0 ) cycle

            no_props = no_props + 1
            props(no_props) = line
         endif
!
! -------- Check the number and get the next line
!
         if ( no_props .ge. max_props ) exit

      enddo
!
! -------- End of file or procedure
!
      close( lu )

      return
      endsubroutine prop_file

! --------------------------------------------------------------------
!   Subroutine: prop_get_string
!   Author:     Arjen Markus
!   Purpose:    Get the string value for a property
!   Context:    Used by applications
!   Summary:
!               Go through the list of props to check the
!               chapter. When the right chapter is found, check
!               for the key.
!               Only set the value if the key matches
!   Arguments:
!   chapter     Name of the chapter or "*" to get any key
!   key         Name of the key (case-sensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
! --------------------------------------------------------------------
!
      subroutine prop_get_string( chapter, key, value )

      character(len=*) chapter, key, value
      integer ip, k, istart, kend, length
      logical ignore
!
! -------- Handle chapters
!
      ignore = chapter(1:1) .eq. '*'
!
! -------- Find the chapter first
!
      istart = 1
      if ( .not. ignore ) then
         istart = no_props + 1
         do ip = 1,no_props
            if ( props(ip)(1:1) .eq. '[' ) then
               if ( props(ip)(2:) .eq. chapter ) then
                  istart = ip + 1
                  exit
               endif
            endif
         enddo
      endif
!
! -------- Find the key
!          To do:
!          Remove leading blanks
!          Note:
!          Work around an apparent problem with the SUN Fortran 90
!          compiler
!
      length = len( value )
      do ip = istart,no_props
         if ( props(ip)(1:1) .eq. '[' .and. .not. ignore ) then
            exit
         endif

         k = index( props(ip), '=' )
         if ( k .gt. 0 ) then
            if ( props(ip)(1:k-1) .eq. key ) then
               kend = min( max_length, length+k+1 )   ! er stond -1, dat is fout! GP
               value = trim( props(ip)(k+1:kend) )
               exit
            endif
         endif
      enddo
!
! -------- End of procedure
!
      return
      end subroutine prop_get_string

! --------------------------------------------------------------------
!   Subroutine: prop_get_integer
!   Author:     Arjen Markus
!   Purpose:    Get the integer value for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to integer.
!   Arguments:
!   chapter     Name of the chapter or "*" to get any key
!   key         Name of the key (case-sensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
! --------------------------------------------------------------------
!
      subroutine prop_get_integer( chapter, key, value )

      character(len=*) chapter, key
      integer       value

      character(len=255) prop_value
      character(len=20)  format
      integer            length

      prop_value = ' '
      call prop_get_string( chapter, key, prop_value )
!
! -------- Extract the integer part
!
      length = len_trim( prop_value )
      if ( length .ne. 0 ) then
         write( format, '(a,i5,a)' ) '(i', length, ')'
         read(  prop_value, format ) value
      endif

      return
      end subroutine prop_get_integer

! --------------------------------------------------------------------
!   Subroutine: prop_get_real
!   Author:     Arjen Markus
!   Purpose:    Get the real value for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to real.
!   Arguments:
!   chapter     Name of the chapter or "*" to get any key
!   key         Name of the key (case-sensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
! --------------------------------------------------------------------
!
      subroutine prop_get_real( chapter, key, value )

      character(len=*) chapter, key
      real          value

      character(len=255) prop_value
      character(len=20)  format
      integer            length

      prop_value = ' '
      call prop_get_string( chapter, key, prop_value )
!
! -------- Extract the real part
!
      length = len_trim( prop_value )
      if ( length .ne. 0 ) then
         write( format, '(a,i5,a)' ) '(f', length, '.0)'
         read(  prop_value, format ) value
      endif

      return
      end subroutine prop_get_real

! --------------------------------------------------------------------
!   Subroutine: prop_get_logical
!   Author:     Arjen Markus
!   Purpose:    Get the logical value for a property
!   Context:    Used by applications
!   Summary:
!               Use prop_get_string to get the string value.
!               Convert it to logical.
!   Arguments:
!   chapter     Name of the chapter or "*" to get any key
!   key         Name of the key (case-sensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
! --------------------------------------------------------------------
!
      subroutine prop_get_logical( chapter, key, value )

      character(len=*) chapter, key
      logical       value

      character(len=255) prop_value
      integer            k1, k2

      character(len=100) truth, falsity
      data truth / 'Y|YES|yes|Yes|T|TRUE|true|True|J|JA|Ja|ja|W|WAAR|Waar|waar'/
      data falsity / 'N|NO|no|No|F|FALSE|false|False|N|NEE|Nee|nee|O|ONWAAR|Onwaar|onwaar'/

      prop_value = ' '
      call prop_get_string( chapter, key, prop_value )
!
! -------- Extract the logical part
!
      k1 = index( truth,   trim(prop_value) )
      k2 = index( falsity, trim(prop_value) )

      if ( k1 .gt. 0 ) value = .true.
      if ( k2 .gt. 0 ) value = .false.

      return
      end subroutine prop_get_logical

      endmodule RR_PROPERTIES

! -------------------------------------------------------------------
! Small test program: module PROPERTIES
! -------------------------------------------------------------------
!
!     program testprop
!     use PROPERTIES
!     character*50 value
!     integer      ivalue
!     real         rvalue
!     logical      lvalue
!
!     Contents "testprop.ini":
!     * Comments to follow
!     *[test]
!     *test=Should not appear (at all)
!     * First test
!     [test]
!     dotest=true
!     * Second test
!     * test=Found it (should not appear)
!     * Third test
!     [anyChapter]
!     test=found
!     intvalue=100
!     intvalue2=100.0
!     realvalue=100.01
!     realvalue2=100.01e3
!     realvalue3=100.01aa
!     logvalue=yes
!
!     call prop_file( 'testprop.ini' )
!
!     write(*,*) 'Expected: value = true'
!     value = 'false'
!     call prop_get( 'test', 'dotest', value )
!     write(*,*) '     Got: value = ', value
!
!     write(*,*) 'Expected: value = ---- (not in chapter)'
!     value = '----'
!     call prop_get( 'test', 'test', value )
!     write(*,*) '     Got: value = ', value
!
!     write(*,*) 'Expected: value = found (in any chapter)'
!     call prop_get( '*', 'test', value )
!     write(*,*) '     Got: value = ', value
!
!     write(*,*) 'Expected: value = 100 (in any chapter)'
!     call prop_get( '*', 'intvalue', ivalue )
!     write(*,*) '     Got: value = ', ivalue
!
!     write(*,*) 'Expected: value = 100 (in any chapter; or an error)'
!     call prop_get( '*', 'intvalue2', ivalue )
!     write(*,*) '     Got: value = ', ivalue
!
!     write(*,*) 'Expected: value = 100.01 (in any chapter)'
!     call prop_get( '*', 'realvalue', rvalue )
!     write(*,*) '     Got: value = ', rvalue
!
!     write(*,*) 'Expected: value = 100.01e3 (in any chapter)'
!     call prop_get( '*', 'realvalue2', rvalue )
!     write(*,*) '     Got: value = ', rvalue
!
!     write(*,*) 'Expected: value = 100.01 (in any chapter; or error)'
!     call prop_get( '*', 'realvalue3', rvalue )
!     write(*,*) '     Got: value = ', rvalue
!
!     write(*,*) 'Expected: value = true (in any chapter; or an error)'
!     call prop_get( '*', 'logvalue', lvalue )
!     write(*,*) '     Got: value = ', lvalue
!
!     end
