!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  
!  
! DOC
!
!  propert.f - Subroutines for handling properties (key-value pairs)
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
!   Author:    Arjen Markus / Stef Hummel
!   Purpose:   Handle props (key-value pairs) in Fortran 90
!   Context:   To be used by applications
! --------------------------------------------------------------------
!
module Dio_Prop

       implicit none

       integer, parameter, private                   :: PropRegular = 1
       integer, parameter, private                   :: PropArray   = 2

       integer, parameter, private                   :: max_props    = 200
       integer, parameter, private                   :: max_length   = 256

       integer, parameter, private                   :: max_arrayProps   = 10
       integer, parameter, private                   :: max_arrPropLines = 200

       integer, parameter, private                   :: max_format_length = 15
       integer, parameter, private                   :: nIntValsOnLine   = 4
       character(len=max_format_length), parameter, private :: cIntFormat = '(4I)'

       type arrayProp
           character(len=max_length), dimension(:), pointer  :: arrLines
           integer                                           :: no_arrLines
       end type arrayProp

       type (arrayProp), dimension(max_arrayProps)           :: arrProp
       integer, private                                      :: no_arrProps

       character(len=max_length), dimension(1:max_props)     :: props
       integer                  , dimension(1:max_props)     :: propType
       integer                  , dimension(1:max_props)     :: arrPropIndex
       integer, private                                      :: no_props
       integer, private                                      :: propPos

       interface prop_file
          module procedure prop_file_by_lu
          module procedure prop_file_by_name
       end interface

       interface prop_get
          module procedure prop_get_string
          module procedure prop_get_integer
          module procedure prop_get_real
          module procedure prop_get_logical
       end interface

       interface prop_read_array
          module procedure prop_read_array_integer
       end interface

       !*
       !* Added by Stef Hummel
       !*

       !
       ! Get Array properties
       !
       interface prop_get_1d_array
          module procedure prop_get_1d_array_integer
       end interface

       interface prop_get_2d_array
          module procedure prop_get_2d_array_integer
       end interface

       !
       ! String compare modes
       !
       integer, parameter :: CaseSens   = 1
       integer, parameter :: CaseInsens = 2
       !
       ! lun-id range for dio ASC/BIN/HIS streams
       !
       integer, parameter :: dioStartLun =  800
       integer, private   :: dioEndLun   = 4000


contains

! --------------------------------------------------------------------
!   Subroutine: prop_file_by_lu
!   Author:     Arjen Markus / Stef Hummel
!   Purpose:    Read the props from file
!   Context:    Called before calls to prop_get
!   Summary:
!               Read the props file, store the lines with
!               chapters and key-value pairs.
!   Arguments:
!   filename    Name of the file to read
! --------------------------------------------------------------------
!
      function prop_file_by_lu( lu ) result(retVal)

      logical retVal

      integer lu

      integer k, eof
      character(Len=max_length) line
      logical isProp, isArrProp
      real realDum
      integer intDum, iReadRes

      no_props = 0
      no_arrProps = 0
      
      !DBG reslun = 12
      !DBG open(reslun, file='prop_file.txt')

      retVal = .true.
!
! -------- To do:
!          Get rid of leading blanks
      do
         line = ''
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
            isProp = .true.
            k = index( 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz', line(1:1) )
            if ( k .le. 0 ) isProp = .false.

            k = index( line, '=' )
            if ( k .le. 0 ) isProp = .false.

            if ( isProp ) then
                no_props = no_props + 1
                props(no_props) = line
                propType(no_props) = PropRegular
            else
!
! -------------- other line, check for numbers
!                if avalaible, these are the values of an array-property
!
                isArrProp = .false.
                if (len_trim(line) < 1) cycle
                read(line,*, iostat=iReadRes) intDum
                if ( iReadRes .eq. 0 ) then
                    isArrProp = .true.
                else
                    read(line,*, iostat=iReadRes) realDum
                    if ( iReadRes .eq. 0 ) then
                        isArrProp = .true.
                    endif
                endif
                if ( .not. isArrProp ) cycle

                if (propType(no_props) .eq. PropRegular) then
!
! -------------------- value line, last prop. is an Array-property
!

                   no_arrprops = no_arrprops + 1
                   arrPropIndex(no_props) = no_arrprops
                   propType(no_props) = PropArray
                   arrProp( no_arrprops ) % no_arrLines = 0
                   allocate(arrProp( no_arrprops ) % arrLines(max_arrPropLines))
                   ! TODO: cleanUp
                endif

                arrProp( no_arrprops ) % no_arrLines = arrProp( no_arrprops ) % no_arrLines + 1
                arrProp( no_arrprops ) % arrLines(arrProp( no_arrprops ) % no_arrLines) = line
                !DBG write(reslun,*) 'A: ', no_arrprops, ' ', arrProp( no_arrprops ) % no_arrLines, &
                !DBG         arrProp( no_arrprops ) % arrLines(arrProp( no_arrprops ) % no_arrLines)
            endif
!
! -------- Check the number and get the next line
!
         endif

         if ( no_props .ge. max_props ) then
             retVal = .false.
             exit
         endif

      enddo
!
! -------- End of file or procedure
!
      !DBG close(reslun)

      return
      end function prop_file_by_lu

! --------------------------------------------------------------------
!   Subroutine: prop_file
!   Author:     Arjen Markus / Stef Hummel
!   Purpose:    Read the props from file
!   Context:    Called before calls to prop_get
!   Summary:
!               Read the props file, store the lines with
!               chapters and key-value pairs.
!   Arguments:
!   filename    Name of the file to read
! --------------------------------------------------------------------
!
      function prop_file_by_name( filename ) result(retVal)

      logical retVal
      character(Len=*) filename

      integer lu, ierror

      retVal = .false.

      open(newunit=lu, file = filename, status = 'old', iostat=ierror)
      if ( ierror == 0 ) then
          retVal = prop_file_by_lu(lu)
          close( lu )
      endif

      end function prop_file_by_name

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
      function prop_get_string( chapter, key, value ) result(retVal)

      logical retVal
      character(Len=*) chapter, key, value

      integer ip, k, istart, kend, length
      logical ignore

      retVal = .false.
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
            !DBG write(*,*) 'Prop: ', props(ip)
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
            if ( StringsEqual(CaseInsens, props(ip)(1:k-1), key) ) then
               kend = min( max_length, length+k-1 )
               value = trim( props(ip)(k+1:kend) )
               propPos = ip
               retVal = .true.
               exit
            endif
         endif
      enddo
!
! -------- End of procedure
!
      return
      end function prop_get_string

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
      function prop_get_integer( chapter, key, value ) result(retVal)

      logical retVal
      character(Len=*) chapter, key
      integer       value

      character(len=255) prop_value
      character(len=20)  format
      integer            length

      retVal = .false.
      prop_value = ' '
      if ( prop_get_string( chapter, key, prop_value ) ) then
!
! -------- Extract the integer part
!
          length = len_trim( prop_value )
          if ( length .ne. 0 ) then
             write( format, '(a,i5,a)' ) '(i', length, ')'
             read(  prop_value, format ) value
             retVal = .true.
          endif

      endif

      return
      end function prop_get_integer

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
      function prop_get_real( chapter, key, value ) result(retVal)

      logical retVal
      character(Len=*) chapter, key
      real          value

      character(len=255) prop_value
      character(len=20)  format
      integer            length

      retVal = .false.
      prop_value = ' '
      if ( prop_get_string( chapter, key, prop_value ) ) then
!
! -------- Extract the real part
!
          length = len_trim( prop_value )
          if ( length .ne. 0 ) then
             write( format, '(a,i5,a)' ) '(f', length, '.0)'
             read(  prop_value, format ) value
             retVal = .true.
          endif
      endif

      return
      end function prop_get_real

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
      function prop_get_logical( chapter, key, value ) result(retVal)

      logical retVal
      character(Len=*) chapter, key
      logical       value

      character(len=255) prop_value
      integer            k1, k2

      character(len=100) truth, falsity
      data truth / &
        'Y|YES|yes|Yes|T|TRUE|true|True|J|JA|Ja|ja|W|WAAR|Waar|waar'/
      data falsity / &
        'N|NO|no|No|F|FALSE|false|False|N|NEE|Nee|nee|O|ONWAAR|Onwaar|onwaar'/

      retVal = .false.
      prop_value = ' '
      if ( prop_get_string( chapter, key, prop_value ) ) then
!
! -------- Extract the logical part
!
          k1 = index( truth,   trim(prop_value) )
          k2 = index( falsity, trim(prop_value) )

          if ( k1 .gt. 0 ) value = .true.
          if ( k2 .gt. 0 ) value = .false.
          retVal = .true.
      endif

      return
      end function prop_get_logical


!*
!* Added by Stef Hummel
!*

! --------------------------------------------------------------------
!   Function:   prop_read_array_integer
!   Author:     Stef Hummel
!   Purpose:    Get the values of an array property
!   Context:    Internal function
!   Summary:    
!   Arguments:  
!               value   array for values
! --------------------------------------------------------------------
!
      function prop_read_array_integer( value, pos ) result(retVal)

      logical retVal

      integer, pointer, dimension(:) :: value
      integer            pos, nValues, i, nFullLines, nRestVals, valIndex, iReadRes
      character(len=max_format_length) :: restFormat

      nValues = size(value)

      nFullLines = nValues / nIntValsOnLine
      nRestVals  = nValues - nFullLines * nIntValsOnLine

      retVal = .true.
      valIndex = 1
      do i = 1 , nFullLines
         !DBG write(*,*) 'pos :', pos, ' i: ', i
         !DBG write(*,*) 'format: ', cIntFormat
         !DBG write(*,*) 'line: ', trim(arrProp( pos ) % arrLines(i))
         read(arrProp( pos ) % arrLines(i), *, iostat=iReadRes) value(valIndex:valIndex+3)
         if ( iReadRes .ne. 0 ) then
              retVal = .false.
         endif
         valIndex = valIndex + nIntValsOnLine
      end do

      if ( nRestVals .gt. 0 ) then
          valIndex = nFullLines * nIntValsOnLine + 1
          write(restFormat, '(a,i5,a)' ) '(i', nRestVals, ')'
          !DBG write(*,*) 'format: ', restFormat
          !DBG write(*,*) 'line: ', trim(arrProp( pos ) % arrLines(i))
          read(arrProp( pos ) % arrLines(nFullLines+1), *, iostat=iReadRes) value(valIndex:valIndex+nRestVals-1)
          if ( iReadRes .ne. 0 ) then
               retVal = .false.
          endif
      endif

      return
      end function prop_read_array_integer


! --------------------------------------------------------------------
!   Subroutine: prop_get_1d_array_integer
!   Author:     Stef Hummel
!   Purpose:    Get array of integer values for a property
!   Context:    Called by [n]d-array
!   Summary:
!               Check if the property is an array-property
!               Read the values.
!   Arguments:
!   chapter     Name of the chapter or "*" to get any key
!   key         Name of the key (case-sensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
! --------------------------------------------------------------------
!
    function prop_get_1d_array_integer( chapter, key, valSize ) result(value)

    integer, pointer, dimension(:) :: value
    integer            valSize
    character(Len=*) chapter, key

    character(len=255) prop_value
    integer            length

    nullify(value)
    prop_value = ' '
    if ( prop_get_string( chapter, key, prop_value ) ) then
!
! -------- Extract the integer part
!
        length = len_trim( prop_value )
        if ( length .eq. 0 ) then
           !DBG write(*,*) 'propPos: ', propPos
           !DBG write(*,*) 'arrPropIndex: ', arrPropIndex(propPos)
           allocate(value(valSize))
           if ( .not. prop_read_array(value, arrPropIndex(propPos)) ) then
              deallocate(value)
              nullify(value)
           endif
        endif
    endif

    return
    end function prop_get_1d_array_integer


! --------------------------------------------------------------------
!   Subroutine: prop_get_2d_array_integer
!   Author:     Stef Hummel
!   Purpose:    Get array of integer values for a property
!   Context:    Used by applications
!   Summary:
!               Check if the property is an array-property
!               Read the values.
!   Arguments:
!   chapter     Name of the chapter or "*" to get any key
!   key         Name of the key (case-sensitive)
!   value       Value of the key (not set if the key is not found,
!               so you can set a default value)
! --------------------------------------------------------------------
!
    function prop_get_2d_array_integer( chapter, key, dim1, dim2 ) result(value)

    integer, pointer, dimension(:,:) :: value
    character(Len=*) chapter, key
    integer :: dim1,dim2

    integer, pointer, dimension(:)   :: value1D
    integer :: d1,d2

    nullify(value)

    if ( (dim1 .gt. 0 ) .and. (dim2 .gt. 0 ) ) then
        value1D => prop_get_1d_array_integer(chapter, key, dim1 * dim2)

        if ( associated(value1D) ) then
            allocate(value(dim1,dim2))
            do d1 = 1, dim1
                do d2 = 1, dim2
                    value(d1,d2) = value1D( d1 + (d2 - 1 )*dim1) 
                enddo
            enddo
        endif
    endif

    return
    end function prop_get_2d_array_integer


!
! Support functions for string compare
!
subroutine UpperCase(STRING)
    ! arguments
    Character(Len=*), intent(INOUT) :: STRING ! incoming /resulting (lowercase) string
    ! locals
    Integer          :: I,J,L
    L=LEN(STRING)
    I=0
100 If (STRING(I+1:).NE.' ') Then
        I=I+1
        J=ICHAR(STRING(I:I))
        If (J .GT. 96 .AND. J .LT. 123) STRING(I:I)=CHAR(J-32)
        If (I.LT.L) GOTO 100
    Endif
    Return
end subroutine UpperCase

subroutine LowerCase(STRING)
    ! arguments
    Character(Len=*), intent(INOUT) :: STRING ! incoming /resulting (lowercase) string
    ! locals
    Integer          :: I,J,L
    L=LEN(STRING)
    I=0
200 If (STRING(I+1:).NE.' ') Then
        I=I+1
        J=ICHAR(STRING(I:I))
        If (J .GT. 64 .AND. J .LT. 91) STRING(I:I)=CHAR(J+32)
        If (I.LT.L) GOTO 200
    Endif
    Return
end subroutine LowerCase

function StringsEqual(mode, str1, str2) result(retVal)
    ! return value
    logical                      :: retVal     ! .true. if strings are equal
    ! arguments
    integer, intent(IN)          :: mode       ! CaseSens(itive) or CaseInsens(itive)
    Character(Len=*), intent(IN) :: str1, str2 ! incoming /resulting (lowercase) string
    ! locals
    Character(Len=max_length)    :: locStr1, locStr2 ! copy of strings, to convert to lowercase
    ! body:
    retVal = .false.
    if (mode .eq. CaseSens) then
        if (str1 .eq. str2) retVal = .true.
    else if (mode .eq. CaseInsens) then
        locStr1 = str1 ; call LowerCase(locStr1)
        locStr2 = str2 ; call LowerCase(locStr2)
        if (locStr1 .eq. locStr2) retVal = .true.
    endif
end function StringsEqual

! When all open-statements use the newunit-specifier this function can be removed
function DioNewLun() result(retVal)

    ! return value
    integer :: retVal  ! new lun

    ! locals
    logical :: opend   ! lun already open?
    integer :: lu      ! lun loop counter

    ! body: find not-open lun

    retVal = 0
    do lu = DioStartLun, DioEndLun 
        inquire( lu, opened = opend )
        if ( .not. opend ) then
            retVal = lu
            exit
        endif
    enddo

end function DioNewLun


!*
!* Close
!*


endmodule Dio_Prop

