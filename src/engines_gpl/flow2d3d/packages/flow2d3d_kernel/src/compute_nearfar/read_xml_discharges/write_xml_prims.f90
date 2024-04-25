module write_xml_primitives
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
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
!  $Id: write_xml_prims.f90 5889 2016-02-24 18:29:31Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/read_xml_discharges/write_xml_prims.f90 $
!!--description-----------------------------------------------------------------
!
! write_xml_prims.f90 - Write routines for primitive data
!
! $Id: write_xml_prims.f90 5889 2016-02-24 18:29:31Z mourits $
!
! Arjen Markus
!
! General information:
! This module is part of the XML-Fortran library. Its
! purpose is to write individual items to an XML
! file using the right tag. It is used by the code generated
! by the make_xml_reader program.
!
! TODO: 2D versions of word_array and line_array
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use xmlparse
    implicit none

    interface write_to_xml_real_array
        module procedure write_to_xml_real_array_1d
        module procedure write_to_xml_real_array_2d
    end interface
    interface write_to_xml_double_array
        module procedure write_to_xml_double_array_1d
        module procedure write_to_xml_double_array_2d
    end interface
    interface write_to_xml_integer_array
        module procedure write_to_xml_integer_array_1d
        module procedure write_to_xml_integer_array_2d
    end interface
    interface write_to_xml_logical_array
        module procedure write_to_xml_logical_array_1d
        module procedure write_to_xml_logical_array_2d
    end interface
    interface write_to_xml_word
       module procedure write_to_xml_string
    end interface
    interface write_to_xml_line
       module procedure write_to_xml_string
    end interface

contains

! write_to_xml_integer --
!    Routine to write a single integer to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    value       Value to be written
!
subroutine write_to_xml_integer( info, tag, indent, value )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    integer, intent(in)                          :: value

    character(len=100)                           :: indentation

    indentation = ' '
    write( info%lun, '(4a,i0,3a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>', value, '</', trim(tag), '>'

end subroutine write_to_xml_integer

! write_to_xml_integer_1dim --
!    Routine to write an array of integers to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    values      Values to be written
!
subroutine write_to_xml_integer_1dim( info, tag, indent, values )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    integer, dimension(:), intent(in)            :: values

    integer                                      :: i

    do i = 1,size(values)
        call write_to_xml_integer( info, tag, indent, values(i) )
    enddo

end subroutine write_to_xml_integer_1dim

! write_to_xml_real --
!    Routine to write a single real value (single precision) to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    value       Value to be written
!
subroutine write_to_xml_real( info, tag, indent, value )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real, intent(in)                             :: value

    character(len=100)                           :: indentation
    character(len=12)                            :: buffer

    indentation = ' '
    write( buffer, '(1pg12.4)' ) value
    write( info%lun, '(8a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>', trim(adjustl(buffer)), '</', trim(tag), '>'

end subroutine write_to_xml_real

! write_to_xml_real_1dim --
!    Routine to write an array of reals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    values      Values to be written
!
subroutine write_to_xml_real_1dim( info, tag, indent, values )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real, dimension(:), intent(in)               :: values

    integer                                      :: i

    do i = 1,size(values)
        call write_to_xml_real( info, tag, indent, values(i) )
    enddo

end subroutine write_to_xml_real_1dim

! write_to_xml_double --
!    Routine to write one real value (double precision) to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    value       Value to be written
!
subroutine write_to_xml_double( info, tag, indent, value )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real(kind=kind(1.0d0)), intent(in)           :: value

    character(len=100)                           :: indentation
    character(len=16)                            :: buffer

    indentation = ' '
    write( buffer, '(1pg16.7)' ) value
    write( info%lun, '(8a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>', trim(adjustl(buffer)), '</', trim(tag), '>'

end subroutine write_to_xml_double

! write_to_xml_double_1dim --
!    Routine to write an array of double precision reals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    values      Values to be written
!
subroutine write_to_xml_double_1dim( info, tag, indent, values )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real(kind=kind(1.0d00)), dimension(:), intent(in) :: values

    integer                                      :: i

    do i = 1,size(values)
        call write_to_xml_double( info, tag, indent, values(i) )
    enddo

end subroutine write_to_xml_double_1dim

! write_to_xml_string --
!    Routine to write one string to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    value       Value to be written
!
subroutine write_to_xml_string( info, tag, indent, value )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    character(len=*), intent(in)                 :: value

    character(len=100)                           :: indentation

    !
    ! NOTE: No guards against <, >, & and " yet!
    ! NOTE: difference needed between words and lines?
    !
    indentation = ' '
    write( info%lun, '(8a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>', trim(value), '</', trim(tag), '>'

end subroutine write_to_xml_string

! write_to_xml_word_1dim --
!    Routine to write an array of single words to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    value       Value to be written
!
subroutine write_to_xml_word_1dim( info, tag, indent, values )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    character(len=*), dimension(:), intent(in)   :: values

    integer                                      :: i

    do i = 1,size(values)
        call write_to_xml_string( info, tag, indent, values(i) )
    enddo
end subroutine write_to_xml_word_1dim

! write_to_xml_line_1dim --
!    Routine to write an array of strings (whole lines) to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    values      Values to be written
!
subroutine write_to_xml_line_1dim( info, tag, indent, values )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    character(len=*), dimension(:), intent(in)   :: values

    integer                                      :: i

    do i = 1,size(values)
        call write_to_xml_string( info, tag, indent, values(i) )
    enddo

end subroutine write_to_xml_line_1dim

! write_to_xml_logical --
!    Routine to write one logical to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    value       Value to be written
!
subroutine write_to_xml_logical( info, tag, indent, value )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    logical, intent(in)                          :: value

    character(len=100)                           :: indentation

    indentation = ' '
    if ( value ) then
        write( info%lun, '(8a)' ) indentation(1:min(indent,100)), &
            '<', trim(tag), '>true</', trim(tag), '>'
    else
        write( info%lun, '(8a)' ) indentation(1:min(indent,100)), &
            '<', trim(tag), '>false</', trim(tag), '>'
    endif

end subroutine write_to_xml_logical

! write_to_xml_logical_1dim --
!    Routine to write an array of logicals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    values      Values to be written
!
subroutine write_to_xml_logical_1dim( info, tag, indent, values )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    logical, dimension(:), intent(in)            :: values

    integer                                      :: i

    do i = 1,size(values)
        call write_to_xml_logical( info, tag, indent, values(i) )
    enddo

end subroutine write_to_xml_logical_1dim

! write_to_xml_integer_array_1d --
!    Routine to write an array of integers to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_integer_array_1d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    integer, dimension(:), intent(in)            :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do i = 1,size(array),10
        i2 = min( i + 9, size(array) )
        write( info%lun, '(a,10i12)' ) indentation(1:min(indent+4,100)), &
            ( array(j) ,j = i,i2 )
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_integer_array_1d

! write_to_xml_integer_array_2d --
!    Routine to write a 2D array of integers to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_integer_array_2d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    integer, dimension(:,:), intent(in)          :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j, k

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do k = 1,size(array,2)
        do i = 1,size(array,1),10
            i2 = min( i + 9, size(array,1) )
            write( info%lun, '(a,10i12)' ) indentation(1:min(indent+4,100)), &
                ( array(j,k) ,j = i,i2 )
        enddo
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_integer_array_2d

! write_to_xml_real_array_1d --
!    Routine to write an array of single precision reals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_real_array_1d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real, dimension(:), intent(in)               :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do i = 1,size(array),10
        i2 = min( i + 9, size(array) )
        write( info%lun, '(a,10g12.4)' ) indentation(1:min(indent+4,100)), &
            ( array(j) ,j = i,i2 )
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_real_array_1d

! write_to_xml_real_array_2d --
!    Routine to write a 2D array of single precision reals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_real_array_2d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real, dimension(:,:), intent(in)             :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j, k

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do k = 1,size(array,2)
        do i = 1,size(array,1),10
            i2 = min( i + 9, size(array,1) )
            write( info%lun, '(a,10g12.4)' ) indentation(1:min(indent+4,100)), &
                ( array(j,k) ,j = i,i2 )
        enddo
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_real_array_2d

! write_to_xml_double_array_1d --
!    Routine to write an array of double precision reals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_double_array_1d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real(kind=kind(1.0d0)), dimension(:), intent(in) :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do i = 1,size(array),5
        i2 = min( i + 4, size(array) )
        write( info%lun, '(a,5g20.7)' ) indentation(1:min(indent+4,100)), &
            ( array(j) ,j = i,i2 )
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_double_array_1d

! write_to_xml_double_array_2d --
!    Routine to write an array of double precision reals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_double_array_2d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    real(kind=kind(1.0d0)), dimension(:,:), intent(in) :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j, k

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do k = 1,size(array,2)
        do i = 1,size(array,1),5
            i2 = min( i + 4, size(array,1) )
            write( info%lun, '(a,5g20.7)' ) indentation(1:min(indent+4,100)), &
                ( array(j,k) ,j = i,i2 )
        enddo
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_double_array_2d

! write_to_xml_logical_array_1d --
!    Routine to write an array of logicals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_logical_array_1d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    logical, dimension(:), intent(in)            :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do i = 1,size(array),10
        i2 = min( i + 9, size(array) )
        write( info%lun, '(a,10a)' ) indentation(1:min(indent+4,100)), &
            ( merge('true  ', 'false ', array(j)) ,j = i,i2 )
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_logical_array_1d

! write_to_xml_logical_array_2d --
!    Routine to write an array of logicals to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_logical_array_2d( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    logical, dimension(:,:), intent(in)          :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j, k

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do k = 1,size(array,2)
        do i = 1,size(array,1),10
            i2 = min( i + 9, size(array,1) )
            write( info%lun, '(a,10a)' ) indentation(1:min(indent+4,100)), &
                ( merge('true  ', 'false ', array(j,k)) ,j = i,i2 )
        enddo
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_logical_array_2d

! write_to_xml_word_array --
!    Routine to write an array of words to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_word_array( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    character(len=*), dimension(:), intent(in)   :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do i = 1,size(array),10
        i2 = min( i + 9, size(array) )
        write( info%lun, '(a,20a)' ) indentation(1:min(indent+4,100)), &
            ( trim(array(j)) , ' ' ,j = i,i2 )
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_word_array

! write_to_xml_line_array --
!    Routine to write an array of lines to the XML file
!
! Arguments:
!    info        XML parser structure
!    tag         The tag in question
!    indent      Number of spaces for indentation
!    array       Values to be written
!
subroutine write_to_xml_line_array( info, tag, indent, array )
    type(XML_PARSE), intent(in)                  :: info
    character(len=*), intent(in)                 :: tag
    integer, intent(in)                          :: indent
    logical, dimension(:), intent(in)            :: array

    character(len=100)                           :: indentation
    integer                                      :: i, i2, j

    indentation = ' '

    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '<', trim(tag), '>'
    do i = 1,size(array)
        write( info%lun, '(a)' ) indentation(1:min(indent+4,100)), &
            array(i)
    enddo
    write( info%lun, '(4a)' ) indentation(1:min(indent,100)), &
        '</', trim(tag), '>'

end subroutine write_to_xml_line_array

end module write_xml_primitives
