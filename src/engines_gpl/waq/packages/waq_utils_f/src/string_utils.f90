!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!
!
module m_string_utils
    
    implicit none

    private
    public join_strings, contains_any, contains_only_valid_chars

    contains

    function join_strings(strings, separator) result(concatenated_string)
        !<  Returns a single string by joining all the strings in the array <strings>, divided by the given separator.
        character(*), intent(in)  :: strings(:)           !< Array containing strings to be joined
        character(*), intent(in)  :: separator            !< Separator that will be placed among the strings of the array to join.
        character(:), allocatable :: concatenated_string  !< Result of joining all string in the array.
        integer :: i

        ! Allocate memory for the concatenated string
        allocate(character(len=0) :: concatenated_string)

        ! Concatenate the strings with the separator
        do i = 1, size(strings)
            if (i > 1) then
                concatenated_string = trim(adjustl(concatenated_string)) // separator
            end if
            concatenated_string = concatenated_string // trim(adjustl(strings(i)))
        end do
    end function join_strings
    
    logical function contains_any(whole_string, substring_array)
        !< Returns true if any of the substrings in <substring_array> is contained in <whole_string>.
        character(*), dimension(:), intent(in) :: substring_array !< Array containing multiple (sub)strings.
        character(*), intent(in)               :: whole_string    !< String to check if any of the substrings in contained inside.
        
        integer :: idx, i
        
        contains_any = .false.
        idx = 0
        do i = 1, size(substring_array)
            idx = max(idx, index(trim(whole_string), trim(substring_array(i)) ))
        end do
        if (idx>0) then
            contains_any = .true.
        end if
            
    end function contains_any

    logical function contains_only_valid_chars(names_array, valid_characters)
        character(*), dimension(:), intent(in) :: names_array      !< Array with all names to validate
        character(*), intent(in)               :: valid_characters !< Characters permitted in names
    
        integer       :: i, j
        character(len=len(names_array(1))) :: arrows_invalid_chars
        logical       :: current_name_is_valid

        contains_only_valid_chars = .true.
        do i = 1, size(names_array)
            arrows_invalid_chars = repeat(' ', len(names_array(1)))
            current_name_is_valid = .true.
            do j=1, len(trim(names_array(i)))
                !idx =  verify(names_array(i)(j:j), valid_characters)
                if (verify(names_array(i)(j:j), valid_characters)/=0) then
                    arrows_invalid_chars(j:j) = '^'
                    current_name_is_valid = .false.
                end if
            end do
            if (.not.current_name_is_valid) then
                contains_only_valid_chars = .false.
                write(*,*) "Error: invalid characters found in the name:"
                write(*,*) names_array(i)
                write(*,*) arrows_invalid_chars
            end if
        end do
    end function contains_only_valid_chars

end module m_string_utils