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
!
!
module m_validate_input

    use m_string_utils, only: join_strings, contains_any, contains_only_valid_chars, &
                              starts_with_valid_char

    implicit none

    private
    
    public validate_units, validate_names

    contains
    subroutine validate_units(units, logging_unit)
        !< Validates if the string <units> contains any of the invalid unit expressions defined in this subroutine.
        character(*), intent(in) :: units        !< Units string to validate that it doesn't contain any invalid expressions.
        integer, intent(in)      :: logging_unit !< Number of the logging unit to which messages are sent.
        
        character(len=:), allocatable  :: units_message
        character(len=4), dimension(4) :: invalid_units = &
           (/ "m**2", "m^2 ", "m**3", "m^3 " /)
        
        if (contains_any(units, invalid_units)) then
            units_message = join_strings(invalid_units, ',')
            units_message =  'The units definition: ' // trim(units) // ' is invalid. The following units are not allowed: '// &
                  units_message
            write(logging_unit, *) units_message
            stop 'Program stopped. Invalid units found. More info can be found in the log file.'
        end if
    end subroutine validate_units

    subroutine validate_names(names_array, logging_unit)
        !< Validates if all characters in an array of names (strings) <names_array> are valid.
        !< If not, detailed information is sent to the user screen, and the program stops.
        character(*), dimension(:), intent(in) :: names_array  !< Array with all names to validate
        integer, intent(in)                    :: logging_unit !< Number of the logging unit to which messages are sent.

        character(26), parameter :: lower_case_alph = 'abcdefghijklmnopqrstuvwxyz'
        character(26), parameter :: upper_case_alph = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        character(10), parameter :: digits = '0123456789'
       
        character(52) :: valid_start_characters = lower_case_alph // upper_case_alph             !< Characters permitted as start of names
        character(63) :: valid_characters = lower_case_alph // upper_case_alph // digits // '_'  !< Characters permitted in names

         if (.not.starts_with_valid_char(names_array, valid_start_characters, logging_unit)) then
            write(logging_unit,*) 'Only the following characters may start a name:'
            write(logging_unit,*) valid_start_characters
            stop 'Program stopped. Invalid character(s) found at start of name(s). More info can be found in the log file.'
         end if
        if (.not. contains_only_valid_chars(names_array, valid_characters, logging_unit)) then
            write(logging_unit,*) 'Only the following characters are allowed in names:'
            write(logging_unit, *) valid_characters
            stop 'Program stopped. Invalid character(s) found in name(s). More info can be found in the log file.'
        end if
    end subroutine validate_names

end module m_validate_input