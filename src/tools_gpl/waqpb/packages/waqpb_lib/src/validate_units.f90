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
module m_validate_units

    use m_string_utils, only: join_strings, contains_any

    implicit none

    private
    
    public validate_units

    contains
    subroutine validate_units(units)
        !< Validates if the string <units> contains any of the invalid unit expressions defined in this subroutine.
        character(*), intent(in) :: units !< Units string to validate that it doesn't contain any invalid expressions.
        
        character(len=:), allocatable  :: units_message
        character(len=4), dimension(4) :: invalid_units = &
           (/ "m**2", "m^2 ", "m**3", "m^3 " /)
        
        if (contains_any(units, invalid_units)) then
            units_message = join_strings(invalid_units, ',')
            stop 'The units definition: ' // trim(units) // ' is invalid. The following units are not allowed: '// &
                  units_message // '. Program stopped.'
        end if
    end subroutine validate_units

end module m_validate_units