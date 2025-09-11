!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

  module m_obtain_number_decimals
  
  implicit none
  contains
  
  
    integer function obtain_num_decimals_version(version)
      ! This works up till 3 decimals in the version number. That should be sufficient

      real, intent(in)  :: version
      real              :: version_increment

      version_increment = version
      
      ! Determine the number of decimal places in the version number
      obtain_num_decimals_version = 0
      do while (abs(version_increment - int(version_increment)) > 1e-3)
        version_increment = version_increment * 10.0
        obtain_num_decimals_version = obtain_num_decimals_version + 1
      end do

    end function obtain_num_decimals_version

end module m_obtain_number_decimals