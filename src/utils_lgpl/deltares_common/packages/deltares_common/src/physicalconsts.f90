module physicalconsts
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
!!--description-----------------------------------------------------------------
!
! This module defines some general physical constants 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use precision
   implicit none
   private
   
   public :: celsius_to_kelvin
   public :: kelvin_to_celsius

   real(kind=hp), parameter, public :: earth_radius = 6378137_hp        !< earth radius (m)
   real(kind=hp), parameter, public :: dtol_pole    = 0.0001_hp         !< pole tolerance in degrees
   real(kind=hp), parameter, public :: C_TO_KELVIN  = 273.15_hp         !< conversion offset between Celsius and Kelvin
   real(kind=hp), parameter, public :: stf          = 5.6705085e-8_hp   !< Stefan's constant =5.6705085e-8 [W/m^2/K^4]
                                                                        !! (see 19308-part-iv-physical-processes.pdf from ECMWF;
                                                                        !!  it differs slightly from the value after the redefinition of SI in 2019)

   interface celsius_to_kelvin
      module procedure celsius_to_kelvin_hp
      module procedure celsius_to_kelvin_sp
   end interface celsius_to_kelvin

   interface kelvin_to_celsius
      module procedure kelvin_to_celsius_hp
      module procedure kelvin_to_celsius_sp
   end interface kelvin_to_celsius

contains

   elemental function celsius_to_kelvin_hp(celsius) result(kelvin)
      real(kind=hp), intent(in) :: celsius
      real(kind=hp) :: kelvin
      kelvin = celsius + C_TO_KELVIN
   end function celsius_to_kelvin_hp

   elemental function celsius_to_kelvin_sp(celsius) result(kelvin)
      real(kind=sp), intent(in) :: celsius
      real(kind=sp) :: kelvin
      kelvin = celsius + real(C_TO_KELVIN, sp)
   end function celsius_to_kelvin_sp

   elemental function kelvin_to_celsius_hp(kelvin) result(celsius)
      real(kind=hp), intent(in) :: kelvin
      real(kind=hp) :: celsius
      celsius = kelvin - C_TO_KELVIN
   end function kelvin_to_celsius_hp

   elemental function kelvin_to_celsius_sp(kelvin) result(celsius)
      real(kind=sp), intent(in) :: kelvin
      real(kind=sp) :: celsius
      celsius = kelvin - real(C_TO_KELVIN, sp)
   end function kelvin_to_celsius_sp

end module physicalconsts
