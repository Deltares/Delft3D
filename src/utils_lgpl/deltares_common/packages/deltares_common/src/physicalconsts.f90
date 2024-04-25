module physicalconsts
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
   !
   ! high precision constants
   !

   real(kind=hp), parameter, public :: earth_radius = 6378137_hp        !< earth radius (m)
   real(kind=hp), parameter, public :: dtol_pole    = 0.0001_hp         !< pole tolerance in degrees
   real(kind=hp), parameter, public :: CtoKelvin    = 273.15_hp         !< conversion offset between Celsius and Kelvin
   real(kind=hp), parameter, public :: stf          = 5.6705085e-8_hp   !< Stefan's constant =5.6705085e-8 [W/m^2/K^4]
                                                                        !! (see 19308-part-iv-physical-processes.pdf from ECMWF;
                                                                        !!  it differs slightly from the value after the redefinition of SI in 2019)

end module physicalconsts
