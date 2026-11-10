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

!> This module defines some general mathematical constants like pi and
!! conversion factors from degrees to radians and from (earth) days to
!! seconds.
!!
!! This module does NOT include physical constants like earth radius and
!! gravity, or application dependent constants like missing values.
module m_mathconstants

   use precision
   implicit none
   
   ! single precision constants
   real(sp), parameter :: EE_SP      = exp(1.0_sp)                      !< ee = 2.718281...
   real(sp), parameter :: PI_SP      = 4.0_sp*atan(1.0_sp)              !< pi = 3.141592...
   real(sp), parameter :: TWOPI_SP   = 8.0_sp*atan(1.0_sp)              !< 2pi
   real(sp), parameter :: SQRT2_SP   = sqrt(2.0_sp)                     !< sqrt(2)
   real(sp), parameter :: DEGRAD_SP  = 4.0_sp*atan(1.0_sp)/180.0_sp     !< conversion factor from degrees to radians (pi/180)
   real(sp), parameter :: RADDEG_SP  = 180.0_sp/(4.0_sp*atan(1.0_sp))   !< conversion factor from radians to degrees (180/pi)
   real(sp), parameter :: DAYSEC_SP  = 24.0_sp*60.0_sp*60.0_sp          !< conversion factor from earth day to seconds
   real(sp), parameter :: YEARSEC_SP = 365.0_sp*24.0_sp*60.0_sp*60.0_sp !< conversion factor from earth year to seconds (non-leap)
   real(sp), parameter :: EPS_SP     = epsilon(1.0_sp)                  !< epsilon for sp
   
   ! flexible precision constants
   real(fp), parameter :: EE         = exp(1.0_fp)                      !< ee = 2.718281.../2.71828182845904...
   real(fp), parameter :: PI         = 4.0_fp*atan(1.0_fp)              !< pi = 3.141592.../3.14159265358979...
   real(fp), parameter :: TWOPI      = 8.0_fp*atan(1.0_fp)              !< 2pi
   real(fp), parameter :: SQRT2      = sqrt(2.0_fp)                     !< sqrt(2)
   real(fp), parameter :: DEGRAD     = 4.0_fp*atan(1.0_fp)/180.0_fp     !< conversion factor from degrees to radians (pi/180)
   real(fp), parameter :: RADDEG     = 180.0_fp/(4.0_fp*atan(1.0_fp))   !< conversion factor from radians to degrees (180/pi)
   real(fp), parameter :: DAYSEC     = 24.0_fp*60.0_fp*60.0_fp          !< conversion factor from earth day to seconds
   real(fp), parameter :: YEARSEC    = 365.0_fp*24.0_fp*60.0_fp*60.0_fp !< conversion factor from earth year to seconds (non-leap)
   real(fp), parameter :: EPS_FP     = epsilon(1.0_fp)                  !< epsilon for fp
   
   ! high precision constants
   real(hp), parameter :: EE_HP        = exp(1.0_hp)                      !< ee = 2.71828182845904...
   real(hp), parameter :: PI_HP        = 4.0_hp*atan(1.0_hp)              !< pi = 3.14159265358979...
   real(hp), parameter :: TWOPI_HP     = 8.0_hp*atan(1.0_hp)              !< 2pi
   real(hp), parameter :: SQRT2_HP     = sqrt(2.0_hp)                     !< sqrt(2)
   real(hp), parameter :: DEGRAD_HP    = 4.0_hp*atan(1.0_hp)/180.0_hp     !< conversion factor from degrees to radians (pi/180)
   real(hp), parameter :: RADDEG_HP    = 180.0_hp/(4.0_hp*atan(1.0_hp))   !< conversion factor from radians to degrees (180/pi)
   real(hp), parameter :: DAYSEC_HP    = 24.0_hp*60.0_hp*60.0_hp          !< conversion factor from earth day to seconds
   real(hp), parameter :: YEARSEC_HP   = 365.0_hp*24.0_hp*60.0_hp*60.0_hp !< conversion factor from earth year to seconds (non-leap)
   real(hp), parameter :: EPS_HP       = epsilon(1.0_hp)                  !< epsilon for hp
      
   ! double precision constants
   real(kind=dp), parameter :: EPS_DP = epsilon(1.0_dp)                  !< epsilon for dp
      
end module m_mathconstants
