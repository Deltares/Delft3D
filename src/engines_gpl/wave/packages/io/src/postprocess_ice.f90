subroutine postprocess_ice (mmax     ,nmax    , &
                          & ice_frac ,floe_dia, &
                          & hs       )

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
!!--description-----------------------------------------------------------------
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer                    , intent(in)  :: mmax       ! number of columns
    integer                    , intent(in)  :: nmax       ! number of rows
    real, dimension(mmax, nmax), intent(in)  :: ice_frac   ! sea_ice_area_fraction
    real, dimension(mmax, nmax), intent(in)  :: floe_dia   ! floe_diameter
    real, dimension(mmax, nmax), intent(out) :: hs         ! significant wave height
!
! Local variables
!
    integer                         :: m
    integer                         :: n
!
!! executable statements -------------------------------------------------------
!
    do n = 1, nmax
       do m = 1, mmax
          if (ice_frac(m,n) > 0.1) then
             hs(m,n) = 0.0
          endif
       enddo
    enddo
end subroutine postprocess_ice
