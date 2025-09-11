subroutine santoss_abreu(hrms, km, d, r_ab, phi_ab, urms, tp, nt, tw, uorb)
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
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only:sqrt2, pi
    use ieee_arithmetic, only: ieee_is_nan

    implicit none
!
! arguments
!
    real(fp)                , intent(in)  :: hrms
    real(fp)                , intent(in)  :: km
    real(fp)                , intent(in)  :: d
    real(fp)                , intent(in)  :: r_ab
    real(fp)                , intent(in)  :: phi_ab
    real(fp)                , intent(in)  :: urms
    real(fp)                , intent(in)  :: tp
    integer                 , intent(in)  :: nt
!
    real(fp), dimension(nt) , intent(out) :: tw
    real(fp), dimension(nt) , intent(out) :: uorb
!
! local variables
!
    integer                :: i
    real(fp)               :: f_ab
    real(fp)               :: uw
    real(fp)               :: dt
    real(fp)               :: omega
!
!! executable statements -------------------------------------------------------
!
!   input variables f and n for Abreu formula
!
    f_ab  = sqrt(1.0_fp-r_ab**2)  ! underneath eq. (7)

!   determine orbital amplitude (linear wave theory)
    uw    = sqrt2*urms

!   determine orbital velocities serie with Abreu et al. (2010)
    dt    = tp/nt
    tw(1) = 0.0_fp
    omega = 2*pi/tp
    do i = 1,nt
        uorb(i) = uw*f_ab*(sin(omega*tw(i))+r_ab*sin(phi_ab)/ &
                & (1.0_fp+sqrt(1.0_fp-r_ab**2)))/(1.0_fp-r_ab*cos(omega*tw(i)+phi_ab))
        if (ieee_is_nan(uorb(i))) uorb(i) = 0.0_fp
        if (i /= nt) then
          tw(i+1) = tw(i)+dt
        endif
    enddo
end subroutine santoss_abreu
