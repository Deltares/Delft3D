subroutine wavenr(h         ,t         ,k         ,ag        )
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!    Function: Approximation of the dispersion, original sub-
!              routine Disp10 (L, T, h, g)
! Method used: relation according to linear wave theory:
!
!  = k TANH k
!
! h k = 2 pi h / L and w = (2 pi / T) sqrt (h/g), and L as
! nown.
! ational function approximation is made of the form :
!
!     1 + a1 w^2 + a2 w^4 + a3 w^6 + a4 w^8 + a5 w^10 + a6 w^12
! w^2 ---------------------------------------------------------
!     1 + b1 w^2 + b2 w^4 + b3 w^6 + b4 w^8           + a6 w^10
!
! ing the exact values for L for:
!  0.4, 0.7, 1.0, 1.3, 1.6, 1.95, 2.35, 2.9, 3.8 and 6.5,
!  a relative error less than 1.7E-6 for all w.
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
    !
!
! Local parameters
!
    real(hp), parameter :: a1 = 5.060219360721177D-01, a2 = 2.663457535068147D-01,&
                         & a3 = 1.108728659243231D-01, a4 = 4.197392043833136D-02,&
                         & a5 = 8.670877524768146D-03, a6 = 4.890806291366061D-03,&
                         & b1 = 1.727544632667079D-01, b2 = 1.191224998569728D-01,&
                         & b3 = 4.165097693766726D-02, b4 = 8.674993032204639D-03
!
! Global variables
!
    real(fp), intent(in)               :: h  !!  Waterheight
    real(fp), intent(out)              :: k  !!  Approxiamation of wave lenght
    real(fp), intent(in)               :: t  !!  Period
    real(fp), intent(in)               :: ag !!  Gravitational acceleration
!
!
! Local variables
!
    real(hp)               :: den                  ! Denominator 
    real(hp)               :: kd                   ! Double value for K 
    real(hp)               :: num                  ! Numerator 
    real(hp)               :: ome2                 ! Omega 
!
!
!! executable statements -------------------------------------------------------
!
    ome2 = (2.0D0*pi_hp/real(t, hp))**2*real(h, hp)/real(ag, hp)
    !
    num = 1.0D0 +                                                               &
        & ome2*(a1 + ome2*(a2 + ome2*(a3 + ome2*(a4 + ome2*(a5 + ome2*a6)))))
    den = 1.0D0 + ome2*(b1 + ome2*(b2 + ome2*(b3 + ome2*(b4 + ome2*a6))))
    kd = sqrt(ome2*num/den)/real(h, hp)
    k = real(kd, fp)
end subroutine wavenr
