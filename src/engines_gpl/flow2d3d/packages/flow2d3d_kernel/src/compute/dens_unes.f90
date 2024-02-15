subroutine dens_unes(temp, salt, rhouns, rhods, rhodt)
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
!!--description-----------------------------------------------------------------
!
!    Function: Computes water density from temperature and
!              salinity using equation of state (rhowat).
!              
! Method used: Equation of state following UNESCO, (UNESCO,
!              Algorithms for computation of fundamental 
!              properties of seawater, UNESCO technical papers
!              in marine science, 1983)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use precision
    implicit none
!
! Global variables
!
    real(fp), intent(in)     :: salt
    real(fp), intent(in)     :: temp
    real(fp), intent(out)    :: rhouns
    real(fp), intent(out)    :: rhods
    real(fp), intent(out)    :: rhodt
!
! Local variables
!
    real(fp)                            :: s
    real(fp)                            :: sq
    real(fp)                            :: rhwa
    real(fp)                            :: asal
    real(fp)                            :: bsal
    real(fp), dimension(5)              :: t
    real(fp), dimension(0:5), parameter :: cf = &
    (/ 999.842594_fp   ,&
         6.793952e-2_fp  ,&
        -9.095290e-3_fp  ,&
         1.001685e-4_fp  ,&
        -1.120083e-6_fp  ,&
         6.536332e-9_fp  /)
    real(fp), dimension(0:4), parameter :: ca = &
    (/   8.24493e-1_fp ,&
        -4.0899e-3_fp  ,&
         7.6438e-5_fp  ,&
        -8.2467e-7_fp  ,&
         5.3875e-9_fp  /)
    real(fp), dimension(0:2), parameter :: cb = &
    (/  -5.72466e-3_fp  ,&
         1.0227e-4_fp   ,&
        -1.6546e-6_fp   /)
    real(fp)                , parameter :: csal = 4.8314e-4_fp
!
!! executable statements -------------------------------------------------------
!
    t(1)   = temp
    t(2)   = temp*t(1)
    t(3)   = temp*t(2)
    t(4)   = temp*t(3)
    t(5)   = temp*t(4)
    !
    s      = salt
    sq     = sqrt(max(0.0_fp,s))
    !
    rhwa   = cf(0) + cf(1)*t(1) + cf(2)*t(2) + cf(3)*t(3) + cf(4)*t(4) &
           &       + cf(5)*t(5)
    asal   = ca(0) + ca(1)*t(1) + ca(2)*t(2) + ca(3)*t(3) + ca(4)*t(4)
    bsal   = cb(0) + cb(1)*t(1) + cb(2)*t(2)
    !
    rhouns = rhwa + (asal+bsal*sq+csal*s) * s
    !
    rhods  = asal + 1.5_fp*bsal*sq + 2.0_fp*csal*s
    !
    rhodt  = cf(1) +  2.0_fp*cf(2)*t(1) + 3.0_fp*cf(3)*t(2) &
           &       +  4.0_fp*cf(4)*t(3) + 5.0_fp*cf(5)*t(4) &
           &       + (ca(1) + 2.0_fp*ca(2)*t(1) + 3.0_fp*ca(3)*t(2) &
           &       +  4.0_fp*ca(4)*t(3)) * s &
           &       + (cb(1) + 2.0_fp*cb(2)*t(1)) * sq * s 
end subroutine dens_unes
