subroutine ua_rvr(facas,    facsk,    sws,    h,    hrms, &
               &  rlabda, urms, ua)
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
! computes velocity asymmetry due to waves according to
! Ruessink et al. 2009 JGR
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
    !
    ! Arguments
    !
    real(fp), intent(in)          :: facas
    real(fp), intent(in)          :: facsk
    integer, intent(in)           :: sws
    real(fp), intent(in)          :: rlabda
    real(fp), intent(in)          :: hrms
    real(fp), intent(in)          :: h
    real(fp), intent(in)          :: urms
    real(fp), intent(out)         :: ua
    !
    ! Local variables
    !
    real(fp)       :: m1, m2, m3, m4, m5, m6
    real(fp)       :: urs
    real(fp)       :: bm
    real(fp)       :: b1
    real(fp)       :: waveno
    real(fp)       :: sk
    real(fp)       :: as
    !
    ! Constants
    !
    m1 = 0_fp       ! a = 0
    m2 = 0.857_fp   !0.7939;  ! b = 0.79 +/- 0.023
    m3 = -0.471_fp  !-0.6065; ! c = -0.61 +/- 0.041
    m4 = 0.297_fp   !0.3539;  ! d = -0.35 +/- 0.032
    m5 = 0.815_fp   !0.6373;  ! e = 0.64 +/- 0.025
    m6 = 0.672_fp   !0.5995;  ! f = 0.60 +/- 0.043
    !
    waveno = twopi / max(rlabda,1.0e-12_fp)
    urs = 3.0_fp/8.0_fp*sqrt(2.0_fp)*hrms*waveno/(waveno*h)**3                    !Ursell number
    urs = max(urs,1e-12_fp)
    bm = m1 + (m2-m1)/(1+exp((m3-log10(urs))/m4));
    b1 = (-90.0_fp+90.0_fp*tanh(m5/urs**m6))*pi/180.0_fp
    sk = bm*cos(b1)                                            !Skewness (eq 8)
    as = bm*sin(b1)                                            !Asymmetry(eq 9)
    ua = sws*(facsk*sk-facas*as)*urms
   
end subroutine ua_rvr