subroutine santoss_ripple(d50, uwc, uwt, delta, g, aw, rh, rl)
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
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!   Calculate ripple length and ripple height    
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi
    implicit none
!
! arguments
!
    real(fp)  , intent(in)  :: d50
    real(fp)  , intent(in)  :: uwc
    real(fp)  , intent(in)  :: uwt
    real(fp)  , intent(in)  :: delta
    real(fp)  , intent(in)  :: g
    real(fp)  , intent(in)  :: aw
!
    real(fp)  , intent(out) :: rh
    real(fp)  , intent(out) :: rl
!
! local variables
!
    real(fp)                :: mn
    real(fp)                :: ml
    real(fp)                :: nn
    real(fp)                :: nl
    real(fp)                :: psimax
!
!! executable statements -------------------------------------------------------
!
    psimax = max(uwc,uwt)**2 / (delta*g*d50)
    
    if (d50 <= 0.00022_fp) then
        mn = 0.55_fp
        ml = 0.73_fp
    elseif (d50 >= 0.0003_fp) then
        mn = 1.0_fp
        ml = 1.0_fp
    else
        mn = 0.55_fp+0.45_fp*(1000.0_fp*d50-0.22_fp)/(0.3_fp-0.22_fp)
        ml = 0.73_fp+0.27_fp*(1000.0_fp*d50-0.22_fp)/(0.3_fp-0.22_fp)
    endif
    if (psimax <= 190.0_fp) then
        nn = 1.0_fp
        nl = 1.0_fp
    elseif (psimax >= 240.0_fp) then
        nn = 0.0_fp
        nl = 0.0_fp
    else
        nn = 0.5_fp*(1.0_fp+cos(2.0_fp*pi*(psimax-190.0_fp)/(2.0_fp*(240.0_fp-190.0_fp))))
        nl = 0.5_fp*(1.0_fp+cos(2.0_fp*pi*(psimax-190.0_fp)/(2.0_fp*(240.0_fp-190.0_fp))))
    endif

    rh = aw*mn*nn*(0.275_fp-0.022_fp*psimax**0.42_fp)
    rl = aw*ml*nl*(1.970_fp-0.440_fp*psimax**0.21_fp)
end subroutine santoss_ripple