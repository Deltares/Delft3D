subroutine soursin_3d(h1          ,thick0      ,thick1      ,sigsed      ,thicksed    , &
                    & r0          ,vicmol      ,sigmol      ,seddif      ,rhosol      , &
                    & caks        ,ws          ,aks         , &
                    & sour_ex     ,sour_im     ,sink        )
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
!    Function: Computes the sour and sink terms for the 3D case
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    real(fp), intent(in)  :: caks
    real(fp), intent(in)  :: h1
    real(fp), intent(in)  :: r0
    real(fp), intent(in)  :: rhosol
    real(fp), intent(in)  :: seddif
    real(fp), intent(in)  :: sigsed  
    real(fp), intent(in)  :: sigmol
    real(fp), intent(in)  :: thicksed
    real(fp), intent(in)  :: thick0
    real(fp), intent(in)  :: thick1
    real(fp), intent(in)  :: vicmol
    real(fp), intent(in)  :: ws
    real(fp), intent(in)  :: aks
    real(fp), intent(out) :: sour_ex
    real(fp), intent(out) :: sour_im
    real(fp), intent(out) :: sink
!
! Local variables
!
    real(fp) :: a0kmx
    real(fp) :: a0kmxb
    real(fp) :: alpha1
    real(fp) :: alpha2
    real(fp) :: caksrho
    real(fp) :: ckmxb
    real(fp) :: dcdz
    real(fp) :: diffus
    real(fp) :: dz
    real(fp) :: r00
    real(fp) :: rouse
    real(fp) :: temp0
    real(fp) :: temp1
    real(fp) :: zkmx
    real(fp) :: zkmxb
!
!! executable statements -------------------------------------------------------
!
    !
    ! If the reference concentration is larger than the computed concentration
    ! in layer kmx (=kmaxsd) then diffusion will lead to upward transport. Test
    ! based on concentration at previous timestep.
    !
    if (caks*rhosol > r0) then
       caksrho  = max(1.0e-4_fp,caks*rhosol)
       r00      = max(r0, 1.0e-7_fp)
       diffus   = vicmol/sigmol + seddif
       zkmx     = h1*(1.0_fp + sigsed)
       dz       = zkmx - aks
       !
       if (comparereal(aks,0.0_fp) == 0) then
          !
          ! Concentration near bed has been given.
          !
          ckmxb = caksrho
          !
          ! Concentration gradient at bed equals settling velocity divided by
          ! diffusivity when in equilibrium.
          !
          dcdz  = -ws*ckmxb/diffus
       else
          !
          ! More accurate estimation of concentration and concentration gradient
          ! at bottom of kmx cell. See also the description with equations in the
          ! manual.
          !
          ! Compute the exponent of the Rouse profile --- also known as Van Rijn's
          ! suspension number Z or (modified) Rouse number --- based on
          ! concentration r0 at zkmx and reference concentration caks at aks.
          !
          a0kmx    = (aks*(h1 - zkmx))/(zkmx*(h1 - aks))
          rouse    = log(r00/caksrho)/log(a0kmx)
          !
          ! Estimate concentration at lower interface of layer kmx.
          !
          zkmxb    = zkmx - 0.5_fp*thicksed*h1
          a0kmxb   = (aks*(h1 - zkmxb))/(zkmxb*(h1 - aks))
          ckmxb    = caksrho*a0kmxb**rouse
          !
          ! Estimate concentration gradient at lower interface of layer kmx.
          !
          temp0    = rouse * caksrho * a0kmxb**(rouse - 1.0_fp)
          temp1    = -(aks*h1)/((h1 - aks)*zkmxb**2)
          dcdz     = temp0*temp1
       endif
       !
       ! Determine alpha1 (ratio of concentration at lower interface over
       ! concentration at cell centre) and alpha2 (ratio of gradient at lower
       ! interface over gradient between cell centre and aks)
       !
       alpha1   = ckmxb/r00
       if (abs(r00 - caksrho) < 1.0e-5_fp) then
          alpha2 = 1.0_fp
       else
          alpha2 = dcdz/((r00 - caksrho)/dz)
       endif
       alpha1 = min(max(0.1_fp, alpha1), 10.0_fp)
       alpha2 = min(max(0.1_fp, alpha2), 10.0_fp)
       !
       ! Compute explicit source and implicit sink terms
       !
       sour_ex = alpha2*caksrho*diffus/dz
       sour_im = alpha2*diffus/dz
       sink    = alpha1*ws
       !
       ! Source and sink terms are calculated per unit
       ! volume
       !
       sour_ex = max(0.0_fp, sour_ex) / thick0
       sour_im = max(0.0_fp, sour_im) / thick1
       sink    = max(0.0_fp, sink) / thick1
    else
       !
       ! (if downward diffusion into the bed would occur)
       ! No source term is calculated (diffusion ignored)
       ! use simple estimate for settling flux out of
       ! bottom SAND cell
       !
       sour_ex = 0.0_fp
       sour_im = 0.0_fp
       sink    = ws/thick1
    endif
end subroutine soursin_3d
