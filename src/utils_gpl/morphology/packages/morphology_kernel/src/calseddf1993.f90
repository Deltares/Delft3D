subroutine calseddf1993(ustarc    ,ws        ,h1        ,kmax      ,sig       , &
                      & thick     ,dicww     ,tauwav    ,tauc      ,ltur      , &
                      & eps       ,vonkar    ,difvr     ,deltas    ,epsbed    , &
                      & epsmax    ,epsmxc    ,seddif    )
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
! Compute sediment diffusion coefficient
! Van Rijn (1993)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Arguments
!
    integer                    , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                    , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    real(fp)                   , intent(in)  :: deltas
    real(fp), dimension(0:kmax), intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: eps
    real(fp)                   , intent(in)  :: epsbed
    real(fp)                   , intent(in)  :: epsmax
    real(fp)                   , intent(in)  :: h1
    real(fp), dimension(kmax)  , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: tauc
    real(fp)                   , intent(in)  :: tauwav
    real(fp), dimension(kmax)  , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: ustarc
    real(fp)                   , intent(in)  :: vonkar
    real(fp), dimension(0:kmax), intent(in)  :: ws     !  Description and declaration in esm_alloc_real.f90
    logical                    , intent(in)  :: difvr
    !
    real(fp)                   , intent(out) :: epsmxc
    real(fp), dimension(0:kmax), intent(out) :: seddif !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                     :: k
    real(fp)                    :: beta
    real(fp)                    :: betaef
    real(fp)                    :: epscur
    real(fp)                    :: epswav
    real(fp)                    :: hs
    real(fp)                    :: zi
!
!! executable statements -------------------------------------------------------
!
    !
    ! calculate vertical sediment diffusion coefficient
    !
    if (ustarc > eps) then
       !
       ! Beta factor assumed constant over the depth, using ws at
       ! water surface (approx. clear water). Beta limited to
       ! range 1 - 1.5 following Van Rijn (1993)
       !
       beta = 1.0_fp + 2.0_fp*(ws(1)/ustarc)**2
       beta = min(max(1.0_fp, beta), 1.5_fp)
    else
       beta = 1.0_fp
    endif
    !
    epsmxc = 0.25_fp * vonkar * beta * ustarc * h1
    if (ltur==0 .or. ltur==1 .or. difvr) then
       !
       ! if algebraic or K-L turbulence model or difvr = .true. then
       ! calculate sediment mixing according to Van Rijn based on his
       ! parabolic-linear mixing distribution for current-related mixing
       !
       ! set vertical sediment mixing values for waves and currents at water surface
       !
       epswav    = epsmax
       epscur    = epsmxc
       seddif(0) = sqrt(epscur**2 + epswav**2)
       !
       ! loop through vertical
       !
       do k = 1, kmax
          !
          ! calculate level of lower cell interface
          ! epscur as described by Van Rijn 1984
          !
          zi = (1.0_fp + sig(k) - thick(k)/2.0_fp) * h1
          if (zi >= 0.5_fp*h1) then
             epswav = epsmax
             epscur = epsmxc
          else
             if (zi > deltas) then
                epswav = epsbed + (epsmax-epsbed) * (zi-deltas) / (0.5_fp*h1-deltas)
             else
                epswav = epsbed
             endif
             epscur = epsmxc * (1.0_fp-(1.0_fp-2.0_fp*zi/h1)**2)
          endif
          !
          ! set vertical sediment mixing values for waves and currents
          !
          seddif(k) = sqrt(epscur**2 + epswav**2)
       enddo
    else
       !
       ! set vertical sediment mixing values based on K-epsilon turbulence model
       ! note: beta factor should only be applied to current-related mixing
       ! this is rather rough method to estimate the proportion of the mixing
       ! that is due to current.
       !
       if (tauwav+tauc > eps) then
          betaef = 1.0_fp + (beta-1.0_fp) * tauc / (tauwav+tauc)
       else
          betaef = beta
       endif
       do k = 0, kmax
          seddif(k) = dicww(k) * betaef
       enddo
    endif
end subroutine calseddf1993
