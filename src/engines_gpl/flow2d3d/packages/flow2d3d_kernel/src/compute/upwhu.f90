subroutine upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
               & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
               & s0        ,dpu       ,umean     ,hu        ,gdp       )
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
!    Function: Switch which makes it possible to use
!              upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)     , pointer :: dco
    real(fp)     , pointer :: dgcuni
    logical      , pointer :: nonhyd
    character(8) , pointer :: dpuopt
    character(6) , pointer :: momsol
    integer      , pointer :: nh_level
!
! Global variables
!
    integer                                               , intent(in) :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                            :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                               , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                               , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer                                                            :: nmmaxj !  Description and declaration in dimens.igs
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: kspu   !  Description and declaration in esm_alloc_int.f90
    logical                                               , intent(in) :: zmodel !  Description and declaration in procs.igs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: umean  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer             :: nm    ! Counter for loop 1,NMMAX 
    integer             :: nmd
    integer             :: nmu
    integer             :: nmuu
    real(fp)            :: ds1
    real(fp)            :: ds2
    real(fp)            :: h1
    real(fp)            :: h2
    real(fp)            :: level
    real(fp) , external :: slim
    integer             :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    dco      => gdp%gdnumeco%dco
    dgcuni   => gdp%gdnumeco%dgcuni
    nonhyd   => gdp%gdprocs%nonhyd
    dpuopt   => gdp%gdnumeco%dpuopt
    momsol   => gdp%gdnumeco%momsol
    nh_level => gdp%gdnonhyd%nh_level
    nm_pos   =  1
    !
    ! In case DPUOPT = 'UPW' apply upwind approach for for the total
    ! water depth. This implies that upwind also applies for DPU (both
    ! in SIGMA and Z-model)
    ! Reference: "A staggered conservative scheme for every Froude
    !             number in rapidly varied shallow water flows"; by
    !             GS Stelling and SPA Duijnmeijer
    !
    if (dpuopt=='UPW' .and. momsol /= 'finvol') then
       do nm = 1, nmmax
          nmu = nm + icx
          if (kcu(nm)==1) then
             if (umean(nm)>=0.001) then
                dpu(nm) = real(dps(nm),fp)
             elseif (umean(nm)<= - 0.001) then
                dpu(nm) = real(dps(nmu),fp)
             else
                dpu(nm) = min(real(dps(nmu),fp), real(dps(nm),fp))
             endif
          endif
       enddo
       !
       ! exchange depth with neighbours for parallel runs
       !
       call dfexchg ( dpu, 1, 1, dfloat, nm_pos, gdp )
    endif
    !
    ! Apply Upwind of water level if:
    !  -  HU(initial) < DCO (in Z-model not yet tested) OR
    !  -  KSPU        > 0   (in Z-model not yet tested) OR
    !  -  DPUOPT      = 'UPW'
    !
    ! Start by computing HU(initial) and see whether it is < DCO value
    ! and upwind should be applied (one of the criteria above).
    !
    ! In SIGMA case HU(initial) is computed with the mean water level
    ! In Z-model HU(initial) is always computed with the upwind
    ! formulation for the water level
    ! HU must always be computed, even if KCU is not 1
    ! condition
    !
    do nm = 1, nmmax
       nmu = nm + icx
       if (zmodel) then
          if (nonhyd .and. nh_level==nh_full) then
            hu(nm) = 0.5_fp*(s0(nm) + s0(nmu)) + dpu(nm)
          else
            hu(nm) = max(s0(nmu), s0(nm)) + dpu(nm)
          endif
       else
          hu(nm) = 0.5_fp*(s0(nm) + s0(nmu)) + dpu(nm)
       endif
       if (kcu(nm) == 1) then
          if ( hu(nm) < dco    .or. &
             & kspu(nm, 0) > 0 .or. &
             & dpuopt == 'UPW' .or. &
             & zmodel               ) then
             if (umean(nm) > 0.001_fp) then
                hu(nm) = s0(nm) + dpu(nm)
             elseif (umean(nm) < -0.001_fp) then
                hu(nm) = s0(nmu) + dpu(nm)
             else
                hu(nm) = max(s0(nmu), s0(nm)) + dpu(nm)
             endif
          endif
          !
          ! HU determination for flooding option
          ! Special approach for steep bottom slopes
          !
          if (momsol=='flood ') then
             if (umean(nm) >= 0.001_fp) then
                nmd  = nm  - icx
                ds1  = (s0(nm )-s0(nmd)) * kcu(nmd)
                ds2  = (s0(nmu)-s0(nm )) * kcu(nm )
                if (kspu(nm,0)>0 .or. kspu(nmd,0)>0) then
                   ds2 = 0.0_fp
                endif
                level= s0(nm) + slim(ds1,ds2)
                hu(nm) = level + dpu(nm)
                if (real(dps(nm),fp) > real(dps(nmu),fp)+dgcuni) then
                   h1     = 2.0_fp * hu(nm) / 3.0_fp
                   h2     = s0(nmu) + dpu(nm)
                   hu(nm) = MIN(hu(nm) , MAX(h1,h2))
                endif
             elseif (umean(nm) <= - 0.001_fp) then
                nmuu = nmu + icx
                ds1  = (s0(nmuu)-s0(nmu)) * kcu(nmu)
                ds2  = (s0(nmu )-s0(nm )) * kcu(nm )
                if (kspu(nm,0)>0 .or. kspu(nmu,0)>0) then
                   ds2 = 0.0_fp
                endif
                level  = s0(nmu) - slim(ds1,ds2)
                hu(nm) = level + dpu(nm)
                if (real(dps(nm),fp)+dgcuni < real(dps(nmu),fp)) then
                   h1     = 2.0_fp * hu(nm) / 3.0_fp
                   h2     = s0(nm) + dpu(nm)
                   hu(nm) = MIN(hu(nm) , MAX(h1,h2))
                endif
             else 
                hu(nm) = dpu(nm) + MAX(s0(nmu),s0(nm))
             endif
          endif
       endif
    enddo
    !
    ! exchange height with neighbours for parallel runs
    !
    call dfexchg ( hu, 1, 1, dfloat, nm_pos, gdp )
    call upwhu_dd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
                & s0        ,dpu       ,umean     ,hu        ,gdp       )
end subroutine upwhu
