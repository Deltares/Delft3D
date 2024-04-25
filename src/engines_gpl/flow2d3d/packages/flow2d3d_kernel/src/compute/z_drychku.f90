subroutine z_drychku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                   & kcs       ,kfu       ,kcu       ,kspu      ,kfsmax    , &
                   & kfsmin    ,kfsz1     ,kfuz1     ,kfumin    ,kfumn0    ,kfumax    , &
                   & kfumx0    ,hu        ,s1        ,dpu       ,dps       , &
                   & umean     ,u0        ,u1        ,dzu0      ,dzu1      , &
                   & dzs1      ,zk        ,kfsmx0    ,guu       ,qxk       , &
                   & gdp       )
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
!  $Id: z_drychku.f90 1044 2011-11-21 21:22:12Z platzek $
!  : https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20111115_13532_z-model_improvements_oss-merge/engines_gpl/flow2d3d/packages/kernel/src/compute/z_drychku.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: This routine checks the drying and flooding at ve-
!              locity points and sets the value of the mask
!              arrays to zero.
!              Always upwind-approach for wet cross section.
!
! Method used: Upwind-approach for wet cross section.
!              Fixed layer approach
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer      , pointer :: nh_level
    logical      , pointer :: nonhyd
    logical      , pointer :: zmodel
    character(6) , pointer :: momsol
    real(fp)     , pointer :: dzmin
!
! Global variables
!
    integer                                                         :: icx    !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                         :: j      !  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                         :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfumn0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: qxk    !  Description and declaration in esm_alloc_real.f90    
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(0:kmax)                     , intent(in)  :: zk
!
! Local variables
!
    integer  :: k
    integer  :: kkmin
    integer  :: kkmax
    integer  :: nm
    integer  :: nmd
    integer  :: nmu
    integer  :: nm_pos
    real(fp) :: dzutot
    real(fp) :: hnm
    real(fp) :: s1u
    real(fp) :: zkk
    logical  :: found
    real(fp) :: dpuavg
!
!! executable statements -------------------------------------------------------
!
    nh_level => gdp%gdnonhyd%nh_level
    nonhyd   => gdp%gdprocs%nonhyd
    zmodel   => gdp%gdprocs%zmodel
    momsol   => gdp%gdnumeco%momsol
    dzmin    => gdp%gdzmodel%dzmin
    !
    ! Update the layer administration for the newly determined water levels S1
    !
    kkmin = 0
    kkmax = 0
    umean = 0.0_fp
    !
    do nm = 1, nmmax
       nmu = nm + icx
       if (kcu(nm)/=0 .and. kcs(nm)*kcs(nmu)>0) then
          !
          ! First determine umean for the top layer(s) of cells NM and NMU
          ! to find the upwind direction
          !
          !nmu   = nm + icx
          !
          ! Determine layers participating in velocity point
          !
          ! kkmin = min(kfsmx0(nm), kfsmx0(nmu))
          ! kkmax = max(kfsmx0(nm), kfsmx0(nmu))
          !
          ! kkmin below the bottom in NM or NMU?
          !
          !kkmin = max( kkmin, max(kfsmin(nm), kfsmin(nmu)) )
          !
          if (kfu(nm) == 1) then
             hnm = 0.0_fp
             do k = kfumn0(nm), kfumx0(nm)
                umean(nm) = umean(nm) + u0(nm,k)*dzu0(nm,k)
                hnm       = hnm + dzu0(nm,k)
             enddo
             umean(nm) = umean(nm) / max(hnm, 0.01_fp)
          else
          endif
       endif
    enddo
    !
    ! For NH-model without ADI: UMEAN = u0(nm,kfumx0(nm)), Z_MOMCOR
    !
    ! in case of non-hydrostatic finite volume modeling 
    ! (1) dzu at the surface should not depend on the upwind
    ! recontruction of the depth values and (2) an improved 
    ! reconstruction of the bottom levels is used
    !
    if (momsol=='finvol') then
        do nm = 1, nmmax
          nmu = nm + icx
          if (kcu(nm)==1) then
             !
             ! within single layer: MEAN
             ! in case of vertical wall across multiple layers: smooth MIN approach
             !
             dpuavg = 0.5_fp * (real(dps(nmu),fp) + real(dps(nm),fp))
             dpu(nm) = min( -zk( kfumin(nm)-1) , dpuavg )
             !
             ! original MIN reconstruction
             !dpu(nm) = min(real(dps(nmu),fp), real(dps(nm),fp))
          endif
       enddo
    endif  
    !
    ! Compute new total water depths in velocity points
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
             & s1        ,dpu       ,umean     ,hu        ,gdp       )
    do nm = 1, nmmax
       nmu = nm + icx
       nmd = nm - icx
       !
       ! Set new layer administration in the computational points
       !
       if (kcu(nm)/=0) then
          !
          ! s1u is used for setting kfumax
          !
          if (umean(nm) > 0.0_fp) then
             s1u = s1(nm)
          elseif (umean(nm) < 0.0_fp) then
             s1u = s1(nmu)
          else
             s1u = max(s1(nm), s1(nmu))
          endif
          !
          ! for couple points
          !
          if ((kcu(nmd)>=1) .and. (kcu(nm)==3)) then
             s1u = s1(nm)
          elseif ((kcu(nmu)>=1) .and. (kcu(nm)==3)) then
             s1u = s1(nmu)
          else
          endif
          !
          ! Determine KFUMAX, using S1U, starting from the top layer KMAX
          !
          do k = kmax, kfumin(nm), -1
             kfuz1(nm, k) = 0
             !
             ! 15-3-2007 change to allow S1 > ZK(KMAX), needed for NH-models
             !
             if ( zk(k - 1)+dzmin <= s1u .or. (s1u>zk(kmax) .and. k==kmax) .or. (s1u<=zk(1) .and. k==1) ) then
                kfumax(nm) = k
                exit
             endif
          enddo
          !
          ! Find the (new) location of the bed layer
          !
          do k = 1, kmax
             if ( zk(k)-dzmin >= -dpu(nm) .or. k==kmax) then
                kfumin(nm) = k
                exit
             endif
          enddo
          !
          ! Set kfuz1 but overwrite kfuz1 at points with gates
          !
          kkmin = min(kfsmin(nm), kfsmin(nmu))
          kkmax = max(kfsmax(nm), kfsmax(nmu))
          do k = kkmin, kfumin(nm)-1
             kfuz1(nm,k) = 0
          enddo
          do k = kfumin(nm), kkmax
             if (kspu(nm, 0)*kspu(nm, k)==4 .or. kspu(nm, 0)*kspu(nm, k)==10) then
                kfuz1(nm, k) = 0
             else
                if (umean(nm) > 0.0_fp) then
                   kfuz1(nm, k) = kfsz1(nm,k)*kfu(nm)
                elseif (umean(nm) < 0.0_fp) then
                   kfuz1(nm, k) = kfsz1(nmu,k)*kfu(nm)
                elseif (k <= kfumax(nm)) then
                   kfuz1(nm, k) = kfu(nm)
                else
                   kfuz1(nm, k) = 0
                endif
             endif
          enddo
          !
          ! Compute DZU1, only for active wet points
          !
          dzutot = 0.0_fp
          do k = kfumin(nm), kfumax(nm)
             if (kfumin(nm) == kfumax(nm)) then
                dzu1(nm, k) = hu(nm)
             elseif (k == kfumin(nm)) then
                dzu1(nm, k) = zk(k) + dpu(nm)
             elseif (k == kfumax(nm)) then
                if (nonhyd .and. nh_level==nh_full) then
                   !dzu1(nm, k) = min(zk(k)-zk(k-1) , hu(nm)-dzutot)
                   dzu1(nm, k) = hu(nm) - dzutot
                else
                   dzu1(nm, k) = hu(nm) - dzutot
                endif
             else
                dzu1(nm, k) = zk(k) - zk(k - 1)
             endif
             dzutot = dzutot + dzu1(nm, k)
          enddo
          !
          ! Reset dzu1 to zero otherwise
          !
          do k = kfumax(nm) + 1, kmax
             dzu1(nm, k) = 0.0_fp
          enddo
          !
          ! A "trick" to ensure that "wet" cells that were dry
          ! obtain a velocity
          !
          ! If it is a dry point, the velocity is not copied.
          !
          if (kfu(nm) /= 0) then 
             do k = kfumx0(nm)+1, kfumax(nm)
                u1(nm, k) = u1(nm, kfumx0(nm))
             enddo
             if (kfsmax(nm) >= kfsmax(nmu)) then
                do k = kfumax(nm)+1, kfsmax(nm)
                   u1(nm, k) = u1(nm, kfumx0(nm))
                enddo
             else
                do k = kfumax(nm)+1, kfsmax(nmu)
                   u1(nm, k) = u1(nm, kfumx0(nm))
                enddo
             endif
             !
             ! Morphology
             !
             do k = kfumin(nm), kfumn0(nm)-1
                u1(nm, k) = u1(nm, kfumn0(nm))
             enddo
          endif
       endif
    enddo
    !
    ! Determine umean based on the complete water depth
    !
    umean = 0.0_fp
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          hnm = 0.0_fp
          do k = kfumin(nm), kfumax(nm)
             umean(nm) = umean(nm) + u1(nm,k)*dzu1(nm,k)
             hnm       = hnm + dzu1(nm,k)
          enddo
          umean(nm) = umean(nm) / max(hnm, 0.01_fp)
       else
       endif
    enddo
    !call dfexchg ( kfumax, 1, 1   , dfint, nm_pos, gdp )
    !call dfexchg ( kfuz1 , 1, kmax, dfint, nm_pos, gdp )
    !call dfexchg ( dzu1  , 1, kmax, dfloat, nm_pos, gdp )
    !
end subroutine z_drychku
