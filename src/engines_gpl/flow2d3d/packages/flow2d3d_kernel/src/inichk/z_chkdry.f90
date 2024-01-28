subroutine z_chkdry(j         ,nmmaxj    ,nmmax     ,kmax      ,lstsci    , &
                  & ltur      ,icx       ,icy       ,kcu       , &
                  & kcv       ,kcs       ,kfu       ,kfv       ,kfs       , &
                  & kspu      ,kspv      ,kfuz1     ,kfvz1     ,kfsz1     , &
                  & kfumin    ,kfumax    ,kfvmin    ,kfvmax    ,kfsmin    , &
                  & kfsmax    ,dpu       ,dpv       ,hu        ,hv        , &
                  & hkru      ,hkrv      ,s1        ,dps       ,u1        , &
                  & v1        ,umean     ,vmean     ,r1        ,rtur1     , &
                  & guu       ,gvv       ,qxk       ,qyk       ,dzu1      , &
                  & dzv1      ,dzs1      ,zk        ,gdp       )
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
!    Function: ONLY for ZMODEL:
!              - Initiates the depth values at velocity points
!                and check drying on velocity points
!              - Calculate umean and vmean (depth averaged
!                velocities)
!              - Redefine s1, u1, v1 and r1 according to new
!                values of mask arrays
!              - Calculate qxk and qyk
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer        , pointer :: lundia
    real(fp)       , pointer :: drycrt
    real(fp)       , pointer :: dryflc
    logical        , pointer :: zmodel
    logical        , pointer :: kfuv_from_restart
    real(fp)       , pointer :: dzmin
    
!
! Global variables
!
    integer                                                                 :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir.
                                                                                      !!  If icx=1 then computation proceeds in the Y-dir.
    integer                                                                 :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                                 :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                                      !!  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                                 :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                 :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: kspv   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dpv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hkru   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hkrv   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)              :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(out) :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(0:kmax)                             , intent(in)  :: zk
!             
! Local variables
!
    integer  :: k       ! Help var. 
    integer  :: kkmin   ! Help var. 
    integer  :: kkmax   ! Help var. 
    integer  :: kd      
    integer  :: ku      
    integer  :: l       ! Help var. 
    integer  :: mask    ! Help var. 
    integer  :: ndm     ! Help var. NM-ICY 
    integer  :: nm      ! Help var. loops 1,nmmax and j,nmmaxj 
    integer  :: nmd
    integer  :: nmu
    integer  :: num
    integer  :: nm_pos  ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp) :: dzfound ! Thinnest layer found in the initial grid distribution
    real(fp) :: hnm
    real(fp) :: drytrsh
    real(fp) :: hucres
    real(fp) :: hvcres
    real(fp) :: s1u
    real(fp) :: s1v
    logical  :: found
!
!! executable statements -------------------------------------------------------
!
    lundia             => gdp%gdinout%lundia
    zmodel             => gdp%gdprocs%zmodel
    drycrt             => gdp%gdnumeco%drycrt
    dryflc             => gdp%gdnumeco%dryflc
    kfuv_from_restart  => gdp%gdrestart%kfuv_from_restart
    dzmin              => gdp%gdzmodel%dzmin
    !
    drytrsh = drycrt
    nm_pos  = 1
    !
    !NOTE: that the contents of KFS are here identical to KCS
    !      (except for boundary points)
    !
    ! Initialize global arrays
    ! (HU, HV, QXK and QYK are initialized in esm_alloc_real)
    ! (KFS/U/V are already initialised in Z_INIZM)
    !
    umean = 0.0_fp
    vmean = 0.0_fp
    !
    do nm = 1, nmmax
       !
       ! Check based on KFU/V but also on HU because HU can be zero
       ! despite KFU/V = 1. Altering of KFU/V value (to 0, when
       ! velocity point is dry)is done after UPWHU
       !
       ! First determine umean for the top layer(s) of cells NM and NMU
       ! to find the upwind direction
       !
       !nmu   = nm + icx
       !
       ! Determine layers participating in velocity point
       !
       !kkmin = min(kfsmax(nm), kfsmax(nmu))
       !kkmax = max(kfsmax(nm), kfsmax(nmu))
       !
       ! kkmin below the bottom in NM or NMU?
       !
       !kkmin = max( kkmin, max(kfsmin(nm), kfsmin(nmu)) )
       !
       if (kfu(nm) == 1 .and. hu(nm)>=drytrsh) then
          hnm = 0.0_fp
          !do k = kkmin, kkmax
          do k = kfumin(nm), kfumax(nm)
             umean(nm) = umean(nm) + u1(nm,k)*dzu1(nm,k)
             hnm       = hnm + dzu1(nm,k)
          enddo
          umean(nm) = umean(nm) / max(hnm, 0.01_fp)
       else
       endif
       !
       !num   = nm + icy
       !
       ! Determine layers participating in velocity point
       !
       !kkmin = min(kfsmax(nm), kfsmax(num))
       !kkmax = max(kfsmax(nm), kfsmax(num))
       !
       ! kkmin below the bottom in NM or NUM?
       !
       !kkmin = max( kkmin, max(kfsmin(nm), kfsmin(num)) )
       !
       if (kfv(nm) == 1 .and. hv(nm)>=drytrsh) then
          hnm = 0.0_fp
          !do k = kkmin, kkmax
          do k = kfvmin(nm), kfvmax(nm)
             vmean(nm) = vmean(nm) + v1(nm,k)*dzv1(nm,k)
             hnm       = hnm + dzv1(nm,k)
          enddo
          vmean(nm) = vmean(nm) / max(hnm, 0.01_fp)
       else
       endif
    enddo
    !
    ! Redefine S1 in case they are smaller then DPS and reset the mask
    ! arrays KFU,KFV and KFS
    ! -icx := -1 in m-direction, -icy := -1 in n-direction
    ! In Z_INIZM all relevant depths and waterlevels are already checked.
    ! This may be redundant
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - icy
       if (kcs(nm) /= 0) then
          if (s1(nm)<= - real(dps(nm),fp)) then
             s1(nm)   = -real(dps(nm),fp)
             kfu(nm)  = 0
             kfu(nmd) = 0
             kfv(nm)  = 0
             kfv(ndm) = 0
             kfs(nm)  = 0
             do k = 1, kmax
                kfuz1(nm, k)  = 0
                kfuz1(nmd, k) = 0
                kfvz1(nm, k)  = 0
                kfvz1(ndm, k) = 0
                kfsz1(nm, k)  = 0
             enddo
          endif
       endif
    enddo
    !
    ! Delft3D-16494: NOT NECESSARY? Could the loop above also be done with kcs/=0 instead of kcs>0
    ! otherwise also need to communicate kfu/kfv and s1?
    !
    ! exchange mask array kfs with neighbours for parallel runs
    !
    !call dfexchg ( kfs, 1, 1, dfint, nm_pos, gdp )
    !call dfexchg ( kfu, 1, 1, dfint, nm_pos, gdp )
    !call dfexchg ( kfv, 1, 1, dfint, nm_pos, gdp )
    !
    ! Check whether the minimum layer thickness (0.1*Dryflc) is smaller 
    ! than one of the internal layers for the Z-model, 
    ! so excluding the bottom and free-surface layers, which can be thin
    !
    dzfound     = drytrsh
    found       = .false.
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          do k = kfsmin(nm)+1, kfsmax(nm)-1
             if (dzmin > dzs1(nm,k)) then
                found  = .true.
                dzfound = min(dzfound, dzs1(nm,k))
             endif
         enddo
       endif
    enddo
    if (found) then
       call prterr(lundia, 'U190',  'Minimum layer thickness (0.1*Dryflc) is too large for the vertical')
       write (lundia, '(a,f10.4,a)')'            grid layering. Decrease Dryflc to smaller than ', 10.0_fp*dzfound, ' m,'
       write (lundia, '(a)')        '            to avoid problems with the vertical layering.'
    endif
    !
    ! Calculate HU and HV
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kfu       ,kspu      ,dps       , &
             & s1        ,dpu       ,umean     ,hu        ,gdp       )
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icy       , &
             & zmodel    ,kcs       ,kfv       ,kspv      ,dps       , &
             & s1        ,dpv       ,vmean     ,hv        ,gdp       )
   !
    ! Check for dry velocity points
    ! Approach for 2D weirs (following WAQUA)
    ! HUCRES is initially set to extreme large value to guarantee
    ! the MIN operator works as planned
    !
    do nm = 1, nmmax
       nmu = nm + icx
       num = nm + icy
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
       ! s1v is used for setting kfvmax
       !
       if (vmean(nm) > 0.0_fp) then
          s1v = s1(nm)
       elseif (vmean(nm) < 0.0_fp) then
          s1v = s1(num)
       else
          s1v = max(s1(nm), s1(num))
       endif
       !
       ! set hucres / hvcres
       !
       hucres = 1.0e+9_fp
       if (abs(kspu(nm, 0)) == 9) then
          hucres = s1u + hkru(nm)
       endif
       !
       hvcres = 1.0e+9_fp
       if (abs(kspv(nm, 0)) == 9) then
          hvcres = s1v + hkrv(nm)
       endif
       if (.not.kfuv_from_restart) then
          if (kfu(nm)*min(hu(nm), hucres)<dryflc .and. kcu(nm)*kfu(nm)==1) then
             kfu(nm) = 0
          endif
          if (kfv(nm)*min(hv(nm), hvcres)<dryflc .and. kcv(nm)*kfv(nm)==1) then
             kfv(nm) = 0
          endif
       endif
       !
       ! Determine KFUMAX, using S1U, starting from the top layer KMAX
       !
       do k = kmax, kfumin(nm), -1
          kfuz1(nm, k) = 0
          !
          ! 15-3-2007 change to allow S1 > ZK(KMAX), needed for NH-models
          !
          if ( kcu(nm) > 0 .and. (zk(k - 1)+dzmin <= s1u .or. (s1u>zk(kmax) .and. k==kmax)) ) then
             kfumax(nm) = k
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
                kfuz1(nm, k) = kfsz1(nm,k)
             elseif (umean(nm) < 0.0_fp) then
                kfuz1(nm, k) = kfsz1(nmu,k)
             elseif (k <= kfumax(nm)) then
                kfuz1(nm, k) = 1
             else
                kfuz1(nm, k) = 0
             endif
          endif
       enddo
       !
       ! Determine KFVMAX, using S1V, starting from the top layer KMAX
       !
       do k = kmax, kfvmin(nm), -1
          kfvz1(nm, k) = 0
          !
          ! 15-3-2007 change to allow S1 > ZK(KMAX), needed for NH-models
          !
          if ( kcv(nm) > 0 .and. (zk(k - 1)+dzmin <= s1v .or. (s1v>zk(kmax) .and. k==kmax)) ) then
             kfvmax(nm) = k
             exit
          endif
       enddo
       !
       ! Set kfuz1 but overwrite kfuz1 at points with gates
       !
       kkmin = min(kfsmin(nm), kfsmin(num))
       kkmax = max(kfsmax(nm), kfsmax(num))
       do k = kkmin, kfvmin(nm)-1
          kfvz1(nm,k) = 0
       enddo
       do k = kfvmin(nm), kkmax
          if (kspv(nm, 0)*kspv(nm, k)==4 .or. kspv(nm, 0)*kspv(nm, k)==10) then
             kfvz1(nm, k) = 0
          else
             if (vmean(nm) > 0.0_fp) then
                kfvz1(nm, k) = kfsz1(nm,k)
             elseif (vmean(nm) < 0.0_fp) then
                kfvz1(nm, k) = kfsz1(num,k)
             elseif (k <= kfvmax(nm)) then
                kfvz1(nm, k) = 1
             else
                kfvz1(nm, k) = 0
             endif
          endif
       enddo
    enddo
    !
    ! Delft3D-16494: NOT NECESSARY?
    !
    ! exchange mask arrays with neighbours for parallel runs
    !
    call dfexchg ( kfu   , 1, 1   , dfint, nm_pos, gdp )
    call dfexchg ( kfv   , 1, 1   , dfint, nm_pos, gdp )
    !call dfexchg ( kfumax, 1, 1   , dfint, nm_pos, gdp )
    !call dfexchg ( kfvmax, 1, 1   , dfint, nm_pos, gdp )
    !call dfexchg ( kfuz1 , 1, kmax, dfint, nm_pos, gdp )
    !call dfexchg ( kfvz1 , 1, kmax, dfint, nm_pos, gdp )
    !
    ! set KFS to 0 if the surrounding velocity points are dry
    !
    do nm = 1, nmmax
       if (kcs(nm) > 0) then
          nmd = nm - icx
          ndm = nm - icy
          kfs(nm) = max(kfu(nm), kfu(nmd), kfv(nm), kfv(ndm))
          if (kfs(nm) == 1) then
             do k = kfsmin(nm), kmax
                if (k <= kfsmax(nm)) then
                   kfsz1(nm, k) = 1
                else
                   kfsz1(nm, k) = 0
                endif
             enddo
          else
             !kfsmax(nm) = -1
             do k = kfsmin(nm), kmax
                kfsz1(nm, k) = 0
             enddo
          endif
       endif
    enddo
    !
    ! exchange mask array kfs with neighbours for parallel runs
    !
    call dfexchg ( kfs  , 1, 1   , dfint, nm_pos, gdp )
    call dfexchg ( kfsz1, 1, kmax, dfint, nm_pos, gdp )
    !
    ! Mask initial arrays
    !
    do nm = 1, nmmax
       mask = min(1, abs(kcs(nm)))
       s1(nm) = s1(nm)*mask
       if (kfs(nm) == 1) then
          !
          ! Currently wet computational point
          ! r1 must be set for the active layers
          !
          do k = 1, kmax
             do l = 1, lstsci
                r1(nm, k, l) = r1(nm, k, l)*kfsz1(nm, k)
             enddo
          enddo
       elseif (kcs(nm) == 0) then
          !
          ! Not a computational point
          ! r1 must be set to zero
          !
          do k = 1, kmax
             do l = 1, lstsci
                r1(nm, k, l) = 0.0_fp
             enddo
          enddo
       endif
       do k = 0, kmax
          ku = min(k+1, kmax)
          kd = max(k  , 1   )
          do l = 1, ltur
             mask            = min(1, kfsz1(nm,kd) + kfsz1(nm,ku))
             rtur1(nm, k, l) = rtur1(nm, k, l) * mask
          enddo
       enddo
    enddo
    !
    ! Calculate flows in x- and y- direction
    !
    do nm = 1, nmmax
       do k = 1, kmax
          u1 (nm, k) = u1(nm, k)* kfuz1(nm, k)
          qxk(nm, k) = guu(nm)  * dzu1(nm, k) * u1(nm, k)
          v1 (nm, k) = v1(nm, k)* kfvz1(nm, k)
          qyk(nm, k) = gvv(nm)  * dzv1(nm, k) * v1(nm, k)
       enddo
    enddo
end subroutine z_chkdry
