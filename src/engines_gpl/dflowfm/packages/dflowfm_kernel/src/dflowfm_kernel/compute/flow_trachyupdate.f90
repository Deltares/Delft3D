!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

subroutine flow_trachyupdate()
    use unstruc_messages
    use unstruc_files, only: mdia
    use m_flow, only: kmx, u1, ucx_mor, ucy_mor, cftrt, hu, hs, frcu, ifrcutp, cftrtfac, jacftrtfac, lnkx
    use m_flowgeom, only: ndx, lnx, lnx1d, lne2ln, ln2lne, nd, bob, bl
    use m_physcoef
    use m_trachy
    use m_trtrou
    use m_flowparameters, only: eps8, epshs, jacali, jawave, flowwithoutwaves
    use network_data, only: numl, lne
    use m_monitoring_crosssections
    use m_observations, only: valobs, IPNT_S1
    use m_calibration, only: calibration_backup_frcu
    use m_sediment
    use m_bedform,  only: bfmpar
    use m_alloc
    use m_vegetation, only: alfav
    use m_waves, only: ustokes
    !
    implicit none
    !
    integer, pointer :: ntrtcrs
    integer, pointer :: ntrtobs
    !
    integer n, L, LF, k, KL, KR, kb, kt
    integer :: icrs
    integer :: iobs
    integer :: itrtcrs
    integer :: itrtobs
    integer :: ierr
    !
    logical             :: error
    logical, save       :: init_trt = .true.              !< first time function is called
    !
    integer                                , pointer   :: nxx
    integer                                , pointer   :: lsedtot
    integer                                , pointer   :: i50
    integer                                , pointer   :: i90
    real(fp)             , dimension(:)    , pointer   :: rhosol
    real(fp)             , dimension(:,:)  , pointer   :: dxx

    logical                                , pointer   :: spatial_bedform
    real(fp)             , dimension(:)    , pointer   :: bedformD50
    real(fp)             , dimension(:)    , pointer   :: bedformD90
    real(fp)             , dimension(:)    , pointer   :: rksr
    real(fp)             , dimension(:)    , pointer   :: rksmr
    real(fp)             , dimension(:)    , pointer   :: rksd
    !
    real(fp)         , dimension(:)    , allocatable   :: u1eul
    !
    error = .false.
    lfdxx = .false.
    !
    if (stm_included) then
       nxx             => stmpar%morpar%nxx
       dxx             => sedtra%dxx
       i50             => stmpar%morpar%i50
       i90             => stmpar%morpar%i90
       rhosol          => stmpar%sedpar%rhosol
       lsedtot         => stmpar%lsedtot
       lfdxx = .true.
    end if
    ! these always exist
    spatial_bedform => bfmpar%spatial_bedform
    bedformD50      => bfmpar%bedformD50
    bedformD90      => bfmpar%bedformD90
    rksr            => bfmpar%rksr
    rksmr           => bfmpar%rksmr
    rksd            => bfmpar%rksd
    !
    ! prepare cross-section information to pass to trachytopes module
    !
    ntrtcrs => trachy_fl%gen%ntrtcrs
    do itrtcrs=1,ntrtcrs
         icrs = trachy_fl%gen%crs(itrtcrs)%id
         if (.not. (icrs == TRACHY_UNDEFINED)) then
            trachy_fl%gen%crs(itrtcrs)%val = crs(icrs)%sumvalcur(IPNT_Q1C)
         endif
    end do
    !
    ! prepare observation-station information to pass to trachytopes module
    !
    ntrtobs => trachy_fl%gen%ntrtobs
    do itrtobs=1,ntrtobs
        iobs = trachy_fl%gen%obs(itrtobs)%id
        if (.not. (iobs == TRACHY_UNDEFINED)) then
            trachy_fl%gen%obs(itrtobs)%val = valobs(IPNT_S1, iobs)
        endif
    end do
    !
    !
    if (update_umag) then
        !
        ! JRE: fixes for 3D models
        ! - Trachy needs bottom layer umod in 3D cases
        ! - D3D4 uses GLM velocities for trachytopes. Not sure if that is conceptually correct, to discuss.
        !   For now, I added the code to use eulerian vector for consistency
        !
        if (jawave>0 .and. .not. flowWithoutWaves) then
           if (.not. allocated(u1eul)) then
              allocate(u1eul(1:lnkx), stat=ierr)
           endif
           u1eul = u1 - ustokes
           call setucxucy_mor(u1eul)
        else
           call setucxucy_mor(u1)
        endif
        !
        if (kmx==0) then
           umag = hypot(ucx_mor,ucy_mor)
        else
           do k=1,ndx
              call getkbotktop(k,kb,kt)
              umag(k) = hypot(ucx_mor(kb),ucy_mor(kb))
           enddo
        endif   
    end if
    !
    ! Update water levels and link info (open or closed) on net-links
    !
    do L = 1, numl
        kL = lne(1,L) ; kR = lne(2,L)
        if (kL == 0 .and. kR == 0) cycle
        LF = lne2ln(L)
        if (LF > 0) then
            ! flow link crosses with net link
            if (hu(LF) > 0) then
                kcu_trt(L) = 1     ! warning: kcu arrays in Delft3d and Dflow-FM have different meanings
            else
                kcu_trt(L) = 0
            end if
            hu_trt(L)  = hu(LF)
            trachy_fl%dir(1)%blu_trt(L)  = min( bob(1,LF), bob(2,LF) )
        else
            ! net link lies on boundary, take neighbouring flow node value.
            hu_trt(L)  = hs(trachy_fl%dir(1)%lin(1,L))
            trachy_fl%dir(1)%blu_trt(L) = bl(trachy_fl%dir(1)%lin(1,L))
            kcu_trt(L) = 0        !link is not on flow-link --> closed boundary  ! TO DO: this line causes that the roughness is not computed in trtrou (important for morphology in combination with trachtopes along closed boundaries)
        end if
    enddo
    !
    ! Update background friction and water level
    !
    if (init_trt) then
        !
        do L = 1, numl
            kL = lne(1,L) ; kR = lne(2,L)
            if (kL == 0 .and. kR == 0 ) cycle
            trachy_fl%dir(1)%zsu_prev(L) = trachy_fl%dir(1)%blu_trt(L) + hu_trt(L)
        end do
        !
        ! Skip 1d friction types. For now the ifrcutp is always equal to 1, while FRCU contains the actual (calculated) Chezy value
        do LF = lnx1D + 1, lnx
            if (ifrcutp(LF) /= ifrctypuni) then
                error = .true.
                write (msgbuf, '(a,i0,a,i0,a,i0,a)') 'Local roughness type (', ifrcutp(LF), ') must match the global UnifFrictType (', ifrctypuni, ') when used in combination with Trachytopes. (Flow link=', LF, ').'
                call err_flush()
            end if
        end do
        do L = 1, numl
            kL = lne(1,L) ; kR = lne(2,L)
            if (kL == 0 .and. kR == 0) cycle
            LF = lne2ln(L)
            if (LF > 0) then
                cftrt(L,3) = frcu(LF)  !link is on flow-link
            else
                k = trachy_fl%dir(1)%lin(1,L)  ! neighbouring flow node
                cftrt(L,3) = 0.0
                do n = 1,nd(k)%lnx
                    LF = iabs(nd(k)%ln(n))  ! neighbouring flow links to flow node
                    cftrt(L,3) = cftrt(L,3) + frcu(LF)
                end do
                cftrt(L,3) = cftrt(L,3)/max(nd(k)%lnx,1)
            end if
        enddo
    init_trt = .false.
    end if
    !
    ! Perform computation of vegetation and alluvial roughness
    !
    if (stm_included) then
       call trtrou(mdia     ,kmaxtrt   ,numl      ,            &                          
                & cftrt     ,rouflo    ,linit     ,dx_trt    , &
                & hu_trt    ,kcu_trt   ,sig       ,            &
                & z0rou     ,1         ,waqol     ,trachy_fl , &
                & umag      ,1         ,numl      ,1         , ndx      , &                      ! first entry in row r(u1) should be gdp%gderosed%umod !!WO-temp
                & rhomean   ,ag        ,vonkar    , viskin   , &              ! ~z0 used for what?   ~viskin instead of vicmol (Delft3D)
                & eps8      ,epshs     ,spatial_bedform      ,bedformD50,bedformD90, &
                & rksr      ,rksmr     ,rksd      ,error,  &
                & lfdxx     ,nxx       ,lsedtot   ,dxx       , i50      , i90 ,    &
                & rhosol)
    else
        call trtrou(mdia     ,kmaxtrt   ,numl      , &
                & cftrt     ,rouflo    ,linit     ,dx_trt    , &
                & hu_trt    ,kcu_trt   ,sig       , &
                & z0rou     ,1         ,waqol     ,trachy_fl , &
                & umag      ,1         ,numl      ,1         , ndx      , &
                & rhomean   ,ag        ,vonkar    , viskin   , &
                & eps8      ,epshs     ,spatial_bedform      ,bedformD50,bedformD90, &
                & rksr      ,rksmr     ,rksd      ,error,  &
                & lfdxx     ,2         ,1   )      ! nxx, lsedtot
    end if

    if (error) then
        call mess(LEVEL_ERROR, 'Error computing trachytopes', mdia)
    end if
    !
    ! Return vegetation and aluvial roughness to flow-links
    !
    do LF = 1, lnx
        L        = ln2lne(LF)
        frcu(LF) = cftrt(L,2)   !--> cfrou (L,1) to do Delft3D check ...
        if (jacftrtfac == 1) then
           frcu(LF) = frcu(LF) * cftrtfac(LF)  ! This has probably become obsolete
        end if
    end do
    !
    if (trachy_resistance) then
        do LF = 1, lnx
            L = ln2lne(LF)
            alfav(LF) = trachy_fl%dir(1)%rttfu(L,1)/2.0_fp
        enddo
    endif
    !
    if (jacali == 1) then
        call calibration_backup_frcu()
    endif

end subroutine flow_trachyupdate
