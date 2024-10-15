subroutine restart_trim_flow(lundia    ,error     ,restid1   ,lturi     ,mmax      , &
                           & nmaxus    ,kmax      ,lstsci    ,ltur      , &
                           & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                           & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                           & dpd       ,ex_nfs    ,namcon    ,coninit   ,rdum      , &
                           & idum      ,gdp       )
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
! Reads initial field condition records from a trim-file
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties, only: prop_get
    use time_module, only: ymd2jul, datetime_to_string, parse_ud_timeunit, date2mjd, jul2mjd
    use globaldata
    use string_module, only: remove_leading_spaces
    use netcdf, only: nf90_open, nf90_inq_dimid, nf90_inquire_dimension, nf90_get_var, nf90_inquire_variable, NF90_NOWRITE, nf90_inq_varid, nf90_get_att, NF90_GLOBAL, NF90_MAX_VAR_DIMS, nf90_sync, nf90_close
    use system_utils, only: exifil
    use rdarray, only: rdvar, rdarray_nm, rdarray_nmk, rdarray_nmkl
    use dfparall, only: inode, master, dfint, dfdble, dfmax
    use nan_check_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    include 'fsm.i'
    include 'tri-dyn.igd'
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer        , pointer :: julday
    real(fp)       , pointer :: tstart
    real(fp)       , pointer :: dt
    logical        , pointer :: temp
    logical        , pointer :: const
    logical        , pointer :: htur2d
    integer        , pointer :: i_restart
    logical        , pointer :: dp_from_map_file
    logical        , pointer :: kfuv_from_restart
    character(16)  , pointer :: rst_layer_model
    logical        , pointer :: rst_dp
    logical        , pointer :: roller
    integer        , pointer :: filetype
    character(256) , pointer :: filename
    character(256) , pointer :: restid
    real(hp)       , pointer :: morft
    real(hp)       , pointer :: morft0
    real(fp)       , pointer :: bed
    !
    integer       , dimension(:,:)       , pointer :: iarrc
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
!
! Global variables
!
    integer                                                                    , intent(in)  :: idum    !< value used for initialization of integer arrays
    integer                                                                    , intent(in)  :: kmax
    integer                                                                    , intent(in)  :: lstsci
    integer                                                                    , intent(in)  :: ltur
    integer                                                                    , intent(out) :: lturi
    integer                                                                                  :: lundia
    integer                                                                    , intent(in)  :: mmax
    integer                                                                    , intent(in)  :: nmaxus
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(out) :: kfu
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(out) :: kfv
    integer, dimension(lstsci)                                                               :: coninit ! Flag=1 if constituent is initialized, all 0 upon entry
    logical                                                                                  :: error
    logical                                                                                  :: ex_nfs  ! Flag indicating whether Nefis restart files exist
    real(fp)                                                                   , intent(in)  :: rdum    !< value used for initialization of real arrays
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: dpd
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: s1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: umnldf
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: vmnldf
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(out) :: rtur1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: u1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: v1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(out) :: r1
    character(*)                                                                             :: restid1
    character(20), dimension(lstsci + ltur)                                    , intent(in)  :: namcon !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    logical                               :: has_morft
    logical                               :: has_umean
    integer                               :: fds
    integer                               :: lrid        ! character variables for files Help var., length of restid
    integer, external                     :: crenef
    integer, external                     :: getelt
    integer, external                     :: inqelm
    integer, external                     :: clsnef
    integer                               :: iddim
    integer                               :: idvar
    integer                               :: ierror
    integer, external                     :: inqmxi
    integer, external                     :: neferr
    integer                               :: ii
    integer, dimension(2)                 :: itdate
    integer                               :: itmapc
    integer                               :: iunit
    integer                               :: iyear
    integer                               :: imonth
    integer                               :: iday
    integer                               :: ihour
    integer                               :: imin
    integer                               :: isec
    integer                               :: julday_restart
    integer                               :: l
    integer                               :: ll
    integer                               :: max_index
    integer                               :: rst_lstsci
    integer                               :: rst_ltur
    integer, dimension(3,5)               :: cuindex
    logical                               :: found
    real(fp)                              :: dtm          ! time step in tunits  (flexible precision)
    real(fp)                              :: tunit        ! time unit in seconds (flexible precision)
    real(fp)                              :: t_restart
    real(fp)                              :: rjulday_restart
    real(fp)                              :: rjuldiffs    ! difference of Julian dates in seconds
    real(fp), dimension(:,:,:,:), pointer :: rst_r1
    real(fp), dimension(:,:,:,:), pointer :: rst_rtur1
    character(16)                         :: grnam1
    character(16)                         :: grnam2
    character(16)                         :: grnam3
    character(16)                         :: grnam4
    character(20), dimension(:), allocatable :: rst_namcon
    character(1024)                       :: error_string
    character(256)                        :: dat_file
    character(256)                        :: def_file
    character(256)                        :: nc_file
    character(256)                        :: timeunitstr
    integer(pntrsize)           , pointer :: eroll1
    integer(pntrsize)           , pointer :: ewave1
    integer(pntrsize)           , pointer :: fxw
    integer(pntrsize)           , pointer :: fyw
    integer(pntrsize)           , pointer :: hrms    
    integer(pntrsize)           , pointer :: qxkr
    integer(pntrsize)           , pointer :: qxkw
    integer(pntrsize)           , pointer :: qykr
    integer(pntrsize)           , pointer :: qykw       
    integer(pntrsize)           , pointer :: wsu
    integer(pntrsize)           , pointer :: wsv  
    integer(pntrsize)           , pointer :: guu
    integer(pntrsize)           , pointer :: gvv
    integer                               :: m    
    integer                     , pointer :: mfg
    integer                     , pointer :: mlg
    integer                               :: n 
    integer                     , pointer :: nfg
    integer                     , pointer :: nlg
    integer                     , pointer :: nmaxgl
    integer                     , pointer :: mmaxgl   
    character(8)                          :: elmtyp
    integer                               :: nbytsg
    character(16)                         :: elmqty
    character(16)                         :: elmunt
    character(64)                         :: elmdes
    integer                               :: elmndm
    integer, dimension(5)                 :: elmdms
    integer, dimension(NF90_MAX_VAR_DIMS) :: dimids
!
! Data statements
!
    data grnam1/'map-const'/
    data grnam2/'map-info-series'/
    data grnam3/'map-series'/
    data grnam4/'map-sed-series'/
!
!! executable statements -------------------------------------------------------
!
    julday              => gdp%gdinttim%julday
    tstart              => gdp%gdexttim%tstart
    dt                  => gdp%gdexttim%dt
    temp                => gdp%gdprocs%temp
    const               => gdp%gdprocs%const
    htur2d              => gdp%gdprocs%htur2d
    roller              => gdp%gdprocs%roller 
    i_restart           => gdp%gdrestart%i_restart
    dp_from_map_file    => gdp%gdrestart%dp_from_map_file
    kfuv_from_restart   => gdp%gdrestart%kfuv_from_restart
    rst_layer_model     => gdp%gdrestart%rst_layer_model
    rst_dp              => gdp%gdrestart%rst_dp
    filetype            => gdp%gdrestart%filetype
    filename            => gdp%gdrestart%filename
    restid              => gdp%gdrestart%restid
    morft               => gdp%gdmorpar%morft
    morft0              => gdp%gdmorpar%morft0
    bed                 => gdp%gdmorpar%bed
    eroll1              => gdp%gdr_i_ch%eroll1
    ewave1              => gdp%gdr_i_ch%ewave1
    fxw                 => gdp%gdr_i_ch%fxw
    fyw                 => gdp%gdr_i_ch%fyw
    hrms                => gdp%gdr_i_ch%hrms
    qxkr                => gdp%gdr_i_ch%qxkr
    qxkw                => gdp%gdr_i_ch%qxkw
    qykr                => gdp%gdr_i_ch%qykr
    qykw                => gdp%gdr_i_ch%qykw
    wsu                 => gdp%gdr_i_ch%wsu
    wsv                 => gdp%gdr_i_ch%wsv
    guu                 => gdp%gdr_i_ch%guu
    gvv                 => gdp%gdr_i_ch%gvv
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    !
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    iarrc               => gdp%gdparall%iarrc
    
    !
    ! dp_from_map_file=false: do not read depth from map file
    !
    call prop_get(gdp%mdfile_ptr, '*', 'dp_from_map_file', dp_from_map_file)
    restid       = restid1
    error        = .false.
    error_string = ' '
    fds          = -1
    call remove_leading_spaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    nc_file  = restid(1:lrid)//'.nc'
    filetype = -999
    ierror   = -999
    if (inode == master) then
        if (exifil(dat_file) .and. exifil(def_file)) then
            ierror   = crenef(fds, dat_file, def_file, ' ', 'r')
            filename = restid(1:lrid)
            filetype = FTYPE_NEFIS
        elseif (exifil(nc_file)) then
            ierror   = nf90_open(nc_file, NF90_NOWRITE, fds)
            filename = nc_file
            filetype = FTYPE_NETCDF
        endif
    endif
    call dfbroadc_gdp ( filetype, 1, dfint, gdp )
    call dfbroadc_gdp ( ierror  , 1, dfint, gdp )
    if (ierror/= 0) then
       error = .true.
       goto 9999
    endif
    !
    ex_nfs = .true.
    if (filetype==FTYPE_NEFIS) then
       write(lundia, '(a)') 'Restarting from ' // trim(dat_file) // ' and ' // trim(def_file)
    else
       write(lundia, '(a)') 'Restarting from ' // trim(nc_file)
    endif
    !
    ! Now also reading KFU and KFV from restart file
    !
    kfuv_from_restart = .true.
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    ! the master opens and reads the grid file 
    ! 
    if ( inode == master ) then
        !
        if (filetype==FTYPE_NEFIS) then
            ierror = inqmxi(fds, grnam3, max_index)
            if (ierror/= 0) then
               ierror = neferr(0,error_string)
               call prterr(lundia    ,'P004'    , error_string)
               error = .true.
               goto 999 ! end of master block
            endif
        else
            ierror = nf90_inq_dimid(fds, 'time', iddim)
            ierror = nf90_inquire_dimension(fds, iddim, len=max_index)
        endif
        !
        if (i_restart==0 .and. filetype==FTYPE_NEFIS) then
            !
            ! if the restart time index hasn't been specified by the user, find the time
            ! that corresponds to the start time of the simulation
            !
            call rdvar(fds, filename, filetype, grnam1, &
                     & 1, gdp, ierror, lundia, dtm, 'DT')
            if (ierror==0) call rdvar(fds, filename, filetype, grnam1, &
                                    & 1, gdp, ierror, lundia, tunit, 'TUNIT')
            if (ierror/=0) then
               ierror = neferr(0,error_string)
               call prterr(lundia    ,'P004'    , error_string)
               error = .true.
               goto 999 ! end of master block
            endif
            !
            ! determine if there is a difference in reference dates (simulation vs restart file)
            !
            call rdvar(fds, filename, filetype, grnam1, &
                     & 1, gdp, ierror, lundia, itdate, 'ITDATE')
            julday_restart = ymd2jul(itdate(1))
            rjuldiffs      = real(julday_restart - julday,fp)*86400.0_fp
            !
            ! look for restart time on nefis map file
            !
            found = .false.
            write(lundia,'(a,a)') 'Looking for time ',datetime_to_string(julday,real(tstart,hp)/1440.0_hp)
            do ii = max_index,1,-1 ! assume last time on map file has highest probability of being the requested time step
               call rdvar(fds, filename, filetype, grnam2, &
                        & ii, gdp, ierror, lundia, itmapc, 'ITMAPC')
               if (ierror/= 0) then
                  ierror = neferr(0,error_string)
                  call prterr(lundia    ,'P004'    , error_string)
                  error = .true.
                  goto 999 ! end of master block
               endif
               ! compute t_restart in minutes relative to simulation reference date
               t_restart = (dtm*itmapc*tunit + rjuldiffs) / 60.0_fp
               if (abs(tstart-t_restart) < 0.5_fp*dtm) then
                  write(lundia, '(a,i5,a,a)') 'Using time index ',ii,' associated with time ',datetime_to_string(julday,real(t_restart,hp)/1440.0_hp)
                  i_restart = ii
                  found     = .true.
                  exit ! restart time found on map file
               end if
            enddo
        elseif (i_restart==0 .and. filetype==FTYPE_NETCDF) then
            !
            ! if the restart time index hasn't been specified by the user, find the time
            ! that corresponds to the start time of the simulation
            !
            ! determine if there is a difference in reference dates (simulation vs restart file)
            !
            ! time -> units -> "seconds since 1990-08-05 00:00:00"
            ierror          = nf90_inq_varid(fds, 'time', idvar)
            ierror          = nf90_get_att(fds, idvar, 'units', timeunitstr)
            ierror          = parse_ud_timeunit(timeunitstr, iunit, iyear, imonth, iday, ihour, imin, isec)
            rjulday_restart = date2mjd(iyear,imonth,iday,ihour,imin,real(isec,hp))
            rjuldiffs       = (rjulday_restart - jul2mjd(julday))*86400.0_fp
            tunit           = real(iunit,fp)
            !
            ! look for restart time on netcdf map file
            !
            found = .false.
            write(lundia,'(a,a)') 'Looking for time ',datetime_to_string(julday,real(tstart,hp)/1440.0_hp)
            do ii = max_index,1,-1 ! assume last time on map file has highest probability of being the requested time step
               ierror = nf90_get_var(fds, idvar, itmapc, start=(/ ii /))
               if (ierror/= 0) then
                  ierror = neferr(0,error_string)
                  call prterr(lundia    ,'P004'    , error_string)
                  error = .true.
                  goto 999 ! end of master block
               endif
               ! compute t_restart in minutes relative to simulation reference date
               t_restart = (itmapc*tunit + rjuldiffs) / 60.0_fp
               if (abs(tstart-t_restart) < 1/60.0_fp ) then ! time step undefined for NetCDF file, so match within 1 second accuracy
                  write(lundia, '(a,i5,a,a)') 'Using time index ',ii,' associated with time ',datetime_to_string(julday,real(t_restart,hp)/1440.0_hp)
                  i_restart = ii
                  found     = .true.
                  exit ! restart time found on map file
               end if
            enddo
        elseif (i_restart<0 .or. i_restart>max_index) then
            write(lundia, '(a,i5,a,i5,a)') 'Invalid time index ',i_restart,' requested (should be in the range 1 to ',max_index,')'
            found = .false.
        else
            write(lundia, '(a,i5,a)') 'Using time index ',i_restart,' as requested.'
            found = .true.
        endif
        !
        rst_lstsci = 0
        rst_ltur   = 0
        if (.not. found) then
           call prterr(lundia, 'P004', 'Restart time not found on restart file.')
           ierror = 1
        else
           !
           ! Read the type of layer model from the map file. 
           ! Parameter LAYER_MODEL may not be present. Was initialized with value 'UNKNOWN' in RDIC.f90
           ! The value is needed to check for consistency when applying the z-model with ZTBML=#Y#, 
           ! see subroutine CHKSET.
           !
           has_morft = .true.
           has_umean = .true.
           !
           if (filetype==FTYPE_NEFIS) then
               ierror = getelt(fds, 'map-const', 'LAYER_MODEL', cuindex, 1, 16, rst_layer_model)
               ierror = getelt(fds, 'map-const', 'LSTCI', cuindex, 1, 4, rst_lstsci)       
               ierror = getelt(fds, 'map-const', 'LTUR', cuindex, 1, 4, rst_ltur)
               !
               if (dp_from_map_file) then
                  elmndm = 5
                  ierror = inqelm (fds, 'DPS', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
                  if (ierror/= 0) then
                     write(lundia, '(a)') 'No bed level data on restart file available:'
                     write(lundia, '(a)') 'using bed level data as prescribed in master definition file.'
                     dp_from_map_file = .false.
                     ierror = 0
                  else
                     write(lundia, '(a)') 'Bed level data read from restart file.'
                  endif
               endif
               !
               ! The flag rst_dp is used to set DPSOPT=DP.
               ! This ensures that the DPD values are copied into DPS in subroutine caldps
               ! Differences may occur when DPU/DPV depend on (the original) DPD
               ! The flag rst_dp is also used to check whether 
               !
               rst_dp = dp_from_map_file
               !
               if (ierror == 0) then
                  ierror = inqelm (fds, 'UMNLDF', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
                  if (ierror /= 0) then
                     has_umean = .false.
                     ierror = 0
                  endif
               endif
               !
               if (ierror == 0 .and. rst_lstsci > 0) then
                  allocate(rst_namcon(rst_lstsci+rst_ltur), stat = ierror)
                  if (ierror == 0) ierror = getelt(fds, 'map-const', 'NAMCON', cuindex, 1, 20*(rst_lstsci+rst_ltur), rst_namcon)
                  if (ierror /= 0) ierror = neferr(0,error_string)
               endif
               !
               ierror = inqelm (fds, 'MORFT', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
               if (ierror /= 0) then
                  has_morft = .false.
                  ierror = 0
               endif
           else
               ierror = nf90_get_att(fds, nf90_global, 'LAYER_MODEL', rst_layer_model)
               !
               if (ierror==0) then
                   ierror = nf90_inq_varid(fds, 'R1', idvar)
                   if (ierror==0) then
                       ierror = nf90_inquire_variable(fds, idvar, dimids=dimids)
                       if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(4), len=rst_lstsci)
                   else
                       ierror = 0
                       rst_lstsci = 0
                   endif
               endif
               !
               if (ierror==0) then
                   ierror = nf90_inq_varid(fds, 'RTUR1', idvar)
                   if (ierror==0) then
                       ierror = nf90_inquire_variable(fds, idvar, dimids=dimids)
                       if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(4), len=rst_ltur)
                   else
                       ierror = 0
                       rst_ltur = 0
                   endif
               endif
               !
               if (dp_from_map_file .and. ierror==0) then
                   ierror = nf90_inq_varid(fds, 'DPS', idvar)
                   if (ierror/= 0) then
                       write(lundia, '(a)') 'No bed level data on restart file available:'
                       write(lundia, '(a)') 'using bed level data as prescribed in master definition file.'
                       ierror = 0
                       dp_from_map_file = .false.
                   else
                       write(lundia, '(a)') 'Bed level data read from restart file.'
                   endif
               endif
               !
               ! The flag rst_dp is used to set DPSOPT=DP.
               ! This ensures that the DPD values are copied into DPS in subroutine caldps
               ! Differences may occur when DPU/DPV depend on (the original) DPD
               ! The flag rst_dp is also used to check whether 
               !
               rst_dp = dp_from_map_file
               !
               if (ierror == 0) then
                   ierror = nf90_inq_varid(fds, 'UMNLDF', idvar)
                   if (ierror/= 0) then
                      has_umean = .false.
                      ierror = 0
                   endif
               endif
               !
               if (ierror == 0 .and. rst_lstsci > 0) then
                  allocate(rst_namcon(rst_lstsci), stat = ierror)
                  ierror = nf90_inq_varid(fds, 'NAMCON', idvar)
                  if (ierror == 0) ierror = nf90_get_var(fds, idvar, rst_namcon)
               endif
               !
               if (ierror == 0) then
                   ierror = nf90_inq_varid(fds, 'MORFT', idvar)
                   if (ierror/= 0) then
                      has_morft = .false.
                      ierror = 0
                   endif
               endif
           endif
           !
           do l = 1,lstsci
              found = .false.
              do ll = 1,rst_lstsci
                  if (rst_namcon(ll)==namcon(l)) then
                     found = .true.
                     coninit(l) = ll
                     exit
                  endif
              enddo
              if (.not.found) then
                 if (namcon(l)=='Secondary flow') then
                    write(lundia, '(3A)') 'No restart data found for "',trim(namcon(l)),'"; using 0.'
                    coninit(l) = -1
                 else
                    write(lundia, '(3A)') 'No restart data found for "',trim(namcon(l)),'"; using constant default value.'
                 endif
              endif
           enddo
        endif
        !
        ! Read morphological time associated with depth from map file.
        !
        morft0 = 0.0_hp
        if (dp_from_map_file) then
           if (has_morft) then
              call rdvar(fds, filename, filetype, 'map-infsed-serie', &
                       & i_restart, gdp, ierror, lundia, morft0, 'MORFT')
           else
              write(lundia, '(3A)') 'No morphological time found on restart file; starting from TStart.'
           endif
        endif
        !
        ierror = 0
999     continue
    endif
    !
    call dfbroadc_gdp ( ierror, 1, dfint, gdp )
    if (ierror /= 0) goto 9999
    !
    call dfbroadc_gdp ( rst_lstsci, 1, dfint, gdp )
    call dfbroadc_gdp ( rst_ltur, 1, dfint, gdp )
    call dfbroadc_gdp ( coninit, lstsci, dfint, gdp)
    !
    call dfbroadc_gdp ( has_umean, 1, dfint, gdp)
    !
    call dfbroadc_gdp ( dp_from_map_file, 1, dfint, gdp )
    rst_dp = dp_from_map_file
    if (dp_from_map_file) then
        call dfbroadc_gdp ( morft0, 1, dfdble, gdp )
        morft = morft0
    endif
    !
    ! element 'S1'
    !
    call rdarray_nm(fds, filename, filetype, grnam3, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, s1, 'S1', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'DPS'
    !
    if (dp_from_map_file) then
       call rdarray_nm(fds, filename, filetype, grnam4, i_restart, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, dpd, 'DPS', rdum)
       if (ierror /= 0) goto 9999
    endif
    !
    ! element 'U1' & 'V1'
    !
    call rdarray_nmk(fds, filename, filetype, grnam3, i_restart, &
                  & nf, nl, mf, ml, iarrc, gdp, &
                  & 1, kmax, ierror, lundia, u1, 'U1', rdum)
    if (ierror /= 0) goto 9999
    call rdarray_nmk(fds, filename, filetype, grnam3, i_restart, &
                  & nf, nl, mf, ml, iarrc, gdp, &
                  & 1, kmax, ierror, lundia, v1, 'V1', rdum)
    if (ierror /= 0) goto 9999
    !
    ! element 'UMNLDF' & 'VMNLDF'
    !
    if (has_umean) then
       !
       ! UMNLDF: filtered velocity U-component for subgrid viscosity model
       !
       call rdarray_nm(fds, filename, filetype, grnam3, i_restart, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, umnldf, 'UMNLDF', rdum)
       if (ierror /= 0) goto 9999
       call rdarray_nm(fds, filename, filetype, grnam3, i_restart, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, vmnldf, 'VMNLDF', rdum)
       if (ierror /= 0) goto 9999
    elseif (htur2d) then
       call prterr(lundia, 'P004', 'umean (umnldf and/or vmnldf) not found in restart-data, but using 2D turbulence')
       call d3stop(1, gdp)
    endif
    !
    ! element 'KFU' & 'KFV'
    !
    call rdarray_nm(fds, filename, filetype, grnam3, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, kfu, 'KFU', idum)
    if (ierror /= 0) goto 9999
    call rdarray_nm(fds, filename, filetype, grnam3, i_restart, &
                 & nf, nl, mf, ml, iarrc, gdp, &
                 & ierror, lundia, kfv, 'KFV', idum)
    if (ierror /= 0) goto 9999
    !
    ! Constituents sal, temp, constituents
    !
    if (rst_lstsci>0) then
        allocate(rst_r1(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, rst_lstsci), stat = ierror)
        rst_r1 = 0.0_fp
        call rdarray_nmkl(fds, filename, filetype, grnam3, i_restart, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & 1, kmax, rst_lstsci, ierror, lundia, rst_r1, 'R1', rdum)
        if (ierror /= 0) goto 9999
        do l = 1,lstsci
            if (coninit(l)>0) then
                r1(:,:,:,l) = rst_r1(:,:,:,coninit(l))
                coninit(l) = 1
            elseif (coninit(l)<0) then
                r1(:,:,:,l) = 0.0_fp
                coninit(l) = 1
            endif
        enddo
        deallocate(rst_r1)
    endif
    !
    ! Turbulence
    !
    if (rst_ltur > 0 .and. ltur > 0) then
       if (rst_ltur >= ltur) then
           ! both k and epsilon initialized
           lturi = 0
       else ! 0 < rst_ltur < ltur
           ! only k initialized
           lturi = -2
       endif
       !
       allocate(rst_rtur1(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, rst_ltur), stat = ierror)
       rst_rtur1 = 0.0_fp
       call rdarray_nmkl(fds, filename, filetype, grnam3, i_restart, &
                     & nf, nl, mf, ml, iarrc, gdp, &
                     & 0, kmax, rst_ltur, ierror, lundia, rst_rtur1, 'RTUR1', rdum)
       if (ierror /= 0) goto 9999
        do l = 1,min(ltur,rst_ltur)
            rtur1(:,:,:,l) = rst_rtur1(:,:,:,l)
        enddo
       deallocate(rst_rtur1)
    else        
       ! no turbulent parameter initialized
       lturi = ltur
    endif
    !
    ! Check restart data for not-a-numbers
    !
    if (.not. nan_check(s1    , 'S1 (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    if (dp_from_map_file) then
       if (.not. nan_check(dpd   , 'DPS (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    endif
    if (.not. nan_check(u1    , 'U1 (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb, 1)) ierror = 1
    if (.not. nan_check(v1    , 'V1 (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb, 1)) ierror = 1
    if (has_umean /= 0) then
       if (.not. nan_check(umnldf, 'UMNLDF (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
       if (.not. nan_check(vmnldf, 'VMNLDF (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb)) ierror = 1
    endif
    if (rst_lstsci>0 .and. lstsci>0) then
       if (.not. nan_check(r1    , 'R1 (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb, 1, 1)) ierror = 1
    endif
    if (rst_ltur>0 .and. ltur>0) then
       if (.not. nan_check(rtur1 , 'RTUR1 (restart-file)', lundia, gdp%d%nlb, gdp%d%mlb, 0, 1)) ierror = 1
    endif
    call dfreduce_gdp( ierror, 1, dfint, dfmax, gdp )
    if (ierror /= 0) goto 9999
    !
    ! restart ROLLER model
    !
    if (roller) then
       call restart_trim_roller(lundia    ,error     ,fds       ,filename  ,filetype  ,   &
                              & i_restart ,r(ewave1) ,r(eroll1) ,r(qxkr)   , &
                              & r(qykr)   ,r(qxkw)   ,r(qykw)   ,r(fxw)    ,r(fyw)    , &
                              & r(wsu)    ,r(wsv)    ,r(guu)    ,r(gvv)    , &
                              & r(hrms)   ,rdum      ,gdp       )
    endif
    !
9999 continue
    if (ierror /= 0) error = .true.
    if (inode == master) then
       if (filetype == FTYPE_NETCDF) then
          ierror = nf90_sync(fds); call nc_check_err(lundia, ierror, "sync file", filename)
          ierror = nf90_close(fds); call nc_check_err(lundia, ierror, "closing file", filename)
       elseif (filetype == FTYPE_NEFIS) then
          ierror = clsnef(fds)
       endif
       if (ierror/= 0) error = .true.
    endif
end subroutine restart_trim_flow
