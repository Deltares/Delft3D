subroutine flow_nefis_restart(lundia    ,error     ,restid1   ,lturi     ,mmax      , &
                            & nmaxus    ,kmax      ,lstsci    ,ltur      ,w1        , &
                            & s1        ,u1        ,v1        ,r1        ,rtur1     , &
                            & umnldf    ,vmnldf    ,kfu       ,kfv       , &
                            & dp        ,ex_nfs    ,namcon    ,coninit   ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: flow_nefis_restart.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/flow_nefis_restart.f90 $
!!--description-----------------------------------------------------------------
! Reads initial field condition records from an
! NEFIS flow output map file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
!
    use globaldata
    use string_module
    !
    use dfparall
    !
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
    character(256) , pointer :: restid
    real(hp)       , pointer :: morft
    real(hp)       , pointer :: morft0
    real(fp)       , pointer :: bed
    integer                 , pointer :: cutcell
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: dpH
    real(fp), dimension(:,:), pointer :: poros
!
! Global variables
!
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
    logical                                                                                  :: ex_nfs !  Flag indicating whether Nefis restart files exist
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: dp
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: s1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: umnldf
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)              , intent(out) :: vmnldf
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur), intent(out) :: rtur1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: u1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)        , intent(out) :: v1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)      , intent(out) :: w1
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci), intent(out) :: r1
    character(*)                                                                             :: restid1
    character(20), dimension(lstsci + ltur)                                    , intent(in)  :: namcon !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                               :: lrid        ! character variables for files Help var., length of restid
    integer, external                     :: crenef
    integer, external                     :: getelt
    integer, external                     :: clsnef
    integer                               :: ierror
    integer                               :: fds
    integer, external                     :: inqmxi
    integer, external                     :: neferr
    integer                               :: ii
    integer                               :: itmapc
    integer                               :: l
    integer                               :: ll
    integer                               :: max_index
    integer                               :: rst_lstci
    integer                               :: rst_ltur
    integer, dimension(3,5)               :: cuindex
    integer, dimension(3,5)               :: uindex
    integer, dimension(:,:,:,:), pointer  :: ibuff
    logical                               :: found
    logical                               :: errRESTtime
    integer                               :: has_umean
    real(fp)                              :: dtm          ! time step in minutes (flexible precision)
    real(fp)                              :: t_restart
    real(sp)                              :: dtms         ! time step in minutes (single precision)
    real(sp), dimension(:,:,:,:), pointer :: sbuff
    character(20), dimension(:), allocatable :: rst_namcon
    character(1024)                       :: error_string
    character(256)                        :: dat_file
    character(256)                        :: def_file
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
    real(sp), dimension(:,:), allocatable :: s1_g
    real(sp), dimension(:,:), allocatable :: dp_g
    real(sp), dimension(:,:), allocatable :: dpL_g
    real(sp), dimension(:,:), allocatable :: dpH_g
    real(sp), dimension(:,:), allocatable :: poros_g
    real(sp), dimension(:,:,:), allocatable :: u1_g
    real(sp), dimension(:,:,:), allocatable :: v1_g        
    real(sp), dimension(:,:,:), allocatable :: w1_g        
    real(sp), dimension(:,:), allocatable :: umnldf_g
    real(sp), dimension(:,:), allocatable :: vmnldf_g
    integer, dimension(:,:), allocatable :: kfu_g
    integer, dimension(:,:), allocatable :: kfv_g        
    real(sp), dimension(:,:,:,:), allocatable :: r1_g
    real(sp), dimension(:,:,:,:), allocatable :: rtur1_g
    integer                                   :: ier1, ier2
    integer, dimension(10)                    :: ierr
!
!! executable statements -------------------------------------------------------
!
    cutcell => gdp%gdimbound%cutcell
    dpL     => gdp%gdimbound%dpL
    dpH     => gdp%gdimbound%dpH
    poros   => gdp%gdimbound%poros
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
    ! dp_from_map_file=false: do not read depth from map file
    !
    call prop_get_logical(gdp%mdfile_ptr, '*', 'dp_from_map_file', dp_from_map_file)
    restid       = restid1
    nullify(ibuff)
    nullify(sbuff)
    error        = .false.
    errRESTtime  = .false.
    error_string = ' '
    call remove_leading_spaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    
    if (inode == master) then
       ierror = crenef(fds, dat_file, def_file, ' ', 'r')
    endif
    call dfbroadc_gdp ( ierror, 1, dfint, gdp )
    if (ierror/= 0) then
       error = .true.
       goto 9999
    endif
    ex_nfs = .true.
    write(lundia, '(a)') 'Restarting from ' // trim(dat_file) // ' and ' // trim(def_file)

    !
    ! Now also reading KFU and KFV from restart file
    !
    kfuv_from_restart = .true.
    !
    ! initialize group index time dependent data
    !
    uindex (3,1) = 1 ! increment in time
    uindex (1,1) = 1
    uindex (2,1) = 1
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    ! the master opens and reads the grid file 
    ! 
    if ( inode /= master ) goto 50 
    !
    ierror = getelt(fds, 'map-const', 'DT', cuindex, 1, 4, dtms)
    !
    dtm = dtms
    if (ierror/= 0) then
       ierror = neferr(0,error_string)
       call prterr(lundia    ,'P004'    , error_string)
       error = .true.
       goto 9999
    endif
    !
    ! Read the type of layer model from the map file. 
    ! Parameter LAYER_MODEL may not be present. Was initialized with value 'UNKNOWN' in RDIC.f90
    ! The value is needed to check for consistency when applying the z-model with ZTBML=#Y#, 
    ! see subroutine CHKSET.
    !
    ierror = getelt(fds, 'map-const', 'LAYER_MODEL', cuindex, 1, 16, rst_layer_model)
    !
    ierror = inqmxi(fds, 'map-series', max_index)
    if (ierror/= 0) then
       ierror = neferr(0,error_string)
       call prterr(lundia    ,'P004'    , error_string)
       error = .true.
       goto 9999
    endif
    !
    ! look for restart time on nefis map file
    !
    found = .false.
    do ii = max_index,1,-1 ! assume last time on map file has highest probability of being the requested time step
       uindex (1,1) = ii
       uindex (2,1) = ii
       ierror = getelt(fds, 'map-info-series', 'ITMAPC', uindex, 1, 4, itmapc)
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       t_restart = dtm*itmapc
       if (abs(tstart-t_restart) < 0.5_fp*dtm) then
          write(lundia, '(a,i5,a,e20.4)') 'using field ',ii,' associated with time T = ',t_restart
          i_restart = ii
          found     = .true.
          exit ! restart time found on map file
       end if
    enddo
    !
    if (.not. found) then
       call prterr(lundia    ,'P004'    , &
            & 'Restart time not found on restart file ' // trim(dat_file))
       errRESTtime = .true.
       error = .true.
       goto 9999
    else
       uindex (1,1) = i_restart
       uindex (2,1) = i_restart
       !
       ! The following parameters use a nmaxus*mmax*kmax*1 buffer:
       ! S1, DPS, DPL, DPH , U1, V1, UMNLDF, VMNLDF
       !
       allocate(sbuff(nmaxgl, mmaxgl, kmax, 1), stat = ier1)
       allocate(s1_g(nmaxgl,mmaxgl), stat = ier2)
       if (ier1 /= 0 .or. ier2 /= 0) then
          call prterr(lundia, 'G020', 's1_g')
          call d3stop(1, gdp)
       endif
       !
       ! S1
       !
       ierror = getelt( fds , 'map-series', 'S1', uindex, 1, mmaxgl*nmaxgl*4, sbuff )
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       s1_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
       if (.not. nan_check(s1_g, 's1_g (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! Only read depth when dp_from_map_file=true
       !
       if (dp_from_map_file) then
          !
          ! DPS
          !
          ierror = getelt( fds , 'map-sed-series', 'DPS', uindex, 1, mmaxgl*nmaxgl*4, sbuff )
          if (ierror/= 0) then
             write(lundia, '(a)') 'No bed level data on restart file available:'
             write(*,*) 'No bed level data on restart file available: PROBABLY using hdt and quarterdt for printing. Remove them. If fixed bed comment just press enter and continue'
              !PAUSE
          !   STOP
             
             write(lundia, '(a)') 'using bed level data as prescribed in master definition file.'
             dp_from_map_file = .false.
          else
             !
             ! The read DPS is placed in array DP
             !
             write(lundia, '(a)') 'Bed level data read from restart file.'
             allocate(dp_g(nmaxgl,mmaxgl), stat = ier1)
             if (ier1 /= 0) then
                call prterr(lundia, 'G020', 'dp_g')
                call d3stop(1, gdp)
             endif
             dp_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
             if (.not. nan_check(dp_g, 'dp_g (restart-file)', lundia)) call d3stop(1, gdp)
             !
             ! Read associated morphological time from map file.
             !
             ierror     = getelt(fds, 'map-infsed-serie', 'MORFT',  uindex, 1, 8, morft0)
             if (ierror/=0) morft0 = 0.0_hp
             morft = morft0
          endif
          if (cutcell>0) then
             !note1: here dpL and dpH overwrite the values read from depth files in subroutine rddept
             !note2: dpL and dpH for now are in map-series, consider to move it to map-sed-series
             !
             ! DPL
             !
             ierror = getelt( fds , 'map-series', 'dpL', uindex, 1, mmaxgl*nmaxgl*4, sbuff )
             if (ierror/= 0) then
                write(lundia, '(a)') 'No lower bed level data on restart file available:'
             !   call d3stop(1, gdp)
             else
                write(lundia, '(a)') 'Lower bed level data read from restart file.'
                allocate(dpL_g(nmaxgl,mmaxgl), stat = ier1)
                if (ier1 /= 0) then
                   call prterr(lundia, 'G020', 'dpL_g')
                   call d3stop(1, gdp)
                endif
                dpL_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
                if (.not. nan_check(dpL_g, 'dpL_g (restart-file)', lundia)) call d3stop(1, gdp)
             endif
             !
             ! DPH
             !
             ierror = getelt( fds , 'map-series', 'dpH', uindex, 1, mmaxgl*nmaxgl*4, sbuff )
             if (ierror/= 0) then
                write(lundia, '(a)') 'No upper bed level data on restart file available:'
              !  call d3stop(1, gdp)
             else
                write(lundia, '(a)') 'Lower bed level data read from restart file.'
                allocate(dpH_g(nmaxgl,mmaxgl), stat = ier1)
                if (ier1 /= 0) then
                   call prterr(lundia, 'G020', 'dpH_g')
                   call d3stop(1, gdp)
                endif
                dpH_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
                if (.not. nan_check(dpH_g, 'dpH_g (restart-file)', lundia)) call d3stop(1, gdp)
             endif
             !
             ! poros
             !
             ierror = getelt( fds , 'map-series', 'poros', uindex, 1, mmaxgl*nmaxgl*4, sbuff )
             if (ierror/= 0) then
                write(lundia, '(a)') 'No porosity (VOF) data on restart file available:'
              !  call d3stop(1, gdp)
             else
                write(lundia, '(a)') 'Porosity (VOF) data read from restart file.'
                allocate(poros_g(nmaxgl,mmaxgl), stat = ier1)
                if (ier1 /= 0) then
                   call prterr(lundia, 'G020', 'poros_g')
                   call d3stop(1, gdp)
                endif
                poros_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
                if (.not. nan_check(poros_g, 'poros_g (restart-file)', lundia)) call d3stop(1, gdp)
             endif
          endif
       endif
       !
       ! U1
       !
       ierror = getelt( fds , 'map-series', 'U1', uindex, 1, mmaxgl*nmaxgl*kmax*4, sbuff )
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       allocate(u1_g(nmaxgl,mmaxgl,kmax), stat = ier1)
       if (ier1 /= 0) then
          call prterr(lundia, 'G020', 'u1_g')
          call d3stop(1, gdp)
       endif
       u1_g(1:nmaxgl,1:mmaxgl,1:kmax) = sbuff(1:nmaxgl,1:mmaxgl,1:kmax,1)
       if (.not. nan_check(u1_g, 'u1_g (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! V1
       !
       ierror = getelt( fds , 'map-series', 'V1', uindex, 1, mmaxgl*nmaxgl*kmax*4, sbuff )
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       allocate(v1_g(nmaxgl,mmaxgl,kmax), stat = ier2)
       if (ier2 /= 0) then
          call prterr(lundia, 'G020', 'v1_g')
          call d3stop(1, gdp)
       endif
       v1_g(1:nmaxgl,1:mmaxgl,1:kmax) = sbuff(1:nmaxgl,1:mmaxgl,1:kmax,1)
       if (.not. nan_check(v1_g, 'v1_g (restart-file)', lundia)) call d3stop(1, gdp)
       !
       ! UMNLDF: filtered velocity U-component for subgrid viscosity model
       !
       has_umean = 1
       ierror = getelt( fds , 'map-series', 'UMNLDF', uindex, 1, mmaxgl*nmaxgl*4, sbuff)
       if (ierror/= 0) then
          if (htur2d) then
             ierror = neferr(0,error_string)
             call prterr(lundia    ,'U190'    , error_string)
          endif
          has_umean = 0
       else
          allocate(umnldf_g(nmaxgl,mmaxgl), stat = ier1)
          if (ier1 /= 0) then
             call prterr(lundia, 'G020', 'umnldf_g')
             call d3stop(1, gdp)
          endif
          umnldf_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
          if (.not. nan_check(umnldf_g, 'umnldf_g (restart-file)', lundia)) call d3stop(1, gdp)
       endif
       !
       ! VMNLDF: filtered velocity V-component for subgrid viscosity model
       !
       ierror = getelt( fds , 'map-series', 'VMNLDF', uindex, 1, mmaxgl*nmaxgl*4, sbuff)
       if (ierror/= 0) then
          if (htur2d) then
             ierror = neferr(0,error_string)
             call prterr(lundia    ,'U190'    , error_string)
          endif
          has_umean = 0
       else
          allocate(vmnldf_g(nmaxgl,mmaxgl), stat = ier1)
          if (ier1 /= 0) then
             call prterr(lundia, 'G020', 'vmnldf_g')
             call d3stop(1, gdp)
          endif
          vmnldf_g(1:nmaxgl,1:mmaxgl) = sbuff(1:nmaxgl,1:mmaxgl,1,1)
          if (.not. nan_check(vmnldf_g, 'vmnldf_g (restart-file)', lundia)) call d3stop(1, gdp)
       endif
       !
       allocate(ibuff(nmaxgl, mmaxgl, 1, 1), stat = ier1)
       allocate(kfu_g(nmaxgl,mmaxgl), stat = ier2)
       if (ier1 /= 0 .or. ier2 /= 0) then
          call prterr(lundia, 'G020', 'kfu_g')
          call d3stop(1, gdp)
       endif
       !
       ! KFU: current active/inactive status of U point
       !
       ierror = getelt( fds , 'map-series', 'KFU', uindex, 1, mmaxgl*nmaxgl*4, ibuff)
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       kfu_g=0
       kfu_g(1:nmaxgl,1:mmaxgl) = ibuff(1:nmaxgl,1:mmaxgl,1,1)
       !
       ! KFV: current active/inactive status of V point
       !
       ierror = getelt( fds , 'map-series', 'KFV', uindex, 1, mmaxgl*nmaxgl*4, ibuff)
       if (ierror/= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       allocate(kfv_g(nmaxgl,mmaxgl), stat = ier2)
       if (ier2 /= 0) then
          call prterr(lundia, 'G020', 'kfv_g')
          call d3stop(1, gdp)
       endif
       kfv_g=0
       kfv_g(1:nmaxgl,1:mmaxgl) = ibuff(1:nmaxgl,1:mmaxgl,1,1)
       !
       ! Constituents sal, temp, constituents
       ! Use nmaxus*mmax*kmax*lstsci buffer
       !
       ierror = getelt(fds, 'map-const', 'LSTCI', cuindex, 1, 4, rst_lstci)       
       if (ierror == 0) ierror = getelt(fds, 'map-const', 'LTUR', cuindex, 1, 4, rst_ltur)
       if (ierror /= 0) then
          ierror = neferr(0,error_string)
          call prterr(lundia    ,'P004'    , error_string)
          error = .true.
          goto 9999
       endif
       if (lstsci /= 0) then
          if (rst_lstci > 0) then
             allocate(rst_namcon(rst_lstci+rst_ltur), stat = ier1)
             if (ier1 /= 0) then
                call prterr(lundia, 'G020', 'rst_lstci')
                call d3stop(1, gdp)
             endif
             ierror = getelt(fds, 'map-const', 'NAMCON', cuindex, 1, 20*(rst_lstci+rst_ltur), rst_namcon)
             if (ierror/= 0) then
                ierror = neferr(0,error_string)
                call prterr(lundia    ,'P004'    , error_string)
                error = .true.
                goto 9999
             endif
          endif
          !
          deallocate(sbuff)
          allocate(sbuff(nmaxgl, mmaxgl, kmax, rst_lstci), stat = ier1)
          allocate(r1_g(nmaxgl,mmaxgl, kmax, lstsci), stat = ier2)
          if (ier1 /= 0 .or. ier2 /= 0) then
             call prterr(lundia, 'G020', 'r1_g')
             call d3stop(1, gdp)
          endif
          r1_g = 0.0_sp ! need initialization for nan_check if parameter is not available on restart file
          !
          if (rst_lstci > 0) then
             ierror = getelt( fds , 'map-series', 'R1', uindex, 1, mmaxgl*nmaxgl*kmax*rst_lstci*4, sbuff )
             if (ierror/= 0) then
                ierror = neferr(0,error_string)
                call prterr(lundia    ,'P004'    , error_string)
                error = .true.
                goto 9999
             endif
          endif
          !
          do l = 1,lstsci
             found = .false.
             do ll = 1,rst_lstci
                 if (rst_namcon(ll)==namcon(l)) then
                    found = .true.
                    coninit(l) = 1
                    r1_g(1:nmaxgl,1:mmaxgl,1:kmax,l) = sbuff(1:nmaxgl,1:mmaxgl,1:kmax,ll)
                    exit
                 endif
             enddo
             if (.not.found) then
                if (namcon(l)=='Secondary flow') then
                   write(lundia, '(3A)') 'No restart data found for "',trim(namcon(l)),'"; using 0.'
                   ! r1_g(1:nmaxgl,1:mmaxgl,1:kmax,l) = 0.0_sp already initialized after allocation
                   coninit(l) = 1
                else
                   write(lundia, '(3A)') 'No restart data found for "',trim(namcon(l)),'"; using constant default value.'
                   ! coninit(l) = 0
                endif
             endif
          enddo
          if (.not. nan_check(r1_g, 'r1_g (restart-file)', lundia)) call d3stop(1, gdp)
          !
          if (rst_lstci > 0) then
             deallocate(rst_namcon)
          endif
       endif
       !
       ! vertical velocity (uses nmaxus*mmax*0:kmax buffer)
       !
       if (kmax>1) then
          deallocate(sbuff)
          allocate(sbuff(nmaxgl, mmaxgl, 0:kmax,1), stat = ier1)
          allocate(w1_g(nmaxgl,mmaxgl, 0:kmax), stat = ier2)
          if (ier1 /= 0 .or. ier2 /= 0) then
             call prterr(lundia, 'G020', 'w1')
             call d3stop(1, gdp)
          endif
!         get w1
          ierror = getelt( fds , 'map-series', 'W', uindex, 1, mmaxgl*nmaxgl*(kmax+1)*4, sbuff )
          !sbuff = 0._fp
          if (ierror/= 0) then
             ierror = neferr(0,error_string)
             call prterr(lundia    ,'P004'    , error_string)
             error = .true.
             goto 9999
          endif
          w1_g(1:nmaxgl,1:mmaxgl,0:kmax) = sbuff(1:nmaxgl,1:mmaxgl,0:kmax,1)
          if (.not. nan_check(w1_g, 'w1_g (restart-file)', lundia)) call d3stop(1, gdp)
       endif
       !
       ! Turbulence
       ! Use nmaxus*mmax*0:kmax*ltur buffer
       !
       lturi = 0
       if (ltur > 0) then
          if (ltur == rst_ltur) then
             deallocate(sbuff)
             allocate(sbuff(nmaxgl, mmaxgl, 0:kmax, ltur), stat = ier1)
             allocate(rtur1_g(nmaxgl,mmaxgl, 0:kmax, 1:ltur), stat = ier2)
             if (ier1 /= 0 .or. ier2 /= 0) then
                call prterr(lundia, 'G020', 'rtur1_g')
                call d3stop(1, gdp)
             endif
             ierror = getelt( fds , 'map-series', 'RTUR1', uindex, 1, mmaxgl*nmaxgl*(kmax+1)*rst_ltur*4, sbuff )
             if (ierror/= 0) then
                ierror = neferr(0,error_string)
                call prterr(lundia    ,'P004'    , error_string)
                error = .true.
                goto 9999
             endif
             rtur1_g(1:nmaxgl,1:mmaxgl,0:kmax,1:ltur) = sbuff(1:nmaxgl,1:mmaxgl,0:kmax,1:ltur)
             if (.not. nan_check(rtur1_g, 'rtur1_g (restart-file)', lundia)) call d3stop(1, gdp)
          else        
             write(lundia, *) 'Turbulence model is not compatible with previous simulation, default initialisation will be used'
          endif
       endif
    endif
    !
    !    end of master part
    !
    ! todo:
    ! - master should not jump to 9999 or call d3stop while reading data,
    !   but jump to 50 and scatter error-flag.
    ! - the same hold for the other nodes when allocate fails.
    ! - scattering all flags in one call is more efficient,
    !   but takes more lines of code.
    !
    ! scatter information to all nodes
50 continue
    !
    ! first scatter flags that are changed by the master
    ! (as ltur1, r1 etc) to all other domains!
    !
    call dfbroadc_gdp ( rst_lstci, 1, dfint, gdp )
    call dfbroadc_gdp ( rst_ltur, 1, dfint, gdp )
    call dfbroadc_gdp ( lturi, 1, dfint, gdp )
    call dfbroadc_gdp ( dp_from_map_file, 1, dfint, gdp )
    call dfbroadc_gdp ( has_umean, 1, dfint, gdp)
    call dfbroadc_gdp ( coninit, lstsci, dfint, gdp)
    !
    ! If dp_from_map_file then the read DPS is placed in array DP
    ! The flag rst_dp is used to set DPSOPT=DP
    ! This ensures that the DP values are copied into DPS in subroutine caldps
    ! Differences may occur when DPU/DPV depend on (the original) DP
    !
    rst_dp = dp_from_map_file

    call dfsync(gdp)
      
    if ( inode /= master ) then 
       ierr(8:10) = 0
       allocate (s1_g(nmaxgl,mmaxgl), stat=ierr(1)) 
       if (dp_from_map_file) then 
          allocate (dp_g (nmaxgl,mmaxgl), stat=ierr(10))
          if (cutcell>0) then
             allocate (dpL_g(nmaxgl,mmaxgl), stat=ierr(10))
             allocate (dpH_g(nmaxgl,mmaxgl), stat=ierr(10))
             allocate (poros_g(nmaxgl,mmaxgl), stat=ierr(10))
          endif
       endif
       allocate (u1_g(nmaxgl,mmaxgl,kmax), stat = ierr(2)) 
       allocate (v1_g(nmaxgl,mmaxgl,kmax), stat = ierr(3)) 
       if (kmax>1) allocate (w1_g(nmaxgl,mmaxgl,kmax), stat = ierr(2)) !note: ierr(2)
       allocate (umnldf_g(nmaxgl,mmaxgl), stat = ierr(4)) 
       allocate (vmnldf_g(nmaxgl,mmaxgl), stat = ierr(5))
       allocate (kfu_g(nmaxgl,mmaxgl), stat = ierr(6)) 
       allocate (kfv_g(nmaxgl,mmaxgl), stat = ierr(7))
       if (lstsci > 0 .and. lstsci == rst_lstci) allocate (r1_g(nmaxgl,mmaxgl,kmax,1:lstsci), stat = ierr(8))
       if (ltur > 0 .and. ltur == rst_ltur)  allocate (rtur1_g(nmaxgl,mmaxgl,0:kmax,1:ltur), stat = ierr(9))
       if (any(ierr(1:10) /= 0)) then
          call prterr(lundia, 'G020', 'restart-data')
          call d3stop(1, gdp)
       endif
    endif
    !
    ! check if umean is found; necessary for 2D turbulence
    !
    if (htur2d .and. has_umean == 0) then
       call prterr(lundia, 'P004', 'umean (umnldf and/or vmnldf) not found in restart-data, but using 2D turbulence')
       call d3stop(1, gdp)
    endif
    !
    ! scatter arrays s1 etc to all nodes. Note: the broadc must use 'dfreal'
    ! since the arrays are single precision! Otherwise, intractable memory errors will occur. 
    !
    call dfbroadc_gdp ( s1_g, nmaxgl*mmaxgl, dfreal, gdp )   
    if (dp_from_map_file) then
       call dfbroadc_gdp ( dp_g , nmaxgl*mmaxgl, dfreal, gdp ) 
       if (cutcell>0) then
          call dfbroadc_gdp ( dpL_g, nmaxgl*mmaxgl, dfreal, gdp ) 
          call dfbroadc_gdp ( dpH_g, nmaxgl*mmaxgl, dfreal, gdp ) 
          call dfbroadc_gdp ( poros_g, nmaxgl*mmaxgl, dfreal, gdp )
       endif 
    endif
    call dfbroadc_gdp ( u1_g, nmaxgl*mmaxgl*kmax, dfreal, gdp )
    call dfbroadc_gdp ( v1_g, nmaxgl*mmaxgl*kmax, dfreal, gdp )
    if (kmax>1) call dfbroadc_gdp ( w1_g, nmaxgl*mmaxgl*kmax+1, dfreal, gdp )
    if (has_umean /= 0) then
       call dfbroadc_gdp ( umnldf_g, nmaxgl*mmaxgl, dfreal, gdp )
       call dfbroadc_gdp ( vmnldf_g, nmaxgl*mmaxgl, dfreal, gdp )
    endif
    call dfbroadc_gdp ( kfu_g, nmaxgl*mmaxgl, dfint, gdp )
    call dfbroadc_gdp ( kfv_g, nmaxgl*mmaxgl, dfint, gdp )
    if (lstsci > 0 .and. lstsci  == rst_lstci) call dfbroadc_gdp ( r1_g, nmaxgl*mmaxgl*kmax*lstsci, dfreal, gdp )
    if (ltur > 0  .and. ltur == rst_ltur )  call dfbroadc_gdp ( rtur1_g, nmaxgl*mmaxgl*(kmax+1)*ltur, dfreal, gdp )    
    !
    !
    ! 
    ! put copies of parts of s1 etc for each subdomain 
    ! 
    call dfsync ( gdp ) 
    do m = mfg, mlg 
       do n = nfg, nlg 
          s1(n-nfg+1,m-mfg+1) = s1_g(n,m) 
          u1(n-nfg+1,m-mfg+1,1:kmax) = u1_g(n,m,1:kmax)
          v1(n-nfg+1,m-mfg+1,1:kmax) = v1_g(n,m,1:kmax)
          if (kmax>1) w1(n-nfg+1,m-mfg+1,0:kmax) = w1_g(n,m,0:kmax)
          ! copy also kfu and kfv flags, however closed boundaries should remain closed
          ! unfortunately kcu/kcv are not yet initialized. The check: if (kcuv==0) kfuv=0
          ! will be carried out later on.
          kfu(n-nfg+1,m-mfg+1) = kfu_g(n,m)
          kfv(n-nfg+1,m-mfg+1) = kfv_g(n,m)
       enddo 
    enddo 
    if (has_umean /= 0) then
       do m = mfg, mlg 
          do n = nfg, nlg 
             umnldf(n-nfg+1,m-mfg+1) = umnldf_g(n,m)
             vmnldf(n-nfg+1,m-mfg+1) = vmnldf_g(n,m)
          enddo 
       enddo 
    endif

    if (dp_from_map_file) then
       do m = mfg, mlg 
          do n = nfg, nlg 
             dp (n-nfg+1,m-mfg+1) = dp_g(n,m)      
             if (cutcell>0) then    
                dpL(n-nfg+1,m-mfg+1) = dpL_g(n,m)    
                dpH(n-nfg+1,m-mfg+1) = dpH_g(n,m)    
                poros(n-nfg+1,m-mfg+1) = poros_g(n,m)  
             endif
          enddo
       enddo    
    endif

    if (lstsci > 0 .and. lstsci  == rst_lstci) then
       do m = mfg, mlg 
          do n = nfg, nlg 
             r1(n-nfg+1,m-mfg+1,1:kmax,1:lstsci) = r1_g(n,m,1:kmax,1:lstsci)          
          enddo
       enddo    
    endif 
       
    if (ltur > 0  .and. ltur == rst_ltur ) then
       do m = mfg, mlg 
          do n = nfg, nlg 
             rtur1(n-nfg+1,m-mfg+1,0:kmax,1:ltur) = rtur1_g(n,m,0:kmax,1:ltur)          
          enddo
       enddo   
    endif       

    deallocate(u1_g, v1_g, stat = ierr(1))
    if (kmax>1) deallocate(w1_g, stat = ierr(1))
    deallocate(s1_g, stat = ierr(2))
    ierr(3:8) = 0
    if (allocated(umnldf_g)) deallocate(umnldf_g, stat = ierr(3))
    if (allocated(vmnldf_g)) deallocate(vmnldf_g, stat = ierr(4))
    deallocate(kfu_g, kfv_g, stat=ierr(5))
    if (dp_from_map_file) deallocate(dp_g, stat = ierr(6))
    if (lstsci > 0 .and. lstsci  == rst_lstci) deallocate(r1_g, stat = ierr(7))
    if (ltur > 0  .and. ltur == rst_ltur ) deallocate(rtur1_g, stat = ierr(8))
    if (any(ierr(1:8) /= 0)) then
       call prterr(lundia, 'U021', 'flow_nefis_restart: memory de-allocate error')
    endif

    call dfsync(gdp)


    if (inode == master) then
       if (associated(sbuff)) deallocate (sbuff)
       if (associated(ibuff)) deallocate (ibuff)
       ierror = clsnef(fds) 
    endif

    if (roller) then
       call roller_nefis_restart(lundia    ,error     ,restid1,   &
               & i_restart, r(ewave1) ,r(eroll1) ,r(qxkr)   , &
                          & r(qykr)   ,r(qxkw)   ,r(qykw)   ,r(fxw)    ,r(fyw)    , &
                          & r(wsu)    ,r(wsv)    ,r(guu)    ,r(gvv)    , &
                          & r(hrms)   ,gdp       )  
    endif   
                
9999 continue
    if  (errRESTtime == .true.) then
       write(lundia, '(300a)') 'Restart time has to be chosen among the following values (with a precision of half dt): '
       do ii = 1,max_index
          uindex (1,1) = ii
          uindex (2,1) = ii
          ierror = getelt(fds, 'map-info-series', 'ITMAPC', uindex, 1, 4, itmapc)
          t_restart = dtm*itmapc
          write(lundia, '(i15,f25.15)') ii,t_restart
       enddo
    endif
    !
    ! todo:
    ! no communication since last call to dfsync, so it can be removed?
    !
end subroutine flow_nefis_restart
