subroutine setwav(comfil    ,lundia    ,error     ,mmax       ,nmax       , &
                & nmaxus    ,itimc     ,ntwav     ,itlen      ,timwav     , &
                & norow     ,noroco    ,irocol    ,ifcore     ,dps        , &
                & s0        ,uorb      ,tp        ,teta       ,dis        , &
                & wsu       ,wsv       ,grmasu    ,grmasv     ,hrms       , &
                & ubot      ,wlen      ,hrmcom     ,tpcom      , &
                & dircom    ,discom    ,wsucom    ,wsvcom     ,msucom     , &
                & msvcom    ,ubcom     ,wlcom      ,rlabda     , &
                & dircos    ,dirsin    ,ewave0    ,roller     ,wavcmp     , &
                & ewabr0    ,wsbodyu   ,wsbodyv   ,wsbodyucom ,wsbodyvcom , &
                & waveol    ,gdp       )
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
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use flow2d3d_timers
    use datagroups
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                 , pointer :: eps
    logical                  , pointer :: first
    type (datagroup)         , pointer :: group
    real(fp)                 , pointer :: gammax
    real(fp)                 , pointer :: rhow
    real(fp)                 , pointer :: ag
    integer                  , pointer :: iro
    logical                  , pointer :: tps_from_com
    logical                  , pointer :: ubot_from_com
    logical                  , pointer :: wlen_from_com
    logical                  , pointer :: only_distot_from_com
!
! Global variables
!
    integer                                                          , intent(in)  :: itimc      !!  Current time step counter for 2D
                                                                                                 !!  system
    integer                                                          , intent(in)  :: itlen      !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: lundia     !  Description and declaration in inout.igs
    integer                                                                        :: mmax       !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmax       !  Description and declaration in esm_alloc_int.f90
    integer                                                                        :: nmaxus     !  Description and declaration in esm_alloc_int.f90
    integer                                                          , intent(in)  :: noroco     !  Description and declaration in esm_alloc_int.f90
    integer                                                          , intent(in)  :: norow      !  Description and declaration in esm_alloc_int.f90
    integer                                                          , intent(in)  :: ntwav      !!  Total number of timesteps on com-
                                                                                                 !!  munication file (to read from)
    integer                                                          , intent(in)  :: waveol     !  Wave setting (0 = offline, 1 = offline mimicking online, 2 = online)
    integer   , dimension(2)                                                       :: ifcore     !!  Time indices (cell id's) of the wave
                                                                                                 !!  functions which are in core available
    integer   , dimension(5, noroco)                                 , intent(in)  :: irocol     !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(ntwav)                                     , intent(in)  :: timwav     !!  Array with time steps on comm. file
                                                                                                 !!  for wave results
    logical                                                                        :: error      !!  Flag=TRUE if an error is encountered
    logical                                                          , intent(in)  :: roller
    logical                                                          , intent(in)  :: wavcmp
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub,4)              :: dis        !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)  , intent(in)  :: dps        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)  , intent(out) :: ewabr0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)  , intent(out) :: ewave0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: grmasu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: grmasv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: hrms       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: rlabda     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)  , intent(in)  :: s0         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: teta       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: tp         !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: uorb       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: wsu        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: wsv        !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: wsbodyu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: wsbodyv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: ubot       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                :: wlen       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: dircom     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: dircos     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: dirsin     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: discom     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: hrmcom     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: msucom     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: msvcom     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: tpcom      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: ubcom      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: wlcom      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: wsucom     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: wsvcom     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: wsbodyucom !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nmaxus, mmax, 2)                                         :: wsbodyvcom !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                   :: comfil     !!  Name for communication file
                                                                                                 !!  com-<case><label>
!
! Local variables
!
    integer                         :: datlen
    integer                         :: deflen
    integer                         :: elmndm
    integer                         :: fd_nef ! NEFIS file descriptor
    integer                         :: i      ! Hulp var. 
    integer                         :: ibf    ! Code for open boundary corresponding with cell irocol(1),irocol(2) 
    integer                         :: ibl    ! Code for open boundary corresponding with cell irocol(1),irocol(3) 
    integer                         :: ic
    integer                         :: ierror
    integer                         :: ind
    integer                         :: kmaxk  ! Number of layers in the z-dir. for frdint which is 1 here (2dh) 
    integer                         :: m
    integer                         :: mf
    integer                         :: mfu
    integer                         :: ml
    integer                         :: mlu
    integer                         :: n
    integer                         :: nf
    integer                         :: nfu
    integer                         :: nhulp                ! Hulp var. 
    integer                         :: nl
    integer                         :: nlu
    integer                         :: nready               ! Flag for determination of inter- polation coefficient 
    integer                         :: ntimwa               ! Time index of first function 
    integer                         :: ntimwb               ! Time index of second function 
    integer                         :: tact                 ! Actual time step number 
    integer      , dimension(3)     :: uindex               !  Lowest allowed reference number
    integer      , external         :: clsnef
    integer      , external         :: crenef
    integer      , external         :: getels
    integer      , external         :: inqelm
    real(fp)     , external         :: realfileversion
    real(fp)                        :: atimw                ! Interpolation factor for first function 
    real(fp)                        :: btimw                ! Interpolation factor for second function
    real(fp)                        :: latestcomfileversion ! Latest version of COM-files
    real(fp)                        :: actualcomfileversion ! Actual version of the COM-file
    real(fp)                        :: hw
    real(fp)                        :: k
    real(fp)                        :: k0
    real(fp)                        :: k0h
    real(fp)                        :: omega
    real(fp)                        :: per
    real(fp)                        :: uorb1
    character(16), dimension(1)     :: cdum16               ! Help array to read/write Nefis files 
    character(256)                  :: datnam
    character(256)                  :: defnam
    character(16)                   :: funam                ! Name of element which has to be read 
    character(16)                   :: grpnam               ! Data-group name defined for the COM-files (WAVTIM) 
    character(80)                   :: message              ! Message to diagnostic file
!
! Data statements
!
    data grpnam/'WAVTIM'/
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_COM, grpnam, group)
    !
    eps                  => gdp%gdconst%eps
    first                => group%first
    gammax               => gdp%gdnumeco%gammax
    rhow                 => gdp%gdphysco%rhow
    ag                   => gdp%gdphysco%ag
    iro                  => gdp%gdphysco%iro
    tps_from_com         => gdp%gdprocs%tps_from_com
    ubot_from_com        => gdp%gdprocs%ubot_from_com
    wlen_from_com        => gdp%gdprocs%wlen_from_com
    only_distot_from_com => gdp%gdprocs%only_distot_from_com
    !
    ! Initialize local variables
    !
    kmaxk = 1
    !
    ! Set up the element dimensions
    !
    if (first) then
       !
       ! Set up the element chracteristics
       !
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'TIMWAV', ' ', IO_INT4, 2, (/nmaxus, mmax/), ' ', 'Time of wave field rel. to reference date/time', '[ TSCALE]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'HRMS', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Root mean square wave height', '[   M   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'TP', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Peak wave period', '[   S   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DIR', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Mean direction of wave propagation relative to east ccw', '[  DEG  ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DISTOT', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave energy dissipation rate (total)', '[  W/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DISSURF', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave energy dissipation rate at the free surface', '[  W/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DISWCAP', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave energy dissipation rate due to white capping', '[  W/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'DISBOT', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave energy dissipation rate at the bottom', '[  W/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'FX', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave forcing term at surface level in u-point', '[  N/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'FY', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave forcing term at surface level in v-point', '[  N/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'WSBU', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave forcing term in water body in u-point', '[  N/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'WSBV', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave forcing term in water body in v-point', '[  N/M2 ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'MX', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave-induced volume flux in u-point', '[ M3/SM ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'MY', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Wave-induced volume flux in v-point', '[ M3/SM ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'TPS', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Smoothed peak wave period', '[   S   ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'UBOT', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Orbital motion near the bottom', '[  M/S  ]')
       call addelm(gdp, lundia, FILOUT_COM, grpnam, 'WLEN', ' ', IO_REAL4, 2, (/nmaxus, mmax/), ' ', 'Mean wave length', '[   M   ]')
       !
       ! in case of roller: first will be set to .false. later in this routine
       if (.not. roller) first = .false.
       !
       ! Set the latest COM-file version
       !
       !cdum16(1) = '00.00.00.00'
       !call getcomfileversionstring_flow2d3d(cdum16(1))
       !latestcomfileversion = realfileversion(cdum16(1), 16)
       !
       ! Aggregate file names
       !
       ind = len_trim(comfil)+1
       datnam              = comfil
       datnam(ind:ind + 3) = '.dat'
       call remove_leading_spaces(datnam, datlen)
       !
       defnam              = comfil
       defnam(ind:ind + 3) = '.def'
       call remove_leading_spaces(defnam, deflen)
       !
       ! Read the actual COM-file version from the COM-file
       !
       fd_nef               = -1
       ierror               = 0
       ierror               = crenef(fd_nef, datnam(1:datlen), defnam(1:deflen), ' ', 'r')
       uindex(1:3)          = 1
       cdum16               = '00.00.00.00'
       ierror               = getels(fd_nef, 'com-version', 'FILE-VERSION', uindex, 1, 16, cdum16 )
       actualcomfileversion = realfileversion(cdum16(1), 16)
       ierror               = clsnef(fd_nef)
       !
       if (ierror /= 0) goto 9999
       !
       ! Now set flags for which parameters are to be read/written from/to the COM-file
       ! for the present COM-fileversion
       !
       if (actualcomfileversion > 3.5499_fp) then
          only_distot_from_com = .false.
       else
          only_distot_from_com = .true.
          !
          ! Using old COM-file, will read only total dissipation from it
          ! Write warning to the diagnostic file
          !
          write (message, '(2a)') 'Using old COM-file with version ', cdum16
          call prterr(lundia, 'G051', message)
          write (lundia, '(a)') '            Not reading/writing all available data from/to it'
       endif
    endif
    !
    ! determination of reduced time and extrapolation coefficient
    !
    nready = 0
    tact   = itimc
    if (ntwav == 1) then
       ntimwa = 1
       ntimwb = 0
       atimw  = 1.0_fp
       btimw  = 0.0_fp
    elseif (waveol>0) then
       ntimwa = 1
       do i = ntwav, 2, -1
          if (tact>timwav(i)) then
             ntimwa = i
             exit
          endif
       enddo
       ntimwb = 0
       atimw  = 1.0_fp
       btimw  = 0.0_fp
    else
       if (itlen>0) then
          !
          ! periodic functions
          !
          if (tact>=timwav(ntwav)) then
             nhulp = (tact - timwav(ntwav))/itlen + 1
             tact = tact - nhulp*itlen
             if (tact<timwav(1)) then
                ntimwb = 1
                ntimwa = ntwav
                btimw = real(tact - timwav(ntwav) + itlen,fp)       &
                      & /real(timwav(1) - timwav(ntwav) + itlen,fp)
                nready = 1
             endif
          elseif (tact<=timwav(1)) then
             nhulp = (timwav(1) - tact)/itlen + 1
             tact = tact + nhulp*itlen
             if (tact>timwav(ntwav)) then
                ntimwb = 1
                ntimwa = ntwav
                btimw = real(tact - timwav(ntwav),fp)               &
                      & /real(timwav(1) - timwav(ntwav) + itlen,fp)
                nready = 1
             endif
          else
          endif
       else
          !
          ! a-periodic functions
          !
          if (tact <= timwav(1)) then
             ntimwa = 1
             ntimwb = 2
             tact = timwav(1)
             btimw = 0.0
             nready = 1
          elseif (tact>timwav(ntwav)) then
             ntimwa = ntwav - 1
             ntimwb = ntwav
             tact = timwav(ntwav)
             btimw = 1.0
             nready = 1
          else
          endif
       endif
       if (nready==0) then
          !
          ! Determination of interpolation coefficient
          !
          do i = 2, ntwav
             if (tact<=timwav(i)) then
                ntimwb = i
                ntimwa = i - 1
                btimw = real(tact - timwav(i - 1),fp)/real(timwav(i) - timwav(i - 1),fp)
                exit
             endif
          enddo
       endif
       atimw = 1.0 - btimw
    endif
    !
    ! Read the two selected timesteps of the functions and perform
    ! the interpolation. Array rlabda is used as work array.
    ! The wave height hrms is necessary for computing the orbital
    ! velocity uorb and for breaking wave induced turbulence
    !
    funam = 'HRMS'
    call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
              & funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,hrms      ,hrmcom    ,gdp       )
    if (error) goto 9999
    !
    if (tps_from_com) then
       !
       ! Read TPS from the com-file instead of TP
       ! Put it in array tp
       !
       funam = 'TPS'
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                 & funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,tp        ,tpcom     ,gdp       )
    else
       funam = 'TP'
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                 & funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,tp        ,tpcom     ,gdp       )
    endif
    if (error) goto 9999
    !
    ! Read the direction and interpolate by calling dirint
    !
    funam = 'DIR'
    call dirint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
              & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
              & funam     ,ntimwa    ,ntimwb    , &
              & atimw     ,btimw     ,teta      ,dircom    ,dircos    , &
              & dirsin    ,hrmcom    ,gdp       )
    if (error) goto 9999
    !
    if (.not. roller) then
       if (.not. only_distot_from_com) then
          !
          ! Read parameter DISTOT from the COM-file
          !
          funam = 'DISTOT'
          call frdint(comfil    ,lundia    ,error     ,ifcore  ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam  , &
                    & funam     ,ntimwa  ,ntimwb    , &
                    & atimw     ,btimw     ,dis(:,:,1),discom  ,gdp       )
          if (error) goto 9999
          !
          ! Parameters DISSURF, DISWCAP and DISBOT will also be on the COM-file
          ! when DISTOT is there
          !
          funam = 'DISSURF'
          call frdint(comfil    ,lundia    ,error     ,ifcore  ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam  , &
                    & funam     ,ntimwa  ,ntimwb    , &
                    & atimw     ,btimw     ,dis(:,:,2),discom  ,gdp       )
          if (error) goto 9999
          !
          funam = 'DISWCAP'
          call frdint(comfil    ,lundia    ,error     ,ifcore  ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam  , &
                    & funam     ,ntimwa  ,ntimwb    , &
                    & atimw     ,btimw     ,dis(:,:,3),discom  ,gdp       )
          if (error) goto 9999
          !
          funam = 'DISBOT'
          call frdint(comfil    ,lundia    ,error     ,ifcore  ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam  , &
                    & funam     ,ntimwa  ,ntimwb    , &
                    & atimw     ,btimw     ,dis(:,:,4),discom  ,gdp       )
          if (error) goto 9999
       
       else
          ! 
          ! Only read DISS (total wave dissipation) from the COM-file in case of old COM-file.
          !
          error     = .false.
          funam     = 'DISS'
          call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                    & funam     ,ntimwa    ,ntimwb    , &
                    & atimw     ,btimw     ,dis(:,:,1),discom    ,gdp       )
          if (error) goto 9999
          !
          ! Set the other components of the dissipation array, such that 
          ! they are used correctly in the turbulence model
          !
          dis(:,:,2) = dis(:,:,1)
          dis(:,:,3) = 0.0_fp
          dis(:,:,4) = 0.0_fp
       endif
       !
       funam = 'FX'
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                 & funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,wsu       ,wsucom    ,gdp       )
       if (error) goto 9999
       !
       funam = 'FY'
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                 & funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,wsv       ,wsvcom    ,gdp       )
       if (error) goto 9999
       !
       if (.not. only_distot_from_com) then
          !
          ! Parameters WSBU and WSBV will only be on the COM-file when DISTOT
          ! is there
          !
          funam = 'WSBU'
          call frdint(comfil    ,lundia    ,error     ,ifcore     ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam     , &
                    & funam     ,ntimwa     ,ntimwb    , &
                    & atimw     ,btimw     ,wsbodyu   ,wsbodyucom ,gdp       )
          if (error) goto 9999
          !
          funam = 'WSBV'
          call frdint(comfil    ,lundia    ,error     ,ifcore     ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam     , &
                    & funam     ,ntimwa     ,ntimwb    , &
                    & atimw     ,btimw     ,wsbodyv   ,wsbodyvcom ,gdp       )
          if (error) goto 9999
       else
          wsbodyu = 0.0_fp
          wsbodyv = 0.0_fp
       endif
       !
       funam = 'MX'
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                 & funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,grmasu    ,msucom    ,gdp       )
       if (error) goto 9999
       !
       funam = 'MY'
       call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                 & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                 & funam     ,ntimwa    ,ntimwb    , &
                 & atimw     ,btimw     ,grmasv    ,msvcom    ,gdp       )
       if (error) goto 9999
       !
       if (ubot_from_com) then
          funam = 'UBOT'
          call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                    & funam     ,ntimwa    ,ntimwb    , &
                    & atimw     ,btimw     ,ubot      ,ubcom     ,gdp       )
          if (error) goto 9999
       endif
       
       if (wlen_from_com) then
          funam = 'WLEN'
          call frdint(comfil    ,lundia    ,error     ,ifcore    ,mmax      , &
                    & nmax      ,kmaxk     ,nmaxus    ,grpnam    , &
                    & funam     ,ntimwa    ,ntimwb    , &
                    & atimw     ,btimw     ,wlen      ,wlcom     ,gdp       )
          if (error) goto 9999
       endif
    endif
    !
    ! Store the time index nrs which are in core now.
    !
    ifcore(1) = ntimwa
    ifcore(2) = ntimwb
    !
    ! Compute the orbital velocity and wave length.
    !
    do m = 1, mmax
       do n = 1, nmax
          hw  = max(0.01_fp, real(dps(n, m),fp) + s0(n, m))
          per = max(0.01_fp, tp(n, m))
          !
          ! Prevent unrealistic Hrms in shallow water
          !
          hrms(n, m) = min(hrms(n, m), gammax*hw)
          omega      = 2.0*pi/per
          k0         = omega*omega/ag
          k0h        = k0*hw
          if (k0h>pi) then
             k = k0
          elseif (k0h<0.005) then
             k = omega/sqrt(ag*hw)
          else
             call wavenr(hw        ,per       ,k         ,ag        )
          endif
          if (wlen_from_com) then
             rlabda(n, m) = wlen(n,m)
          else
             rlabda(n, m) = 2.0*pi/k
          endif
          if (k*hw<80.) then
             if (ubot_from_com) then
                uorb(n,m) = ubot(n,m)
             else
                uorb1      = 0.5*hrms(n, m)*omega/sinh(k*hw)
                uorb(n, m) = uorb1*sqrt(pi)/2.0
             endif
          else
             uorb(n, m) = 0.0
          endif
       enddo
    enddo
    if (roller) then
       if (first .and. .not.wavcmp) then
          !
          ! Initialise EWAVE0 using hrms, all points; before first step
          ! NOTE: SETWAV is called before timeloop with EEWAVE1 as
          ! actual argument
          !
          do m = 1, mmax
             do n = 1, nmax
                ewave0(n, m) = rhow*ag*hrms(n, m)*hrms(n, m)/8.
             enddo
          enddo
       endif
       if (first) ewabr0 = 0.0
       first = .false.
    endif
    !
    ! Calculate values of uorb, tp, and hrms at open boundaries
    ! Rows
    !
    do ic = 1, norow
       n   = irocol(1, ic)
       mfu = irocol(2, ic)
       ml  = irocol(3, ic)
       ibf = irocol(4, ic)
       ibl = irocol(5, ic)
       mf  = mfu - 1
       mlu = ml + 1
       !
       ! Left side of row
       !
       if (ibf/=1) then
          uorb(n, mf)   = uorb(n, mfu)
          tp(n, mf)     = tp(n, mfu)
          hrms(n, mf)   = hrms(n, mfu)
          rlabda(n, mf) = rlabda(n, mfu)
          teta(n, mf)   = teta(n, mfu)
          if (roller .and. ibf /= 8 .and. .not. wavcmp) then
             ewave0(n, mf) = rhow*ag*hrms(n, mfu)*hrms(n, mfu)/8.
          endif
       endif
       !
       ! Right side of row
       !
       if (ibl/=1) then
          uorb(n, mlu)   = uorb(n, ml)
          tp(n, mlu)     = tp(n, ml)
          hrms(n, mlu)   = hrms(n, ml)
          rlabda(n, mlu) = rlabda(n, ml)
          teta(n, mlu)   = teta(n, ml)
          if (roller .and. ibl /= 8 .and. .not. wavcmp) then
             ewave0(n, mlu) = rhow*ag*hrms(n, ml)*hrms(n, ml)/8.
          endif
       endif
    enddo
    !
    ! Columns
    !
    if (.not.(roller .and. wavcmp)) then
       do ic = norow + 1, noroco
          m   = irocol(1, ic)
          nfu = irocol(2, ic)
          nl  = irocol(3, ic)
          ibf = irocol(4, ic)
          ibl = irocol(5, ic)
          nf  = nfu - 1
          nlu = nl + 1
          !
          ! Lower side of column
          !
          if (ibf/=1) then
             uorb(nf, m)   = uorb(nfu, m)
             tp(nf, m)     = tp(nfu, m)
             hrms(nf, m)   = hrms(nfu, m)
             rlabda(nf, m) = rlabda(nfu, m)
             teta(nf, m)   = teta(nfu, m)
             if (roller .and. ibf /= 8 .and. .not. wavcmp) then
                ewave0(nf, m) = rhow*ag*hrms(nfu, m)*hrms(nfu, m)/8.
             endif
          endif
          !
          ! Upper side of column
          !
          if (ibl/=1) then
             uorb(nlu, m)   = uorb(nl, m)
             tp(nlu, m)     = tp(nl, m)
             hrms(nlu, m)   = hrms(nl, m)
             rlabda(nlu, m) = rlabda(nl, m)
             teta(nlu, m)   = teta(nl, m)
             if (roller .and. ibl /= 8 .and. .not. wavcmp) then
                ewave0(nlu, m) = rhow*ag*hrms(nl, m)*hrms(nl, m)/8.
             endif
          endif
       enddo
    else
       do ic = norow + 1, noroco
          m   = irocol(1, ic)
          nfu = irocol(2, ic)
          nl  = irocol(3, ic)
          ibf = irocol(4, ic)
          ibl = irocol(5, ic)
          nf  = nfu - 1
          nlu = nl + 1
          !
          ! Lower side of column
          !
          if (ibf/=1) then
             teta(nf, m) = teta(nfu, m)
          endif
          !
          ! Upper side of column
          !
          if (ibl/=1) then
             teta(nlu, m) = teta(nl, m)
          endif
       enddo
    endif
 9999 continue
end subroutine setwav
