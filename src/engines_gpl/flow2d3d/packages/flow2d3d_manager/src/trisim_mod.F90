module mod_trisim
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
!    Function: Main routine for the 2d / 3d program
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!
contains
!


!
!==============================================================================
integer function trisim_init(numdom, nummap, context_id, fsm_flags, runid_arg, olv_handle, gdp) result(retval)
!
!!--declarations----------------------------------------------------------------
!
    use precision
    use SyncRtcFlow
    use dfparall
    use flow2d3d_timers
    use d3d_olv_class
    !
    use m_openda_exchange_items, only : openda_buffer_initialize
    !
    ! global data declaration; compare with include 'globdat.igd'
    !
    use globaldata
    use string_module
    implicit none
    type(globDat)  , target   :: gdp
    !
    integer                       , pointer :: lundia
    integer                       , pointer :: lunprt
    integer                       , pointer :: iphisi
    integer, dimension(:)         , pointer :: ipmap
    logical                       , pointer :: dredge
    logical                       , pointer :: struct
    integer                       , pointer :: numdomains
    integer                       , pointer :: nummappers
    character(6)                  , pointer :: prognm
    character(256)                , pointer :: runid
    integer                       , pointer :: rtcmod
    include 'fsm.i'
!
! Parameters
!
    integer       , intent(in)  :: context_id
    integer       , intent(in)  :: fsm_flags
    integer       , intent(in)  :: numdom        ! Number of subdomains (0 means single domain)
                                                 ! as detected by hydra
    integer       , intent(in)  :: nummap        ! Number of mappers (one for each DD boundaries connected with this subdomain)
                                                 ! as detected by hydra
    character(*)                :: runid_arg
    type(olvhandle)             :: olv_handle
!
! Local variables
!
    integer        , pointer            :: itinit
    integer        , pointer            :: itstrt
    integer                             :: fsmstatus
    integer                             :: i
    integer                             :: ic           ! Length of character parameter CASE 
    integer                             :: icheck
    integer        , pointer            :: it01         ! Reference date in yymmdd 
    integer        , pointer            :: it02         ! Reference time in hhmmss 
    integer        , pointer            :: itima        ! Time to start simulation (N * tscale) according to DELFT3D conventions 
    integer        , pointer            :: itlen        ! Lenght of the tide cycle 
    integer                             :: lenid
    integer                             :: lunid
    integer                             :: luntri       ! Unit number for trigger file for TRISIM for running programs simultaniously 
    integer                             :: nhystp
    integer                  , external :: newlun
    integer                  , external :: fsmtrf
    logical                             :: ex
    logical                             :: init         ! Flag=TRUE when initialisation is required (always the case if FLOW is used stand alone) 
    logical                             :: lexist
    logical                             :: opend        ! Help logical var. to determine whether each of the output files was opened 
    real(fp)       , pointer            :: tscale       ! Basic unit time 
    character(12)                       :: filmrs       ! File name for DELFT3D_MOR FLOW input file (MD-flow.xxx) 
    character(12)                       :: filsim       ! Name for trigger file for TRISIM for running programs simultaniously 
    character(256)                      :: case         ! Project identification (a non-blank character string presumed) 
    character(256) , pointer            :: comfil       ! Communication file name 
    character(256) , pointer            :: filmd        ! File name for MD FLOW file 
    character(256) , pointer            :: trifil       ! File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def) 
    character(5)                        :: filid
    character(5)   , pointer            :: versio       ! Version nr. of the current package 
!! executable statements -------------------------------------------------------
!
    ! Initialization using a semaphore
    ! Related vseminit is in tricom.f90
    !
    call pseminit
    !
    ! Initializes MPI
    !
    call dfinitmpi
    !
    ! initialize GDP structure
    ! allocate(gdp) MUST have been done in trisim/trisim_dll
    !
    ! openDA buffer
    !
    call openda_buffer_initialize
    !    
    call gdp_alloc(gdp)
    call initsafe(gdp)
    call timers_init(gdp)
    call timer_start(timer_total, gdp)
    call timer_start(timer_init, gdp)
    !
    ! esm/fsm initialization
    !
    fsmstatus = fsmini (context_id, fsm_flags)
    !
    runid        => gdp%runid 
    lundia       => gdp%gdinout%lundia
    lunprt       => gdp%gdinout%lunprt
    iphisi       => gdp%gdinttim%iphisi
    ipmap        => gdp%gdinttim%ipmap
    itinit       => gdp%gdinttim%itinit
    itstrt       => gdp%gdinttim%itstrt    
    dredge       => gdp%gdprocs%dredge
    struct       => gdp%gdprocs%struct
    numdomains   => gdp%gdprognm%numdomains
    nummappers   => gdp%gdprognm%nummappers
    prognm       => gdp%gdprognm%prognm
    rtcmod       => gdp%gdrtc%rtcmod
    it01         => gdp%gdtricom%it01
    it02         => gdp%gdtricom%it02
    itima        => gdp%gdtricom%itima
    itlen        => gdp%gdtricom%itlen
    tscale       => gdp%gdtricom%tscale
    comfil       => gdp%gdtricom%comfil
    trifil       => gdp%gdtricom%trifil
    versio       => gdp%gdtricom%versio
    filmd        => gdp%gdtricom%filmd
    !
    ! Initialize local parameters, including IPHISI and IPMAP(1)
    ! in case program crashes the test below can be performed anyway
    !
    
    init     = .true.
    filmrs   = ' '
    !
    iphisi   = 0
    ipmap(1) = -1
    !
    rtcmod   = noRTC
    icheck   = 0
    !
    ! Store numdom (counted by Hydra) in numdomains (in GDP-structure)
    ! For single domain cases, Hydra does not count at all and numdom is zero.
    !
    numdomains = max(1,numdom)
    !
    ! Store nummap (counted by Hydra) in nummappers (in GDP-structure)
    !
    nummappers = nummap
    !
    ! runid_arg may be set by the C main routines or by OpenDA/OpenMI
    !
    runid    = runid_arg
    if (runid == ' ') then
       !
       ! First try to read runid from file called RUNID
       ! This simplifies fluidmud synchronisation
       !
       filid = 'runid'
       inquire (file = filid, exist = ex)
       if (ex) then
          open (newunit=lunid, file = filid, form = 'formatted', status = 'old')
          read (lunid, '(a)') runid
          close (lunid)
       else
          runid = ' '
       endif
    else
       !
       ! Remove (possible) trailing '\0' (and all characters behind it) from c-code
       !
       do i = 1, len(runid)
          if (ichar(runid(i:i)) == 0 .or. ichar(runid(i:i)) == 10) then
             runid(i:) = ' '
             exit
          endif
       enddo
    endif
    runid = adjustl(runid)
    !
    ! Platform dependent initialization
    !
    call pldep
    !
    ! Run TDATOM
    !
    if (.not.parll .or. inode == master) then
       call tdatmain(runid, filmrs, icheck, gdp) 
    endif
    call dfbroadc_gdp(icheck, 1, dfint,gdp)
    call dfsync(gdp)
    if (icheck /= 0 ) then
       write (*, '(a)') 'ABORT: error returned by tdatmain'
       lundia = 0
       call d3stop(1,gdp)
    endif
    !
    ! Set program name (after running tdatom)
    !
    prognm = 'TRISIM'
    !
    ! Start FLOW simulation program
    !
    call remove_leading_spaces(runid     ,lenid     )
    if (init) then
       !
       ! Read  dimensions of arrays and declare array pointers
       !
       call tripoi(runid, filmrs, versio, filmd, gdp)
       if (gdp%errorcode > 0) then
          if (btest(rtcmod,dataFromRTCToFLOW)) then
             call timer_start(timer_wait, gdp)
             call syncflowrtc_quit
             call timer_stop(timer_wait, gdp)
             rtcmod = noRTC
          endif
          !
          ! Error: try to close and return -1
          !
          call d3stop(1, gdp)
          retval = trisim_close(gdp)
          retval = -1
          return
          !
       endif
    endif
    !
    ! Initialize time frame parameters for stand alone program
    !
    it01   = 0
    it02   = 0
    !
    itlen  = 0
    tscale = 1.0
    !
    itima  = 0
    !
    ! Initialize communication file name
    ! NOTE: case may never be only blanks
    !
    case = runid
    call remove_leading_spaces(case      ,ic        )
    !
    comfil(1:4) = 'com-'
    comfil(5:)  = case(1:ic)
    !
    trifil(1:5) = 'trix-'
    trifil(6:)  = case(1:ic)
    !
    ! Insert node number in file names
    !
    if ( parll ) then
       write(comfil(5+ic:5+ic+4),'(a,i3.3)') '-',inode
    endif
    !
    ! Start FLOW simulation
    !
    olv_handle%fields => null()
    call tricom_init(olv_handle, gdp)
    !
    ! Error status of tricom_init is returned via gdp%errorcode
    !
    if (gdp%errorcode /= 0) then
        retVal = -1
    else
        retVal = 0
    endif
    !
    ! for OpenDA purposes, itinit and itfinish replace itstrt and itstop
    !
    itinit = itstrt
    !    
end function trisim_init
!
!
!
!----------------------------------------------------------------------
integer function trisim_step(olv_handle, gdp) result(retval)
    use globaldata
    use string_module
    use d3d_olv_class
    !
    implicit none
    !
    type(globdat)  , target   :: gdp
    type(olvhandle)           :: olv_handle
    !
    RetVal = 0
    !
    ! the subroutine called 'tricom_verify' form the BAW-version contains
    ! part VII and VIII of the initialisation. That subroutine is not needed here and
    ! part VII and VIII can be found at the end of tricom_init. (VT)
    !
    call tricom_step(olv_handle, gdp)
    if (gdp%errorcode /= 0) then
        retVal = -1
    else
        retVal = 0
    endif
    !
end function trisim_step
!
!
!
!-----------------------------------------------------------------------
integer function trisim_finish(olv_handle, gdp) result(retVal)
    use globaldata
    use string_module
    use dfparall
    use d3d_olv_class
    !    
    implicit none
    !
    ! global    
    type(olvhandle)           :: olv_handle
    type(globdat)  , target   :: gdp
    !
    ! local
    integer          :: i
    integer, pointer :: lundia
    !
    ! body
    lundia => gdp%gdinout%lundia 
    retval = 0

    call tricom_finish(olv_handle, gdp)

    write(lundia,*)
    write(lundia,'(a)') '*** Simulation finished *******************************************************'
    write(lundia,*)

    if (gdp%errorcode>0) then
    endif
    !
    ! Write diagnostics and close file
    ! The "do-loop, dfsync, if i=inode" ensures that trisim_close is called sequentially for all partitions.
    ! This is needed to avoid clashes, for example on general TMP-files that all partitions want to delete.
    !
    do i=1,nproc
       call dfsync(gdp)
       if (i == inode) then
          retval = trisim_close(gdp)
       endif
    enddo
    !
end function trisim_finish
!
!
!
!-----------------------------------------------------------------------
integer function trisim_close(gdp) result (retval)
    use flow2d3d_timers
    use meteo
    use globaldata
    use string_module
    !    
    implicit none
    !    
    type(globdat), target :: gdp
    !  
    character(256)                , pointer :: filmd        ! File name for MD FLOW file 
    integer                       , pointer :: lundia
    integer                       , pointer :: lunprt
    integer                       , pointer :: iphisi    
    integer                       , pointer :: numdomains
    integer                       , pointer :: nummappers
    integer        , dimension(:) , pointer :: ipmap    
    character(256)                , pointer :: runid
    character(5)                  , pointer :: versio
    !
    include 'flow_steps_f.inc'
    !
    integer                     :: nhystp
    logical                     :: opend        ! Help logical var. to determine whether each of the output files was opened 
    logical                     :: init         ! Flag=TRUE when initialisation is required (always the case if FLOW is used stand alone) 
    !
    ! body
    !
    runid        => gdp%runid
    versio       => gdp%gdtricom%versio
    filmd        => gdp%gdtricom%filmd
    lundia       => gdp%gdinout%lundia
    lunprt       => gdp%gdinout%lunprt
    iphisi       => gdp%gdinttim%iphisi    
    ipmap        => gdp%gdinttim%ipmap
    nummappers   => gdp%gdprognm%nummappers
    !
    ! Write diagnostics and close file
    !
    init = .true.
    !
    call timer_stop(timer_total, gdp)
    call timers_finish(gdp)
    if (init) then
       call triend(runid, gdp)
       if (gdp%errorcode > 0) then
          !
          ! User interaction is removed
          !
          !
          ! To avoid statemachine problems:
          !
          nhystp = nxtstp(d3dflow_init, gdp)
       endif
    endif
    !
    ! Close intermediate files TMP//runid//.*
    !
    call delfil(runid     ,filmd     ,gdp       )
    !
    ! Close diagnostic file, print file and dredge file
    ! No prints requested (LUNPRT still unit 8 see SYSINI) delete file
    !
    inquire (lundia, opened = opend)
    if (opend) close (lundia)
    if (iphisi>0 .or. ipmap(1)>=0) then
       !
       ! Only then the tri-prt file can actually be present
       !
       inquire (lunprt, opened = opend)
       if (opend) then
          close (lunprt)
       endif
    endif
    !
    call deallocmeteo(gdp%runid)
    !
    ! Tell gaws (Global ADI Wang Solver) and mapper we're done.
    !
    if (nummappers >= 1) then
        call gwsslv(-1)
        nhystp = nxtstp(d3dflow_finish, gdp)
    endif
    !
    ! Stop simulation performance measurement
    !
    !
    retval = 0  
    !
    
end function trisim_close
!
!
!
!==============================================================================
integer function trisim_initialise_single_step(gdp) result(retval)
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    integer        , pointer :: itinit
    integer        , pointer :: itstrt
    integer        , pointer :: itstop
    integer        , pointer :: itfinish
    !
    ! body
    !
    itinit       => gdp%gdinttim%itinit
    itstrt       => gdp%gdinttim%itstrt
    itstop       => gdp%gdinttim%itstop
    itfinish     => gdp%gdinttim%itfinish
    retval = 0
    itinit = itstrt
    itstop = itstrt + 1
    if (itstop > itfinish) then
      retVal = -1
    endif
end function trisim_initialise_single_step
!
!
!
!==============================================================================
integer function trisim_prepare_next_step(gdp) result(retval)
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    integer           , pointer :: itinit
    integer           , pointer :: itstrt
    integer           , pointer :: itstop
    integer           , pointer :: itfinish
    !
    ! body
    !
    itinit       => gdp%gdinttim%itinit
    itstrt       => gdp%gdinttim%itstrt
    itstop       => gdp%gdinttim%itstop
    itfinish     => gdp%gdinttim%itfinish
    retval = 0
    itstrt = itstop
    itstop = itstrt + 1
end function trisim_prepare_next_step
!
!
!
!==============================================================================
integer function trisim_check_step(gdp) result(retval)
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp

    integer                             , pointer :: itstop
    integer                             , pointer :: itfinish
    !
    ! body
    !
    itstop       => gdp%gdinttim%itstop
    itfinish     => gdp%gdinttim%itfinish
    retval = 0
    if (itstop > itfinish) then
      retVal = -1
    endif
end function trisim_check_step
!
!
!
!---------------------------------------------------------------
end module mod_trisim
