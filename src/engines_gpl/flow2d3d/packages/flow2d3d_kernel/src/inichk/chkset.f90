subroutine chkset(lundia    ,error     ,sferic    ,method    ,trasol    , &
                & dischy    ,solver    ,disctr    ,ktemp     , &
                & keva      ,iphisi    ,gdp       )
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
! Checks the combination of Domain Decomposition with various program modes
! Checks the combination of Z-model with various program modes
! The combination Domain Decomposition and Z-model is allowed
! Dredge is only allowed in combination with 3D morphology
! Checks the combination of 2-D and non-hydrostatic
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
    integer                       , pointer :: kmax
    integer                       , pointer :: lsec
    integer                       , pointer :: lstsci
    integer                       , pointer :: ltur
    integer                       , pointer :: ltur2d
    integer                       , pointer :: nto
    integer                       , pointer :: ntoq
    integer                       , pointer :: ndro
    logical                       , pointer :: multi
    integer                       , pointer :: nh_level
    character(8)                  , pointer :: dpsopt
    character(8)                  , pointer :: dpuopt

    character(6)                  , pointer :: momsol
    logical                       , pointer :: drogue
    logical                       , pointer :: temp
    logical                       , pointer :: dredge
    logical                       , pointer :: wave
    integer                       , pointer :: waveol
    logical                       , pointer :: xbeach
    logical                       , pointer :: iweflg
    logical                       , pointer :: struct
    logical                       , pointer :: sedim
    logical                       , pointer :: htur2d
    logical                       , pointer :: mudlay
    logical                       , pointer :: nonhyd
    logical                       , pointer :: couplemod
    logical                       , pointer :: zmodel
    logical                       , pointer :: roller
    logical                       , pointer :: bubble
    integer                       , pointer :: numdomains
    integer                       , pointer :: nummappers
    character(16)                 , pointer :: rst_layer_model
    integer                       , pointer :: rtcmod
    integer                       , pointer :: nrcmp
    logical                       , pointer :: flcut
    logical                       , pointer :: fl45
    logical                       , pointer :: waqol
    integer                       , pointer :: itcomi
    logical                       , pointer :: ztbml
    logical                       , pointer :: ztbml_upd_r1
!
! Global variables
!
    integer     , intent(in)  :: iphisi      !  Description and declaration in inttim.igs
    integer     , intent(in)  :: keva        !  Description and declaration in tricom.igs
    integer     , intent(in)  :: ktemp       !  Description and declaration in tricom.igs
    integer                   :: lundia      !  Description and declaration in inout.igs
    logical     , intent(out) :: error       !!  Flag=TRUE if an error is encountered
    logical     , intent(in)  :: sferic      !  Description and declaration in tricom.igs
    character(*), intent(in)  :: dischy      !  Description and declaration in tricom.igs
    character(*), intent(in)  :: disctr      !  Description and declaration in tricom.igs
    character(*)              :: method      !  Description and declaration in tricom.igs
    character(*), intent(in)  :: solver      !  Description and declaration in tricom.igs
    character(*)              :: trasol      !  Description and declaration in tricom.igs
!
! Local variables
!
    integer        :: ierror                 ! Error counter
    integer        :: iwarn                  ! Warning counter
    character(100) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    kmax                => gdp%d%kmax
    lsec                => gdp%d%lsec
    lstsci              => gdp%d%lstsci
    ltur                => gdp%d%ltur
    ltur2d              => gdp%d%ltur2d
    nto                 => gdp%d%nto
    ntoq                => gdp%d%ntoq
    ndro                => gdp%d%ndro
    multi               => gdp%gdmorpar%multi
    nh_level            => gdp%gdnonhyd%nh_level
    dpsopt              => gdp%gdnumeco%dpsopt
    dpuopt              => gdp%gdnumeco%dpuopt
    momsol              => gdp%gdnumeco%momsol
    drogue              => gdp%gdprocs%drogue
    temp                => gdp%gdprocs%temp
    dredge              => gdp%gdprocs%dredge
    wave                => gdp%gdprocs%wave
    waveol              => gdp%gdprocs%waveol
    xbeach              => gdp%gdprocs%xbeach
    iweflg              => gdp%gdprocs%iweflg
    struct              => gdp%gdprocs%struct
    sedim               => gdp%gdprocs%sedim
    htur2d              => gdp%gdprocs%htur2d
    mudlay              => gdp%gdprocs%mudlay
    nonhyd              => gdp%gdprocs%nonhyd
    couplemod           => gdp%gdprocs%couplemod
    zmodel              => gdp%gdprocs%zmodel
    roller              => gdp%gdprocs%roller
    bubble              => gdp%gdprocs%bubble
    numdomains          => gdp%gdprognm%numdomains
    nummappers          => gdp%gdprognm%nummappers
    rst_layer_model     => gdp%gdrestart%rst_layer_model
    rtcmod              => gdp%gdrtc%rtcmod
    nrcmp               => gdp%gdtfzeta%nrcmp
    flcut               => gdp%gdtmpfil%flcut
    fl45                => gdp%gdtmpfil%fl45
    waqol               => gdp%gdwaqpar%waqol
    itcomi              => gdp%gdinttim%itcomi
    ztbml               => gdp%gdzmodel%ztbml
    ztbml_upd_r1        => gdp%gdzmodel%ztbml_upd_r1
    !
    ierror = 0
    iwarn  = 0
    !
    ! For checking functionality combinations:
    ! Must DD be specified as nummappers>=1 or numdomains>1?
    ! The behaviour around a DD boundary is the same when comparing one domain
    ! with an internal DD boundary with two domains. So use nummappers>=1.
    !
    if (nummappers >= 1) then
       !
       ! errors
       !
       if (iweflg) then
          call prterr(lundia    ,'M001'    ,'Internal Wave option'          )
          ierror = ierror + 1
       endif
       if (mudlay) then
          call prterr(lundia    ,'M001'    ,'Fluid Mud'          )
          ierror = ierror + 1
       endif
       if (solver(1:2)=='gs') then
          call prterr(lundia    ,'M001'    ,'Gauss Seidel solver')
          ierror = ierror + 1
       endif
       if (nonhyd .and. nh_level == nh_full) then
          call prterr(lundia    ,'P004'    ,'The combination of fully non-hydrostatic mode and domain decomposition is not allowed')
          ierror = ierror + 1
       endif
       !
       ! warnings
       !
       if (roller) then
          call prterr(lundia    ,'M002'    ,'Roller model'       )
          iwarn = iwarn + 1
       endif
       if (ltur2d>0) then
          call prterr(lundia    ,'M002'    ,'Q2E 2D Turb.model'  )
          iwarn = iwarn + 1
       endif
       if (nrcmp>0) then
          call prterr(lundia    ,'M002'    ,'Tide generating forces'        )
          iwarn = iwarn + 1
       endif
       if (lsec>0) then
          call prterr(lundia    ,'M002'    ,'Secondary Flow'     )
          iwarn = iwarn + 1
       endif
       if (ndro>0) then
          call prterr(lundia    ,'M002'    ,'Drogues' )
          iwarn = iwarn + 1
       endif
       if (disctr(1:4)=='expl') then
          call prterr(lundia    ,'M002'    ,'Explicit method'    )
          iwarn = iwarn + 1
       endif
       if (sferic) then
          call prterr(lundia    ,'M002'    ,'Spherical coordinate'          )
          iwarn = iwarn + 1
       endif
       if (momsol /= 'cyclic') then
          call prterr(lundia    ,'M002'    ,'Momentum solver other than cyclic'  )
          iwarn = iwarn + 1
       endif
       if (couplemod) then
          call prterr(lundia    ,'M002'    ,'Online Couple'      )
          iwarn = iwarn + 1
       endif
    endif
    !
    ! Z-MODEL case
    !
    if (zmodel) then
       !
       ! errors
       !
       if (couplemod) then
          call prterr(lundia    ,'Z011'    ,'Online Couple'      )
          ierror = ierror+ 1
       endif
       if (iweflg) then
          call prterr(lundia    ,'Z011'    ,'Internal Wave option'          )
          ierror = ierror+ 1
       endif
       if (mudlay) then
          call prterr(lundia    ,'Z011'    ,'Fluid Mud'          )
          ierror = ierror+ 1
       endif
       if (solver(1:2)=='gs') then
          call prterr(lundia    ,'Z011'    ,'Gauss Seidel solver')
          ierror = ierror+ 1
       endif
       if (ltur2d>0) then
          call prterr(lundia    ,'Z011'    ,'Q2E 2D Turb.model'  )
          ierror = ierror+ 1
       endif
       if (lsec>0) then
          call prterr(lundia    ,'Z011'    ,'Secondary Flow'     )
          errtxt = 'Use Sigma version of Delft3D-FLOW instead'
          call prterr(lundia    ,'U021'    ,errtxt    )
          ierror = ierror+ 1
       endif
       if (disctr(1:4)=='expl') then
          call prterr(lundia    ,'Z011'    ,'Explicit method'    )
          ierror = ierror+ 1
       endif
       if (roller) then
          call prterr(lundia    ,'Z011'    ,'Roller model'       )
          ierror = ierror+ 1
       endif
       if (sedim .and. (kmax==1)) then
          call prterr(lundia    ,'Z011'    ,'2D morphology'       )
          ierror = ierror+ 1
       endif
       if (ztbml .and. rst_layer_model == 'UNKNOWN') then
          call prterr(lundia    ,'U190'    ,'Restarting with modified layering (ZTBML=#Y#) from a restart/map')
          write (lundia, '(a)') '          file without layer model information. Restart may be non-conservative.'
          ierror = ierror + 1
       endif    
       if (.not. ztbml .and. rst_layer_model == 'Z-MODEL, ZTBML') then
          call prterr(lundia    ,'P004'    ,'Restarting from Z-model run with modified bed-layering only allowed')
          write (lundia, '(a)') '          with again modified layering: ZTBML=#Y#'
          ierror = ierror + 1
       endif
       !
       ! warnings
       !
       if (sedim) then
          call prterr(lundia    ,'Z012'    ,'3D Morphology'      )
          iwarn = iwarn+ 1
       endif
       if (method(1:3)/='adi') then
          call prterr(lundia    ,'Z011'    ,method    )
          method = 'adi          '
          errtxt = 'ADI numerical method will be applied instead'
          call prterr(lundia    ,'U190'    ,errtxt    )
          iwarn = iwarn + 1
       endif
       if (ktemp/=5 .and. ktemp>0) then
          call prterr(lundia    ,'Z012'    ,'Heat model other than option 5')
          iwarn = iwarn + 1
       endif
       if (keva>0) then
          call prterr(lundia    ,'Z012'    ,'Evaporation model'  )
          iwarn = iwarn + 1
       endif
       if (nrcmp>0) then
          call prterr(lundia    ,'Z012'    ,'Tide generating forces'        )
          iwarn = iwarn + 1
       endif
       if (ntoq>0) then
          call prterr(lundia    ,'Z012'    ,'Q-H boundary'       )
          iwarn = iwarn + 1
       endif
       if (ndro>0) then
          call prterr(lundia    ,'Z012'    ,'Drogues' )
          iwarn = iwarn + 1
       endif
       if (sferic) then
          call prterr(lundia    ,'Z012'    ,'Spherical coordinate'          )
          iwarn = iwarn + 1
       endif
       if (htur2d) then
          call prterr(lundia    ,'Z012'    ,'HLES'    )
          iwarn = iwarn + 1
       endif
       if (struct) then
          call prterr(lundia    ,'Z012'    ,'Structures'         )
          iwarn = iwarn + 1
       endif
       if (ztbml .and. rst_layer_model == 'Z-MODEL') then
          errtxt = 'Z-model with modified bed-layering (ZTMBL) activated. Restarting from'
          call prterr(lundia    ,'Z013'    , errtxt)
          write (lundia, '(a)') '            unmodified layering, with conservative remapping near bottom'
          iwarn        = iwarn + 1
          ztbml_upd_r1 = .true.
       endif
    else
       !
       ! SIGMA-MODEL case
       !
       !
       ! errors
       !
       if (fl45 .or. flcut) then
          errtxt = 'Combination of sigma-model and cut cells is not available'
          call prterr(lundia ,'U021' ,errtxt )
          ierror = ierror+ 1
       endif
       if (nonhyd) then
          errtxt = 'Combination of sigma-model and non-hydrostatic mode is not available'
          call prterr(lundia ,'U021' ,errtxt )
          ierror = ierror + 1
       endif
       if (roller) then
          if(.not. wave) then
             call prterr(lundia    ,'P004'    ,'Roller model is activiated.  Wave process must be activated')
             ierror = ierror + 1
          endif
       endif
       !
       ! warnings
       !
       if (bubble) then
          errtxt = 'Combination of sigma-model and bubble screens has not yet been tested'
          call prterr(lundia ,'Z013' ,errtxt )
          iwarn = iwarn + 1
       endif
    endif
    !
    !
    !
    if (dredge) then
       !
       ! errors
       !
       if (.not. sedim) then
          call prterr(lundia    ,'P004'    ,'Dredging is only allowed in combination with 3D Morphology'  )
          ierror = ierror+ 1
       endif
    endif
    !
    !
    !
    if (multi) then
       call prterr(lundia, 'U190', 'Mormerge: Mass balance only satisfied for all runs together')
       iwarn = iwarn + 1
    endif
    !
    !
    !
    if (dpsopt=='DP' .and. dpuopt=='MEAN') then
       call prterr(lundia, 'P004', 'The combination of dpsopt=DP and dpuopt=MEAN is invalid')
       ierror = ierror+ 1
    endif
    !
    !
    !
    if (waqol) then
       if (sedim) then
          call prterr(lundia, 'P004', 'Morphology/sediments can not be switched on in both FLOW and WAQ')
          ierror = ierror+ 1
       endif
    endif
    !
    !
    !
    if (iphisi > 0 .and. lstsci > 7) then
       !
       ! The output to ascii file can be written for a maximum of 7 constituents (prthis.f90)
       !
       call prterr(lundia, 'P004', 'Writing to a tri-prt (ascii) file is not supported when using more than 7 constituents')
       ierror = ierror+ 1
    endif
    !
    if (kmax == 1 .and. nonhyd) then
       !
       ! 2D (kmax == 1) and non-hydrostatic is not allowed
       !
       call prterr(lundia, 'P004', 'The combination 2D and non-hydrostatic is not allowed')
       ierror = ierror+ 1
    endif
    !
    if (wave .and. waveol>0 .and. (.not.xbeach) .and. itcomi==1) then
       !
       ! Calling wave every timestep is not supported (yet).
       ! If tried, FLOW does not read wave information at all.
       !
       call prterr(lundia, 'P004', 'Calling wave every timestep is not supported.')
       ierror = ierror+ 1
    endif
    !
    if (parll) then
       !
       ! errors
       !
       if (drogue) then
          errtxt = 'Drogues/walking monitor points are not available in parallel computations'
          call prterr(lundia ,'U021' ,errtxt )
          ierror = ierror+ 1
       endif
    endif
    !
    !
    !
    if (ierror > 0) then
       error = .true.
    endif
end subroutine chkset
