module sync_flowwave
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
! Organizes the communication between the executable
! doing the water calculation and the executable
! doing the wave calculation.
! This module is the interface between DelftIO and
! Delft3D-FLOW.
! Method used: Handled by DelftIO
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision
use dio_plt_rw
use dio_2dfield_rw
!
implicit none
!
! Module parameters
!
integer, parameter :: max_num_subdoms = 100    ! max # subdomains
                                              ! (= #locations in DioPlt)
!
! Derived Type definitions (PLT datasets)
!
type comminfotype
   type (dioplttype) :: flow_to_wave  ! PLT (loc.-name used for case name)
   type (dio2dftype) :: wave_to_flow  ! 1 integer, result flag.
end type comminfotype
!
! Module variables
!
! See 'Interface' for public enum.s and functions.
!
private            
!
! Names, enumerations and sizes
!
character(Len=DioMaxParLen)   , parameter               :: param_1_string = 'Parameter_1'
character(Len=DioMaxParLen)   , parameter               :: param_2_string = 'Parameter_2'
character(Len=DioMaxParLen)   , parameter               :: param_3_string = 'Parameter_3'
character(Len=DioMaxParLen)   , parameter               :: param_4_string = 'Parameter_4'
!
character(Len=DioMaxStreamLen), parameter               :: flow_to_wave_plt_name = 'FLOW2WAVE_DATA'
character(Len=DioMaxStreamLen), parameter               :: wave_to_flow_2df_name = 'WAVE2FLOW_DATA'
!
character(Len=DioMaxStreamLen), parameter               :: mud_to_wave_plt_name  = 'MUD2WAVE_DATA'
character(Len=DioMaxStreamLen), parameter               :: wave_to_mud_2df_name  = 'WAVE2MUD_DATA'
!
character(Len=diomaxloclen), dimension(max_num_subdoms) :: subdomains     ! subdomain names
character(Len=diomaxloclen), dimension(max_num_subdoms) :: mudsubdomains  ! subdomain names of mud layer
!
integer, parameter    :: num_to_pars              = 4 ! Communicate 4 parameters at a time
!
integer, parameter    :: m_result_size            = 1 ! sizes of returned 2df dataset
integer, parameter    :: n_result_size            = 1
!
integer, parameter    :: flow_wave_comm_dont_wait = 0
integer, parameter    :: flow_wave_comm_do_wait   = 1
!
integer, private           , save :: numPassedDomains         = 0
integer, private           , save :: numPassedMudDomains      = 0
type (comminfotype), target, save :: comminfowater
type (comminfotype), target, save :: comminfomud
type (comminfotype), pointer      :: comminfo
logical                           :: waitForResult            = .true.
logical, private           , save :: initialized              = .false.
logical, private           , save :: mudinitialized           = .false.
!
! Interface
!
integer, public, parameter :: flow_wave_comm_result_ok    =  0
integer, public, parameter :: flow_wave_comm_error        = -1
!
integer, public, parameter :: flow_wave_comm_perform_step = 1
integer, public, parameter :: flow_wave_comm_finalize     = 2
!
public flow_to_wave_init
public wave_from_flow_init
!
public flow_to_wave_command
public wave_from_flow_command
!
public wave_to_flow_status
public flow_from_wave_status
!
!
contains
!
!
!==============================================================================
function flow_to_wave_init(case_name, flow_IT01, flow_timestep, &
                         & numdomains, mud, parll, wait) result(ierror)
    implicit none
    !
    ! Result value
    !
    integer :: ierror ! 0: succesfull initialized communication with WAVE
    !
    ! Arguments
    !
    character(*)      , intent(in) :: case_name     ! flow runid
    integer           , intent(in) :: flow_IT01     ! parameter IT01 in flow
                                                    ! (reference date)
    integer           , intent(in) :: numdomains    ! number of domains
    real(fp)          , intent(in) :: flow_timestep ! flow timestep in seconds
    logical           , intent(in) :: mud           ! water (false) or mud layer (true)
    logical           , intent(in) :: parll         ! flag for parallel flow run
    logical, optional , intent(in) :: wait          ! Wait until WAVE is finished?
                                                    ! (default true)
    !
    ! Local variables (for DIO communication)
    !
    character(Len=diomaxparlen), dimension(num_to_pars)                  :: par_names   ! 'Parameter'
    real(hp)                   , dimension(num_to_pars, max_num_subdoms) :: out_values  ! outgoing param. values
    integer                                                              :: num_dom     ! local version of numPassedDomains/numPassedMudDomains
    integer                                                              :: i
    !
    ! Executable statements ---------------------------------------------------
    !
    ierror = 0
    !
    ! Perferm action only for the last domain
    !
    if (mud) then
       comminfo => comminfomud
       numPassedMudDomains = numPassedMudDomains + 1
       num_dom = numPassedMudDomains
    else
       comminfo => comminfowater
       numPassedDomains = numPassedDomains + 1
       num_dom = numPassedDomains
    endif
    
    if (num_dom > max_num_subdoms) then
       write(*,'(a)') '*** ERROR Dimension max_num_subdoms too small in module sync_flowwave'
       ierror = 1
       return
    endif
    
    if (mud) then
       mudsubdomains(num_dom) = case_name
    else
        if (parll) then
            do i = 1,numdomains
                write(subdomains(i),'(A,A,I3.3)') trim(case_name),'-',i
            enddo
        else
            subdomains(num_dom) = case_name
        endif
    endif

    if (parll .or. num_dom == numdomains) then

        if (mud) then
           numPassedMudDomains = 0
        else
           numPassedDomains = 0
        endif

        if ( present(wait) ) waitForResult = wait
        ! Define outgoing data set (commands from FLOW to WAVE)
        !
        par_names(1) = param_1_string
        par_names(2) = param_2_string
        par_names(3) = param_3_string
        par_names(4) = param_4_string

        if ( numdomains < max_num_subdoms ) then
            if (mud) then
                mudsubdomains(numdomains+1:) = ''
            else
                subdomains(numdomains+1:) = ''
            endif
        endif
        if (mud) then
            comminfo%flow_to_wave = diopltdefine(mud_to_wave_plt_name, dio_plt_double, &
                                                 par_names           , mudsubdomains)
        else
            comminfo%flow_to_wave = diopltdefine(flow_to_wave_plt_name, dio_plt_double, &
                                                 par_names            , subdomains)
        endif

        !
        ! Send initial values (it01 and timestep, wait flag)
        !

        out_values      = 0.0_hp
        out_values(1,1) = real(flow_IT01,hp)
        out_values(2,1) = real(flow_timestep,hp)
        if ( waitForResult ) then
            out_values(3,1) = real(flow_wave_comm_do_wait,hp)
        else
            out_values(3,1) = real(flow_wave_comm_dont_wait,hp)
        endif
        out_values(4,1) = real(numdomains,hp)

        call diopltput( comminfo%flow_to_wave , out_values )

        if ( waitForResult ) then
            !
            ! Prepare comm. channel for result flags
            !
            if (mud) then
                comminfo%wave_to_flow = dio2dfgetdataset(wave_to_mud_2df_name)
            else
                comminfo%wave_to_flow = dio2dfgetdataset(wave_to_flow_2df_name)
            endif
            !
            ! Get result flag for wave initialization
            !
            if (flow_from_wave_status(mud) == flow_wave_comm_result_ok) then
               ierror = 0
            else
               ierror = 1
            endif
        endif
        if (ierror==0) then
            if (mud) then
                mudinitialized = .true.
            else
                initialized = .true.
            endif
        endif
    endif
end function flow_to_wave_init
!
!
!==============================================================================
function wave_from_flow_init(subdom_names, flow_IT01, flow_timestep, mud) result(num_subdomains)
    implicit none
    !
    ! Result value
    !
    integer :: num_subdomains ! <0 : error, >= 1: #subdomains
    !
    ! Arguments
    !
    character(*), dimension(:), intent(out) :: subdom_names  ! flow subdom. names
    integer                   , intent(out) :: flow_IT01     ! parameter IT01 in flow
                                                             ! (reference date)
    real(hp)                  , intent(out) :: flow_timestep ! flow timestep in seconds
    logical                   , intent(in)  :: mud           ! water (false) or mud layer (true)

    ! Local variables (for DIO communication)
    !
    character(diomaxparlen), dimension(:)  , pointer                 :: par_names         ! must be 'Parameter'
    character(diomaxloclen), dimension(:)  , pointer                 :: dio_subdom_names  ! <case name>
    real(hp)               , dimension(:,:), pointer                 :: in_values         ! incoming param. values
    !
    ! Executable statements ---------------------------------------------------
    !
    num_subdomains = -1
    !
    ! Receive incoming data set (commands from FLOW to WAVE)
    !
    if (mud) then
        comminfo => comminfomud
        comminfo%flow_to_wave = diopltgetdataset(mud_to_wave_plt_name)
    else
        comminfo => comminfowater
        comminfo%flow_to_wave = diopltgetdataset(flow_to_wave_plt_name)
    endif
    par_names        => diopltgetpars(comminfo%flow_to_wave)
    dio_subdom_names => diopltgetlocs(comminfo%flow_to_wave)
    if ( associated(par_names) .and. associated(dio_subdom_names) ) then
        if ( par_names(2) == param_2_string ) then
            subdom_names = dio_subdom_names
            !
            ! Recieve initial values (ito1 an timestep)
            !
            if (diopltget( comminfo%flow_to_wave, in_values ) ) then
                flow_IT01     = nint(in_values(1,1))
                flow_timestep = in_values(2,1)
                if ( nint(in_values(3,1)) == flow_wave_comm_do_wait ) then
                    waitForResult = .true.
                else
                    waitForResult = .false.
                endif
                num_subdomains = nint(in_values(4,1))
            endif
        endif
    endif
    !
    if ( num_subdomains >= 1 .and. waitForResult ) then
        !
        ! Prepare comm. channel for result flags
        !
        if (mud) then
            comminfo%wave_to_flow = dio2dfdefine(wave_to_mud_2df_name, dio_plt_integer, &
                                                 m_result_size       , n_result_size   )
        else
            comminfo%wave_to_flow = dio2dfdefine(wave_to_flow_2df_name, dio_plt_integer, &
                                                 m_result_size        , n_result_size   )
        endif
    endif
end function wave_from_flow_init
!
!
!==============================================================================
function flow_to_wave_command(command, numdomains, mud, num_steps) result(ierror)
    implicit none
    ! Result value
    !
    integer :: ierror ! 0: succesfull initialized communication with WAVE
    !
    ! Arguments
    !
    integer, intent(in)           :: command    ! perform_step or finalize
    integer, intent(in)           :: numdomains ! number of domains
    logical, intent(in)           :: mud        ! water (false) or mud layer (true)
    integer, intent(in), optional :: num_steps  ! #wave steps (itb)
    !
    ! Local variables (for DIO communication)
    !
    real(hp), dimension(num_to_pars, max_num_subdoms)          :: out_values ! outgoing param. values
    integer                                                    :: num_dom    ! local version of numPassedDomains/numPassedMudDomains
    logical                                                    :: init       ! local version of initialized/mudinitialized
    !
    ! Local variables (for coupling with DD)
    !
    ! Executable statements ---------------------------------------------------
    !
    ierror = 0
    !
    ! only for the last domain activate WAVE module
    !
    if (mud) then
        comminfo => comminfomud
        numPassedMudDomains = numPassedMudDomains + 1
        num_dom = numPassedMudDomains
        init    = mudinitialized
    else
        comminfo => comminfowater
        numPassedDomains = numPassedDomains + 1
        num_dom = numPassedDomains
        init    = initialized
    endif
    !
    if (num_dom == numdomains .and. init) then
       if (mud) then
           numPassedMudDomains = 0
       else
           numPassedDomains = 0
       endif
       !
       ! Send timestep or finalize command
       !
       out_values      = 0.0_hp
       out_values(1,1) = real(command,hp)
       if ( present(num_steps) ) out_values(2,1) = real(num_steps,hp)
       !
       call diopltput( comminfo%flow_to_wave , out_values )
       !
       ierror = 0
       !
       if ( waitForResult ) then
           !
           ! Get result flag for wave initialization
           !
           if (flow_from_wave_status(mud) == flow_wave_comm_result_ok) then
              ierror = 0
           else
              ierror = 1
           endif
       endif
    endif
end function flow_to_wave_command
!
!
!==============================================================================
function wave_from_flow_command(command, mud, num_steps) result(success)
    implicit none
    !
    ! Result value
    !
    logical :: success ! .true.: succesfull initialized communication with WAVE
    !
    ! Arguments
    !
    integer, intent(out)           :: command   ! perform_step or finalize
    logical, intent(in)            :: mud       ! water (false) or mud layer (true)
    integer, intent(out), optional :: num_steps ! #wave steps (itb)
    !
    ! Local variables (for DIO communication)
    !
    real(hp), dimension(:,:), pointer                 :: in_values   ! incoming param. values
    !
    ! Executable statements ---------------------------------------------------
    !
    success = .false.
    if (mud) then
        comminfo => comminfomud
    else
        comminfo => comminfowater
    endif
    !
    if ( present(num_steps) ) num_steps = 0
    !
    ! Receive timestep or finalize command
    !
    if (diopltget( comminfo%flow_to_wave, in_values ) ) then
        command = nint(in_values(1,1))
        if ( present(num_steps) ) num_steps = nint(in_values(2,1))
        success = .true.
    endif
end function wave_from_flow_command
!
!
!==============================================================================
subroutine wave_to_flow_status(status, mud)
    implicit none
    !
    ! Arguments
    !
    integer, intent(in) :: status    ! wave OK or not
    logical, intent(in) :: mud       ! water (false) or mud layer (true)
    !
    ! Local variables (for DIO communication)
    !
    integer, dimension(m_result_size, n_result_size)   :: out_results ! outgoing result flag
    !
    ! Executable statements ---------------------------------------------------
    !
    if (mud) then
        comminfo => comminfomud
    else
        comminfo => comminfowater
    endif
    !
    ! Send result
    !
    out_results(1,1) = status
    call dio2dfput(comminfo%wave_to_flow, out_results)
end subroutine wave_to_flow_status
!
!
!==============================================================================
function flow_from_wave_status(mud) result(status)
    implicit none
    !
    ! Result value
    !
    integer :: status ! OK / ERROR
    !
    ! Arguments
    !
    logical, intent(in) :: mud       ! water (false) or mud layer (true)
    !
    ! Local variables (for DIO communication)
    !
    integer, dimension(:,:), pointer :: in_results ! incoming result flag
    !
    ! Executable statements ---------------------------------------------------
    !
    status = flow_wave_comm_error
    if (mud) then
        comminfo => comminfomud
    else
        comminfo => comminfowater
    endif
    !
    ! Receive result set (indicating result of given command)
    !
    if ( dio2dfget ( comminfo%wave_to_flow, in_results ) ) then
        if ( in_results(1,1) == flow_wave_comm_result_ok ) then
            status = flow_wave_comm_result_ok
        endif
    endif
end function flow_from_wave_status


end module sync_flowwave

