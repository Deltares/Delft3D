subroutine d3stop(iexit, gdp)
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
!    Function: Terminates execution wit error code.
!              Is driver for CSTOP and gives possibility to
!              handle e.g. communication.
!              Reason to create was the implementation of
!              coupling with RTC.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use SyncRtcFlow
    use sync_flowcouple
    use sync_flowwave
    use d3d_sobek
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
    integer                       , pointer :: lundia
    logical                       , pointer :: wave
    integer                       , pointer :: waveol
    logical                       , pointer :: mudlay
    logical                       , pointer :: mudwave
    logical                       , pointer :: coupleact
    logical                       , pointer :: couplemod
    logical                       , pointer :: sbkol
    integer                       , pointer :: numdomains
    integer                       , pointer :: rtcact
    integer                       , pointer :: rtc_domainnr
!
! Global variables
!
    integer :: iexit !!  Exit return value
!
! Local variables
!
    integer :: ierror ! Value is non-zero if error is encountered
    integer :: idate
    integer :: idumda ! Dummy Date 
    integer :: istate ! Status for RTC
    integer :: waittime ! millisec
!
!! executable statements -------------------------------------------------------
!
    lundia       => gdp%gdinout%lundia
    wave         => gdp%gdprocs%wave
    waveol       => gdp%gdprocs%waveol
    mudlay       => gdp%gdprocs%mudlay
    mudwave      => gdp%gdprocs%mudwave
    coupleact    => gdp%gdprocs%coupleact
    couplemod    => gdp%gdprocs%couplemod
    sbkol        => gdp%gdprocs%sbkol
    numdomains   => gdp%gdprognm%numdomains
    rtc_domainnr => gdp%gdrtc%rtc_domainnr
    rtcact       => gdp%gdrtc%rtcact
    !
    idumda =  0
    istate = -1
    idate  =  0
    !
    ! In case of a serious problem:
    ! A programmer may wish to stop the calculation immediately, without closing
    ! all communications, but with the generation of a core dump.
    ! The routine d_hydro_coredump causes a core dump if specified in the main input file.
    ! This call must be the first serious action in d3stop.
    !
    call d_hydro_coredump
    !
    ! Check if RTC-connection is active and if so
    ! send (negative) status to shut down RTC
    !
    if (rtcact==RTCmodule .and. rtc_domainnr==1) then
       call syncflowrtc_close
    endif
    !
    ! Check if Couple-connection is active and if so
    ! send (negative) status to shut down Couple
    !
    if (couplemod .and. coupleact) then
       write(*,*) '--------------'
       write(*,*) 'FLOW: SEND call waiting for COUPLE'
       call syncflowcouple_send(istate, gdp%gdcoup%flowtocouple, &
                              &         gdp%gdcoup%putvalue  )
       write(*,*) 'FLOW: SEND call finished'
       write(*,*) 'FLOW: CLOSE call waiting for COUPLE'
       call syncflowcouple_close(gdp%gdcoup%flowtocouple, gdp%gdcoup%coupletoflow)
       write(*,*) 'FLOW: CLOSE call finished'
       write(*,*) '--------------'
    endif
    !
    ! Check if Wave-connection is active and if so then only the master partition
    ! sends a (negative) status to shut down Wave.
    !
    if (waveol==2) then
       if (.not.parll .or. (parll .and. inode == master)) then
       ierror = flow_to_wave_command(flow_wave_comm_finalize, &
                                   & numdomains, mudlay, -1)
       endif
    endif
    !
    ! Check if Wave-Mud-connection is active and if so send (negative) status
    ! to shut down Wave.
    !
    if (mudwave) then
       ierror = flow_to_wave_command(flow_wave_comm_finalize, &
                                   & numdomains, mudlay, -1)
    endif
    !
    ! Close Communication with Sobek if Delft3D-FLOW has encountered an error
    !
    if (sbkol) then
       write(*,*) 'D3D-FLOW encountered an error: Sending finalize signal to  Sobek ...'
       call D3S_Finalize('D3D-FLOW', 'Sobek')
       write(*,*) '... continue'
    endif
    !
    write(*,*) 'Flow exited abnormally'
    if (lundia /= 0) then
       call prterr(lundia, 'P004', 'Flow exited abnormally')
       write(*,*) 'Check diagnosis file'
       close (lundia)
    endif
    !
    ! Abort mpi, if needed
    ! This may also cause a direct termination, so wait a while to give other partitions the chance to finish properly.
    ! Do not call dfsync: this may block the FLOW termination, when an error occurs in only one partition.
    !
    waittime = 10000
    call CUTIL_SLEEP(waittime)
    if (parll) then
       call dfexitmpi(1)
    endif
    !
    ! Terminate now
    !
    ! call cstop(iexit, char(0))
    call throwexception()
end subroutine d3stop
