subroutine rtc_comm_get(cursec    ,cbuvrt    ,nsluv     ,qsrcrt    ,nsrc     ,gdp       )
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
! This routine receives data from the RTC module
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use flow2d3d_timers
    use SyncRtcFlow
    use globaldata
    use dfparall, only: parll, dfloat, dfsum
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                       , pointer :: ifirstrtc
    integer                       , pointer :: kmax
    integer                       , pointer :: lundia
    integer      , dimension(:,:) , pointer :: mnrtcsta
    integer                       , pointer :: parget_offset
    integer                       , pointer :: rtc_domainnr
    integer                       , pointer :: rtc_ndomains
    integer                       , pointer :: rtcact
    logical                       , pointer :: anyRTCtoFLOW
    integer                       , pointer :: rtcmod
    integer                       , pointer :: stacnt
    integer                       , pointer :: tnparget
    real(fp)     , dimension(:,:) , pointer :: tparget
!
! Global variables
!
    integer                      , intent(in) :: nsluv  ! number of barriers
    integer                      , intent(in) :: nsrc   ! number of discharge points
    real(fp)                     , intent(in) :: cursec ! Current simulation time in seconds 
    real(fp), dimension(2, nsluv)             :: cbuvrt ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(2, nsrc)              :: qsrcrt ! Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                :: id
    integer                                :: rtcsta      ! Status from RTC: If < 0, RTC quits And Flow also must quit. 
    logical                                :: error
    character(80)                          :: msgstr
!
!! executable statements -------------------------------------------------------
!
    ifirstrtc      => gdp%gdrtc%ifirstrtc
    kmax           => gdp%d%kmax
    lundia         => gdp%gdinout%lundia
    mnrtcsta       => gdp%gdrtc%mnrtcsta
    parget_offset  => gdp%gdrtc%parget_offset
    rtc_domainnr   => gdp%gdrtc%rtc_domainnr
    rtc_ndomains   => gdp%gdrtc%rtc_ndomains
    rtcact         => gdp%gdrtc%rtcact
    anyRTCtoFLOW   => gdp%gdrtc%anyRTCtoFLOW
    rtcmod         => gdp%gdrtc%rtcmod
    stacnt         => gdp%gdrtc%stacnt
    tnparget       => gdp%gdrtc%tnparget
    tparget        => gdp%gdrtc%tparget
    !
    ! RTC  -> FLOW : get steering parameters for current date and time
    !
    if (rtcact == RTCviaBMI) then
       !
       ! set appropriate flag when values are set via BMI
       !
       do id = 1, nsluv
          if (comparereal(cbuvrt(2,id), -998.0_fp) == 1) then
             cbuvrt(1,id) = 1.0_fp
          endif
       enddo
       do id = 1, nsrc
          if (comparereal(qsrcrt(2,id), -998.0_fp) == 1) then
             qsrcrt(1,id) = 1.0_fp
          endif
       enddo
       !
       ! For BMI exchange, we are finished here
       !
       return
    endif
    !
    call timer_start(timer_wait, gdp)
    !
    ! get barrier levels
    !
    rtcsta = 0
    if (anyRTCtoFLOW) then
       !
       ! communication with RTC occurs only by the master domain
       !
       if (rtc_domainnr == 1) then
          call syncflowrtc_get(rtcsta, tparget, tnparget)
       else
          tparget = 0.0_fp
       endif
       !
       ! distribute data to all domains
       !
       if (parll) then
           call dfreduce (tparget, 2*tnparget, dfloat, dfsum, error, msgstr )
       elseif (rtc_ndomains > 1) then
           call dd_rtccommunicate(tparget, 2*tnparget)
       endif
       !
       ! copy data to local array
       !
       do id = 1, nsluv
          cbuvrt(:,id) = tparget(:,parget_offset+id)
       enddo
       do id = 1, nsrc
          qsrcrt(:,id) = tparget(:,parget_offset+nsluv+id)
       enddo
    endif
    call timer_stop(timer_wait, gdp)
    !
    if (rtcsta < 0) then
       rtcact = noRTC
       write (*, '(a)') ' '
       write (*, '(a)') ' Stop signal from RTC '
       call prterr(lundia, 'P004', 'Stop signal from RTC ')
       call d3stop(1, gdp)
    endif
end subroutine rtc_comm_get
