subroutine rtc_comm_get(cursec    ,cbuvrt    ,nsluv     ,gdp       )
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
!  $Id: rtc_comm_get.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/timedep/rtc_comm_get.f90 $
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
    logical                       , pointer :: rtcact
    logical                       , pointer :: anyRTCtoFLOW
    integer                       , pointer :: rtcmod
    integer                       , pointer :: stacnt
    integer                       , pointer :: tnparget
    real(fp)     , dimension(:,:) , pointer :: tparget
!
! Global variables
!
    integer                      , intent(in) :: nsluv  ! number of barriers
    real(fp)                     , intent(in) :: cursec ! Current simulation time in seconds 
    real(fp), dimension(2, nsluv)             :: cbuvrt ! Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                :: id
    integer                                :: rtcsta      ! Status from RTC: If < 0, RTC quits And Flow also must quit. 
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
       if (rtc_ndomains > 1) then
          call rtccommunicate(tparget, 2*tnparget)
       endif
       !
       ! copy data to local array
       !
       do id = 1, nsluv
          cbuvrt(:,id) = tparget(:,parget_offset+id)
       enddo
    endif
    call timer_stop(timer_wait, gdp)
    !
    if (rtcsta < 0) then
       rtcact = .false.
       write (*, '(a)') ' '
       write (*, '(a)') ' Stop signal from RTC '
       call prterr(lundia, 'P004', 'Stop signal from RTC ')
       call d3stop(1, gdp)
    endif
end subroutine rtc_comm_get
