subroutine rtc_comm_init(error     ,nambar    ,namcon    ,gdp       )
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
!  $Id: rtc_comm_init.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/timedep/rtc_comm_init.f90 $
!!--description-----------------------------------------------------------------
!
! This routine initializes the communication with the RTC module
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
    real(fp)                      , pointer :: hdt
    integer                       , pointer :: ifirstrtc
    integer                       , pointer :: itfinish
    integer                       , pointer :: itstrt
    integer                       , pointer :: julday
    integer                       , pointer :: kmax
    integer                       , pointer :: lstsci
    integer                       , pointer :: lundia
    integer                       , pointer :: mlb
    integer      , dimension(:,:) , pointer :: mnrtcsta
    integer                       , pointer :: mub
    character(20), dimension(:)   , pointer :: namrtcsta
    integer                       , pointer :: nlb
    integer                       , pointer :: nsluv
    integer                       , pointer :: nub
    integer                       , pointer :: numdomains
    integer                       , pointer :: parget_offset
    integer                       , pointer :: parput_offset
    integer                       , pointer :: rtc_domainnr
    integer                       , pointer :: rtc_ndomains
    logical                       , pointer :: rtcact
    logical                       , pointer :: anyRTCtoFLOW
    logical                       , pointer :: anyFLOWtoRTC
    integer                       , pointer :: rtcmod
    real(fp)     , dimension(:)   , pointer :: s1rtcsta
    integer                       , pointer :: stacnt
    real(fp)                      , pointer :: timsec
    integer                       , pointer :: tnparget
    integer                       , pointer :: tnlocput
    real(fp)     , dimension(:,:) , pointer :: tparput
    character(80), dimension(:)   , pointer :: tlocget_names
    character(80), dimension(:)   , pointer :: tlocput_names
    character(80), dimension(:)   , pointer :: tparput_names
    real(fp)     , dimension(:,:) , pointer :: zrtcsta
!
! Global variables
!
    logical                                                                                                   :: error  ! Flag=TRUE if an error is encountered 
    character(20) , dimension(gdp%d%nsluv)                                                                    :: nambar ! names of all parameters to get from RTC
    character(20) , dimension(gdp%d%lstsci)                                                                   :: namcon ! Description and declaration in ckdim.f90

!
! Local variables
!
    integer                           :: i
    integer                           :: iacdat      ! Actual simulation day for RTC 
    integer                           :: iactim      ! Actual simulation time for RTC 
    integer                           :: iloc
    integer                           :: istat
    integer                           :: k
    integer                           :: l
    integer                           :: n
    real(fp), dimension(:,:), pointer :: nparams
    logical                           :: success      ! Flag = false when an error is encountered
!
!! executable statements -------------------------------------------------------
!
    hdt            => gdp%gdnumeco%hdt
    ifirstrtc      => gdp%gdrtc%ifirstrtc
    itfinish       => gdp%gdinttim%itfinish
    itstrt         => gdp%gdinttim%itstrt
    julday         => gdp%gdinttim%julday
    kmax           => gdp%d%kmax
    lstsci         => gdp%d%lstsci
    lundia         => gdp%gdinout%lundia
    mlb            => gdp%d%mlb
    mnrtcsta       => gdp%gdrtc%mnrtcsta
    mub            => gdp%d%mub
    namrtcsta      => gdp%gdrtc%namrtcsta
    nlb            => gdp%d%nlb
    nsluv          => gdp%d%nsluv
    nub            => gdp%d%nub
    numdomains     => gdp%gdprognm%numdomains
    parget_offset  => gdp%gdrtc%parget_offset
    parput_offset  => gdp%gdrtc%parput_offset
    rtc_domainnr   => gdp%gdrtc%rtc_domainnr
    rtc_ndomains   => gdp%gdrtc%rtc_ndomains
    rtcact         => gdp%gdrtc%rtcact
    anyRTCtoFLOW   => gdp%gdrtc%anyRTCtoFLOW
    anyFLOWtoRTC   => gdp%gdrtc%anyFLOWtoRTC
    rtcmod         => gdp%gdrtc%rtcmod
    s1rtcsta       => gdp%gdrtc%s1rtcsta
    stacnt         => gdp%gdrtc%stacnt
    timsec         => gdp%gdinttim%timsec
    tnparget       => gdp%gdrtc%tnparget
    tnlocput       => gdp%gdrtc%tnlocput
    tparput        => gdp%gdrtc%tparput
    tlocget_names  => gdp%gdrtc%tlocget_names
    tlocput_names  => gdp%gdrtc%tlocput_names
    tparput_names  => gdp%gdrtc%tparput_names
    zrtcsta        => gdp%gdrtc%zrtcsta
    !
    if (rtcmod /= noRTC) then
      !
      call timer_start(timer_wait, gdp)
      if (numdomains > 1) then
         call rtcstartcommunication(rtc_domainnr, rtc_ndomains)
         rtc_domainnr = rtc_domainnr+1
      else
         rtc_ndomains = 1
         rtc_domainnr = 1
      endif
      !
      ! Determine total number of parameters to be received
      ! and collect all parameter names
      !
      if (rtc_ndomains > 1) then
         allocate(nparams(2,rtc_ndomains),stat=istat)
         if (istat /= 0) goto 999
         nparams = 0.0_fp
         !
         nparams(1,rtc_domainnr) = real(nsluv,fp) ! # parameters get
         nparams(2,rtc_domainnr) = real(stacnt*kmax,fp) ! # parameters put
         !
         call rtccommunicate(nparams, 2*rtc_ndomains)
         !
         tnparget = 0
         tnlocput = 0
         parget_offset = 0
         parput_offset = 0
         do i = 1, rtc_ndomains
            n = nint(nparams(1,i))
            tnparget = tnparget + n
            if (i<rtc_domainnr) parget_offset = parget_offset + n
            !
            n = nint(nparams(2,i))
            tnlocput = tnlocput + n
            if (i<rtc_domainnr) parput_offset = parput_offset + n
         enddo
         deallocate(nparams,stat=istat)
         if (istat /= 0) goto 999
      else
         tnparget = nsluv
         parget_offset = 0
         tnlocput = stacnt * kmax
         parput_offset = 0
      endif
      anyRTCtoFLOW = tnparget > 0
      anyFLOWtoRTC = tnlocput > 0
      !
      allocate(gdp%gdrtc%tparget(2,tnparget),stat=istat)
      if (istat /= 0) goto 999
      allocate(gdp%gdrtc%tlocget_names(tnparget),stat=istat)
      if (istat /= 0) goto 999
      tlocget_names => gdp%gdrtc%tlocget_names
      tlocget_names = ' '
      do i = 1,nsluv
         tlocget_names(parget_offset+i) = nambar(i)
      enddo
      !
      if (rtc_ndomains > 1) then
         call rtccharcommunicate(tlocget_names, tnparget)
      endif
      !
      if (anyFLOWtoRTC) then
         !
         ! Collect parameters for this domain
         !
         allocate(gdp%gdrtc%tparput(2+lstsci,tnlocput),stat=istat)
         if (istat /= 0) goto 999
         allocate(gdp%gdrtc%tlocput_names(tnlocput),stat=istat)
         if (istat /= 0) goto 999
         allocate(gdp%gdrtc%tparput_names(2+lstsci),stat=istat)
         if (istat /= 0) goto 999
         tparput => gdp%gdrtc%tparput
         tlocput_names => gdp%gdrtc%tlocput_names
         tparput_names => gdp%gdrtc%tparput_names
         !
         tparput = 0.0_fp
         !
         tparput_names(1) = 'Elevation'
         tparput_names(2) = 'Water level'
         do l = 1,lstsci
            tparput_names(2+l) = namcon(l)
         enddo
         !
         iloc = parput_offset
         tlocput_names = ' '
         do i = 1,stacnt
            do k = 1,kmax
               iloc = iloc + 1
               write(tlocput_names(iloc),'(2a,i0)') trim(namrtcsta(i)), '_', k
            enddo
         enddo
         !
         ! Collect parameters from all domains
         !
         if (rtc_ndomains>1) then
            call rtccharcommunicate(tlocput_names, tnlocput)
         endif
      endif
      !
      ! Communication with RTC occurs only by the master domain
      !
      if (rtc_domainnr == 1) then
         call timdat(julday, 0.0_fp, iacdat, iactim)
         !
         call syncflowrtc_init(error, tlocget_names, tnparget, 80, &
                             & itfinish - itstrt, anyFLOWtoRTC, &
                             & anyRTCtoFLOW, iacdat, hdt)
      endif
      call timer_stop(timer_wait, gdp)
      if (error) then
         rtcact = .false.
         call prterr(lundia    ,'J020'    ,'SyncFlowRtc_Init'   )
      else
         rtcact = .true.
      endif
    else
      anyRTCtoFLOW = .false.
      anyFLOWtoRTC = .false.
      if (numdomains > 1) then
         !
         ! Notify the rtc iterator that this subdomain
         ! is not interested in rtc communication
         ! If numdomains=1, there is no rtc iterator
         !
         call rtcnocommunication()
      endif
    endif
    return
    !
999 continue
    call prterr(lundia    ,'P004'    ,'Memory allocation error in RTC_COMM_INIT'   )
end subroutine rtc_comm_init
