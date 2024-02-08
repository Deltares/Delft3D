subroutine rtc_comm_put(kfs       ,kfsmin    ,kfsmax    ,sig       , &
                      & zk        ,s1        ,dps       ,r0        , &
                      & nsluv     ,cbuv      ,nsrc      ,disch     , &
                      & gdp       )
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
! This routine sends data to the RTC module
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
    integer                         , pointer :: ifirstrtc
    integer                         , pointer :: kmax
    integer                         , pointer :: lstsci
    integer      , dimension(:,:)   , pointer :: mnrtcsta
    integer                         , pointer :: parput_offset
    integer                         , pointer :: rtc_domainnr
    integer                         , pointer :: rtc_ndomains
    integer                         , pointer :: rtcmod
    integer                         , pointer :: rtcact
    logical                         , pointer :: anyFLOWtoRTC
    integer                         , pointer :: stacnt
    real(fp)                        , pointer :: timsec
    integer                         , pointer :: tnlocput
    integer                         , pointer :: tnparput
    real(fp)     , dimension(:,:)   , pointer :: tparput
    character(80), dimension(:)     , pointer :: tlocput_names
    character(80), dimension(:)     , pointer :: tparput_names
    real(fp)     , dimension(:,:,:) , pointer :: r0rtcsta
    real(fp)     , dimension(:)     , pointer :: s1rtcsta
    real(fp)     , dimension(:,:)   , pointer :: zrtcsta
!
! Global variables
!
    integer                                                             , intent(in)  :: nsluv    !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: nsrc     !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                        , intent(in)  :: kfsmax   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                        , intent(in)  :: kfsmin   !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                        , intent(in)  :: kfs      !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                        , intent(in)  :: dps      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, gdp%d%kmax, gdp%d%lstsci), intent(in)  :: r0
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                        , intent(in)  :: s1       !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(  gdp%d%kmax)                                 , intent(in)  :: sig
    real(fp)  , dimension(0:gdp%d%kmax)                                 , intent(in)  :: zk
    real(fp)  , dimension(4, nsluv)                                     , intent(in)  :: cbuv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nsrc)                                         , intent(in)  :: disch    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                                :: i
    integer                                :: iloc
    integer                                :: k
    integer                                :: l
    logical                                :: success
!
!! executable statements -------------------------------------------------------
!
    ifirstrtc      => gdp%gdrtc%ifirstrtc
    kmax           => gdp%d%kmax
    lstsci         => gdp%d%lstsci
    mnrtcsta       => gdp%gdrtc%mnrtcsta
    parput_offset  => gdp%gdrtc%parput_offset
    rtc_domainnr   => gdp%gdrtc%rtc_domainnr
    rtc_ndomains   => gdp%gdrtc%rtc_ndomains
    rtcmod         => gdp%gdrtc%rtcmod
    rtcact         => gdp%gdrtc%rtcact
    anyFLOWtoRTC   => gdp%gdrtc%anyFLOWtoRTC
    stacnt         => gdp%gdrtc%stacnt
    timsec         => gdp%gdinttim%timsec
    tnlocput       => gdp%gdrtc%tnlocput
    tnparput       => gdp%gdrtc%tnparput
    tparput        => gdp%gdrtc%tparput
    tlocput_names  => gdp%gdrtc%tlocput_names
    tparput_names  => gdp%gdrtc%tparput_names
    r0rtcsta       => gdp%gdrtc%r0rtcsta
    s1rtcsta       => gdp%gdrtc%s1rtcsta
    zrtcsta        => gdp%gdrtc%zrtcsta
    !
    ! FLOW -> RTC  : send parameters (salinity levels)
    ! RTC  -> FLOW : no communication
    !
    if (anyFLOWtoRTC) then
       !
       ! fill the r0rtcsta, s1rtcsta, zrtcsta arrays
       !
       call zrtc(gdp%d%mlb, gdp%d%mub, gdp%d%nlb, gdp%d%nub, kfs, kfsmin, &
               & kfsmax, sig, zk, s1, dps, r0, kmax, lstsci, gdp)
       !
       ! for parallel simulations: collect data on master node
       !
       if (lstsci>0) then
           call dfreduce_gdp (r0rtcsta, lstsci*kmax*stacnt, dfloat, dfsum, gdp)
       endif
       call dfreduce_gdp (s1rtcsta,             stacnt, dfloat, dfsum, gdp)
       call dfreduce_gdp ( zrtcsta,        kmax*stacnt, dfloat, dfsum, gdp)
       if (rtcact == RTCmodule) then
          !
          ! Collect parameters for this domain
          !
          tparput = 0.0_fp
          iloc    = parput_offset
          do i = 1,stacnt
             do k = 1,kmax
                iloc = iloc + 1
                tparput(1,iloc) = zrtcsta(k,i)
                tparput(2,iloc) = s1rtcsta(i)
                do l = 1,lstsci
                   tparput(2+l,iloc) = r0rtcsta(l,k,i)
                enddo
             enddo
          enddo
          do i = 1,nsluv
              iloc = iloc + 1
              tparput(1,iloc) = cbuv(1,i)
          enddo
          do i = 1,nsrc
              iloc = iloc + 1
              tparput(lstsci+3,iloc) = disch(i)
          enddo
          !
          ! Collect parameters from all domains
          !
          call timer_start(timer_wait, gdp)
          if (rtc_ndomains>1) then
             call dd_rtccommunicate(tparput, tnparput*tnlocput)
          endif
          !
          ! Communication with RTC occurs only by the master domain
          !
          if (rtc_domainnr == 1) then
             call datatortc(timsec, ifirstrtc, tparput, &
                          & tlocput_names, tnlocput, &
                          & tparput_names, tnparput, success)
          endif
          call timer_stop(timer_wait, gdp)
       endif
    endif
end subroutine rtc_comm_put
