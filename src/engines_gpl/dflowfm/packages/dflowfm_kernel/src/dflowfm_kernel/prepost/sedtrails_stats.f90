!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_sedtrails_stats
   use precision, only: dp
   use m_sedtrails_data

   implicit none

   integer, parameter :: IDX_BL = 1 !< Index for avg bottom level
   integer, parameter :: IDX_HS = 2 !< Index for avg water depth
   integer, parameter :: IDX_UCX = 3 !< Index for avg x velocity
   integer, parameter :: IDX_UCY = 4 !< Index for avg y velocity
   integer, parameter :: IDX_TAUS = 5 !< Index for avg x bed shear stress
   integer, parameter :: IDX_TAUSMAX = 6 !< Index for avg y shear stress
   integer, parameter :: IDX_SBX = 7
   integer, parameter :: IDX_SBY = 8
   integer, parameter :: IDX_SSX = 9
   integer, parameter :: IDX_SSY = 10
   integer, parameter :: IDX_SSC = 11 !< Index for avg sediment concentration
   integer, parameter :: IDX_UA = 12 !< Index for avg x nonlin wave velocity
   integer, parameter :: IDX_VA = 13 !< Index for avg y nonlin wave velocity
   !
   integer, parameter :: is_numndvals = 13 !< Number of variables on flow nodes for which statistics are recorded.

   real(kind=dp), allocatable, target :: is_sumvalsnd(:, :, :)

   character(len=100) :: is_valnamesnd(is_numndvals)
   real(kind=dp), target :: is_dtint !< [s] total time interval since last statistics reset.

contains

   !> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_sedtrails_stats()

      implicit none

      is_valnamesnd(:) = ''
      is_valnamesnd(IDX_BL) = 'bl'
      is_valnamesnd(IDX_HS) = 'hs'
      is_valnamesnd(IDX_UCX) = 'ucx'
      is_valnamesnd(IDX_UCY) = 'ucy'
      is_valnamesnd(IDX_TAUS) = 'taus' ! change to vector comps after sedmor merge
      is_valnamesnd(IDX_TAUSMAX) = 'tausmax'
      is_valnamesnd(IDX_SBX) = 'sbx'
      is_valnamesnd(IDX_SBY) = 'sby'
      is_valnamesnd(IDX_SSX) = 'ssx'
      is_valnamesnd(IDX_SSY) = 'ssy'
      is_valnamesnd(IDX_SSC) = 'ssc'
      is_valnamesnd(IDX_UA) = 'ua'
      is_valnamesnd(IDX_VA) = 'va'

      ! Remaining of variables is handled in reset_integralstats()
      call reset_sedtrails_stats()
   end subroutine default_sedtrails_stats

   !> Resets only stats variables intended for a restart of flow simulation.
   !! Upon loading of new model/MDU, call default_sedtrails_stats() instead.
   subroutine reset_sedtrails_stats()

      implicit none

      is_sumvalsnd(1:is_numndvals, :, :) = 0d0
      is_dtint = 0d0
   end subroutine reset_sedtrails_stats

   subroutine alloc_sedtrails_stats()
      use m_alloc
      use m_fm_erosed, only: lsedtot
      use m_sediment, only: stm_included
      use m_flowgeom, only: ndx

      implicit none

      if (is_numndvals > 0) then
         if (stm_included) then
            call realloc(is_sumvalsnd, (/is_numndvals, ndx, lsedtot/), keepExisting=.false., fill=0d0)
         else
            call realloc(is_sumvalsnd, (/is_numndvals, ndx, 1/), keepExisting=.false., fill=0d0)
         end if
      end if

   end subroutine alloc_sedtrails_stats

   !> Update the (time-)integral statistics for all flow nodes, typically after each time step.
   subroutine update_sedtrails_stats()
      use precision, only: dp
      use m_flowtimes, only: dts
      use m_flow, only: hs, ucx, ucy, taus, kmx, hs, vol1
      use m_flowgeom, only: ndx, bl, ba
      use m_fm_erosed
      use m_transport, only: constituents, ISED1
      use m_sediment, only: sedtot2sedsus, stm_included, sedtra
      use m_flowparameters, only: jawave, flowWithoutWaves, jawaveswartdelwaq, epshu
      use sed_support_routines, only: ruessink_etal_2012
      use m_waves, only: rlabda, hwav, uorb, phiwav
      use m_sferic, only: pi
      use m_gettaus
      use m_gettauswave
      use m_get_kbot_ktop

      implicit none

      integer :: k, l
      integer :: kk, kbot, ktop
      real(kind=dp) :: ssc !< sediment concentration [kg/m3]
      real(kind=dp), parameter :: sqrttwo = sqrt(2d0)
      real(kind=dp), parameter :: halfsqrttwo = 0.5 * sqrttwo
      real(kind=dp) :: twopi
      real(kind=dp), parameter :: facua = 0.1d0 !< scaling factor wave asymmetry/skewness [-]
      real(kind=dp) :: kw !< wave number [rad/m]
      real(kind=dp) :: hw !< sign wave height [m]
      real(kind=dp) :: urms !< rms orbital velocity [m/s]
      real(kind=dp) :: h !< water depth [m]
      real(kind=dp) :: as !< wave asymmetry [-]
      real(kind=dp) :: sk !< wave skewness [-]
      real(kind=dp) :: urs !< ursell number [-]
      real(kind=dp) :: bm !< total (non-dimensional) non-linearity (hypot(sk,as)) [-]
      real(kind=dp) :: phi_phase !< phase angle non-linearity [-]
      real(kind=dp) :: ua !< x component non-linear velocity waves [m/s]
      real(kind=dp) :: va !< y component non-linear velocity waves [m/s]
      real(kind=dp) :: uamag !< magnitude non-linear velocity waves [m/s]

      if (is_numndvals <= 0) then
         return
      end if

      if (jawave == 0 .or. flowWithoutWaves) then ! do not overwrite current+wave induced bed shear stresses from tauwave
         call gettaus(1, 1)
      else
         call gettauswave(jawaveSwartdelwaq)
      end if

      do k = 1, ndx
         is_sumvalsnd(IDX_BL, k, 1) = is_sumvalsnd(IDX_BL, k, 1) + dts * bl(k)
         is_sumvalsnd(IDX_HS, k, 1) = is_sumvalsnd(IDX_HS, k, 1) + dts * hs(k)
         is_sumvalsnd(IDX_TAUS, k, 1) = is_sumvalsnd(IDX_TAUS, k, 1) + dts * taus(k)

         is_sumvalsnd(IDX_UCX, k, 1) = is_sumvalsnd(IDX_UCX, k, 1) + dts * ucx(k) ! assumes depth-averaged value in base node index
         is_sumvalsnd(IDX_UCY, k, 1) = is_sumvalsnd(IDX_UCY, k, 1) + dts * ucy(k)
      end do

      if (stm_included) then
         do k = 1, ndx
            is_sumvalsnd(IDX_TAUSMAX, k, 1) = is_sumvalsnd(IDX_TAUSMAX, k, 1) + dts * sedtra%taub(k)
         end do

         do l = 1, lsedtot
            do k = 1, ndx
               is_sumvalsnd(IDX_SBX, k, l) = is_sumvalsnd(IDX_SBX, k, l) + dts * (sbcx(k, l) + sbwx(k, l))
               is_sumvalsnd(IDX_SBY, k, l) = is_sumvalsnd(IDX_SBY, k, l) + dts * (sbcy(k, l) + sbwy(k, l))
               is_sumvalsnd(IDX_SSX, k, l) = is_sumvalsnd(IDX_SSX, k, l) + dts * (sscx(k, l) + sswx(k, l))
               is_sumvalsnd(IDX_SSY, k, l) = is_sumvalsnd(IDX_SSY, k, l) + dts * (sscy(k, l) + sswy(k, l))
            end do
         end do

         if (kmx == 0) then
            do l = 1, lsed
               do k = 1, ndx
                  is_sumvalsnd(IDX_SSC, k, sedtot2sedsus(l)) = is_sumvalsnd(IDX_SSC, k, sedtot2sedsus(l)) + dts * constituents(ISED1 + l - 1, k)
               end do
            end do
         else
            do l = 1, lsed
               do k = 1, ndx
                  if (hs(k) <= epshu) cycle
                  call getkbotktop(k, kbot, ktop)
                  ssc = 0d0
                  do kk = kbot, ktop
                     ssc = ssc + constituents(ISED1 + l - 1, kk) * vol1(kk)
                  end do
                  is_sumvalsnd(IDX_SSC, k, sedtot2sedsus(l)) = is_sumvalsnd(IDX_SSC, k, sedtot2sedsus(l)) + dts * ssc / ba(k) / hs(k)
               end do
            end do
         end if
      end if

      if (jawave > 0) then
         twopi = 2d0 * pi
         do k = 1, ndx
            h = hs(k)
            if (h > epshu) then
               kw = twopi / max(rlabda(k), 1d-12)
               hw = sqrttwo * hwav(k)
               urms = uorb(k) * halfsqrttwo
               call ruessink_etal_2012(kw, hw, h, sk, as, phi_phase, urs, bm)
               uamag = facua * (sk - as) * urms
               ua = uamag * cosd(phiwav(k))
               va = uamag * sind(phiwav(k))
               is_sumvalsnd(IDX_UA, k, 1) = is_sumvalsnd(IDX_UA, k, 1) + dts * ua
               is_sumvalsnd(IDX_VA, k, 1) = is_sumvalsnd(IDX_VA, k, 1) + dts * va
            end if
         end do
      end if

      is_dtint = is_dtint + dts

   end subroutine update_sedtrails_stats

end module m_sedtrails_stats
