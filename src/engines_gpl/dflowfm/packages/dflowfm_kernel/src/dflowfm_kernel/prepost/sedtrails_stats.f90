!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
   use m_waveconst

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

   integer, parameter, private :: PROFILE_VALUES = 5
   real(kind=dp), parameter, private :: WEIGHT_TOL = 1.0e-12_dp
   real(kind=dp), allocatable, private :: profile_sum(:, :, :)
   real(kind=dp), allocatable, private :: valid_time(:, :)
   real(kind=dp), allocatable, private :: profile_weights(:, :)
   real(kind=dp), allocatable, private :: donor_velocity(:, :)
   integer, allocatable, private :: bottom_layer(:)
   real(kind=dp), private :: sample_bounds(2)
   private :: extend_boundary_vertical_velocity

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

      if (allocated(is_sumvalsnd)) then
         is_sumvalsnd = 0.0_dp
      end if
      if (allocated(profile_sum)) then
         profile_sum = 0.0_dp
         valid_time = 0.0_dp
      end if
      sample_bounds = 0.0_dp
      is_dtint = 0.0_dp
   end subroutine reset_sedtrails_stats

   subroutine alloc_sedtrails_stats()
      use m_alloc, only: realloc
      use m_fm_erosed, only: lsedtot
      use m_sediment, only: stm_included
      use m_flowgeom, only: ndx

      implicit none

      if (is_numndvals > 0) then
         if (stm_included) then
            call realloc(is_sumvalsnd, [is_numndvals, ndx, lsedtot], keepExisting=.false., fill=0.0_dp)
         else
            call realloc(is_sumvalsnd, [is_numndvals, ndx, 1], keepExisting=.false., fill=0.0_dp)
         end if
      end if

   end subroutine alloc_sedtrails_stats

   !> Select the layer-resolved water velocity output.
   logical function sedtrails_profiles_enabled() result(enabled)
      use m_flow, only: kmx
      use m_flowparameters, only: jasedtrails

      enabled = jasedtrails > 0 .and. kmx > 1 .and. &
                (sedtrails_analysis == 'flowvelocity' .or. sedtrails_analysis == 'all')
   end function sedtrails_profiles_enabled

   !> Release data owned by the SedTRAILS statistics.
   subroutine release_sedtrails_stats()
      if (allocated(is_sumvalsnd)) then
         deallocate (is_sumvalsnd)
      end if
      if (allocated(profile_sum)) then
         deallocate (profile_sum, valid_time, profile_weights, donor_velocity, bottom_layer)
      end if
      call reset_sedtrails_stats()
   end subroutine release_sedtrails_stats

   !> Allocate profile integrals and validate the horizontal stencils once.
   subroutine alloc_sedtrails_profiles()
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
      use m_flow, only: kmx, ndkx
      use m_flowgeom, only: ndx
      use messagehandling, only: mess, LEVEL_WARN

      real(kind=dp) :: weights(size(st_wf, 1)), total_weight
      integer :: point, invalid_count
      character(len=160) :: message

      allocate (profile_sum(PROFILE_VALUES, kmx, numk), valid_time(kmx, numk), &
                profile_weights(size(st_wf, 1), numk), donor_velocity(3, ndkx), bottom_layer(ndx))
      call reset_sedtrails_stats()
      profile_weights = 0.0_dp
      invalid_count = 0
      do point = 1, numk
         weights = st_wf(:, point)
         if (all(ieee_is_finite(weights))) then
            where (abs(weights) <= WEIGHT_TOL) weights = 0.0_dp
            total_weight = sum(weights)
            if (all(weights >= 0.0_dp) .and. abs(total_weight - 1.0_dp) <= WEIGHT_TOL .and. &
                all(weights <= 0.0_dp .or. (st_ind(:, point) >= 1 .and. st_ind(:, point) <= ndx))) then
               profile_weights(:, point) = weights / total_weight
               cycle
            end if
         end if
         invalid_count = invalid_count + 1
      end do
      if (invalid_count > 0) then
         write (message, '(a,i0,a)') 'SedTRAILS 3D profiles will be missing at ', invalid_count, &
            ' points with invalid stencils.'
         call mess(LEVEL_WARN, trim(message))
      end if
   end subroutine alloc_sedtrails_profiles

   !> Sample the current layer-center velocity and geometry at output points.
   subroutine update_sedtrails_profiles(status)
      use m_flow, only: ucx, ucy, ucz, kbot, ktop, zws, vol1, hs, ndkx
      use m_flowgeom, only: ndxi
      use m_flowparameters, only: epshu
      use m_flowtimes, only: dts, time1
      use m_get_layer_indices, only: getlayerindices
      use m_reconstruct_ucz, only: reconstructucz
      use m_partitioninfo, only: jampi, update_ghosts, ITYPE_SALL3D, reduce_int1_max
      use m_missing, only: dmiss

      integer, intent(out) :: status !< Nonzero if the velocity exchange failed.

      integer :: cell, layers

      status = 0
      if (.not. allocated(profile_sum) .or. dts <= 0.0_dp) then
         return
      end if

      call reconstructucz(0)
      donor_velocity(1, :) = ucx
      donor_velocity(2, :) = ucy
      donor_velocity(3, :) = ucz
      ! The owner has the full link stencil. All ranks take part in the
      ! exchange, including those with no local SedTRAILS points.
      if (jampi > 0) then
         call update_ghosts(ITYPE_SALL3D, 3, ndkx, donor_velocity, status)
         call reduce_int1_max(status)
         if (status /= 0) then
            return
         end if
      end if

      bottom_layer = 0
      do cell = 1, ndxi
         if (hs(cell) > epshu) then
            call getlayerindices(cell, bottom_layer(cell), layers)
         end if
      end do
      call extend_boundary_vertical_velocity()
      call accumulate_sedtrails_profiles(dts, st_ind, profile_weights, kbot, ktop, bottom_layer, &
                                         donor_velocity, zws, vol1, dmiss, profile_sum, valid_time)
      if (is_dtint <= 0.0_dp) then
         sample_bounds(1) = time1 - dts
      end if
      sample_bounds(2) = time1
   end subroutine update_sedtrails_profiles

   !> Extend physical w across open boundary links for output sampling.
   !! Vertical continuity is solved in the interior columns. Use the linked
   !! interior w at the same native layer, or the nearest active layer if
   !! the layer ranges differ. Keep the boundary u, v and layer geometry.
   subroutine extend_boundary_vertical_velocity()
      use m_flow, only: hs, kbot, ktop, vol1
      use m_flowgeom, only: ln, lnxi, lnx
      use m_flowparameters, only: epshu
      use m_get_layer_indices, only: getlayerindices
      use m_missing, only: dmiss

      integer :: link, boundary, interior, layers, cell, source, offset

      ! Do this after the velocity exchange so that a local open boundary
      ! attached to a halo column uses its owner's reconstructed velocity.
      do link = lnxi + 1, lnx
         boundary = ln(1, link)
         interior = ln(2, link)
         if (hs(boundary) <= epshu .or. bottom_layer(interior) == 0) then
            cycle
         end if
         if (ktop(interior) < kbot(interior)) then
            cycle
         end if

         call getlayerindices(boundary, bottom_layer(boundary), layers)
         offset = kbot(interior) - kbot(boundary) + bottom_layer(boundary) - bottom_layer(interior)
         do cell = kbot(boundary), ktop(boundary)
            source = max(kbot(interior), min(ktop(interior), cell + offset))
            donor_velocity(3, cell) = dmiss
            if (vol1(source) > 0.0_dp) then
               donor_velocity(3, cell) = donor_velocity(3, source)
            end if
         end do
      end do
   end subroutine extend_boundary_vertical_velocity

   !> Accumulate the vector and bounds using the same donors and duration.
   subroutine accumulate_sedtrails_profiles(dt, indices, weights, kb, kt, first_layer, &
                                           velocity, z_interface, volume, missing, sums, duration)
      use, intrinsic :: ieee_arithmetic, only: ieee_is_finite

      real(kind=dp), intent(in) :: dt !< Solver timestep duration, s.
      integer, intent(in) :: indices(:, :) !< Horizontal donor indices.
      real(kind=dp), intent(in) :: weights(:, :) !< Validated horizontal weights.
      integer, intent(in) :: kb(:) !< Compressed bottom cell indices.
      integer, intent(in) :: kt(:) !< Compressed active top cell indices.
      integer, intent(in) :: first_layer(:) !< Native index of bottom cell, zero if dry.
      real(kind=dp), intent(in) :: velocity(:, :) !< Layer-center velocity vector.
      real(kind=dp), intent(in) :: z_interface(:) !< Layer interface elevations, positive up.
      real(kind=dp), intent(in) :: volume(:) !< Layer cell volumes.
      real(kind=dp), intent(in) :: missing !< Missing-value sentinel.
      real(kind=dp), intent(inout) :: sums(:, :, :) !< Integrated vector and two elevations.
      real(kind=dp), intent(inout) :: duration(:, :) !< Valid sampling duration.

      integer :: point, layer, donor, cell, kk
      real(kind=dp) :: sample(PROFILE_VALUES), values(PROFILE_VALUES), weight

      do point = 1, size(indices, 2)
         if (.not. any(weights(:, point) > 0.0_dp)) then
            cycle
         end if
         layer_loop: do layer = 1, size(duration, 1)
            sample = 0.0_dp
            do donor = 1, size(indices, 1)
               weight = weights(donor, point)
               if (weight <= 0.0_dp) then
                  cycle
               end if
               cell = indices(donor, point)
               if (first_layer(cell) == 0 .or. layer < first_layer(cell)) then
                  cycle layer_loop
               end if
               kk = kb(cell) + layer - first_layer(cell)
               if (kk > kt(cell) .or. kk < 2 .or. kk > size(z_interface)) then
                  cycle layer_loop
               end if
               if (volume(kk) <= 0.0_dp) then
                  cycle layer_loop
               end if
               values = [velocity(:, kk), z_interface(kk - 1:kk)]
               if (.not. all(ieee_is_finite(values))) then
                  cycle layer_loop
               end if
               if (any(values == missing) .or. values(5) <= values(4)) then
                  cycle layer_loop
               end if
               sample = sample + weight * values
            end do
            sums(:, layer, point) = sums(:, layer, point) + dt * sample
            duration(layer, point) = duration(layer, point) + dt
         end do layer_loop
      end do
   end subroutine accumulate_sedtrails_profiles

   !> Pack mean profiles for a NetCDF record using the file's dimension order.
   subroutine get_sedtrails_profiles(values, duration, bounds)
      use m_missing, only: dmiss

      real(kind=dp), allocatable, intent(out) :: values(:, :, :) !< Vector and heights (field, point, layer).
      real(kind=dp), allocatable, intent(out) :: duration(:, :) !< Valid duration (point, layer).
      real(kind=dp), intent(out) :: bounds(2) !< Actual time window bounds.

      integer :: point, layer

      duration = transpose(valid_time)
      allocate (values(PROFILE_VALUES, size(duration, 1), size(duration, 2)), source=dmiss)
      do layer = 1, size(duration, 2)
         do point = 1, size(duration, 1)
            if (duration(point, layer) > 0.0_dp) then
               values(:, point, layer) = profile_sum(:, layer, point) / duration(point, layer)
            end if
         end do
      end do
      bounds = sample_bounds
   end subroutine get_sedtrails_profiles

   !> Update the (time-)integral statistics for all flow nodes, typically after each time step.
   subroutine update_sedtrails_stats()
      use precision, only: dp
      use m_flowtimes, only: dts
      use m_flow, only: hs, taus, ucx, ucy, kmx, vol1
      use m_fm_erosed, only: stm_included, sedtra, lsedtot, sbcx, sbwx, sbcy, sbwy, sscx, sswx, sscy, sswy, lsed
      use m_sediment, only: sedtot2sedsus
      use m_gettaus, only: gettaus
      use m_gettauswave, only: gettauswave
      use m_get_kbot_ktop, only: getkbotktop
      use m_flowgeom, only: ndx, bl, ba
      use m_transport, only: constituents, ISED1
      use m_flowparameters, only: jawave, flow_without_waves, jawaveswartdelwaq, epshu
      use sed_support_routines, only: ruessink_etal_2012
      use m_waves, only: rlabda, hwav, uorb, phiwav
      use m_sferic, only: pi

      implicit none

      integer :: k, l
      integer :: kk, kbot, ktop
      real(kind=dp) :: ssc !< sediment concentration [kg/m3]
      real(kind=dp), parameter :: sqrttwo = sqrt(2.0_dp)
      real(kind=dp), parameter :: halfsqrttwo = 0.5 * sqrttwo
      real(kind=dp) :: twopi
      real(kind=dp), parameter :: facua = 0.1_dp !< scaling factor wave asymmetry/skewness [-]
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

      if (jawave == 0 .or. flow_without_waves) then ! do not overwrite current+wave induced bed shear stresses from tauwave
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
                  if (hs(k) <= epshu) then
                     cycle
                  end if
                  call getkbotktop(k, kbot, ktop)
                  ssc = 0.0_dp
                  do kk = kbot, ktop
                     ssc = ssc + constituents(ISED1 + l - 1, kk) * vol1(kk)
                  end do
                  is_sumvalsnd(IDX_SSC, k, sedtot2sedsus(l)) = is_sumvalsnd(IDX_SSC, k, sedtot2sedsus(l)) + dts * ssc / ba(k) / hs(k)
               end do
            end do
         end if
      end if

      if (jawave > NO_WAVES) then
         twopi = 2.0_dp * pi
         do k = 1, ndx
            h = hs(k)
            if (h > epshu) then
               kw = twopi / max(rlabda(k), 1.0e-12_dp)
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
