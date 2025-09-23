!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_sets01zbnd
   implicit none
   private

   public :: sets01zbnd

contains

   !> Sets s1 or s0 water levels at zbndz-type boundaries.
   subroutine sets01zbnd(n01, jasetBlDepth)
      use precision, only: dp
      use m_flowgeom, only: bl, bob, bob0
      use m_flow, only: nbndz, kbndz, zbndz, zbndz0, s1, epshs, hs, ag, u1, u0, s0, bndbldepth, dmiss
      use m_flowtimes, only: alfsmo
      use m_sobekdfm, only: set_1d2d_01
      use m_water_level_boundary, only: correct_water_level_boundary
      use m_boundary_condition_type, only: BOUNDARY_WATER_LEVEL, BOUNDARY_WATER_LEVEL_NEUMANN, &
                                           BOUNDARY_VELOCITY_RIEMANN, BOUNDARY_WATER_LEVEL_OUTFLOW, &
                                           BOUNDARY_DISCHARGE_HEAD
      use m_set_kbot_ktop, only: update_vertical_coordinates_boundary

      integer, intent(in) :: n01 !< Selects whether s0 or s1 has to be set.
      integer, intent(in) :: jasetBlDepth !< Whether or not (1/0) to set the boundary node bed levels, based on depth below s1. Typically only upon model init (based on initial water levels).

      integer :: n, kb, k2, itpbn, L, ibnd
      real(kind=dp) :: water_level_boundary, hh
      logical :: s0_was_updated

      s0_was_updated = .false.

      do n = 1, nbndz ! overrides for waterlevel boundaries
         kb = kbndz(1, n)
         k2 = kbndz(2, n)
         L = kbndz(3, n)
         itpbn = kbndz(4, n)
         select case (itpbn)
         case (BOUNDARY_WATER_LEVEL)
            water_level_boundary = zbndz(n)
            if (alfsmo < 1.0_dp) then
               water_level_boundary = alfsmo * water_level_boundary + (1.0_dp - alfsmo) * zbndz0(n)
            end if
         case (BOUNDARY_WATER_LEVEL_NEUMANN)
            ! positive specified slope leads to inflow
            water_level_boundary = s1(kb)
         case (BOUNDARY_VELOCITY_RIEMANN)
            hh = max(epshs, 0.5_dp * (hs(kb) + hs(k2)))
            water_level_boundary = 2.0_dp * zbndz(n) - zbndz0(n) - sqrt(hh / ag) * u1(L)
         case (BOUNDARY_WATER_LEVEL_OUTFLOW)
            if (u0(L) > 0) then ! on inflow, copy inside
               water_level_boundary = s0(k2)
               if (n01 == 0) then
                  s0(kb) = max(water_level_boundary, bl(kb)) ! TODO: AvD: if single time step is being restarted, then this line will have overwritten some of the old s0 values.
                  s0_was_updated = .true.
               else
                  s1(kb) = max(water_level_boundary, bl(kb))
               end if
            end if
         case (BOUNDARY_DISCHARGE_HEAD)
            water_level_boundary = zbndz(n)
            if (alfsmo < 1.0_dp) then
               water_level_boundary = alfsmo * water_level_boundary + (1.0_dp - alfsmo) * zbndz0(n)
            end if
         end select

         call correct_water_level_boundary(water_level_boundary, kb)

         ! When requested, set bl of bnd nodes to a certain depth below (initial) water level.
         if (jasetBlDepth == 1 .and. allocated(bndBlDepth)) then
            ibnd = kbndz(5, n)
            if (bndBlDepth(ibnd) /= dmiss) then
               bl(kb) = min(bl(kb), water_level_boundary - bndBlDepth(ibnd))
               bob(1, L) = bl(kb)
               bob(2, L) = bl(kb)
               bob0(1, L) = bl(kb)
               bob0(2, L) = bl(kb)
               bl(k2) = bl(kb)
            end if
         end if

         if (itpbn /= BOUNDARY_WATER_LEVEL_OUTFLOW) then
            if (n01 == 0) then
               s0(kb) = max(water_level_boundary, bl(kb)) ! TODO: AvD: if single time step is being restarted, then this line will have overwritten some of the old s0 values.
               hs(kb) = s0(kb) - bl(kb)
               s0_was_updated = .true.
            else
               s1(kb) = max(water_level_boundary, bl(kb))
               hs(kb) = s1(kb) - bl(kb)
            end if
         end if
      end do

      if (s0_was_updated) then
         call update_vertical_coordinates_boundary() ! Ensure that zws and other 3d variables are aligned with the new s0 values.
      end if

      call set_1d2d_01()
   end subroutine sets01zbnd
end module m_sets01zbnd
