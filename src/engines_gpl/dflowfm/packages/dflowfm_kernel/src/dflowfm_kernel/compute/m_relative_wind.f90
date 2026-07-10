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

!> Utilities for correcting wind velocity with the relative motion of the surface flow.
!!
!! The public routines in this module are elemental, so they can be called
!! for scalars and for conformable arrays with the same implementation.
module m_relative_wind

   use precision, only: dp

   implicit none

   private

   public :: compute_wind_relative_to_surface_scalar
   public :: compute_wind_relative_to_surface_on_link

contains

   !> Elemental correction of wind with the relative surface-flow contribution.
   !!
   !! Applies
   !! `corrected_wind = wind - relativewind * surface_velocity`
   !! to one element (scalar call) or element-wise to conformable arrays.
   !!
   !! If `corrected_wind_speed` is present, it returns
   !! `sqrt(corrected_wind_x**2 + corrected_wind_y**2)` per element.
   elemental subroutine compute_wind_relative_to_surface_scalar(wind_x, wind_y, relativewind, surface_u, surface_v, corrected_wind_x, corrected_wind_y, corrected_wind_speed)
      real(kind=dp), intent(in) :: wind_x, wind_y !< Input wind components in global x/y direction [m/s].
      real(kind=dp), intent(in) :: relativewind !< Weight factor for relative-wind correction [-].
      real(kind=dp), intent(in) :: surface_u, surface_v !< Surface-flow components in global x/y direction [m/s].
      real(kind=dp), intent(out) :: corrected_wind_x, corrected_wind_y !< Corrected wind components in global x/y direction [m/s].
      real(kind=dp), intent(out), optional :: corrected_wind_speed !< Magnitude of corrected wind vector [m/s].

      corrected_wind_x = wind_x
      corrected_wind_y = wind_y

      if (relativewind > 0.0_dp) then
         corrected_wind_x = corrected_wind_x - relativewind * surface_u
         corrected_wind_y = corrected_wind_y - relativewind * surface_v
      end if

      if (present(corrected_wind_speed)) then
         corrected_wind_speed = sqrt(corrected_wind_x * corrected_wind_x + corrected_wind_y * corrected_wind_y)
      end if
   end subroutine compute_wind_relative_to_surface_scalar

   !> Elemental link-oriented wind correction using local link velocity components.
   !!
   !! This routine first rotates link-local velocity components (`link_u`, `link_v`)
   !! to global coordinates using (`link_cos`, `link_sin`), then applies the same
   !! relative-wind correction as `compute_wind_relative_to_surface_scalar`.
   !!
   !! Because the routine is elemental, it supports both:
   !! - single-link scalar calls, and
   !! - vectorized calls over conformable link arrays.
   elemental subroutine compute_wind_relative_to_surface_on_link(wind_x, wind_y, relativewind, link_u, link_v, link_cos, link_sin, corrected_wind_x, corrected_wind_y, corrected_wind_speed)
      real(kind=dp), intent(in) :: wind_x, wind_y !< Input wind components in global x/y direction [m/s].
      real(kind=dp), intent(in) :: relativewind !< Weight factor for relative-wind correction [-].
      real(kind=dp), intent(in) :: link_u, link_v !< Link-normal and link-tangential top-layer velocities [m/s].
      real(kind=dp), intent(in) :: link_cos, link_sin !< Cosine and sine of link direction in global coordinates [-].
      real(kind=dp), intent(out) :: corrected_wind_x, corrected_wind_y !< Corrected wind components in global x/y direction [m/s].
      real(kind=dp), intent(out), optional :: corrected_wind_speed !< Magnitude of corrected wind vector [m/s].

      real(kind=dp) :: uxL, uyL

      uxL = link_u * link_cos - link_v * link_sin
      uyL = link_u * link_sin + link_v * link_cos

      call compute_wind_relative_to_surface_scalar(wind_x, wind_y, relativewind, uxL, uyL, corrected_wind_x, corrected_wind_y, corrected_wind_speed)
   end subroutine compute_wind_relative_to_surface_on_link

end module m_relative_wind
