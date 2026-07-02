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
module m_relative_wind

   use precision, only: dp

   implicit none

   private

   interface compute_surface_relative_wind
      module procedure compute_surface_relative_wind_scalar
      module procedure compute_surface_relative_wind_links
   end interface compute_surface_relative_wind

   public :: compute_surface_relative_wind
   public :: compute_surface_relative_wind_on_link

contains

   !> Correct a single wind vector with the relative surface-flow contribution.
   !!
   !! corrected_wind = wind - relativewind * surface_velocity
   !!
   !! If `corrected_wind_speed` is provided, this routine also returns
   !! `sqrt(corrected_wind_x**2 + corrected_wind_y**2)`.
   subroutine compute_surface_relative_wind_scalar(wind_x, wind_y, relativewind, surface_u, surface_v, corrected_wind_x, corrected_wind_y, corrected_wind_speed)
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
   end subroutine compute_surface_relative_wind_scalar

   !> Correct wind for one horizontal link using top-layer link velocity from flow state.
   subroutine compute_surface_relative_wind_on_link(link, wind_x, wind_y, relativewind, corrected_wind_x, corrected_wind_y, corrected_wind_speed)
      use m_flowgeom, only: csu, snu
      use m_flow, only: ltop, u1, v

      integer, intent(in) :: link !< Horizontal link index.
      real(kind=dp), intent(in) :: wind_x, wind_y !< Input wind components in global x/y direction [m/s].
      real(kind=dp), intent(in) :: relativewind !< Weight factor for relative-wind correction [-].
      real(kind=dp), intent(out) :: corrected_wind_x, corrected_wind_y !< Corrected wind components in global x/y direction [m/s].
      real(kind=dp), intent(out), optional :: corrected_wind_speed !< Magnitude of corrected wind vector [m/s].

      real(kind=dp) :: uL, vL, uxL, uyL

      uL = u1(ltop(link))
      vL = v(ltop(link))
      uxL = uL * csu(link) - vL * snu(link)
      uyL = uL * snu(link) + vL * csu(link)

      call compute_surface_relative_wind_scalar(wind_x, wind_y, relativewind, uxL, uyL, corrected_wind_x, corrected_wind_y, corrected_wind_speed)
   end subroutine compute_surface_relative_wind_on_link

   !> Correct link-based wind vectors using top-layer link velocities from the flow state.
   !!
   !! This routine loops over all horizontal links and computes the local
   !! correction by delegating each link to `compute_surface_relative_wind_on_link`.
   subroutine compute_surface_relative_wind_links(wind_x, wind_y, relativewind, corrected_wind_x, corrected_wind_y)
      use m_flowgeom, only: lnx

      real(kind=dp), intent(in) :: wind_x(:), wind_y(:) !< Input wind components on links in global x/y direction [m/s].
      real(kind=dp), intent(in) :: relativewind !< Weight factor for relative-wind correction [-].
      real(kind=dp), intent(out) :: corrected_wind_x(:), corrected_wind_y(:) !< Corrected wind components on links in global x/y direction [m/s].

      integer :: L

      do L = 1, lnx
         call compute_surface_relative_wind_on_link(L, wind_x(L), wind_y(L), relativewind, corrected_wind_x(L), corrected_wind_y(L))
      end do
   end subroutine compute_surface_relative_wind_links

end module m_relative_wind
