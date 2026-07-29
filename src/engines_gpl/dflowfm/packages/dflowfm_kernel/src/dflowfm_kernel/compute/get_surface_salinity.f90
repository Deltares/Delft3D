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

module m_get_surface_salinity
   use precision_basics, only: dp
   use m_flowgeom, only: ndx
   use m_get_kbot_ktop, only: getkbotktop
   
   implicit none

   public :: get_surface_salinity
   public :: get_salinity_reduction_factor_saturation_humidity

   contains

   !> Returns the surface-layer salinity for all horizontal cells.
   subroutine get_surface_salinity(surface_salinity, initialization)
      use MessageHandling, only: err
      use m_flow, only: sa1
      use m_transport, only: isalt

      real(kind=dp), intent(out) :: surface_salinity(ndx)
      logical, intent(in) :: initialization !< initialization phase

      if (initialization) then
         if (.not. allocated(sa1)) then
            call err('get_surface_salinity: salinity is not allocated (Salinity=0); cannot use SalinityDependentEvaporationMethod=2.')
            surface_salinity = 0.0_dp
            return
         end if
         call get_surface_salinity_from_sa1(surface_salinity)
      else
         if (isalt <= 0) then
            call err('get_surface_salinity: salinity constituent not active (isalt<=0); cannot use SalinityDependentEvaporationMethod=2.')
            surface_salinity = 0.0_dp
            return
         end if
         call get_surface_salinity_from_constituents(surface_salinity)
      end if

   end subroutine get_surface_salinity
   
   !> Returns the surface-layer salinity from constituents(isalt,:) for all horizontal cells.
   subroutine get_surface_salinity_from_constituents(surface_salinity)
      use m_transport, only: constituents, isalt

      real(kind=dp), intent(out) :: surface_salinity(ndx)

      integer :: n, kb, kt

      do n = 1, ndx
         call getkbotktop(n, kb, kt)
         surface_salinity(n) = constituents(isalt, kt)
      end do
   end subroutine get_surface_salinity_from_constituents

   !> Returns the surface-layer salinity from sa1 for all horizontal cells.
   subroutine get_surface_salinity_from_sa1(surface_salinity)
      use m_flow, only: sa1

      real(kind=dp), intent(out) :: surface_salinity(ndx)

      integer :: n, kb, kt

      do n = 1, ndx
         call getkbotktop(n, kb, kt)
         surface_salinity(n) = sa1(kt)
      end do
   end subroutine get_surface_salinity_from_sa1

   !> Returns the salinity reduction factor of saturation humidity.
   elemental subroutine get_salinity_reduction_factor_saturation_humidity(surface_salinity, salinity_reduction_factor_saturation_humidity)
      real(kind=dp), intent(in) :: surface_salinity
      real(kind=dp), intent(out) :: salinity_reduction_factor_saturation_humidity
      
      salinity_reduction_factor_saturation_humidity = max(0.0_dp, min(1.0_dp, 1.0_dp - 5.30e-4_dp * max(0.0_dp, surface_salinity)))
   end subroutine get_salinity_reduction_factor_saturation_humidity

end module m_get_surface_salinity
