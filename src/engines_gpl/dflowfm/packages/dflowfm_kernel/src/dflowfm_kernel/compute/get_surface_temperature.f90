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

module m_get_surface_temperature
   use precision_basics, only: dp
   use m_flowgeom, only: ndx
   use m_get_kbot_ktop, only: getkbotktop
   
   implicit none

   public :: get_surface_temperature

   contains

   !> Returns the surface-layer temperature for all horizontal cells.
   subroutine get_surface_temperature(surface_temperature, initialization)
      real(kind=dp), intent(out) :: surface_temperature(ndx)   
      logical, intent(in) :: initialization !< initialization phase
      
      if (initialization) then
         call get_surface_temperature_from_tem1(surface_temperature)
      else
         call get_surface_temperature_from_constituents(surface_temperature)
      end if
      
   end subroutine get_surface_temperature
   
   !> Returns the surface-layer temperature from constituents(itemp,:) for all horizontal cells.
   subroutine get_surface_temperature_from_constituents(surface_temperature)
      use m_transport, only: constituents, itemp

      real(kind=dp), intent(out) :: surface_temperature(ndx)

      integer :: n, kb, kt

      do n = 1, ndx
         call getkbotktop(n, kb, kt)
         surface_temperature(n) = constituents(itemp, kt)
      end do
   end subroutine get_surface_temperature_from_constituents

   !> Returns the surface-layer temperature from tem1 for all horizontal cells.
   subroutine get_surface_temperature_from_tem1(surface_temperature)
      use m_flow, only: tem1

      real(kind=dp), intent(out) :: surface_temperature(ndx)

      integer :: n, kb, kt

      do n = 1, ndx
         call getkbotktop(n, kb, kt)
         surface_temperature(n) = tem1(kt)
      end do
   end subroutine get_surface_temperature_from_tem1

end module m_get_surface_temperature
