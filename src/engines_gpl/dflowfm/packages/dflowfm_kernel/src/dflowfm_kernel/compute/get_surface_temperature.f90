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
   use m_transport, only: constituents, itemp
   use m_flow, only: tem1
   use m_flowparameters, only: temperature_model, TEMPERATURE_MODEL_NONE
   use MessageHandling, only: mess, LEVEL_ERROR
   
   implicit none

   public :: get_surface_temperature

   contains

   !> Returns the surface-layer temperature for all horizontal cells.
   subroutine get_surface_temperature(surface_temperature, initialization)
      real(kind=dp), intent(out) :: surface_temperature(ndx)   
      logical, intent(in) :: initialization !< initialization phase
      
      real(kind=dp), dimension(:), pointer :: temperature_ptr
      integer :: n, kb, kt
      
      if (temperature_model == TEMPERATURE_MODEL_NONE) then
         call mess(LEVEL_ERROR, 'get_surface_temperature: Temperature is turned off in mdu')
         return
      end if

      if (initialization) then
         temperature_ptr => tem1
      else
         temperature_ptr => constituents(itemp,:)
      end if
      
      do n = 1, ndx
         call getkbotktop(n, kb, kt)
         surface_temperature(n) = temperature_ptr(kt)
      end do
   end subroutine get_surface_temperature

end module m_get_surface_temperature
