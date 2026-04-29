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

module m_doforester

   implicit none(type, external)

   private

   public :: doforester

contains

   subroutine doforester()
      use precision, only: dp
      use m_foresterpoint2, only: foresterpoint2
      use m_flow, only: kbot, ktop, max_iterations_vertical_forester, ndkx, vol1, kmxn
      use m_transportdata, only: constituents, numconst
      use timers, only: timon, timstrt, timstop
      use m_flowgeom, only: ndxi
      use m_turbulence, only: kmxx

      ! Local variables
      integer :: i_bottom_layer
      integer :: i_constituent
      integer :: i_flowcell
      integer :: number_of_layers
      integer(4) :: timer_handle
      real(kind=dp), dimension(kmxx) :: a
      real(kind=dp), dimension(kmxx) :: d

      ! Initialization
      timer_handle = 0

      if (timon) then
         call timstrt("doforester", timer_handle)
      end if

      do i_flowcell = 1, ndxi
         i_bottom_layer = kbot(i_flowcell)
         number_of_layers = ktop(i_flowcell) - i_bottom_layer + 1

         ! Apply Forester vertical filter for all constituents
         do i_constituent = 1, numconst
            call foresterpoint2(constituents, numconst, ndkx, i_constituent, vol1(i_bottom_layer:), a, d, number_of_layers, kmxn(i_flowcell), i_bottom_layer, max_iterations_vertical_forester, 1)
         end do
      end do

      if (timon) then
         call timstop(timer_handle)
      end if

   end subroutine doforester

end module m_doforester
