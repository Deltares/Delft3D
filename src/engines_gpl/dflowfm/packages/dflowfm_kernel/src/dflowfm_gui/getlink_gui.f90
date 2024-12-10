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

!> select link for directional refinement in GUI
module m_getlink_gui

   implicit none

contains

   subroutine getlink_GUI(xp, yp, L)
      use precision, only: dp
      use m_qnerror
      use m_ktext
      use m_putget_un
      use m_n_plot_plus_min
      use m_k_plot_plus_min
      use m_tek_link
      use m_is_link
      use m_draw_nu
      implicit none

      real(kind=dp), intent(out) :: xp, yp !< coordinates of clicked point
      integer, intent(out) :: L !< clicked link number

      real(kind=dp) :: zp

      integer :: num, nwhat, nput, numb, key

      L = 0

      call ktext(' Refine net       ', 1, 3, 15)

      num = 0
      nwhat = 0
      nput = 55
      numb = 10
      key = 0

      do
         call DRAWNU(KEY)
         call putget_un(num, nwhat, nput, numb, xp, yp, key)

         if (key == 23) then ! escape
            exit
         else if (key == 21) then ! left mouse button
            call islink(L, xp, yp, zp)
            if (L > 0) then ! link found
               call teklink(L, 31)
               exit
            end if
!        the following is copied from editnetw (zoom, panning)
         else if (key == 43 .or. key == 140) then
            call kplotplusmin(1)
            key = 3
         else if (key == 45 .or. key == 141) then
            call kplotplusmin(-1)
            key = 3
         else if (key == 133) then ! page down
            call nplotplusmin(1)
            key = 3
         else if (key == 143) then ! delete
            call nplotplusmin(-1)
            key = 3
         end if
      end do

      if (L < 1) then
         call qnerror('no link clicked: exitting', ' ', ' ')
      end if

      return
   end subroutine

end module m_getlink_gui
