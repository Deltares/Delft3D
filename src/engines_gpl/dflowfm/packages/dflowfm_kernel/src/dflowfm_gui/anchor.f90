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
module m_anchor
use m_towor
use m_setxor

   implicit none
contains
   subroutine ANCHOR(X, Y)
      use unstruc_colors
      use m_flow, only: nplot
      use m_GlobalParameters, only: INDTP_ALL
      use m_locatora
      use m_dproject
      use m_inflowcell
      use m_disdis
      use m_set_col

      integer :: ma
      integer :: na
      integer :: k
      double precision :: x, y, xx, yy
      real :: xr, yr
      !    VEEG OUDE CROSS UIT EN ZET NIEUWE

      if (X == 0 .and. Y == 0) then
         MA = 25
         NA = 40
         call TOWOR(MA, NA, XA, YA)
      else
         call SETXOR(1)
         call SETCOL(KLANK)
         call dPROJECT(xa, ya, xx, yy, 1); xr = xx; yr = yy
         call IGrMARKER(xr, yr, 2)
         call SETXOR(0)
         XA = X
         YA = Y
      end if

      call inflowcell(XA, YA, k, 1, INDTP_ALL) ! Use anchor for new nplot point (vertical profile)
      if (k > 0) nplot = k

      call SETXOR(1)
      call SETCOL(KLANK)
      call dPROJECT(xa, ya, xx, yy, 1); xr = xx; yr = yy
      call IGrMARKER(xr, yr, 2)
      call SETXOR(0)

      call DISDIS()

      return
   end
end module m_anchor
