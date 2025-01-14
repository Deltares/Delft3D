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
module m_hi_text
   implicit none
contains
   subroutine HITEXT(IVAL, X, Y)
      use precision, only: dp
      use m_colnow
      use m_draw_text

      integer :: ival
      integer :: l
      real(kind=dp) :: x
      real(kind=dp) :: y
!     INTEGER grafisch scherm in current color
      character TEX * 8

      if (NCOLNOW >= 0) then
         if (abs(IVAL) < 100) then
            write (TEX, '(I3)') IVAL
         else if (abs(IVAL) < 10000) then
            write (TEX, '(I5)') IVAL
         else
            write (TEX, '(I8)') IVAL
         end if
         L = len_trim(TEX)
         call DRAWTEXT(real(X), real(Y), TEX(1:L))
      end if
      return
   end
end module m_hi_text
