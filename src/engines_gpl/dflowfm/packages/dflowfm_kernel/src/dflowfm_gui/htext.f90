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
module m_htext
   implicit none
contains
   subroutine HTEXT(VAL, X, Y)
      use m_colnow

      double precision :: val
      double precision :: x
      double precision :: y
!     getal value op grafisch scherm in current color
      character TEXT * 6, TEXT2 * 10

      if (NCOLNOW >= 0) then
         if (-1.000d0 < VAL .and. VAL < 10.000d0) then
            write (TEXT(1:6), '(F6.3)') VAL
            call DRAWTEXT(real(X), real(Y), TEXT)
         else if (-10.000d0 < VAL .and. VAL < 100.000d0) then
            write (TEXT(1:6), '(F6.2)') VAL
            call DRAWTEXT(real(X), real(Y), TEXT)
         else if (-100.000d0 < VAL .and. VAL < 1000.000d0) then
            write (TEXT(1:6), '(F6.1)') VAL
            call DRAWTEXT(real(X), real(Y), TEXT)
         else
            write (TEXT2, '(e10.3)') VAL
            call DRAWTEXT(real(X), real(Y), TEXT2)
         end if
      end if

      return
   end
end module m_htext
