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

module m_tekadmin

   implicit none

contains

   subroutine TEKADMIN(X, Y, I, J)
      use precision, only: dp
      use m_draw_text
      implicit none
      integer :: i
      integer :: j
      integer :: l
      real(kind=dp) :: x
      real(kind=dp) :: y
      character TEX * 11
      if (I <= 9) then
         write (TEX(1:1), '(I1)') I
         L = 2
      else if (I <= 99) then
         write (TEX(1:2), '(I2)') I
         L = 3
      else if (I <= 999) then
         write (TEX(1:3), '(I3)') I
         L = 4
      else
         write (TEX(1:4), '(I4)') I
         L = 5
      end if
      write (TEX(L:L), '(A)') ','
      if (J <= 9) then
         write (TEX(L + 1:L + 1), '(I1)') J
         L = L + 1
      else if (J <= 99) then
         write (TEX(L + 1:L + 2), '(I2)') J
         L = L + 2
      else if (J <= 999) then
         write (TEX(L + 1:L + 3), '(I3)') J
         L = L + 3
      else
         write (TEX(L + 1:L + 4), '(I4)') J
         L = L + 4
      end if
      call DRAWTEXT(real(X), real(Y), TEX(1:L))
      return
   end

end module m_tekadmin
