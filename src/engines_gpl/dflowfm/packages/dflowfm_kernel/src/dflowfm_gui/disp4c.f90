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

module m_disp4c
   use m_rcirc

   implicit none

contains

   subroutine DISP4C(X, Y, N)
      use precision, only: dp
      use M_MISSING
      use m_halt2
      use m_movabs
      use m_lnabs
      implicit none
      integer :: i
      integer :: istart
      integer :: key
      integer :: n
!     LAAT EEN TWEEDIMENSIONALE FUNCTIE ZIEN MET CIRKELS
      real(kind=dp) :: X(N), Y(N)

      if (N <= 0) return
      ISTART = 0
      do I = 1, N
         if (X(I) /= dmiss) then
            if (ISTART == 1) then
               call LNABS(X(I), Y(I))
            else
               call MOVABS(X(I), Y(I))
               ISTART = 1
            end if
            call RCIRC(X(I), Y(I))
         else
            ISTART = 0
         end if
         if (mod(I, 50) == 0) then
            call HALT2(KEY)
            if (KEY == 1) return
         end if
      end do
      return
   end

end module m_disp4c
