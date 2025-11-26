!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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
module m_realpolygon

   use precision, only: dp
   implicit none
contains
   subroutine realPOLYGON(X, Y, N, NCOL)
      use m_set_col, only: setcol
      use m_lnabs, only: lnabs
      use m_ptabs, only: ptabs

      integer :: i
      integer :: n
      integer :: ncol
      real :: X(N), Y(N)

      call SETCOL(NCOL)
      call PTABS(real(X(1), kind=dp), real(Y(1), kind=dp))
      do I = 2, N
         call LNABS(real(X(I), kind=dp), real(Y(I), kind=dp))
      end do
      call LNABS(real(X(1), kind=dp), real(Y(1), kind=dp))
      return
   end
end module m_realpolygon
