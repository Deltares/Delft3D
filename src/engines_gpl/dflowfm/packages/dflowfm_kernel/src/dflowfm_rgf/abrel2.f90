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

module m_abrel2

   implicit none

   private

   public :: abrel2

contains

   subroutine ABREL2(X, Y, D, NN, T)
      use precision, only: dp
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      integer :: j
      integer :: nn
      real(kind=dp) :: X(NN), Y(NN), D(NN)
      real(kind=dp) :: T
      D(1) = 0
      do J = 2, NN
         D(J) = D(J - 1) + DBDISTANCE(X(J - 1), Y(J - 1), X(J), Y(J), jsferic, jasfer3D, dmiss)
      end do
      T = D(NN)

      do J = 1, NN
         D(J) = D(J) / T
      end do
      return
   end subroutine ABREL2

end module m_abrel2
