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
module m_krec5
   implicit none
contains
   subroutine KREC5(XX, YY, XD, YD)
      use precision, only: dp
      use m_pfiller_core

      real(kind=dp) :: xd
      real(kind=dp) :: xx
      real(kind=dp) :: yd
      real(kind=dp) :: yy
      real :: X(4), Y(4)
      X(1) = XX - XD
      Y(1) = YY - YD
      X(2) = XX + XD
      Y(2) = YY - YD
      X(3) = XX + XD
      Y(3) = YY + YD
      X(4) = XX - XD
      Y(4) = YY + YD
      call PFILLERCORE(X, Y, 4)
      return
   end
end module m_krec5
