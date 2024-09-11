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
module m_spldist
   implicit none
contains
   double precision function SPLDIST(X, X2, Y, Y2, XX, YY, TV, N)
      use m_splint
      use m_planedistance

      integer :: n
!     AFSTAND VAN PUNT XX,YY TOT SPLINEPUNT MET PARM TV

      double precision :: X(N), X2(N), Y(N), Y2(N), TV, XX, YY, XV, YV
      TV = max(0d0, min(TV, N - 1d0))
      call SPLINT(X, X2, N, TV, XV)
      call SPLINT(Y, Y2, N, TV, YV)
!     CALL DISTANCE(XV,YV,XX,YY,DIST)
      call PLANEDISTANCE(XV, YV, XX, YY, SPLDIST)
      return
   end function SPLDIST
end module m_spldist
