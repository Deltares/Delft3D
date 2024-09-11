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
module m_getxy
   implicit none
contains
!>     zoek TT in X,Y, en XT,YT met dezelfde afstand geeft als SSQ
   subroutine GETXY(T, X, X2, Y, Y2, imax, N, NT, SSQ, XT, YT, TT, H)
      use m_golddis
      use m_splintxy

      integer :: imax, n, nt
      double precision :: ssq, xt, yt
      double precision :: X(imax), Y(imax), X2(imax), Y2(imax), T(imax)
      double precision, intent(in) :: H !< for curvature adapted meshing

      double precision, intent(out) :: TT

      double precision :: ax, bx, cx, tol, dis

      AX = T(1)
      CX = T(NT)
      BX = (AX + CX) / 2
      TOL = 0.00001d0
!     Dan bijhorende T zoeken
      call GOLDDIS(AX, BX, CX, TOL, X, X2, Y, Y2, N, TT, DIS, SSQ, H)

!     EN punt invullen
      call SPLINTXY(X, Y, X2, Y2, N, TT, XT, YT)

      return
   end
end module m_getxy
