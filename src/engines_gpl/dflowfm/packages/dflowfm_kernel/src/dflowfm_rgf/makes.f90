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

      subroutine MAKES(X, Y, X2, Y2, T, S, S2, imax, N, NT, H)
!     maak X,Y splines + afstandsarray en splines S op basis
!     van NT snijpunten
         !USE DIMENS
         use m_splinxy
         use m_spline
         implicit none
         integer :: imax, n, nt
         double precision :: X(IMAX), Y(IMAX), X2(IMAX), Y2(IMAX), T(IMAX), S(IMAX), S2(IMAX)
         double precision, intent(in) :: H !< for curvature adapted meshing

         integer :: i

         call SPLINXY(X, Y, X2, Y2, N)

         do I = 1, NT
            call GETDIS(X, Y, X2, Y2, N, T(I), S(I), H)
         end do
         call SPLINE(S, NT, S2)
         return
      end subroutine makes
