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

module m_mirrorlinepoint

implicit none

private

public :: mirrorlinepoint

contains

   subroutine MIRRORLINEPOINT(X0, Y0, X3, Y3, X1, Y1, X2, Y2, JA, DIS, XN, YN)
      use precision, only: dp
      use geometry_module, only: getdxdy, dlinedis
      use m_sferic
      use m_missing

      real(kind=dp) :: X0, Y0, X3, Y3, X1, Y1, X2, Y2, DIS, XN, YN, dx0, dy0
      integer :: JA

      call DLINEDIS(X0, Y0, X1, Y1, X2, Y2, JA, DIS, XN, YN, jsferic, jasfer3D, dmiss)
      !DX0 = GETDX(X0,Y0,XN,YN)
      !DY0 = GETDY(X0,Y0,XN,YN)
      call getdxdy(X0, Y0, XN, YN, dx0, dy0, jsferic)
      call dlinedis(X3, Y3, X1, Y1, X2, Y2, JA, DIS, XN, YN, jsferic, jasfer3D, dmiss)

      XN = 2 * XN - X3 + DX0
      YN = 2 * YN - Y3 + DY0

      return
   end subroutine MIRRORLINEPOINT

end module m_mirrorlinepoint
