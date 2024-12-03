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

module m_checktrianglenetcell

implicit none

private

public :: checktrianglenetcell

contains

   subroutine CHECKTRIANGLEnetcell(N, JA, phimin, phimax)
      use M_netw
      use M_SFERIC
      use m_ec_triangle
      use m_missing, only: dxymis
      use geometry_module, only: dcosphi

      double precision :: phimin, phimax
      integer :: n, ja

      integer :: k0, k1, k2, n0, n2, nn
      double precision :: X0, Y0, X1, Y1, X2, Y2, COSPHI, PHI
      JA = 1
      phimin = 1d3; phimax = 0d0
      do NN = 1, 3
         N0 = NN - 1; if (N0 < 1) N0 = N0 + 3
         N2 = NN + 1; if (N2 > 3) N2 = N2 - 3
         K0 = netcell(n)%nod(n0)
         K1 = netcell(n)%nod(nn)
         K2 = netcell(n)%nod(n2)
         ! k0 = INDX(N0,N) ; K1 = INDX(NN,N) ; K2 = INDX(N2,N)
         X0 = Xk(K0); Y0 = Yk(K0)
         X1 = Xk(K1); Y1 = Yk(K1)
         X2 = Xk(K2); Y2 = Yk(K2)
         COSPHI = DCOSPHI(X1, Y1, X0, Y0, X1, Y1, X2, Y2, jsferic, jasfer3D, dxymis)
         PHI = acos(min(max(COSPHI, -1d0), 1d0)) * RD2DG
         phimin = min(phimin, phi)
         phimax = max(phimax, phi)
         if (PHI < TRIANGLEMINANGLE .or. PHI > TRIANGLEMAXANGLE) then ! TOO SHARP
            JA = 0
         end if
      end do
      return
   end subroutine CHECKTRIANGLEnetcell

end module m_checktrianglenetcell
