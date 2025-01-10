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
module m_bilinxy
   use m_ludcmp, only: ludcmp

   implicit none
contains
   subroutine BILINXY(X, Y, XZ, YZ, XP, YP, XP2, YP2, INI)
      use precision, only: dp
      use m_qnerror
      use m_lubksb

      real(kind=dp) :: c
      integer :: i
      integer :: ini
      integer :: japarallel
      real(kind=dp), save :: A(4, 4), BX(4), BY(4)
      real(kind=dp) :: X(4), Y(4), XZ(4), YZ(4), XP, YP, XP2, YP2
      integer, save :: INX(4)
      ! (Zi = AXi + BYi + CXiYi + Di ,i=1,4)
      ! Coefficienten in A, rechterlid in B, opl met LU-decompositie
      if (INI == 1) then
         do I = 1, 4
            A(I, 1) = X(I) - X(1)
            A(I, 2) = Y(I) - Y(1)
            A(I, 3) = (Y(I) - Y(1)) * (X(I) - X(1))
            A(I, 4) = 1
            BX(I) = XZ(I)
            BY(I) = YZ(I)
         end do
         call LUDCMP(A, 4, 4, INX, C, JAPARALLEL)
         if (JAPARALLEL == 1) then
            call qnerror('Problem in Ludcmp', ' ', ' ')
            INI = -1
            return
         end if
         call LUBKSB(A, 4, 4, INX, BX)
         call LUBKSB(A, 4, 4, INX, BY)
      end if
      XP2 = (XP - X(1)) * BX(1) + (YP - Y(1)) * BX(2) + (XP - X(1)) * (YP - Y(1)) * BX(3) + BX(4)
      YP2 = (XP - X(1)) * BY(1) + (YP - Y(1)) * BY(2) + (XP - X(1)) * (YP - Y(1)) * BY(3) + BY(4)
      return
   end
end module m_bilinxy
