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

!     plot stencil for higher-order corrections to screen
      subroutine plotklnup(L)
         use m_flowgeom
         implicit none

         integer, intent(in) :: L !< flowlink number

         double precision :: sln1, sln2, sln3
         integer :: i, ip, k1, k2, kdum

         integer, dimension(3) :: icolor = (/31, 221, 31/)

         i = 0
         do ip = 0, 3, 3
            i = i + 1
            k1 = klnup(1 + ip, L)
            sln1 = slnup(1 + ip, L)
            k2 = abs(klnup(2 + ip, L))
            sln2 = slnup(2 + ip, L)
            sln3 = slnup(3 + ip, L)
            if (k1 /= 0) then
               kdum = abs(k1)
               call cirr(xz(kdum), yz(kdum), icolor(i))
               call dHTEXT(sln1, xz(kdum), yz(kdum), 0d0)
            else
               call cirr(xu(L), yu(L), icolor(3))
            end if
            if (k1 > 0) then
               if (k2 > 0) then
                  call cirr(xz(k2), yz(k2), icolor(i))
                  call dHTEXT(sln2, xz(k2), yz(k2), 0d0)
                  call dHTEXT(sln3, xu(L), yu(L), 0d0)
               end if
            end if
         end do

         return
      end subroutine plotklnup
