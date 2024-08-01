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

   subroutine CLOSETO1Dnetnode(XP1, YP1, N1, dist) !

      use m_netw
      use geometry_module, only: dbdistance
      use m_sferic
      use m_missing

      implicit none
      double precision, intent(in) :: XP1, YP1
      double precision, intent(out) :: dist ! find 1D point close to x,y:
      integer, intent(out) :: n1 ! 1D point found

      double precision :: dismin
      integer :: k, k1, k2, L
      double precision :: dis, dis1, dis2

      N1 = 0
      DISMIN = 9e+33
      do L = 1, numl
         if (kn(3, L) == 1 .or. kn(3, L) == 6) then !  .or. kn(3,L) == 4) THEN
            K1 = kn(1, L); K2 = kn(2, L)
            dis1 = dbdistance(XP1, YP1, Xk(K1), Yk(K1), jsferic, jasfer3D, dmiss)
            dis2 = dbdistance(XP1, YP1, Xk(K2), Yk(K2), jsferic, jasfer3D, dmiss)
            if (dis1 < dis2) then
               k = k1; dis = dis1
            else
               k = k2; dis = dis2
            end if
            if (DIS < DISMIN) then
               N1 = k
               DISMIN = DIS
            end if
         end if
      end do
      dist = dismin
   end subroutine CLOSETO1Dnetnode
