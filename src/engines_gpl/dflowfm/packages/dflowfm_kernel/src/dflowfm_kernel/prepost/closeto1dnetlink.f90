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

      subroutine CLOSETO1Dnetlink(XP1, YP1, N1, XN1, YN1, DIST, kn3channelonly) !

         use m_netw
         use geometry_module, only: dbdistance, dlinedis
         use m_missing, only: dmiss
         use m_sferic, only: jsferic, jasfer3D

         implicit none
         integer :: n1
         double precision :: XP1, YP1, XN1, YN1
         double precision, intent(out) :: DIST !< distance to 1D link
         integer, intent(in) :: kn3channelonly !< Whether or not (1/0) the kn3 code can only be a
         !< regular channel netlink (1 or 6), OR also allows
         !< 1D2D links (5 or 7).

         double precision :: dismin
         integer :: ja, k1, k2, L
         double precision :: xa, ya, xb, yb, dis, xn, yn

         N1 = 0
         DISMIN = 9e+33
         do L = 1, numl
            if (kn(3, L) == 1 .or. kn(3, L) == 6 .or. (kn3channelonly == 0 .and. (kn(3, L) == 5 .or. kn(3, L) == 7))) then !  .or. kn(3,L) == 4) THEN
               K1 = kn(1, L); K2 = kn(2, L)
               XA = Xk(K1)
               YA = Yk(K1)
               XB = Xk(K2)
               YB = Yk(K2)
               call dLINEDIS(XP1, YP1, XA, YA, XB, YB, JA, DIS, XN, YN, jsferic, jasfer3D, dmiss)
               !IF (JA .EQ. 1 .AND. DIS < 0.5D0*DBDISTANCE(XA,YA,XB,YB,jsferic, jasfer3D, dmiss)) THEN
               if (JA == 1) then
                  if (DIS < DISMIN) then
                     N1 = L
                     DISMIN = DIS
                     XN1 = XN; YN1 = YN
                  end if
               end if
            end if
         end do
         !   IF (N1 .NE. 0) THEN
         !       K1 = kn(1,n1) ; K2 = kn(2,n1)
         !       IF (dbdistance(XP1,YP1,Xk(K1),Yk(K1)) < dbdistance(XP1,YP1,Xk(K2),Yk(K2)) ) THEN
         !          N1 = K1
         !       ELSE
         !          N1 = K2
         !       ENDIF
         !   ENDIF

         DIST = DISMIN

      end subroutine CLOSETO1Dnetlink
