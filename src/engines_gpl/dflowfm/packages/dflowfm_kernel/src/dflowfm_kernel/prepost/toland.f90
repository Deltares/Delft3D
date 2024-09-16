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

!>    compute the nearest point on the land boundary
      subroutine TOLAND(XX, YY, JSTART, JEND, JAINVIEW, XV, YV, DISMIN, JOUT, RLOUT) ! SHIFT 1 POINT TO LANDBOUNDARY
         use M_LANDBOUNDARY
         use M_MISSING
         use M_POLYGON
         use geometry_module, only: pinpok
         use m_d_line_dis3
         implicit none
         double precision, intent(in) :: xx, yy !< coordinates of reference point
         integer, intent(in) :: JSTART, JEND !< start end end node of land boundary segment respectively
         integer, intent(in) :: JAINVIEW !< nodes in view only (1) or not (0) or in polygon only (2)
         double precision, intent(out) :: xv, yv !< coordinates of nearest point on land boundary
         double precision, intent(out) :: dismin !< smallest distance to land boundary
         integer, intent(out) :: jout !< index of first node of poly segment on which the point is projected
         double precision, intent(out) :: rLout !< scaled distance of projected point to node jout

         integer :: j, ja, ina, inb, ithread
         logical :: Ldoit
         double precision :: xa, ya, xb, yb, dis, xn, yn, rL, rLdum

!      integer, parameter                 :: IMISS = -999999

         integer, external :: OMP_GET_THREAD_NUM

         XV = XX; YV = YY

         jout = -999
         rlout = -1d0
         if (MXLAN == 0) return

         DISMIN = 9e+33
         inb = IMISS

!     note to self: parallel only if jend-jstart+1 > number
!

!$OMP PARALLEL DO    &
!$OMP PRIVATE(j,ja,ina,inb,Ldoit,xa,ya,xb,yb,dis,xn,yn,rL,rLdum,ithread)
         do J = JSTART, JEND - 1
            Xa = XLAN(J)
            Ya = YLAN(J)
            Xb = XLAN(J + 1)
            Yb = YLAN(J + 1)

            if (Xa /= dXYMIS .and. XB /= dXYMIS) then

               !if ( JAINVIEW.eq.1 ) then
               !   Ldoit = dINVIEW(Xa,Ya,ya) .OR. dINVIEW(Xb,Yb,yb)
               !else
               Ldoit = .true.
               !end if

               if (JAINVIEW == 2) then
                  call pinpok(xa, ya, NPL, XPL, YPL, ina, jins, dmiss)
                  call pinpok(xb, yb, NPL, XPL, YPL, inb, jins, dmiss)
                  if (ina == 1 .and. inb == 1) then
                     Ldoit = .true.
                  else
                     Ldoit = .false.
                  end if
               end if

               if (Ldoit) then
                  call dLINEDIS3(XX, YY, Xa, Ya, Xb, Yb, JA, DIS, XN, YN, RL)
                  RLDUM = RL ! remember the unlimited rL
                  RL = min(max(RL, 0d0), 1d0)
                  if (JA == 1) then
                     if (DIS < DISMIN) then
!$OMP CRITICAL
                        if (DIS < DISMIN) then
                           XV = XN
                           YV = YN
                           DISMIN = DIS
                           JOUT = J
                           RLOUT = RLDUM ! output the unlimited rL
                        end if
!$OMP END CRITICAL
                     end if
                  end if
!               IF (DIS == 0) THEN
!                  DIS = DIS
!               ENDIF
               end if
            end if
         end do
!$OMP END PARALLEL DO

         if (jout == 0) then
            continue
         end if

         return
      end subroutine TOLAND
