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

module m_plusabsi
use m_showreal


implicit none

contains

      subroutine PLUSABSI(XK, YK, ZK, KN, NUMK, NUML, KEY, kndefault)
         use m_menuv3
         use m_getreal
         use m_confrm
         use M_polygon
         use m_missing
         use geometry_module, only: dpinpok
         use gridoperations
         use m_readyy

         integer, parameter :: MAXOP = 64
         integer :: NUMK, NUML, KEY
         double precision XK(NUMK), YK(NUMK), ZK(NUMK), XI, YI, ZI
         integer KN(3, NUML)
         integer, intent(inout) :: kndefault !< Default uniform value (e.g. kn3typ), will be changed too at call site when user changes it in the dialog.
         character(len=40) OPTION(MAXOP), exp(MAXOP)

         double precision :: af
         integer :: ia
         integer :: ichange
         integer :: inhul
         integer :: ja
         integer :: k1
         integer :: k2
         integer :: l
         integer :: maxopt
         integer :: nwhat
         double precision :: rd

         double precision, save :: A

         A = kndefault

         JA = 0
         exp(1) = 'MENU TIG                                '
         exp(2) = 'HOW TO REPLACE THE VALUES               '
         OPTION(1) = 'FIELD = UNIFORM VALUE, only missings    '
         OPTION(2) = 'FIELD = UNIFORM VALUE, all points       '
         OPTION(3) = 'FIELD = MAX(FIELD,UNIFORM VALUE)        '
         OPTION(4) = 'FIELD = MIN(FIELD,UNIFORM VALUE)        '
         OPTION(5) = 'FIELD = FIELD + UNIFORM VALUE           '
         OPTION(6) = 'FIELD = FIELD * UNIFORM VALUE           '
         OPTION(7) = 'FIELD = MISSING VALUE -999.             '
         OPTION(8) = 'SPECIFY UNIFORM VALUE                   '
         MAXOPT = 8
         ICHANGE = 1
10       continue
         NWHAT = ICHANGE
         call SHOWREAL('UNIFORM VALUE = ', A)
         call MENUV3(NWHAT, OPTION, MAXOPT)
         call IWINCLOSE(1)
         if (NWHAT == 0) then
            KEY = 0
            return
         else
            if (NWHAT <= 6) then
               ICHANGE = NWHAT
               if (A == dmiss) then
                  call GETREAL('FIRST SPECIFY UNIFORM VALUE = ', A)
                  if (A /= dmiss) JA = 1
                  goto 10
               else
                  JA = 1
               end if
            else if (NWHAT == 7) then
               ICHANGE = NWHAT
            else if (NWHAT == 8) then
               call GETREAL('SPECIFY UNIFORM VALUE = ', A)
               if (A /= dmiss) then
                  JA = 1
                  kndefault = int(A)
               end if
               goto 10
            end if
         end if

         if (NPL <= 2) then
            call CONFRM('NO POLYGON, SO INCLUDE all FIELD POINTS ? ', JA)
            if (JA == 0) then
               KEY = 0
               return
            end if
         end if
         call SAVENET()
         call READYY('CHANGE FIELD VALUES', 0d0)
         KMOD = max(1, NUML / 100)
         do L = 1, NUML
            if (mod(L, KMOD) == 0) then
               AF = dble(L) / dble(NUML)
               call READYY('CHANGE FIELD VALUES', AF)
            end if
            K1 = KN(1, L)
            K2 = KN(2, L)
            if (K1 == 0 .or. K2 == 0) cycle
            XI = (XK(K1) + XK(K2)) / 2
            YI = (YK(K1) + YK(K2)) / 2
            ZI = (ZK(K1) + ZK(K2)) / 2
            RD = kn(3, L)
            JA = 0
            if (NPL >= 3) then
               call DPINPOK(XI, YI, ZI, NPL, XPL, YPL, INHUL, jins, dmiss)
               if (INHUL == 1) JA = 1
            else
               JA = 1
            end if
            IA = A
            if (JA == 1) then
               if (ICHANGE == 1) then
                  if (RD == dmiss) then
                     kn(3, L) = IA
                  end if
               else if (ICHANGE == 2) then
                  kn(3, L) = IA
               else if (ICHANGE == 3) then
                  if (RD == dmiss) kn(3, L) = max(kn(3, L), IA)
               else if (ICHANGE == 4) then
                  if (RD == dmiss) kn(3, L) = min(kn(3, L), IA)
               else if (ICHANGE == 5) then
                  if (RD == dmiss) kn(3, L) = kn(3, L) + IA
               else if (ICHANGE == 6) then
                  if (RD == dmiss) kn(3, L) = kn(3, L) * IA
               else if (ICHANGE == 7) then
                  kn(3, L) = int(dmiss)
               end if
            end if
         end do
         call READYY('CHANGE FIELD VALUES', -1d0)
         KEY = 3
         return
      end subroutine PLUSABSI

end module m_plusabsi
