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

      subroutine PLUSABSD(XK, YK, ZK, NUMK, KEY, EA)
         use m_polygon
         use m_missing
         use geometry_module, only: dpinpok

         implicit none
         integer, parameter :: MAXOP = 64
         character*40 :: OPTION(MAXOP), exp(MAXOP)
         integer :: NUMK, KEY
         double precision :: XK(NUMK), YK(NUMK), ZK(NUMK), EA(NUMK)
         double precision :: XI, YI, ZI, DA, AF, RD

         integer :: ichange, inhul, ja, k, maxexp, maxopt, nwhat
         double precision, save :: A = 1d0

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
         OPTION(8) = 'FIELD = MISSING VALUE ABOVE UNIF VALUE  '
         OPTION(9) = 'FIELD = MISSING VALUE BELOW UNIF VALUE  '
         OPTION(10) = 'SPECIFY UNIFORM VALUE                   '
         MAXOPT = 10
         ICHANGE = 1
10       continue
         NWHAT = ICHANGE
         call SHOWREAL('UNIFORM VALUE = ', A)
         call MENUV3(NWHAT, OPTION, MAXOPT, EXP, MAXEXP)
         call IWINCLOSE(1)
         if (NWHAT == 0) then
            KEY = 0
            return
         else
            if (NWHAT <= 6 .or. NWHAT == 8 .or. NWHAT == 9) then
               ICHANGE = NWHAT
               if (A == dmiss) then
                  call GETREAL('FIRST SPECIFY UNIFORM VALUE = ', A)
                  if (A /= dmiss) JA = 1
                  goto 10
               else
                  JA = 1
               end if
            else if (NWHAT == 10) then
               call GETREAL('SPECIFY UNIFORM VALUE = ', A)
               if (A /= dmiss) JA = 1
               goto 10
            else
               JA = 1; ICHANGE = NWHAT
            end if
         end if

         if (NPL <= 2) then
            call CONFRM('NO POLYGON, SO INCLUDE all FIELD POINTS ? ', JA)
            if (JA == 0) then
               KEY = 0
               return
            end if
         end if
         ! CALL SAVENET()
         call READYY('CHANGE FIELD VALUES', 0d0)
         do k = 1, NUMK
            if (mod(k, 1000) == 0) then
               AF = dble(K) / dble(NUMK)
               call READYY('CHANGE FIELD VALUES', AF)
            end if
            XI = XK(K)
            YI = YK(K)
            ZI = ZK(K)
            RD = EA(K)
            JA = 0
            if (NPL >= 3) then
               call DPINPOK(XI, YI, ZI, NPL, XPL, YPL, INHUL, jins, dmiss)
               if (INHUL == 1) JA = 1
            else
               JA = 1
            end if
            if (JA == 1) then
               DA = A
               if (ICHANGE == 1) then
                  if (RD == dmiss) then
                     EA(K) = DA
                  end if
               else if (ICHANGE == 2) then
                  EA(K) = DA
               else if (ICHANGE == 3) then
                  if (RD /= dmiss) EA(K) = max(EA(K), DA)
               else if (ICHANGE == 4) then
                  if (RD /= dmiss) EA(K) = min(EA(K), DA)
               else if (ICHANGE == 5) then
                  if (RD /= dmiss) EA(K) = EA(K) + DA
               else if (ICHANGE == 6) then
                  if (RD /= dmiss) EA(K) = EA(K) * DA
               else if (ICHANGE == 7) then
                  EA(K) = dmiss
               else if (ICHANGE == 8) then
                  if (RD /= dmiss .and. EA(K) > DA) EA(K) = DMISS
               else if (ICHANGE == 9) then
                  if (RD /= dmiss .and. EA(K) < DA) EA(K) = DMISS
               end if
            end if
         end do
         call READYY('CHANGE FIELD VALUES', -1d0)
         KEY = 3
         return
      end subroutine PLUSABSD
