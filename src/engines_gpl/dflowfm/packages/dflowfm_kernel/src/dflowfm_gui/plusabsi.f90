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

      SUBROUTINE PLUSABSI(XK,YK,ZK,KN,NUMK,NUML,KEY,kndefault)
      use M_polygon
      USE m_missing
      use network_data, only: kn3typ
      use geometry_module, only: dpinpok
      use gridoperations
      implicit none
      integer, parameter :: MAXOP = 64
      integer :: NUMK, NUML, KEY
      DOUBLE PRECISION XK(NUMK), YK(NUMK), ZK(NUMK), XI, YI, ZI
      INTEGER KN(3,NUML)
      integer, intent(inout) :: kndefault !< Default uniform value (e.g. kn3typ), will be changed too at call site when user changes it in the dialog.
      CHARACTER*40 OPTION(MAXOP),EXP(MAXOP)

      double precision :: af
      integer :: ia
      integer :: ichange
      integer :: inhul
      integer :: ja
      integer :: k1
      integer :: k2
      integer :: l
      integer :: maxexp
      integer :: maxopt
      integer :: nwhat
      double precision :: rd

      double precision, save :: A
      integer, save :: INI = 0
      A   = kndefault

      JA        = 0
      EXP(1)    = 'MENU TIG                                '
      EXP(2)    = 'HOW TO REPLACE THE VALUES               '
      OPTION(1) = 'FIELD = UNIFORM VALUE, only missings    '
      OPTION(2) = 'FIELD = UNIFORM VALUE, all points       '
      OPTION(3) = 'FIELD = MAX(FIELD,UNIFORM VALUE)        '
      OPTION(4) = 'FIELD = MIN(FIELD,UNIFORM VALUE)        '
      OPTION(5) = 'FIELD = FIELD + UNIFORM VALUE           '
      OPTION(6) = 'FIELD = FIELD * UNIFORM VALUE           '
      OPTION(7) = 'FIELD = MISSING VALUE -999.             '
      OPTION(8) = 'SPECIFY UNIFORM VALUE                   '
      MAXOPT    = 8
      ICHANGE   = 1
   10 CONTINUE
      NWHAT = ICHANGE
      CALL SHOWREAL('UNIFORM VALUE = ',A)
      CALL MENUV3(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
      CALL IWINCLOSE(1)
      IF (NWHAT .EQ. 0) THEN
         KEY = 0
         RETURN
      ELSE
         IF (NWHAT .LE. 6) THEN
            ICHANGE = NWHAT
            IF (A .EQ. dmiss) THEN
               CALL GETREAL('FIRST SPECIFY UNIFORM VALUE = ',A)
               IF (A .NE. dmiss) JA = 1
               GOTO 10
            ELSE
               JA = 1
            ENDIF
         ELSE IF (NWHAT == 7) THEN
            ICHANGE = NWHAT
         ELSE IF (NWHAT == 8) THEN
            CALL GETREAL('SPECIFY UNIFORM VALUE = ',A)
            IF (A .NE. dmiss) then
                JA = 1
                kndefault = int(A)
            end if
            GOTO 10
         ENDIF
      ENDIF

      IF (NPL .LE. 2) THEN
         CALL CONFRM('NO POLYGON, SO INCLUDE all FIELD POINTS ? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
      ENDIF
      CALL SAVENET()
      CALL READYY('CHANGE FIELD VALUES', 0d0)
      KMOD = MAX(1,NUML/100)
      do L = 1,NUML
         IF (MOD(L,KMOD) == 0) THEN
            AF = dble(L) / dble(NUML)
            CALL READYY('CHANGE FIELD VALUES', AF)
         ENDIF
         K1 = KN(1,L)
         K2 = KN(2,L)
         IF (K1 .EQ. 0 .OR. K2 .EQ. 0) cycle
         XI = (XK(K1) + XK(K2))/2
         YI = (YK(K1) + YK(K2))/2
         ZI = (ZK(K1) + ZK(K2))/2
         RD = kn(3,L)
         JA = 0
         IF (NPL .GE. 3) THEN
            CALL DPINPOK( XI, YI, ZI, NPL, XPL, YPL, INHUL, jins, dmiss)
            IF (INHUL .EQ. 1) JA = 1
         ELSE
            JA = 1
         ENDIF
         IA = A
         IF (JA .EQ. 1) THEN
            IF (ICHANGE .EQ. 1) THEN
               IF (RD == dmiss) THEN
                  kn(3,L) = IA
               ENDIF
            ELSE IF (ICHANGE .EQ. 2) THEN
                  kn(3,L) = IA
            ELSE IF (ICHANGE .EQ. 3) THEN
               IF (RD == dmiss) kn(3,L) = MAX(kn(3,L),IA)
            ELSE IF (ICHANGE .EQ. 4) THEN
               IF (RD == dmiss) kn(3,L) = MIN(kn(3,L),IA)
            ELSE IF (ICHANGE .EQ. 5) THEN
               IF (RD == dmiss) kn(3,L) = kn(3,L) + IA
            ELSE IF (ICHANGE .EQ. 6) THEN
               IF (RD == dmiss) kn(3,L) = kn(3,L) * IA
            ELSE IF (ICHANGE .EQ. 7) THEN
               kn(3,L) = INT(dmiss)
            ENDIF
         ENDIF
      end do
      CALL READYY('CHANGE FIELD VALUES', -1d0)
      KEY = 3
      RETURN
      END SUBROUTINE PLUSABSI
