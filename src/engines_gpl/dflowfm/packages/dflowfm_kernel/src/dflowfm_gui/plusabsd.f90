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

      SUBROUTINE PLUSABSD(XK,YK,ZK,NUMK,KEY,EA)
      use m_polygon
      use m_missing
      use geometry_module, only: dpinpok

      implicit none
      INTEGER, PARAMETER     :: MAXOP = 64
      CHARACTER*40           :: OPTION(MAXOP),EXP(MAXOP)
      INTEGER                :: NUMK, KEY
      DOUBLE PRECISION       :: XK(NUMK), YK(NUMK), ZK(NUMK), EA(NUMK)
      DOUBLE PRECISION       :: XI, YI, ZI, DA, AF, RD

      INTEGER                :: ichange, inhul, ja, k, maxexp, maxopt, nwhat, kk
      DOUBLE PRECISION, SAVE :: A   = 1D0

      JA         = 0
      EXP(1)     = 'MENU TIG                                '
      EXP(2)     = 'HOW TO REPLACE THE VALUES               '
      OPTION(1)  = 'FIELD = UNIFORM VALUE, only missings    '
      OPTION(2)  = 'FIELD = UNIFORM VALUE, all points       '
      OPTION(3)  = 'FIELD = MAX(FIELD,UNIFORM VALUE)        '
      OPTION(4)  = 'FIELD = MIN(FIELD,UNIFORM VALUE)        '
      OPTION(5)  = 'FIELD = FIELD + UNIFORM VALUE           '
      OPTION(6)  = 'FIELD = FIELD * UNIFORM VALUE           '
      OPTION(7)  = 'FIELD = MISSING VALUE -999.             '
      OPTION(8)  = 'FIELD = MISSING VALUE ABOVE UNIF VALUE  '
      OPTION(9)  = 'FIELD = MISSING VALUE BELOW UNIF VALUE  '
      OPTION(10) = 'SPECIFY UNIFORM VALUE                   '
      MAXOPT     = 10
      ICHANGE    = 1
   10 CONTINUE
      NWHAT = ICHANGE
      CALL SHOWREAL('UNIFORM VALUE = ',A)
      CALL MENUV3(NWHAT,OPTION,MAXOPT,EXP,MAXEXP)
      CALL IWINCLOSE(1)
      IF (NWHAT .EQ. 0) THEN
         KEY = 0
         RETURN
      ELSE
         IF (NWHAT .LE. 6 .or. NWHAT .EQ. 8 .or. NWHAT .EQ. 9) THEN
            ICHANGE = NWHAT
            IF (A .EQ. dmiss) THEN
               CALL GETREAL('FIRST SPECIFY UNIFORM VALUE = ',A)
               IF (A .NE. dmiss) JA = 1
               GOTO 10
            ELSE
               JA = 1
            ENDIF
         ELSE IF (NWHAT == 10) THEN
            CALL GETREAL('SPECIFY UNIFORM VALUE = ',A)
            IF (A .NE. dmiss) JA = 1
            GOTO 10
         ELSE
            JA = 1 ; ICHANGE = NWHAT
         ENDIF
      ENDIF


      IF (NPL .LE. 2) THEN
         CALL CONFRM('NO POLYGON, SO INCLUDE all FIELD POINTS ? ',JA)
         IF (JA .EQ. 0) THEN
            KEY = 0
            RETURN
         ENDIF
      ENDIF
     ! CALL SAVENET()
      CALL READYY('CHANGE FIELD VALUES', 0d0)
      DO 20 K = 1,NUMK
         if (mod (k,1000) == 0) then
            AF = dble(K) / dble(NUMK)
            CALL READYY('CHANGE FIELD VALUES', AF)
         endif
         XI = XK(K)
         YI = YK(K)
         ZI = ZK(K)
         RD = EA(K)
         JA = 0
         IF (NPL .GE. 3) THEN
            CALL DPINPOK( XI, YI, ZI, NPL, XPL, YPL, INHUL, jins, dmiss)
            IF (INHUL .EQ. 1) JA = 1
         ELSE
            JA = 1
         ENDIF
           IF (JA .EQ. 1) THEN
              DA = A
              IF (ICHANGE .EQ. 1) THEN
                 IF (RD == dmiss) THEN
                    EA(K) = DA
                 ENDIF
              ELSE IF (ICHANGE .EQ. 2) THEN
                    EA(K) = DA
              ELSE IF (ICHANGE .EQ. 3) THEN
                 IF (RD /= dmiss) EA(K) = MAX(EA(K),DA)
              ELSE IF (ICHANGE .EQ. 4) THEN
                 IF (RD /= dmiss) EA(K) = MIN(EA(K),DA)
              ELSE IF (ICHANGE .EQ. 5) THEN
                 IF (RD /= dmiss) EA(K) = EA(K) + DA
              ELSE IF (ICHANGE .EQ. 6) THEN
                 IF (RD /= dmiss) EA(K) = EA(K) * DA
              ELSE IF (ICHANGE .EQ. 7) THEN
                 EA(K) = dmiss
              ELSE IF (ICHANGE .EQ. 8) THEN
                 IF (RD /= dmiss .AND. EA(K) > DA) EA(K) = DMISS
              ELSE IF (ICHANGE .EQ. 9) THEN
                 IF (RD /= dmiss .AND. EA(K) < DA) EA(K) = DMISS
              ENDIF
           ENDIF
   20 CONTINUE
      CALL READYY('CHANGE FIELD VALUES', -1d0)
      KEY = 3
      RETURN
      END SUBROUTINE PLUSABSD
