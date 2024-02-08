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

      SUBROUTINE EDITGRIDLINESHIFT(MODE,NFLD,KEY,M1,N1,M2,N2)
      use m_grid
      use unstruc_colors
      implicit none
      integer :: MODE, NFLD, KEY, M1, N1, M2, N2
      integer :: newmode
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      CHARACTER TEX*20, WRDKEY*40, FIELDOP*40

      INTEGER :: NLEVEL, JA, NUM, NWHAT, NPUT, NUMB, JONCE, mp, np, m, n, NCOL
      double precision :: xp, yp
      TEX    =  ' '//FIELDOP(NFLD)
      WRDKEY =  FIELDOP(NFLD)
      NLEVEL =  3
      JA     =  0
      NUM    =  0
      NWHAT  =  0
      NPUT   =  20
      NUMB   =  7
      NCOL   =  NCOLRG
      JONCE  =  0

      MP     = 0
      NP     = 0
      CALL BOTLIN(0,NUMB,KEY)

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(TEX,1,2,15)
      CALL KTEXT(' Now Shift the Line ',1,3,15)
      CALL TEKLN2(Xc, Yc, mmax, nmax, M1, N1, M2, N2, NCOL)

    20 CONTINUE
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      IF (KEY .NE. 23) JONCE = 0

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            CALL QNERROR('Menu is disabled, leave SHIFT LINE ',    &
                         '(Esc or right mouse button)',' ')
            NUM = 0
!           CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         IF (NPUT .EQ. 20) THEN
!           kijken welk punt bij oppakken
            CALL ISPOIN(     Xc,     Yc,     mmax, nmax, MC,     NC,   Zc,    &
                             XP,     YP,     MP,     NP)
!           moet wel op lijn liggen
            IF (M1 .EQ. M2) THEN
               IF (MP .EQ. M1) THEN
                  IF (NP .LT. N1 .OR. NP .GT. N2) THEN
                     CALL QNERROR('Only shift points on the indicated','line',' ')
                     MP = 0
                  ENDIF
               ELSE
                  CALL QNERROR('Only shift points on the indicated','line',' ')
                  MP = 0
               ENDIF
            ENDIF
            IF (N1 .EQ. N2) THEN
               IF (NP .EQ. N1) THEN
                  IF (MP .LT. M1 .OR. MP .GT. M2) THEN
                     CALL QNERROR('Only shift points on the indicated','line',' ')
                     MP = 0
                  ENDIF
               ELSE
                  CALL QNERROR('Only shift points on the indicated','line',' ')
                  MP = 0
               ENDIF
            ENDIF
         ENDIF
         IF ( NPUT .EQ. 20 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,        &
                              MP,     NP,      0        )
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            Xc(MP,NP) = XP
            Yc(MP,NP) = YP
            CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,        &
                              MP,     NP,   NCOL        )
            NPUT   = 20
         ENDIF
         GOTO 20
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         RETURN
      ELSE IF (KEY .EQ. 23) THEN
!        ESCAPE KEY
         JONCE = JONCE + 1
         IF (JONCE .EQ. 1) THEN
            CALL RESTOREgrd()
            KEY = 3
         ELSE
            RETURN
         ENDIF
      ELSE IF (KEY .EQ. 27) THEN
!        TAB
         CALL SHWXYZ(Xc,Yc,Zc,mmax, nmax, MC,NC,0,KEY,M,N)
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT('B Rings Bell',2,6,11)
         CALL OKAY(0)
      ENDIF
!
      GOTO 10
!                           7
      END subroutine editgridlineshift
