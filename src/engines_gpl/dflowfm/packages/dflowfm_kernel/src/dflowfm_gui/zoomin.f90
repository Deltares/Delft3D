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

      SUBROUTINE ZOOMIN(KEY,NPUT)
      use unstruc_colors
      use m_wearelt
      use m_sferic
      use m_sferzoom

      COMMON /LOCATORA/ XLC,YLC,XA,YA,JMOUSE,JASHOW
      double precision :: xlc, ylc, xa, ya
      integer          :: JMOUSE,JASHOW

      double precision :: aspect, dx, dy, xln, yln, xl, yl, X1B,Y1B,X2B,Y2B, c, xl2, yl2
      integer          :: k,nlevel, jadraw, nput, nnn, ja, key


      CHARACTER WRDKEY*40
      integer, PARAMETER :: MAXZOOM = 4
      REAL XYWOLD(MAXZOOM,4)
      SAVE XYWOLD
      integer , save :: NUMZOOM = 0
      IF (NUMZOOM .EQ. 0) THEN
         DO 5 K = 1,MAXZOOM
            XYWOLD(K,1) = (XMIN + XMAX)/2
            XYWOLD(K,2) = (YMIN + YMAX)/2
            XYWOLD(K,3) = (YMAX-YMIN)
    5    CONTINUE
         NUMZOOM = 1
      ENDIF
!     geen entry ALS NET BEZIG PUNT TE ZETTEN
!     BIJ VERLATEN MET KEY = 3, TEKEN OPNIEUW
      WRDKEY   = 'Z   = ZOOMIN ;'
      NLEVEL   = 3
      JADRAW   = 1
!
      IF (NPUT .EQ. 1) RETURN

      CALL IGRLINEWIDTH(2,-1)
      CALL SETCOL(KLZM)
      CALL SETXOR(1)
      CALL BOTLIN(0,5,NNN)
      CALL INQASP(ASPECT)
      XL    = XLC
      YL    = YLC
      dy    = dyh/3d0
      DX    = DY/ASPECT
      IF (JSFERTEK .GE. 1) then
          CALL dPROJECT(XLC,YLC,XL,YL,1)
      !   c   = max(1d-4, cos(dg2rd*min(90d0, abs(yl) ) ) )
      !   dx = dx/c
      endif
      X1B   = XL - DX/2
      X2B   = XL + DX/2
      Y1B   = YL - DY/2
      Y2B   = YL + DY/2

   10 CONTINUE

      IF (JADRAW .EQ. 1) THEN
         CALL BOXnop(X1B,Y1B,X2B,Y2B)
         JADRAW = 0
      ENDIF
      JA   = 0
      KEY  = 999
      CALL READLOCATOR(XL,YL,KEY)
      IF (JSFERTEK .GE. 1) CALL dPROJECT(XLC,YLC,XL,YL,1)

      IF (X2B .GT. X2 .OR. X1B .LT. X1 .OR. Y2B .GT. Y2 .OR. Y1B .LT. Y1    ) THEN
         dy  = dyh
         JA = 1
      ELSE IF (KEY .EQ. 21) THEN
         JA  = 1
      ELSE IF (KEY .EQ. 22) THEN
         JA  = 3
      ELSE IF (KEY .EQ. 90 .OR. KEY .EQ. 90+32) THEN
         DY  = 3d0*dyh
         IF (JSFERTEK .GE. 1) DY = MIN (DY, 179D0)
         JA  = 1
      ELSE IF (KEY .EQ. 23) THEN
         KEY = 3
         CALL SETXOR(0)
         CALL IMOUSECURSORHIDE()
         CALL IGRLINEWIDTH(1,-1)
         RETURN
      ELSE IF (KEY .EQ. 24) THEN
!        F1
         NLEVEL = 3
         CALL HELP(WRDKEY,NLEVEL)
      ELSE IF (KEY .EQ. 25) THEN
!        F2
         CALL HISTOR()
      ELSE IF (KEY .EQ. 162 .OR. KEY .EQ. 160 .OR. KEY .EQ. 45 .OR. KEY .EQ. 43  .OR. KEY .LT. 0  ) THEN
         CALL BOXnop(X1B,Y1B,X2B,Y2B)
         JADRAW = 1
         IF (KEY .EQ. 162 .OR. KEY .EQ. 43) THEN
            DY = DY*1.01 ; IF (JSFERTEK .GE. 1) DY = MIN (DY, 179D0)
         ELSE IF (KEY .EQ. 160 .OR. KEY .EQ. 45) THEN
            DY = DY/1.01
         ENDIF
         DX  = DY/ASPECT
         X1B = XL - DX/2
         X2B = XL + DX/2
         Y1B = YL - DY/2
         Y2B = YL + DY/2
      ELSE IF (KEY .EQ. 143) THEN
         NUMZOOM = NUMZOOM - 1
         IF (NUMZOOM .EQ. 0) NUMZOOM = MAXZOOM
         XL  = XYWOLD(NUMZOOM,1)
         YL  = XYWOLD(NUMZOOM,2)
         DY  = XYWOLD(NUMZOOM,3)
         JA  = 2
      ENDIF

      IF (JA .GE. 1) THEN
         CALL IMOUSECURSORHIDE()
         IF (JA .NE. 3) THEN
            IF (JSFERTEK .GE. 1) then
                CALL dPROJECT(XL,YL,XL2,YL2,2) ; xl = xl2 ; yl = yl2
            endif
            CALL SETWYnew(XL,YL,DY)
         ELSE
            CALL WEAREL()
         ENDIF
         IF ( JA .NE. 2) THEN
!           alleen opslaan als in of uitgezoomd, niet als teruggezoomd
            NUMZOOM = NUMZOOM + 1
            IF (NUMZOOM .EQ. MAXZOOM+1) NUMZOOM = 1
            XYWOLD(NUMZOOM,1) = XL
            XYWOLD(NUMZOOM,2) = YL
            XYWOLD(NUMZOOM,3) = DY
         ENDIF
         XLN  = 0.0
         YLN  = 0.0
         CALL ORGLOCATOR(XLN,YLN)
         KEY  = 3
         CALL SETXOR(0)
         CALL IGRLINEWIDTH(1,-1)
         RETURN
      ENDIF
      GOTO 10

      END
