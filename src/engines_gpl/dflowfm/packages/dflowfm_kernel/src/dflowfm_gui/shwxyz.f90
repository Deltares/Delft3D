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

          ! NOTE: japes is disabled [AvD]
      SUBROUTINE SHWXYZ(X,Y,RD1,mmax, nmax, MC,NC,JAPERS,KEY,M,N)
      use m_missing
      use unstruc_colors
      implicit none

      integer :: mmax, nmax, mc, nc, japers, key, m, n
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), RD1(MMAX,NMAX)
      CHARACTER  WRDKEY*40, OLDKEY*40
      double precision :: XLC, YLC, XA, YA
      integer :: JMOUSE,JASHOW
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW
      integer :: nlevel
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      integer :: jadraw, jonce, jplus, nlevo
      double precision :: XL, YL, RDOL, FAC

      IF (MC .EQ. 0) RETURN
      OLDKEY = WRDKEY
      NLEVO  = NLEVEL
      WRDKEY = 'TAB = DCURSOR;'
      CALL IMOUSECURSORHIDE()
      CALL SETXOR(1)
      JADRAW = 1
      JONCE  = 0
      JPLUS  = 0

      IF (JAPERS .EQ. 1) THEN
         XL = (X1+X2)/2
         YL = (Y1+Y2)/2
      ELSE
         XL = XLC
         YL = YLC
      ENDIF

      CALL CLOSPT(    X,      Y,     mmax, nmax, MC,     NC, &
                     XL,     YL,      M,      N)
      RDOL = RD1(M,N)

   20 CONTINUE

      CALL DISPOS2(X(M,N),Y(M,N))
      CALL DISDEP(M,N,RD1(M,N))
      IF (JADRAW .EQ. 1) THEN
         CALL TEKGPT(      X,      Y,   mmax, nmax, MMAX,   NMAX, &
                           M,      N, NCOLTX,   RD1)
         JADRAW = 0
      ENDIF
      CALL INKEYEVENT(KEY)
      IF (KEY .NE. 27) JONCE = 0
      IF (KEY .NE. 45 .AND. KEY .NE. 160 .AND. &
          KEY .NE. 43 .AND. KEY .NE. 162) JPLUS = 0

      CALL DISPOS2(X(M,N),Y(M,N))
      CALL DISDEP(M,N,RD1(M,N))
      CALL TEKGPT(      X,      Y,   mmax, nmax, MMAX,   NMAX, &
                        M,      N, NCOLTX,   RD1)
      JADRAW = 1
      IF (KEY .EQ. 131) THEN
         M    = MAX(1,M - 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 130) THEN
         M    = MIN(MC,M + 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 128) THEN
         N    = MIN(NC,N + 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 129) THEN
         N    = MAX(1,N - 1)
         RDOL = RD1(M,N)
      ELSE IF (KEY .EQ. 171) THEN
         CALL HELP(WRDKEY,3)
      ELSE IF (KEY .EQ. 45 .OR. KEY .EQ. 160) THEN
         IF (X(M,N) .NE. XYMIS) THEN
            IF (JPLUS .NE. -1) FAC = 1.0
            IF (RD1(M,N) .EQ. DMISS) RD1(M,N) = 6.9
            RD1(M,N) = RD1(M,N) - .01*FAC
            FAC      = FAC*1.05
            JPLUS    = -1
         ENDIF
      ELSE IF (KEY .EQ. 43 .OR. KEY .EQ. 162) THEN
         IF (X(M,N) .NE. XYMIS) THEN
            IF (JPLUS .NE. 1) FAC = 1.0
            IF (RD1(M,N) .EQ. DMISS) RD1(M,N) = 6.9
            RD1(M,N) = RD1(M,N) + .01*FAC
            FAC      = FAC*1.05
            JPLUS    = 1
         ENDIF
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
         RD1(M,N) = DMISS
         CALL SETCOL(0)
         CALL MOVABS(X(M,N),Y(M,N))
         CALL CIR(RCIR)
         CALL DISDEP(M,N,RD1(M,N))
      ELSE IF (KEY .EQ. 27) THEN
         JONCE     = JONCE + 1
         IF (JONCE .GE. 2) THEN
            CALL ORGLOCATOR(X(M,N),Y(M,N))
            CALL IMOUSECURSORSHOW()
            CALL SETXOR(0)
            NLEVEL = NLEVO
            WRDKEY = OLDKEY
            RETURN
         ENDIF
         RD1(M,N) = RDOL
         CALL DISDEP(M,N,RD1(M,N))
      ELSE
         CALL ORGLOCATOR(X(M,N),Y(M,N))
         CALL IMOUSECURSORSHOW()
         CALL SETXOR(0)
         NLEVEL = NLEVO
         WRDKEY = OLDKEY
         RETURN
      ENDIF
      GOTO 20
      END subroutine shwxyz
