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

      SUBROUTINE EDITSPLINES(MODE,KEY)
      use unstruc_colors
      USE M_SPLINES
      use unstruc_display, only: plotSplines
      implicit none
      integer, intent(inout) :: mode, key
      integer :: newmode

!      use rgfblock
!
      CHARACTER WRDKEY*40
      integer :: nlevel
      COMMON /HELPNOW/ WRDKEY,NLEVEL
      integer :: ndraw, IIJ
      COMMON /DRAWTHIS/ ndraw(50)

      integer :: ja, num, numb, ncol, nwhat, nput
      double precision :: xp, yp

      WRDKEY = 'EDIT SPLINES'
      NLEVEL =  2
      JA     =  0
      NUM    =  0
      NWHAT  =  0
      NPUT   = -1
      NUMB   =  9
      NCOL   =  NCOLSP
      NDRAW(15) = 1

      MP     = 0
      NP     = 0
      CALL BOTLIN(0,NUMB,KEY)
!!     TEST
      CALL saveSplines()

    10 CONTINUE
      CALL DRAWNU(KEY)
      CALL KTEXT(' Edit Splines       ',1,2,15)
      !CALL KTEXT(' Click Spline Points',1,3,15)
      CALL putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)

      IF (NUM .NE. 0) THEN
!        ER IS EEN KEUZE
         IF (NUM .EQ. 4) THEN
            MODE = NWHAT
            RETURN
         ELSE
            if (NUM .eq. 5 .and. NWHAT .eq. 2) then
                mp = 0
                np = 0
            endif
            CALL CHOICES(MODE,NUM,NWHAT,KEY)
         ENDIF
      ELSE IF (KEY >= 577) THEN ! Alt+letter switches edit mode.
        call selecteditmode(newmode, key)
        if (newmode > 0 .and. newmode /= mode) then
            mode = newmode
            return
        end if
      ELSE IF (KEY .EQ. 21) THEN
!        INS KEY
         IF (NPUT .EQ.  0 .OR. NPUT .EQ. -2 .OR. NPUT .EQ. -3 .OR. NPUT .EQ. -4 .OR. NPUT .EQ. -5 .OR. NPUT .EQ. -6) THEN
!           kijken welk punt bij deleten en bij oppakken
            CALL isSplinePoint(XP, YP, RCIR, MP, NP)
         ENDIF
         IF ( NPUT .EQ. 0 .AND. MP .NE. 0) THEN
!           punt oppakken
            CALL MOVABS(XP,YP)
            CALL SETCOL(0)
            CALL CIR(RCIR)
            CALL IGRFILLPATTERN(0,0,0)
            CALL SETCOL(NCOL)
            CALL CIR(RCIR)
            CALL IGRFILLPATTERN(4,0,0)
            NPUT = 1
         ELSE IF (NPUT .EQ. 1 .AND. MP .NE. 0) THEN
!           punt neerzetten
            CALL saveSplines()
            CALL plotSplines(MP,     MP,      0)
            call setSplinePoint(MP, NP, XP, YP)
            CALL plotSplines(MP,     MP,   NCOL)
            NPUT   = 0
         ELSE IF (NPUT .EQ. -1) THEN
!           punt toevoegen
            CALL saveSplines()
            CALL plotSplines(MP,     MP,      0)
            call insertSplinePoint(mp, np, xp, yp)
            CALL plotSplines(MP,     MP,   NCOL)
         ELSE IF ( NPUT .EQ. -2 .AND. MP .NE. 0) THEN
!           punt deleten
            CALL saveSplines()
            IIJ = 68
            CALL SETCOL(0)
            CALL MOVABS(XP,YP)
            IF (MP .EQ. 1) THEN
               CALL CIR(1.4*RCIR)
            ELSE
               CALL CIR(RCIR)
            ENDIF
            CALL plotSplines(MP,     MP,      0)
            call delSplinePoint(mp, np)
            CALL plotSplines(MP,     MP,   NCOL)
         ELSE IF ( NPUT .EQ. -3 .AND. MP .NE. 0) THEN
!           hele spline deleten
            CALL saveSplines()
            IIJ = 68
            CALL SETCOL(0)
            CALL MOVABS(XP,YP)
            IF (MP .EQ. 1) THEN
               CALL CIR(1.4*RCIR)
            ELSE
               CALL CIR(RCIR)
            ENDIF
            CALL plotSplines(MP,     MP,      0)
            call delSpline(mp)
            CALL plotSplines(MP,     MP,   NCOL)
         ELSE IF ( ( NPUT .eq. -4 .or. NPUT .eq. -5 ) .AND. MP .NE. 0) THEN
!           move or copy whole spline, get spline
            call savesplines()
            call setcol(0)
            call movabs(xp,yp)
            if ( mp .eq. 1 ) then
               call cir(1.4*rcir)
            else
               call cir(rcir)
            end if
            NPUT = 10*NPUT-1  ! -41 .or. -51
         ELSE IF ( NPUT .eq. -41 .AND. MP .NE. 0) THEN
!           move whole spline, put spline
            CALL plotSplines(MP,     MP,      0)
            call movespline(mp, np, xp, yp)
            CALL plotSplines(MP,     MP,   NCOL)
            NPUT = -4
         ELSE IF ( NPUT .eq. -51 .AND. MP .NE. 0) THEN
!           copy whole spline, put spline
            CALL plotSplines(MP,     MP,      NCOL)   ! plot original spline
            call copyspline(mp, np, xp, yp)
            CALL plotSplines(MP,     MP,   NCOL)
            NPUT = -5
          ELSE IF ( NPUT .eq. -6 .AND. MP .NE. 0) THEN
!           snap spline to landboundary
            CALL plotSplines(MP,     MP,      0)
            call snap_spline(MP)
            CALL plotSplines(MP,     MP,   NCOL)
            MP = 0
         ENDIF
         call dispnode2(mp, np)
      ELSE IF (KEY .EQ. 22) THEN
!        ENTER KEY
         IF (NPUT .EQ. -1 .AND. NP .GE. 2) THEN
            MP = 0
            NP = 0
         ENDIF
      ELSE IF (KEY .EQ. 23) THEN
!        ESC
         CALL restoreSplines()
         KEY = 3
      ELSE IF (KEY .EQ. 73 .OR. KEY .EQ. 73+32) THEN
         IF (NPUT .NE. 1) THEN
!           kijken welk punt dit is t.b.v insert mode (I)
            CALL isSplinePoint(XP, YP, RCIR, MP, NP)
            if (mp/=0 .and. np/=0) then
                ! Point was found, now highlight it temporarily on screen.
                CALL MOVABS(XP,YP)
                CALL SETCOL(0)
                CALL CIR(RCIR)
                CALL IGRFILLPATTERN(0,0,0)
                CALL SETCOL(NCOL)
                CALL CIR(RCIR)
                CALL IGRFILLPATTERN(4,0,0)
            endif
         ENDIF
         NPUT = -1
      ELSE IF (KEY .EQ. 8) THEN    ! Backspace KEY
!        delete all splines (within polygon if any) and stay in previous mode.
         call saveSplines()
         call deleteSelectedSplines()
         key = 3
      ELSE IF (KEY .EQ. 68 .OR. KEY .EQ. 68+32) THEN
!        delete mode losse punten (D)
         NPUT = -2
      ELSE IF (KEY .EQ. 88 .OR. KEY .EQ. 88+32) THEN
!        delete mode hele splines (X)
         NPUT = -3
      ELSE IF (KEY .EQ. 77 .OR. KEY .EQ. 77+32) THEN
!        move whole spline (M)
         NPUT = -4
      ELSE IF (KEY .EQ. 67 .OR. KEY .EQ. 67+32) THEN
!        copy whole spline (C)
         NPUT = -5
      ELSE IF (KEY .EQ. 76 .OR. KEY .EQ. 76+32) THEN
!        snap whole spline to land (L)
         NPUT = -6
      ELSE IF (KEY .EQ. 82 .OR. KEY .EQ. 82+32 .AND. NPUT .NE. 1) THEN
!        replace mode, maar niet bij zetten (R)
         NPUT =  0
      ELSE IF (KEY .EQ. 98) THEN
!        b RINGS BELL
         CALL KTEXT(' B Rings Bell',2,6,11)
         CALL OKAY(0)
      ENDIF
!
      GOTO 10
!
      END subroutine editSplines
