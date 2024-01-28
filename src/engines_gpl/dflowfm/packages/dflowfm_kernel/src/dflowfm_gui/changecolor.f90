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

!----------------------------------------------------------------------
! subroutines from rest.F90
!----------------------------------------------------------------------
      SUBROUTINE CHANGECOLOR(XP,YP)
      use unstruc_colors
      implicit none
      double precision :: dv
      integer :: ic
      integer :: jaauto
      integer :: key
      integer :: n1
      integer :: n2
      integer :: n3
      integer :: ncols
      integer :: nie
      integer :: nis
      integer :: nlevel
      integer :: numcol
      integer :: nv
      double precision :: val
      double precision :: vmax
      double precision :: vmin
      double precision :: xp
      double precision :: yp
      CHARACTER TEX*26, WRDKEY*40

      COMMON /DEPMAX/ VMAX,VMIN,DV,VAL(256),NCOLS(256),NV,NIS,NIE,JAAUTO
      COMMON /HELPNOW/ WRDKEY,NLEVEL

      INTEGER NCL(3)

      IC = 1

      CALL IMOUSECURSORHIDE()
      CALL DISPUT(35)

      CALL GETCOLORNUMBER(XP,YP,NUMCOL,N1,N2,N3)
      NCL(1) = N1
      NCL(2) = N2
      NCL(3) = N3

      CALL SETCOL(NUMCOL)
      CALL DISVALCOLORS (NUMCOL,NCL(1),NCL(2),NCL(3),IC)

   20 CONTINUE

      CALL INKEYEVENT(KEY)

      IF (KEY .EQ. 131) THEN
         IC = IC - 1
         IF (IC .EQ. 0) IC = 3
      ELSE IF (KEY .EQ. 130) THEN
         IC = IC + 1
         IF (IC .EQ. 4) IC = 1
      ELSE IF (KEY .EQ. 128) THEN
         NCL(IC) = MIN(255,NCL(IC) + 1)
         CALL IGRPALETTERGB(NUMCOL,NCL(1),NCL(2),NCL(3))
      ELSE IF (KEY .EQ. 129) THEN
         NCL(IC) = MAX(0  ,NCL(IC) - 1)
         CALL IGRPALETTERGB(NUMCOL,NCL(1),NCL(2),NCL(3))
      ELSE IF (KEY .EQ. 171) THEN
         CALL HELP(WRDKEY,3)
      ELSE IF (KEY .EQ. 13 .OR. KEY .GE. 251 .AND. KEY .LE. 253) THEN
         CALL ORGLOCATOR(XP,YP)
         CALL IMOUSECURSORSHOW()
         RETURN
      ELSE IF (KEY .EQ. 27) THEN
         CALL IGRPALETTERGB(NUMCOL,N1,N2,N3)
         CALL ORGLOCATOR(XP,YP)
         CALL IMOUSECURSORSHOW()
         RETURN
      ENDIF

      CALL SETCOL(NUMCOL)
      CALL DISVALCOLORS(NUMCOL,NCL(1),NCL(2),NCL(3),IC)
      CALL ALLCOLOURS()

      GOTO 20
      END
