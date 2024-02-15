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

      SUBROUTINE putget_un(NUM,NWHAT,NPUT,NUMB,XP,YP,KEY)
      implicit none
      integer :: ja
      integer :: key
      integer :: ndraw
      integer :: nput
      integer :: num
      integer :: numb
      integer :: nwhat
      double precision :: xp
      double precision :: yp
      COMMON /DRAWTHIS/  ndraw(50)

!
      CALL DISPUT(NPUT)

!     IF (KEY .EQ. 3) THEN
         CALL MENUH(0,NUM,NWHAT)
         CALL BOTLIN(0,NUMB,KEY)
         CALL FRAMES(31)
!     ENDIF

!
   20 CONTINUE
      CALL READLOCATOR(XP,YP,KEY)
!
      IF (KEY .GE. 24 .AND. KEY .LE. 26) THEN
         CALL FKEYS(KEY)
         IF (KEY .EQ. 3) RETURN
      ELSE IF (KEY .EQ. 1) THEN
!        BOVEN
         JA = KEY
         CALL MENUH(JA,NUM,NWHAT)
         CALL BOTLIN(0,NUMB,KEY)
         IF (JA .NE. 0) RETURN
      ELSE IF (KEY .EQ. 2) THEN
!        ONDER
         JA = KEY
         CALL BOTLIN(JA,NUMB,KEY)
         IF (JA .NE. 0) RETURN
      ELSE IF (KEY .EQ. 90 .OR. KEY .EQ. 90+32) THEN
!        Z(oomin)
         CALL ZOOMIN(KEY,NPUT)
         RETURN
      ELSE IF (KEY .EQ. 65 .OR. KEY .EQ. 65+32) THEN
!        A(nchor)
         CALL ANCHOR(XP,YP)
       ELSE IF (KEY .EQ. 170 .OR. KEY .EQ. 80 .OR. KEY .EQ. 80+32) THEN
         NDRAW(10) = 1
         KEY = 3
         RETURN
      ELSE
         RETURN
      ENDIF
      GOTO 20
      END
