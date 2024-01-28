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

      SUBROUTINE LOADBITMAP(FILNAM)
      USE M_BITMAP
      USE M_WEARELT
      use string_module, only: find_first_letter, find_first_char
      implicit none
      integer :: ierr
      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: minp
      integer :: ndraw
      integer :: num
      integer :: numbersonline
      LOGICAL JAWEL
      INTEGER INFO(10)
      COMMON /DRAWTHIS/  ndraw(50)
      CHARACTER FILNAM*(*),REC*132

      K1 = find_first_char(FILNAM)
      K2 = len_trim(FILNAM)
      CALL IGRFILEINFO(FILNAM(K1:K2),info,3)
      MXP = INFO(2)
      NXP = INFO(3)

      XB = 0
      YB = 0

      NDRAW(26) = 0
      ALLOCATE(IPIX(1),STAT=IERR)
      IF (MXP .GE. 1 .AND. NXP .GE. 1) THEN
         DEALLOCATE(IPIX)
         ALLOCATE(IPIX(MXP*NXP),STAT=IERR)
!        CALL AERR('IPIX(MXP*NXP)',IERR,MXP*NXP)
         IF (IERR .NE. 0) THEN
            CALL QNERROR('BITMAP TOO LARGE',' ',' ')
         ELSE
            CALL IGRLOADIMAGEDATA(FILNAM(K1:K2),IPIX)
         ENDIF

         L = INDEX(FILNAM,'.')
         INQUIRE(FILE = FILNAM(K1:L)//'xyx', EXIST = JAWEL)

         IF (JAWEL) THEN
            CALL OLDFIL(MINP,FILNAM(K1:L)//'xyx')
            READ(MINP,'(A)',END=999) REC
            NUM = NUMBERSONLINE(REC)
            IF (NUM .EQ. 4) THEN
               READ(REC,*,ERR=888) XP(1),YP(1),XP(3),YP(3)
               XP(2) = XP(3)
               YP(2) = YP(1)
               XP(4) = XP(1)
               YP(4) = YP(3)
            ELSE IF (NUM .EQ. 3) THEN
               READ(REC,*,ERR=777) XP(1),YP(1),XP(3)
               YP(3) = YP(1) + ( XP(3)-XP(1) )*dble(NXP)/dble(MXP)
               XP(2) = XP(3)
               YP(2) = YP(1)
               XP(4) = XP(1)
               YP(4) = YP(3)
            ELSE
               IF (FIND_FIRST_LETTER(REC) .EQ. 1) THEN
                  READ(MINP,'(A)',END=999) REC
                  DO K = 1,4
                     READ(MINP,'(A)',END=999) REC
                     IF (NUMBERSONLINE(REC) .EQ. 2) THEN
                        READ(REC,*,ERR=666) XP(K),YP(K)
                     ELSE IF (NUMBERSONLINE(REC) .EQ. 4) THEN
                        READ(REC,*,ERR=555) XP(K),YP(K),XB(K),YB(K)
                        YB(K) = NXP - YB(K) + 1
                     ENDIF
                  ENDDO
               ELSE
                  CALL QNERROR('Cannot Read *.xyx File', ' ',' ')
               ENDIF
            ENDIF
            call doclose(MINP)
         ELSE
            XP(1) = 0
            YP(1) = 0
            XP(3) = MXP
            YP(3) = NXP
            XP(2) = XP(3)
            YP(2) = YP(1)
            XP(4) = XP(1)
            YP(4) = YP(3)
         ENDIF
         NDRAW(26) = 1
      ENDIF

      IF (XB(1) .EQ. 0) XB(1) = -0.5d0
      IF (YB(1) .EQ. 0) YB(1) = -0.5d0
      IF (XB(2) .EQ. 0) XB(2) = MXP+0.5d0
      IF (YB(2) .EQ. 0) YB(2) = -0.5d0
      IF (XB(3) .EQ. 0) XB(3) = MXP+0.5d0
      IF (YB(3) .EQ. 0) YB(3) = NXP+0.5d0
      IF (XB(4) .EQ. 0) XB(4) = -0.5d0
      IF (YB(4) .EQ. 0) YB(4) = NXP+0.5d0

      RETURN

  999 CALL QNEOFERROR(MINP)
      call doclose(MINP)
      RETURN

  998 CALL QNREADERROR('Trying to Read X1,Y1,X2,XY2,X3,Y3,X4,Y4 but', 'get:'//REC,MINP)
      call doclose(MINP)
      RETURN

  888 CALL QNREADERROR('Trying to Read X1,Y1,X3,Y3 but Get:',REC,MINP)
      call doclose(MINP)
      RETURN

  777 CALL QNREADERROR('Trying to Read X1,Y1,X3 but Getting',REC,MINP)
      call doclose(MINP)
      RETURN

  666 CALL QNREADERROR('Trying to Read four lines X,Y but Getting',REC,MINP)
      call doclose(MINP)
      RETURN

  555 CALL QNREADERROR('Trying to Read four lines X,Y,MP,NP but Get',REC,MINP)
      call doclose(MINP)
      RETURN
      END
