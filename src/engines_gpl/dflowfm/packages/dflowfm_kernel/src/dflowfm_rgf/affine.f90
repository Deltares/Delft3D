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

      SUBROUTINE AFFINE(XX,YY,XG,YG,INI)
      USE M_BITMAP
      use string_module, only: find_first_letter
      implicit none
      integer :: ini
      logical :: jawel
      integer :: k
      integer :: minp
      integer :: numbersonline
      double precision :: xg4
      double precision :: xx4
      double precision :: yg4
      double precision :: yy4
      CHARACTER REC*132
	  DOUBLE PRECISION :: XX,YY,XG,YG
      XX4 = XX ; YY4 = YY

      IF (INI .EQ. 1) THEN

         INQUIRE(FILE = 'AFFINE'//'.xyx', EXIST = JAWEL)

         IF (JAWEL) THEN
            CALL OLDFIL(MINP, 'AFFINE'//'.xyx')
            READ(MINP,'(A)') REC
            IF (find_first_letter(REC) .EQ. 1) THEN
               READ(MINP,'(A)') REC
               DO K = 1,4
                  READ(MINP,'(A)') REC
                  IF (NUMBERSONLINE(REC) .EQ. 2) THEN
                     READ(REC,*) XP(K),YP(K)
                  ELSE IF (NUMBERSONLINE(REC) .EQ. 4) THEN
                     READ(REC,*) XP(K),YP(K),XB(K),YB(K)

                  ENDIF
               ENDDO
            ELSE
               CALL QNERROR('Cannot Read AFFINE.XYX File',' ',' ')
            ENDIF
            CALL DOCLOSE(MINP)
            CALL BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
            INI = 0
         ELSE
            CALL QNERROR ('NO AFFINE.XYX FILE FOUND', ' ',' ')
         ENDIF
      ENDIF

      CALL BILINXY(XP, YP, XB, YB, XX4, YY4, XG4, YG4, INI)
      XG = XG4
	  YG = YG4

      RETURN
	  END
