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

      SUBROUTINE GETCOLORNUMBER(XP,YP,NUMCOL,N1O,N2O,N3O)
      implicit none
      integer :: i
      integer :: n1
      integer :: n1o
      integer :: n2
      integer :: n2o
      integer :: n3
      integer :: n3o
      integer :: numcol
      double precision :: xp
      double precision :: yp
      CALL IGRGETPIXELRGB(real(XP),real(YP),N1O,N2O,N3O)
      do I = 0,255
         CALL SETCOL(I)
         CALL PTABS(XP,YP)
         CALL IGRGETPIXELRGB(real(XP),real(YP),N1,N2,N3)
         IF (N1 .EQ. N1O .AND. N2 .EQ. N2O .AND. N3 .EQ. N3O) THEN
            NUMCOL = I
            CALL DISVALCOLORS(NUMCOL,N1,N2,N3,1)
            RETURN
         ENDIF
      end do
      RETURN
      END
