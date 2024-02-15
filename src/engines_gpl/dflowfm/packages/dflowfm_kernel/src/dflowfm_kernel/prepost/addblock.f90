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

  SUBROUTINE ADDBLOCK(X,Y,Z,JANET)
  use gridoperations
  implicit none
  integer :: ja
  integer :: jav
  integer :: jview
  integer :: k
  integer :: n
  double precision :: xyz
  double precision :: X(4), Y(4), Z
  integer :: JANET

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  INTEGER KK(8)
  DO K = 1,8
     N = K
     IF (K .EQ. 5) THEN
        Z = Z + 1D0
     ENDIF
     IF (K .GE. 5) N = K - 4
     CALL ISNODE2(KK(K), X(N), Y(N), Z)
     IF (KK(K) .LE. 0) THEN
        CALL GIVENEWNODENUM(KK(K))
        XYZ = Z
        CALL SETPOINT(X(N),Y(N),Z,KK(K))
     ENDIF
  ENDDO
  CALL ADDELEM(KK(1),KK(2),JA)
  CALL ADDELEM(KK(2),KK(3),JA)
  CALL ADDELEM(KK(3),KK(4),JA)
  CALL ADDELEM(KK(4),KK(1),JA)

  CALL ADDELEM(KK(5),KK(6),JA)
  CALL ADDELEM(KK(6),KK(7),JA)
  CALL ADDELEM(KK(7),KK(8),JA)
  CALL ADDELEM(KK(8),KK(5),JA)

  CALL ADDELEM(KK(1),KK(5),JA)
  CALL ADDELEM(KK(2),KK(6),JA)
  CALL ADDELEM(KK(3),KK(7),JA)
  CALL ADDELEM(KK(4),KK(8),JA)

  CALL ADDELEM(KK(1),KK(3),JA)
  CALL ADDELEM(KK(5),KK(7),JA)
  CALL ADDELEM(KK(4),KK(5),JA)
  CALL ADDELEM(KK(3),KK(6),JA)
  CALL ADDELEM(KK(4),KK(7),JA)
  CALL ADDELEM(KK(1),KK(6),JA)
  RETURN
  END SUBROUTINE ADDBLOCK
