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

  SUBROUTINE ISNODE2(KP, XP, YP, ZP)  ! X,Y,Z MOETEN ALLEN KLOPPEN
  use m_netw
  use m_wearelt
  implicit none
  integer :: KP
  double precision :: XP, YP, ZP

  double precision :: eps
  integer :: jav
  integer :: jview
  integer :: k
  double precision :: xyz

  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  KP  = 0
  EPS = 0.01d0*RCIR

  DO K = NUMK,1,-1
     IF (ABS(XK(K)-XP) .LT. EPS .AND. ABS(YK(K)-YP) .LT. EPS .AND. ABS(ZK(K)-ZP) .LT. EPS) THEN
         KP = K
         RETURN
     ENDIF
  ENDDO
  RETURN
  END SUBROUTINE ISNODE2
