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

   SUBROUTINE DRIETWEE(XD,YD,ZD,X,Y,Z)
   implicit none
   integer :: jav
   integer :: jview
   double precision :: xyz
   DOUBLE PRECISION XD,YD,ZD,X,Y,Z
   COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
   IF (JVIEW .EQ. 1) THEN        ! NORMAL
      X = XD
      Y = YD
      Z = ZD
   ELSE IF (JVIEW .EQ. 2) THEN   ! FROM LEFT
      X = ZD
      Y = YD
      Z = XD
   ELSE IF (JVIEW .EQ. 3) THEN   ! FROM TOP
      X = XD
      Y = -ZD
      Z = YD
   ELSE IF (JVIEW .EQ. 4) THEN
      !    CALL DVIEW(XD,YD,-ZD,X,Y,Z)
      CALL DVIEW(XD,YD,-ZD,X,Y,Z)
   ELSE !In all other cases (e.g. when HOWTOVIEW is not set, e.g. in the gridgeom library)
      x = xd
      y = yd
      z = zd
   ENDIF
   RETURN
   END SUBROUTINE DRIETWEE
