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

   SUBROUTINE DVIEW(XD,YD,ZD,X,Y,Z)
   use m_missing
   implicit none
   double precision :: ce
   integer :: i
   double precision :: vs
   double precision :: x0s
   double precision :: y0s
   ! GEEF perspectievische COORDINATEN
   ! xD,yD,zD                             :coordinaten te tekenen punt
   ! x0s,y0s                              :waar op scherm ligt kijklijn
   ! X,Y,Z                                :scherm coordinaten
   ! Vs                                   :viewing matrix na viema

   DOUBLE PRECISION XD,YD,ZD,X,Y,Z
   COMMON /VIEWMAT/ VS(4,4), X0S, Y0S
   DIMENSION CE(4)
   ! use z as zd temporarily (zet to zero when zd==dmiss)
   if (zd == dmiss) then
      z = 0
   else
      z = zd
   end if
   DO I = 1,3
      CE(I) = VS(I,1)*XD + VS(I,2)*YD + VS(I,3)*Z + VS(I,4)
   ENDDO
   Z  = CE(3)
   IF (Z .LT. 0) THEN
      Z = dmiss
   ELSE
      X = CE(1)/Z  + X0S
      Y = CE(2)/Z  + Y0S
   ENDIF
   END SUBROUTINE DVIEW
