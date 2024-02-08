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

   SUBROUTINE DSELECTINP(X,Y,N,KIN)
   USE M_POLYGON
   use m_missing, only: dmiss, jins
   use geometry_module, only: dpinpok
   implicit none
   integer :: N
   DOUBLE PRECISION :: X(N), Y(N), ZK
   INTEGER          :: KIN(N)

   integer :: in
   integer :: k
   double precision :: xmaxp
   double precision :: xminp
   double precision :: ymaxp
   double precision :: yminp
   ZK = 1D0

   IF (NPL < 3) THEN
      KIN  = 1
   ELSE
      CALL MINMAXPOL(XMINp, YMINp, XMAXp, YMAXp)
      DO K  = 1,N
         IN = 0
         IF (X(K) >= XMINp .AND. X(K) <= XMAXp .AND. Y(K) >= YMINp .AND. Y(K) <= YMAXp ) THEN
            CALL DPINPOK(X(K), Y(K), ZK, NPL, XPL, YPL, IN, jins, dmiss)
         ENDIF
         KIN(K) = IN
      ENDDO
   ENDIF
   END SUBROUTINE DSELECTINP
