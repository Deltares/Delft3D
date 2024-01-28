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

   SUBROUTINE SELLLINKSINPOL(LIN,N)
   use m_netw
   use m_missing, only: dmiss, jins
   use geometry_module, only: pinpok
   implicit none
   integer :: N
   INTEGER :: LIN(N)

   integer :: in
   integer :: in2
   integer :: k1
   integer :: k2
   integer :: l
   double precision :: xp1
   double precision :: xp2
   double precision :: xplmax
   double precision :: xplmin
   double precision :: yp1
   double precision :: yp2
   double precision :: yplmax
   double precision :: yplmin

   IF (NPL < 3) THEN
      LIN = 1
   ELSE
      CALL MINMAXPOL(XplMIN, YplMIN, XplMAX, YplMAX)
      DO L  = 1,NUML
         K1 = KN(1,L) ; Xp1 = XK(K1)   ; Yp1 = yK(K1)
         K2 = KN(2,L) ; Xp2 = XK(K2)   ; Yp2 = yK(K2)
         IF (Xp1 >= XplMIN .AND. Xp1 <= XplMAX .AND. Yp1 >= YplMIN .AND. Yp1 <= YplMAX  .AND.   &
             Xp2 >= XplMIN .AND. Xp2 <= XplMAX .AND. Yp2 >= YplMIN .AND. Yp2 <= YplMAX ) THEN
            CALL PINPOK(Xp1, Yp1, NPL, XPL, YPL, IN, jins, dmiss)
            CALL PINPOK(Xp2, Yp2, NPL, XPL, YPL, IN2, jins, dmiss)
            LIN(L) = in*in2
         ELSE
            LIN(L) = 0
         ENDIF
      ENDDO
   ENDIF
   END SUBROUTINE SELLLINKSINPOL
