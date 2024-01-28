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

      SUBROUTINE CLOSENETBNDLINK(XP1,YP1,N1)
      use m_netw
      use geometry_module, only: dlinedis
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: n1
      double precision :: xp1
      double precision :: yp1

      double precision :: dismin
      integer :: ja
      integer :: k1
      integer :: k2
      integer :: l
      double precision :: xa,ya,xb,yb,dis,xn,yn

      N1 = 0
      DISMIN = 9E+33
      DO L = 1,NUML
         IF (LNN(L) == 1) THEN
            K1 = KN(1,L) ; K2 = KN(2,L)
            XA = XK(K1)
            YA = YK(K1)
            XB = XK(K2)
            YB = YK(K2)
            CALL dLINEDIS(XP1,YP1,XA,YA,XB,YB,JA,DIS,XN,YN, jsferic, jasfer3D, dmiss)
            IF (JA .EQ. 1) THEN
               IF (DIS .LT. DISMIN) THEN
                  N1 = L
                  DISMIN = DIS
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      END SUBROUTINE CLOSENETBNDLINK
