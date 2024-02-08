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

   SUBROUTINE FINDK(     XL,     YL,    ZL,  KV )
   use m_netw
   implicit none
   double precision :: XL, YL, ZL
   integer :: KV
   integer :: k

   double precision :: RMIN, R, &
                       DX, DY, DZ
   RMIN  = 99D+20

   KV = 0
   DO K = 1,NUMK
      IF (XK(K) .NE. 0) THEN
         DX = XL - XK(K)
         DY = YL - YK(K)
         DZ = ZL - ZK(K)
         R  = DX*DX + DY*DY  + DZ*DZ
         IF (R .LT. RMIN) THEN
            RMIN = R
            KV   = K
         ENDIF
      ENDIF
   ENDDO

   RETURN
   END SUBROUTINE FINDK
