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

   SUBROUTINE CLOSEIN(XA,YA,INNUMP,KIN,NKIN,KK)  ! KK IS HET MEEST DICHTBIJ GELEGEN POINT VAN INNUMP
   use m_netw
   implicit none
   double precision :: xa
   double precision :: ya
   integer :: innump
   INTEGER :: KIN(NKIN)
   integer :: nkin
   integer :: kk

   double precision :: dx
   double precision :: dy
   integer :: k
   integer :: k1
   integer :: nn
   double precision :: ra
   double precision :: ramin
   RAMIN = 1E30

   KK = 0
   DO NN = 1, netcell(INNUMP)%N
      K1 = netcell(INNUMP)%NOD(NN)
      DO K = 1,NKIN
         IF (KIN(K) == K1) THEN
            DX = XK(K1) - XA ; DY = YK(K1) - YA
            RA = SQRT(DX*DX + DY*DY)
            IF ( RA < RAMIN ) THEN
               RAMIN = RA
               KK    = K1
            ENDIF
         ENDIF
      ENDDO
   ENDDO

   RETURN
   END SUBROUTINE CLOSEIN
