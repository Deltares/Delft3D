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

  SUBROUTINE quadsTOTRI()
  use m_netw
  use gridoperations
  implicit none
  double precision :: a
  integer :: k0
  integer :: k1
  integer :: k2
  integer :: k3
  integer :: k4
  integer :: l
  integer :: l12
  integer :: np
  integer :: numtri
  double precision :: r
  DOUBLE PRECISION DLENGTH

  CALL FINDcells(4)  ! quads

  L = NUMTRI
  DO NP = 1,NUMP
     K1 = netcell(NP)%NOD(1)
     K2 = netcell(NP)%NOD(2)
     K3 = netcell(NP)%NOD(3)
     K4 = netcell(NP)%NOD(4)

     CALL FINDEL(K1,K2,L12)
     A = 0 ! EA(L12)
     R = DLENGTH(K1,K2)
     CALL CONNECT(K1,K3,1,A,R)

     L  = L + 1
     K0 = 1 + (L-1)*3
     KTRI(K0)   = K1
     KTRI(K0+1) = K2
     KTRI(K0+2) = K3

     L  = L + 1
     K0 = 1 + (L-1)*3
     KTRI(K0)   = K4
     KTRI(K0+1) = K1
     KTRI(K0+2) = K3
  ENDDO
  NUMTRI = K0+2

  RETURN
  END SUBROUTINE quadsTOTRI
