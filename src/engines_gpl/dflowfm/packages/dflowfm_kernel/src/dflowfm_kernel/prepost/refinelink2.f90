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

  SUBROUTINE REFINELINK2(L12,K12)
  use m_netw
  use gridoperations
  implicit none
  integer :: L12,K12

  integer :: k1
  integer :: k2
  integer :: lnu
  DOUBLE PRECISION :: XM, YM

  K1 = KN(1,L12) ; KC(K1) = 5
  K2 = KN(2,L12) ; KC(K2) = 5

  KN(1,L12) = 0 ; KN(2,L12) = 0

  XM = 0.5D0*(XK(K1) + XK(K2))
  YM = 0.5D0*(YK(K1) + YK(K2))

  CALL DSETNEWPOINT(XM,YM,K12)
  CALL NEWLINK(K1 ,K12,lnu) ! fast version without refinement
  CALL NEWLINK(K12,K2 ,lnu) ! fast version without refinement
  END SUBROUTINE REFINELINK2
