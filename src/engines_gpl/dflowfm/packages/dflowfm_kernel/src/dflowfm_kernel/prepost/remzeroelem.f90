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

  SUBROUTINE REMZEROELEM(LNU)
  use m_netw
  implicit none
  integer :: LNU

  integer :: k
  integer :: l

  NUML = NUML - 1        ! Administratie aanschuiven
  DO L = LNU,NUML
     KN(1,L) = KN(1,L+1)
     KN(2,L) = KN(2,L+1)
     KN(3,L) = KN(3,L+1)
  ENDDO

  DO K = 1,NUMK
     DO L = 1,NMK(K)
        IF (NOD(K)%LIN(L) .GT. LNU) NOD(K)%LIN(L) = NOD(K)%LIN(L) - 1
     ENDDO
  ENDDO

  RETURN
  END SUBROUTINE REMZEROELEM
