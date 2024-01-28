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

  SUBROUTINE COPYPOLTo1Dnet()
  use m_polygon
  USE M_netw
  USE M_MISSING
  use network_data, only: kn3typ
  implicit none

  integer :: k, L, kn3o

  kn3o = kn3typ ; kn3typ = 1

  ! CALL INCREASENETW(NUMK+NPL, NUML+NPL-1)
  DO K = 2,NPL

     if (xpl(k) .ne. dmiss .and. xpl(K-1) .ne. dmiss) then
         call addnetlink(xpl(k-1), ypl(k-1), xpl(k), ypl(k), L)
     endif

  ENDDO

  kn3typ = kn3o
  CALL DELPOL()
  RETURN
  END SUBROUTINE COPYPOLTo1Dnet
