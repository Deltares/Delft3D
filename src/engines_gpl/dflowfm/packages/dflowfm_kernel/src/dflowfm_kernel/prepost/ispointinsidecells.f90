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

 subroutine ispointinsidecells( xz, yz, nn )         ! check if certain point is inside other cells

 use m_netw
 use geometry_module, only: pinpok
 use m_missing, only: jins, dmiss

 implicit none
 double precision :: xz, yz, x(10), y(10)
 integer          :: nn

 ! locals
 integer          :: m,n,k

 do n = 1,nump
    do m = 1,netcell(n)%n
       k = netcell(n)%NOD(m)
       x(m) = xk(k)
       y(m) = yk(k)
    enddo
    call pinpok(xz,yz,netcell(n)%n,x,y,nn, jins, dmiss)
    if (nn .ne. 0) then
       nn = n ; return
    endif
 enddo

 end subroutine ispointinsidecells
