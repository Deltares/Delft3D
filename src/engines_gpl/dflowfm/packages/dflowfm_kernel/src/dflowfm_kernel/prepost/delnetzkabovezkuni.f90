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

  subroutine delnetzkabovezkuni()

  use m_netw
  USE M_MISSING
  use gridoperations

  implicit none
  integer :: k, kk, L, k2, jaweg

  do k = 1,numk
     if (zk(k) .ne. dmiss) then
        if (zk(k) > zkuni) then
            jaweg = 0
            do kk = 1,nmk(k)
               L  = nod(k)%lin(kk)
               k2 = kn(1,L) + kn(2,L) - k
               if (zk(k2) > zkuni .or. zk(k2) == dmiss) then
                  jaweg = jaweg + 1
               endif
            enddo
            if (jaweg == nmk(k) ) then
               xk(k) = dmiss ; yk(k) = dmiss; zk(k) = dmiss
            endif

        endif
     else if (zk(k) == dmiss) then
        xk(k) = dmiss ; yk(k) = dmiss; zk(k) = dmiss
     endif
  enddo

  call setnodadm(0)

  end subroutine delnetzkabovezkuni
