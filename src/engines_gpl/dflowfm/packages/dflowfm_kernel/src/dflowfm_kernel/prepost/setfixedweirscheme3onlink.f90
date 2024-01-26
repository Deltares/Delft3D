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

subroutine setfixedweirscheme3onlink(L)
use m_flowgeom
use m_flow
implicit none
integer :: L, nn, n12,kk,LL

teta(L) = 1d0

if (iadv(L) .ne. 24 .and. iadv(L) .ne. 25) then                      ! no change in advection for Tabellenboek and Villemonte
   do nn  = 1,2
      n12 = ln(nn,L)
      do kk  = 1,nd(n12)%lnx                                         ! and flag non-21 links to perot incoming only
          LL = iabs( nd(n12)%ln(kk) )
          if ( iadv(LL) < 21 .or. iadv(LL) > 25) then
               iadv(LL) = 4
          endif
          teta(LL) = 1d0
      enddo
   enddo
endif

end subroutine setfixedweirscheme3onlink
