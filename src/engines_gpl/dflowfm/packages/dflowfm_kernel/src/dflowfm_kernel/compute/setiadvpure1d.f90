!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

subroutine setiadvpure1D() ! set 103 on default 1D links if pure1D
use m_flowgeom
use m_flow
use m_netw, only: kc
implicit none
integer :: L, n1, n2

kc = 0
do L = 1,lnx
   n1 = ln(1,L); n2 = ln(2,L)
   if (iabs(kcu(L)) == 1) then
      kc(n1)  = kc(n1) + 1
      kc(n2)  = kc(n2) + 1
   endif
enddo

do L = 1,lnx1D
   n1 = ln(1,L); n2 = ln(2,L)
   if (iadv(L) == iadvec1D .or. iadv(L) == 6 .and. kc(n1) == 2 .and. kc(n2) == 2) then
      iadv(L) = 103  ! 103 = qucper (iadv=3)  + pure1D
   endif
enddo

do L  = lnxi+1, lnx
   n2 = ln(2,L)
   if (iabs(kcu(L)) == 1 .and. kc(n2) ==2 ) then
      iadv(L) = 103
   endif
enddo

end subroutine setiadvpure1D
