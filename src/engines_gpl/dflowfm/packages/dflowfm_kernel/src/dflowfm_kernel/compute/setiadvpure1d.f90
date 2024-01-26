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

! update iadvec flag if Pure1D is switched on
subroutine setiadvpure1D(jaPure1D)
use m_flowgeom, only: lnx1d, lnxi, lnx, ln, kcu, iadv
use m_flowparameters, only : iadvec1D
use network_data, only: kc
! integer, dimension(ndx) :: kc !< temporary integer array for determining node type

implicit none

integer, intent(in) :: jaPure1D !< flag specifying type of 1D discretization

integer :: iadv_Pure1D          !< iadvec flag to be used for Pure1D links
integer :: L                    !< link index
integer :: n1                   !< index of from-node
integer :: n2                   !< index of to-node

if (jaPure1D == 0) then
   ! no Pure1D return
   return
   
elseif (jaPure1D < 3) then
   ! stay close to the default behaviour
   iadv_Pure1D = 103
   
else
   ! switch to SOBEK type 1D advection
   iadv_Pure1D = 104

endif

kc = 0
do L = 1,lnx
   n1 = ln(1,L)
   n2 = ln(2,L)
   if (iabs(kcu(L)) == 1) then
      kc(n1)  = kc(n1) + 1
      kc(n2)  = kc(n2) + 1
   endif
enddo

do L = 1,lnx1D
   n1 = ln(1,L); n2 = ln(2,L)
   if (iadv(L) == iadvec1D .or. &
       & (iadv(L) == 6 .and. kc(n1) == 2 .and. kc(n2) == 2)) then
      iadv(L) = iadv_Pure1D
   endif
enddo

do L  = lnxi+1, lnx
   n2 = ln(2,L)
   if (iabs(kcu(L)) == 1 .and. kc(n2) ==2 ) then
      iadv(L) = iadv_Pure1D
   endif
enddo

end subroutine setiadvpure1D
