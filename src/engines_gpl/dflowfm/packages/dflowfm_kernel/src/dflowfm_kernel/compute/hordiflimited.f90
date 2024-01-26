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

double precision function hordiflimited(LL,L,rho_,nx)  ! used to compute a strict horizontal gradient see vkester,stelling 1994
use m_flow
use m_flowgeom
implicit none

integer                       :: L,LL,nx
double precision, intent (in) :: rho_(nx)

integer          :: k1,k2,k2u,k1u,n1,n2,kb1,kb2,kt1,kt2
double precision :: alfu, rho1, rho2, drho1, drho2, drho3, drho4

k1  = ln(1,L)
k2  = ln(2,L)

k2u = k2
n2  = ln(2,LL) ; kb2 = kbot(n2); kt2 = ktop(n2)

if      (zws(k1) < zws(k2) .and. k2u > kb2) then

   do while ( zws(k2u-1) > zws(k1) .and. k2u > kb2)
      k2u = k2u - 1
   enddo

   alfu = zws(k1) - zws(k2u-1) / ( zws(k2u) - zws(k2u-1) )
   rho2 = alfu*rho_(k2u) + (1d0-alfu)*rho_(k2u-1)

else if (zws(k1) > zws(k2) .and. k2u < kt2  ) then

   do while ( zws(k2u) < zws(k1) .and. k2u < kt2)
      k2u = k2u + 1
   enddo

   alfu = zws(k1) - zws(k2u-1) / ( zws(k2u) - zws(k2u-1) )
   rho2 = alfu*rho_(k2u) + (1d0-alfu)*rho_(k2u-1)

else

   rho2 = rho(k2)

endif


k1u = k1
n1  = ln(1,LL) ; kb1 = kbot(n1); kt1 = ktop(n1)

if      (zws(k2) < zws(k1) .and. k1u > kb1) then

   do while ( zws(k1u-1) > zws(k2) .and. k1u > kb1)
      k1u = k1u - 1
   enddo

   alfu = zws(k2) - zws(k1u-1) / ( zws(k1u) - zws(k1u-1) )
   rho1 = alfu*rho_(k1u) + (1d0-alfu)*rho_(k1u-1)

else if (zws(k2) > zws(k1) .and. k1u < kt1  ) then

   do while ( zws(k1u) < zws(k2) .and. k1u < kt1)
      k1u = k1u + 1
   enddo

   alfu = zws(k2) - zws(k1u-1) / ( zws(k1u) - zws(k1u-1) )
   rho1 = alfu*rho_(k1u) + (1d0-alfu)*rho_(k1u-1)

else

   rho1 = rho_(k1)

endif

drho1 = rho2     - rho_(k1)
drho2 = rho_(k2) - rho1

if (limiterhordif == 1) then      ! minmod
   if (drho1*drho2 <=  0) then
      hordiflimited = 0d0
   else if ( abs(drho1) < abs(drho2) ) then
      hordiflimited = drho1
   else
      hordiflimited = drho2
   endif
elseif (limiterhordif == 2) then  ! monotonized central
   drho3 = rho_(k2) - rho_(k1)
   drho4 = 0.5d0*(drho1 + drho2)

   if (drho1*drho2 <=  0) then
      hordiflimited = 0d0
   else if ( abs(drho4) < abs(drho3) ) then
      hordiflimited = drho4
   else if ( min(abs(drho1),abs(drho2)) < abs(drho3) .and. abs(drho3) < max(abs(drho1),abs(drho2)) ) then
      hordiflimited = drho3
   else if (drho1 > 0d0) then
      hordiflimited =  min( abs(drho1),abs(drho2) )
   else
      hordiflimited = -min( abs(drho1),abs(drho2) )
   endif
endif

end function hordiflimited
