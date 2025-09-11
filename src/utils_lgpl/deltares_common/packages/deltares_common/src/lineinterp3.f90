!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
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

 subroutine lineinterp3(xx, yy, zz, vv, ktx, x,y,z,v,n)
 implicit none
 integer          :: ktx, n, k, ip
 double precision :: xx(0:ktx), yy(0:ktx), zz(0:ktx), vv(0:ktx)
 double precision :: x(0:n)   , y(0:n)   , z(0:n)   , v(0:n)
 double precision :: a, b


 ip = 0
 do k = 0, ktx
    do while ( xx(k) > x(ip+1) .and. ip < n - 1 )
       ip = ip + 1
    enddo
    if ( xx(k) <= x(ip) ) then
       yy(k)  =   y(ip)
       zz(k)  =   z(ip)
       vv(k)  =   v(ip)
    else if ( xx(k) > x(ip) .and. xx(k) <= x(ip+1) ) then
       a     = ( xx(k) - x(ip) ) / max( 1d-4 , x(ip+1) - x(ip) ) ; b = 1d0 - a
       yy(k) = b*y(ip) + a*y(ip+1)
       zz(k) = b*z(ip) + a*z(ip+1)
       vv(k) = b*v(ip) + a*v(ip+1)
    else
       yy(k) = y(ip+1)
       zz(k) = z(ip+1)
       vv(k) = v(ip+1)
    endif
 enddo

 end subroutine lineinterp3
