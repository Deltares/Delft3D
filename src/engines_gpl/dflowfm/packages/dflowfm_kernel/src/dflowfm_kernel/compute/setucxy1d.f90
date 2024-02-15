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

! =================================================================================================
! =================================================================================================
subroutine setucxy1D() ! give ucx,ucy magnitude of uc1D, jaPure1D

use m_flowgeom
use m_flow

implicit none
integer          :: n,LL,k2
double precision :: uxy

do n = 1,ndx
   if (uc1D(n) .ne. 0) then
      uxy    = sqrt( ucx(n)*ucx(n) + ucy(n)*ucy(n) )
      if (uxy > 0) then
          uxy    = abs(uc1D(n))/uxy
          ucx(n) = ucx(n)*uxy
          ucy(n) = ucy(n)*uxy
      endif
   endif
enddo

do LL = lnxi+1,lnx          ! bnd
    if (kcu(LL) == -1) then  ! 1D type link
        n = Ln(1,LL) ; k2 = Ln(2,LL)
        if (uc1D(k2) .ne. 0) then
            uxy    = sqrt( ucx(n)*ucx(n) + ucy(n)*ucy(n) )
            if (uxy > 0) then
                uxy    = abs(uc1D(n))/uxy
                ucx(n) = ucx(n)*uxy
                ucy(n) = ucy(n)*uxy
            endif
        endif
    endif
 enddo

 end subroutine setucxy1D
