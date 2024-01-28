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

 subroutine pixcount(xs,ys,zs,jatel)

 USE M_FLOWGEOM
 use m_missing, only: jins, dmiss
 use geometry_module, only: pinpok

 implicit none

 double precision :: xs, ys, zs
 integer :: jatel

 double precision :: xmn, xmx, ymn, ymx
 integer :: nn, k, in
 integer, allocatable, save :: itel(:)
 double precision, allocatable, save :: ztel(:)

 if (jatel == 1) then
    if (.not. allocated (itel) ) then
       allocate(itel(ndx), ztel(ndx) ) ; itel = 0 ; ztel = 0
    endif

    do k  = 1,ndx
       xmn = minval(nd(k)%x) ; xmx = maxval(nd(k)%x)
       ymn = minval(nd(k)%y) ; ymx = maxval(nd(k)%y)
       if (xs <= xmx .and. xs >= xmn .and. ys <= ymx .and. ys >= ymn ) then
          nn = size(nd(k)%x)
          call PINPOK(Xs, Ys, Nn, nd(k)%x, nd(k)%y, IN, jins, dmiss)
          if (IN == 1) then
             itel(k) = itel(k) + 1
             ztel(k) = ztel(k) + zs
             return
          endif
       endif
    enddo
 else
    do k  = 1,ndx
       if (itel(k) .ne. 0) then
          bl(k) =  ztel(k) / dble( itel(k) )
       endif
    enddo
    if (allocated(itel) ) deallocate (itel, ztel)
 endif

 end subroutine pixcount
