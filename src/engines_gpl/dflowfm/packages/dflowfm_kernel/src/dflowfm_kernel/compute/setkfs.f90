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

 subroutine setkfs()                                 ! set kfs
 use m_flow
 use m_flowgeom
 use m_flowtimes

 implicit none

 integer :: L
 integer :: n, kb, ki, ndn

 kfs = 0

 ! open all grid points with positive lateral inflow
 do ndn = 1, ndx
   if (qin(ndn)>1d-12) then
     kfs(ndn) = 1
   endif
 enddo

 if (ivariableteta<=1) then                          ! fully implicit and teta=constant

    do L=1,lnx                                       ! implicit points
       if (hu(L)>0d0) then                            ! if you want hs==0 in dry points, you need hu>epshu here
           kfs(ln(1,L))=1
           kfs(ln(2,L))=1
       endif
    enddo

 else                                                ! set kfs ic. teta; 0=not, 1 =impl, 2 = expl

    do L=1,lnx                                       ! explicit points
       if (hu(L)>0d0) then
           if (teta(L) == 0) then
              kfs(ln(1,L))=2
              kfs(ln(2,L))=2
           else if (teta(L) > 0) then
              kfs(ln(1,L))=1                         ! todo: or bnd, randjes ook altijd impliciet
              kfs(ln(2,L))=1
          endif
       endif
    enddo

 endif

! water-level Neumann boundaries: add boundary cells whose corresponding internal cell is wet (but boundary face is inactive)
 do n=1, nbndz
    kb = kbndz(1,n)
    ki = kbndz(2,n)
    if ( kfs(ki).eq.1 ) then
         kfs(kb) = 1
    end if
 end do

 ! velocity boundaries: Neumann water-level boundaries are applied
 do n=1,nbndu
    kb = kbndu(1,n)
    ki = kbndu(2,n)
    if ( kfs(ki).eq.1 ) then
       kfs(kb) = 1
    end if
 end do

 end subroutine setkfs
