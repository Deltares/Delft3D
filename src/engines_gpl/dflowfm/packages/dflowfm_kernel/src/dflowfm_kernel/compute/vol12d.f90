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

 subroutine VOL12D(japerim)                                 ! and add area's and volumes of 1D and 2D links, japerim=1: also set conveyance
 use m_flowgeom
 use unstruc_channel_flow
 use m_oned_functions
 use m_storage
 use m_flow
 use m_missing
 use m_ship
 use m_VolumeTables

 implicit none

 integer           :: japerim

 integer           :: L, k1, k2, K, n, kk, kb, kt, nl1 , nl2, i, nstor, n1d
 integer           :: loopcount
 double precision  :: hh, slotsav, sl1, sl2
 type(t_storage), dimension(:), pointer :: stors

 nl1 = nonlin1D
 nl2 = nonlin2D
 sl1 = slotw1D
 sl2 = slotw2D

 if (japerim == 1 .or. nonlin > 0) then

   if (japerim == 0 .and. useVolumeTables) then
      ! Compute 1d volumes, using volume tables (still excl. 1D2D contributions)
      do n = ndx2d+1, ndx
         n1d = n - ndx2d
         vol1(n) = vltb(n1d)%getVolume(s1(n))
         a1(n)   = vltb(n1d)%getSurface(s1(n))
      enddo
      if (nonlin1D >= 2) then
         do n = ndx2d+1, ndx
            n1d = n - ndx2d
            vol1(n) = vol1(n) - vltb(n1d)%getVolumeDecreasing(s1m(n))
            a1m(n)  = vltb(n1d)%getSurfaceDecreasing(s1m(n))
         enddo
      endif
   end if

   nstor = network%stors%count
   if (japerim == 0 .and. nstor > 0 .and. .not. useVolumeTables) then
      stors => network%stors%stor
      do i = 1, nstor
         k1 = stors(i)%grid_point
         if (k1 > 0) then
            vol1(k1) = vol1(k1) + get_volume(stors(i), s1(k1))
            a1(k1)   = a1(k1)   + get_surface(stors(i), s1(k1))
         end if
      enddo
   endif

   ! NOTE: In order to speed up the calculation, the loop over all flow links is replaced
   !       by a loop over the wet flow links for the case the flow data has to be calculated.
   !       (JAPERIM == 1). When JAPERIM == 0, all flow links are considered in the loop (no speedup)
   !       Since for the calculation of the flow volumes the water depth at 
   !       either side of the flow link is used, this part cannot be skipped. Therefore it was 
   !       necessary to copy the loops over the flow links.
   if (japerim == 0) then
      loopcount = lnx1D
   else
      loopcount = wetLink2D-1
   endif
      
   do i = 1,loopcount                                  ! regular 1D links
      if (japerim == 0) then
         L = i
      else
         L = onlyWetLinks(i)
      endif
      
      if (kcu(L) == 4) then
         call addlink1D2D(L,japerim)                 ! 1D2D lateral inherits 2D
      else if (kcu(L) == 3) then
         if (ja1D2Dinternallinktype >= 1) then       ! testing one two...
            call addlink1D2Dinternal(L, japerim)
         else
            call addlink1Dkcu3(L,japerim)
         endif
      elseif (japerim == 1 .or. .not. useVolumeTables) then
         call addlink1D(L,japerim)                   ! regular 1D link and original 1D2D internal links
      endif
   enddo
 endif

 if (japerim == 1 ) then
   do i = wetLink2D, wetLinkBnd-1
      L = onlyWetLinks(i)
      call addlink2D(L,japerim)                   ! regular 2D links
   enddo
 else if (nonlin2D > 0) then
    do L = lnx1D + 1, lnxi
       call addlink2D(L,japerim)
    enddo
 endif

 do L   = lnxi+1,lnx
    if (kcu(L) == -1) then
       if (japerim == 0 .and. nonlin1D == 0) cycle
       if (japerim == 1 .or. .not. useVolumeTables) then
         call addlink1D(L,japerim)                   ! 1D boundary links
       endif
    else
       if (japerim == 0 .and. nonlin2D == 0) cycle
       call addlink2D(L,japerim)                   ! 2D boundary links
    endif
 enddo

 if (nshiptxy > 0) then
    if (japerim == 1 .or. nonlin >= 2 .and. japressurehull >= 2) then                     ! and nonlin == 2
       call addship2D(japerim)
       if (japressurehull == 3 .and. japerim == 0) then
          do n = 1,ndx
             vol1(n) = vol1(n) - v1ship(n)
          enddo
       endif
    endif
 endif

 if (japerim == 0) then

    if (nonlin2D > 0) then
       call addclosed_2D_walls()                   ! 2D Dichte wanden
    endif

 endif

 end subroutine VOL12D
