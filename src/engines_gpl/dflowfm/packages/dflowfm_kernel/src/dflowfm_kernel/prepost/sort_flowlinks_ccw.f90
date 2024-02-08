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

!> sort flowlinks in nd%ln counterclockwise (copy-paste and modified from above)
subroutine sort_flowlinks_ccw()
   use m_flowgeom, only: xz, yz, nd, Ndx, ln
   use m_sferic
   use m_alloc
   use geometry_module, only: getdxdy, dcosphi, getdx, getdy
   use stdlib_sorting, only: sort_index

   implicit none

   integer                                     :: k                           ! node number
   integer                                     :: maxlin                      ! array size
   double precision, dimension(:), allocatable :: arglin                      ! dummy array
   integer,          dimension(:), allocatable :: linnrs, inn                 ! dummy arrays

   integer                                     :: k1, k2, L, LL

   integer                                     :: jDupLinks, jOverlapLinks, jSmallAng
   double precision                            :: sl, sm, xcr, ycr, phi0

   double precision                            :: phi, dx, dy, dmaxcosp, dcosp, costriangleminangle

   integer                                     :: lnxx


   maxlin = 6

   allocate(linnrs(maxlin), arglin(maxlin), inn(maxlin))

   do k=1,Ndx
      lnxx = nd(k)%lnx

      if ( lnxx.le.1 ) cycle

      if ( lnxx.gt.maxlin ) then
         maxlin = lnxx
         call realloc(linnrs, maxlin, keepExisting=.true.)
         call realloc(arglin, maxlin, keepExisting=.true.)
         call realloc(inn,    maxlin, keepExisting=.true.)
      end if

      do L=1,lnxx
         K1 = ln(1,iabs(nd(K)%ln(L))); K2 = ln(2,iabs(nd(K)%ln(L)))
         if (K2 == K) then
            K2 = K1
            K1 = K
         end if

         call getdxdy(xz(k1), yz(k1), xz(k2), yz(k2),dx,dy,jsferic)
         if (abs(dx) < 1d-14 .and. abs(dy) < 1d-14) then
            if (dy < 0) then
               phi = -pi/2
            else
               phi = pi/2
            end if
         else
           phi = atan2(dy, dx)
         end if
         if ( L.eq.1 ) then
            phi0 = phi
         end if

         arglin(L) = phi-phi0
         if ( arglin(L).lt.0d0 ) arglin(L) = arglin(L) + 2d0*pi
      end do

      call sort_index(arglin(1:lnxx), inn(1:lnxx))

      linnrs(1:lnxx) = nd(k)%ln(1:lnxx)
      do L=1,lnxx
         nd(k)%ln(L) = linnrs(inn(L))
      end do

   end do ! do k=1,Ndx

   if ( allocated(linnrs) ) deallocate(linnrs)
   if ( allocated(arglin) ) deallocate(arglin)
   if ( allocated(inn) ) deallocate(inn)

   return
end subroutine sort_flowlinks_ccw
