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

! make the dual mesh
subroutine make_dual_mesh()
   use m_alloc
   use m_missing
   use network_data
   use m_flowgeom, only: xz, yz
   use gridoperations

   implicit none

   double precision, dimension(:),   allocatable :: xk_new, yk_new, zk_new
   integer,          dimension(:,:), allocatable :: kn_new
   integer,          dimension(:),   allocatable :: newnode ! new node on link

   integer                                       :: numk_new, numL_new, numcur_k, numcur_L

   integer                                       :: k, kk, k1, k2, kL, kR, L, ic, icL, icR

   if ( netstat.eq.NETSTAT_CELLS_DIRTY ) then
      call findcells(0)
   end if

   call makenetnodescoding()

   call SAVENET()

!  allocate
   allocate(xk_new(numk), yk_new(numk), zk_new(numk))
   allocate(kn_new(3,numL))
   allocate(newnode(numL))

   xk_new = DMISS
   yk_new = DMISS
   yk_new = DMISS
   kn_new = 0
   newnode = 0

!  copy netcell circumcenters to new nodes
   numk_new=0
   numcur_k = ubound(xk_new,1)
   do ic=1,nump
      numk_new = numk_new+1
      call increasenodes(numk_new,numcur_k)

      xk_new(numk_new) = xz(ic)
      yk_new(numk_new) = yz(ic)
      zk_new(numk_new) = DMISS
   end do

!  make new internal links
   numL_new = 0
   numcur_L = ubound(kn_new,2)
   do L=1,numL
      if ( lnn(L).gt.1 .and. kn(3,L).eq.2 ) then
         numL_new = numL_new+1
         call increaselinks(numL_new, numcur_L)

         icL = lne(1,L)
         icR = lne(2,L)
         kn_new(1,numL_new) = icL
         kn_new(2,numL_new) = icR
         kn_new(3,numL_new) = 2
      end if
   end do

!  add boundary nodes and links in cells
   do L=1,numL
      if ( lnn(L).eq.1 ) then
         if ( newnode(L).eq.0 ) then
!           add new node and administer
            numk_new = numk_new+1
            call increasenodes(numk_new, numcur_k)

            k1 = kn(1,L)
            k2 = kn(2,L)
            xk_new(numk_new) = 0.5d0*(xk(k1)+xk(k2))
            yk_new(numk_new) = 0.5d0*(yk(k1)+yk(k2))
            zk_new(numk_new) = DMISS
            newnode(L)   = numk_new

!           add link
            numL_new = numL_new+1
            call increaselinks(numL_new,numcur_L)

            icL = lne(1,L)
            kn_new(1,numL_new) = icL
            kn_new(2,numL_new) = newnode(L)
            kn_new(3,numL_new) = 2
         end if
      end if
   end do

!  add boundary links
   do k=1,numk
      if ( nb(k).eq.2 .or. nb(k).eq.3 ) then
         icL = 0
         icR = 0
         kL  = 0
         kR  = 0
         do kk=1,nmk(k)
            L = nod(k)%lin(kk)
            if ( lnn(L).eq.1 ) then
               if ( icL.eq.0 ) then
                  icL = lne(1,L)
                  kL  = newnode(L)
               else if ( icR.eq.0 ) then
                  icR = lne(1,L)
                  kR  = newnode(L)
               else
                  exit
               end if
            end if
         end do
         if ( kL.ne.0 .and. kR.ne.0 ) then
            if ( icL.ne.icR ) then  ! links in different cells
               numL_new = numL_new+1
               call increaselinks(numL_new,numcur_L)

               kn_new(1,numL_new) = kL
               kn_new(2,numL_new) = kR
               kn_new(3,numL_new) = 2
            else                      ! links in same cell: add common node
               numk_new = numk_new+1
               call increasenodes(numk_new,numcur_k)
               xk_new(numk_new) = xk(k)
               yk_new(numk_new) = yk(k)
               zk_new(numk_new) = DMISS

               numL_new = numL_new+2
               call increaselinks(numL_new,numcur_L)
               kn_new(1,numL_new-1) = kL
               kn_new(2,numL_new-1) = numk_new
               kn_new(3,numL_new-1) = 2

               kn_new(1,numL_new) = numk_new
               kn_new(2,numL_new) = kR
               kn_new(3,numL_new) = 2
            end if
         end if
      end if
   end do

!  delete old network
   call zeronet()

!  allocate new network
   call increasenetw(numk_new, numL_new)

!  set new network dimensions
   numk = numk_new
   numL = numL_new

!  copy to new network
   do k=1,numk
      xk(k) = xk_new(k)
      yk(k) = yk_new(k)
      zk(k) = zk_new(k)
   end do

   do L=1,numL
      kn(1:3,L) = kn_new(1:3,L)
   end do

!  refresh node administration
   call setnodadm(0)
!  mark cell administration as out-of-date
   netstat = NETSTAT_CELLS_DIRTY

1234 continue

!  deallocate
   if ( allocated(xk_new)  ) deallocate(xk_new)
   if ( allocated(yk_new)  ) deallocate(yk_new)
   if ( allocated(zk_new)  ) deallocate(zk_new)
   if ( allocated(kn_new)  ) deallocate(kn_new)
   if ( allocated(newnode) ) deallocate(newnode)

   return

   contains

   subroutine increasenodes(numk_new, numcur_k)
      implicit none

      integer, intent(in)    :: numk_new  !< new number of net nodes
      integer, intent(inout) :: numcur_k  !< current (in) and new (out) array size

      if ( numk_new.gt.numcur_k ) then
         numcur_k = int(1.2d0*dble(numk_new)+1d0)
         call realloc(xk_new, numcur_k, keepExisting=.true., fill=DMISS)
         call realloc(yk_new, numcur_k, keepExisting=.true., fill=DMISS)
         call realloc(zk_new, numcur_k, keepExisting=.true., fill=DMISS)
      end if

      return
   end subroutine

   subroutine increaselinks(numL_new, numcur_L)
      implicit none

      integer, intent(in)    :: numL_new  !< new number of links
      integer, intent(inout) :: numcur_L  !< current (in) and new (out) array size

      if ( numL_new.gt.numcur_L ) then
         numcur_L = int(1.2d0*dble(numL_new)+1d0)
         call realloc(kn_new, (/ 3, numcur_L/), keepExisting=.true., fill=0)
      end if

      return
   end subroutine increaselinks

end subroutine make_dual_mesh
