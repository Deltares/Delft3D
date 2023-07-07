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

!> find in which cells particles are located
subroutine part_findcell(Npart, xxpart, yypart, kpart, ierror)
   use m_partmesh
   use unstruc_messages
   use kdtree2Factory
   use m_sferic, only: jsferic, jasfer3D
   use m_missing, only: jins, dmiss
   use geometry_module, only: pinpok, dbdistance, pinpok3D, cart3Dtospher
   implicit none

   integer,                            intent(in)  :: Npart    !< number of particles
   double precision, dimension(Npart), intent(in)  :: xxpart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
   double precision, dimension(Npart), intent(in)  :: yypart   !< particle x-coordinates, 2D Cartexsion or spherical coordinates (not 3D Cartesian)
   integer,          dimension(Npart), intent(out) :: kpart    !< cell numbers

   integer                           , intent(out) :: ierror   !< error (1) or not (0)

   type(kdtree_instance)                           :: kdtree

   double precision, dimension(3)                  :: xv, yv

   double precision                                :: dmaxsize
   double precision                                :: R2search
   double precision                                :: xx, yy

   integer                                         :: i, ip1, j, k, knode, L, Lp1, N, NN
   integer                                         :: inside

   ierror = 1

!  build kdtree
   call build_kdtree(kdtree, Npart, xxpart, yypart, ierror, jsferic, dmiss)
   if ( ierror.ne.0 ) then
      goto 1234
   end if

   if ( Npart.lt.1 ) then
      ierror = 0
      goto 1234
   end if

   kpart = 0

!  loop over cells
   do k=1,numcells
!     check cell size
      N = jcell2edge(k+1)-jcell2edge(k)
      if ( N.ne.3 ) then
         call mess(LEVEL_ERROR, 'part_findcell: non-triangle')
         goto 1234
      end if

!     get cell polygon
      i=0
      do j = jcell2edge(k),jcell2edge(k+1)-1
         i = i+1
         L = icell2edge(j)
         ip1 = i+1; if ( ip1.gt.3 ) ip1=ip1-3
         Lp1 = icell2edge(j-i+ip1)
!        find common node of L and Lp1
         if ( edge2node(1,L).eq.edge2node(1,Lp1) .or. edge2node(1,L).eq.edge2node(2,Lp1) ) then
            knode = edge2node(1,L)
         else if ( edge2node(2,L).eq.edge2node(1,Lp1) .or. edge2node(2,L).eq.edge2node(2,Lp1) ) then
            knode = edge2node(2,L)
         else  ! should not happen
            continue
         end if
         if ( jsferic.eq.0 ) then
            xv(i) = xnode(knode)
            yv(i) = ynode(knode)
         else
            call Cart3Dtospher(xnode(knode),ynode(knode),znode(knode),xv(i),yv(i),0d0)
         end if
      end do

!     fill query vector
      if ( jsferic.eq.0 ) then
         call make_queryvector_kdtree(kdtree,xzwcell(k),yzwcell(k), jsferic)
      else
         call cart3Dtospher(xzwcell(k),yzwcell(k),zzwcell(k),xx,yy,0d0)
         call make_queryvector_kdtree(kdtree,xx,yy, jsferic)
      end if

!     compute maximum flowcell dimension
      dmaxsize = 0d0
      do i=1,N
         ip1=i+1; if ( ip1.gt.N ) ip1=ip1-N
         dmaxsize = max(dmaxsize, dbdistance(xv(i),yv(i),xv(ip1),yv(ip1),jsferic, jasfer3D, dmiss))
      end do

!     determine square search radius
      R2search = 1.1d0*dmaxsize**2  ! 1.1d0: safety

!     count number of points in search area
      NN = kdtree2_r_count(kdtree%tree,kdtree%qv,R2search)

      if ( NN.eq.0 ) cycle ! no particles found

!     reallocate if necessary
      call realloc_results_kdtree(kdtree,NN)

!     find nearest NN samples
      call kdtree2_n_nearest(kdtree%tree,kdtree%qv,NN,kdtree%results)

!     check if samples are in cell
      do i=1,NN
         j = kdtree%results(i)%idx
         if ( jsferic.eq.0 ) then
            call pinpok(xxpart(j), yypart(j), 3, xv, yv, inside, jins, dmiss)
         else
            call pinpok3D(xxpart(j), yypart(j), 3, xv, yv, inside, dmiss, jins, jsferic, jasfer3D)
         end if

         if ( inside.eq.1 ) then
            if ( kpart(j).eq.0 ) then
               kpart(j) = k
            end if
         end if
      end do
   end do

   ierror = 0

1234 continue

!  deallocate
   if ( kdtree%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(kdtree)

   return
end subroutine part_findcell
