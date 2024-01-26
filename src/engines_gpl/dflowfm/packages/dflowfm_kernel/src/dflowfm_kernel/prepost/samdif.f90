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

!> take difference of samples with second sample set within tooclose distance
subroutine samdif()

   use m_polygon
   use m_samples
   use network_data, only: tooclose
   use kdtree2Factory
   use m_missing
   use m_sferic, only: jsferic, jasfer3D
   use geometry_module, only: dbdistance

   implicit none

   double precision            :: dist

   integer                     :: i, ipnt, ierror
   integer                     :: numnoval

   double precision, parameter :: VAL_NOPNT = 1234d0
   double precision, parameter :: dtol = 1d-8

   if ( NS.lt.1 .or. NS3.lt.2 ) goto 1234

!  build kdtree
   call build_kdtree(treeglob,NS3, xs3, ys3, ierror, jsferic, dmiss)

!  reallocate results vector (fixed size)
   call realloc_results_kdtree(treeglob,1)

   if ( ierror.ne.0 ) goto 1234

   call savesam()

   numnoval = 0   ! count number of samples without a polygon node
   do i=1,Ns
!     fill query vector
      call make_queryvector_kdtree(treeglob,xs(i),ys(i), jsferic)

!     find nearest polygon point
      call kdtree2_n_nearest(treeglob%tree,treeglob%qv,1,treeglob%results)
      ipnt = treeglob%results(1)%idx

      if ( ipnt.gt.0 .and. ipnt.le.Ns3 ) then   ! safety
!        check distance to nearest polygon node
         dist = dbdistance(xs(i),ys(i),xs3(ipnt),ys3(ipnt),jsferic, jasfer3D, dmiss)
         if ( dist.lt.tooclose .and. zs(i).ne.DMISS .and. zs3(ipnt).ne.DMISS ) then
            zs(i) = zs(i) - zs3(ipnt)

!           remove (nearly) zero values
            if ( abs(zs(i)).lt.dtol ) then
               zs(i) = DMISS
            end if
         else
            zs(i) = VAL_NOPNT
            numnoval = numnoval+1
         end if
      else
         zs(i) = VAL_NOPNT
         numnoval = numnoval+1
      end if
   end do

   call delpol()

   if ( numnoval.gt.0 ) then
!     copy unassociated samples to polygon
      call increasepol(numnoval,0)

      NPL = 0
      do i=1,NS
         if ( zs(i).eq.VAL_NOPNT ) then
            zs(i) = DMISS
            NPL=NPL+1
            xpl(NPL) = xs(i)
            ypl(NPL) = ys(i)
            zpl(NPL) = zs(i)
         end if
      end do
   end if

   ierror = 0

1234 continue

!  deallocate kdtree if it was created
   if ( treeglob%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeglob)

   return
end subroutine samdif
