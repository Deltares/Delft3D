!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

! Module for grid operations on 1d networks
   
module odugrid
!use xxx

implicit none

!user defined data types
   contains
!
! Calculates x and y coordinates from UGrid file format
! 
function odu_get_xy_coordinates(branchids, branchoffsets, geopointsX, geopointsY, nbranchgeometrynodes, branchlengths, jsferic, meshXCoords, meshYCoords) result(ierr)

   use geometry_module, only: sphertocart3D, cart3Dtospher
   use stdlib_sorting, only: sort_index
   use m_missing, only : dmiss
   
   integer, intent(in)               :: branchids(:), nbranchgeometrynodes(:)
   double precision, intent(in)      :: branchoffsets(:), geopointsX(:), geopointsY(:), branchlengths(:)
   double precision, intent(inout)   :: meshXCoords(:), meshYCoords(:)
   integer, intent(in)               :: jsferic

   integer                           :: angle, i, iin, k, ierr, ind, branchid, nsegments
   double precision, allocatable     :: branchSegmentLengths(:)
   double precision, allocatable     :: sorted_branch_offsets(:)
   double precision, allocatable     :: xincrement(:), yincrement(:), zincrement(:)
   double precision, allocatable     :: deltaX(:), deltaY(:), deltaZ(:)
   double precision, allocatable     :: cartMeshXCoords(:), cartMeshYCoords(:), cartMeshZCoords(:)
   double precision, allocatable     :: cartGeopointsX(:), cartGeopointsY(:), cartGeopointsZ(:)
   double precision, allocatable     :: meshZCoords(:)
   double precision, allocatable     :: geopointsZ(:)  !returned by sphertocart3D 
   double precision                  :: totalLength, afac, fractionbranchlength, maxlat, previousLength
   integer                           :: nBranchSegments, nbranches, br
   integer                           :: startGeometryNode, endGeometryNode, nGeometrySegments
   integer                           :: startMeshNode, endMeshNode 
   integer, allocatable              :: meshnodemapping(:,:)
   integer, allocatable              :: ibranchsort(:)
      
   ierr = 0
   ! the number of geometry segments is always equal to number of geopoints - 1
   nBranchSegments = size(geopointsX,1) - 1
   allocate(branchSegmentLengths(nBranchSegments))
   
   allocate(deltaX(size(geopointsX,1) - 1))
   allocate(deltaY(size(geopointsX,1) - 1))
   allocate(deltaZ(size(geopointsX,1) - 1))
   
   allocate(xincrement(size(geopointsX,1) - 1))
   allocate(yincrement(size(geopointsX,1) - 1))
   allocate(zincrement(size(geopointsX,1) - 1))
   
   allocate(cartGeopointsX(size(geopointsX,1)))
   allocate(cartGeopointsY(size(geopointsX,1)))
   allocate(cartGeopointsZ(size(geopointsX,1)))
   
   allocate(ibranchsort(size(meshXCoords,1))) ! to locally sort input points in increasing chainage.
   allocate(cartMeshXCoords(size(meshXCoords,1)))
   allocate(cartMeshYCoords(size(meshXCoords,1)))
   allocate(cartMeshZCoords(size(meshXCoords,1)))
   
   !If the coordinates are spherical, transform them in cartesian, so we operate in linear space
   maxlat = 0
   if (jsferic == 1) then
      do i = 1, size(geopointsX,1)
         maxlat = max(maxlat,geopointsX(i))
         call sphertocart3D(geopointsX(i),geopointsY(i),cartGeopointsX(i),cartGeopointsY(i), cartGeopointsZ(i)) 
      end do
   else
      cartGeopointsX(:) = geopointsX(:)
      cartGeopointsY(:) = geopointsY(:)
      cartGeopointsZ(:) = 0
   endif
   
   !map the mesh nodes
   nbranches = size(branchlengths,1)
   allocate(meshnodemapping(2,nbranches))
   ierr = odu_get_start_end_nodes_of_branches(branchids, meshnodemapping(1,:), meshnodemapping(2,:))
   
   ! initialization
   startGeometryNode    = 1
   do br = 1, nbranches
      ! starting and ending nodes
      startMeshNode         =  meshnodemapping(1,br)
      endMeshNode           =  meshnodemapping(2,br)
      !number of geometry segments for the current branch
      nGeometrySegments     = nbranchgeometrynodes(br) - 1
      !ending geometry point
      endGeometryNode       = startGeometryNode + nGeometrySegments
      !calculate the branch lenghts
      totalLength = 0.0d0
      if (startMeshNode==-1 .or. endMeshNode==-1) then
         !update geometry indexes
         startGeometryNode = endGeometryNode + 1
         cycle
      endif
      
      do i = startGeometryNode, endGeometryNode - 1
         deltaX(i) = cartGeopointsX(i+1) - cartGeopointsX(i)
         deltaY(i) = cartGeopointsY(i+1) - cartGeopointsY(i)
         deltaZ(i) = cartGeopointsZ(i+1) - cartGeopointsZ(i)
         branchSegmentLengths(i)= sqrt(deltaX(i)**2+deltaY(i)**2+deltaZ(i)**2)  ! geometric length
         totalLength = totalLength + branchSegmentLengths(i)                    ! total geometric length 
      enddo
      !correct for total "real world" segment length
      if (totalLength > 1.0d-6) then
         afac = branchlengths(br)/totalLength
         branchSegmentLengths(startGeometryNode: endGeometryNode - 1) = branchSegmentLengths(startGeometryNode: endGeometryNode - 1) * afac
      end if
   
      sorted_branch_offsets = branchoffsets
      !calculate the increments
      do i = startGeometryNode, endGeometryNode - 1
         if (branchSegmentLengths(i) > 1.0d-6) then
            ! xincrement, etc... now contain a ratio between the geometric increase per "real world" length
            xincrement(i)  = deltaX(i)/branchSegmentLengths(i)
            yincrement(i)  = deltaY(i)/branchSegmentLengths(i)
            zincrement(i)  = deltaZ(i)/branchSegmentLengths(i)
         else
            xincrement(i)  = 0.d0
            yincrement(i)  = 0.d0   
            zincrement(i)  = 0.d0   
         endif
      enddo
      !now loop over the mesh points
      ! The loop below assumes that the points to be placed (from branchids/offsets) are sorted by increasing chainage per branch.
      call sort_index(sorted_branch_offsets(startMeshNode:endMeshNode), ibranchsort(startMeshNode:endMeshNode))
      ind            = startGeometryNode
      totallength    = 0.d0
      previousLength = 0.d0 
      do i = startMeshNode, endMeshNode
         iin = startMeshNode - 1 + ibranchsort(i)
         !determine max and min lengths
         totalLength = previousLength
         do k = ind, endGeometryNode - 1
            totalLength = totalLength + branchSegmentLengths(k)
            if (totalLength >= branchoffsets(iin)) then
               ind = k
               exit
            endif
         enddo
         if (k == endGeometryNode) then
            ! Safety: if loop exit above was not reached due to roundoff errors
            ! (or branchoffsets(iin) too big), make sure to use the last geometry segment.
            ind = endGeometryNode-1
         end if
         previousLength = totalLength - branchSegmentLengths(ind)

         fractionbranchlength =  branchoffsets(iin) - previousLength                           ! "real world" length
         cartMeshXCoords(iin) = cartGeopointsX(ind) + fractionbranchlength * xincrement(ind)
         cartMeshYCoords(iin) = cartGeopointsY(ind) + fractionbranchlength * yincrement(ind)
         !TODO: this function should also return meshZCoords (it is relevant if coordinates are spheric) 
         cartMeshZCoords(iin) = cartGeopointsZ(ind) + fractionbranchlength * zincrement(ind)
      enddo
      !update geometry indexes
      startGeometryNode = endGeometryNode + 1
   enddo
   
   if (jsferic == 1) then
      do i = 1, size(meshXCoords,1)
         call cart3Dtospher(cartMeshXCoords(i),cartMeshYCoords(i),cartMeshZCoords(i),meshXCoords(i),meshYCoords(i), maxlat) 
      end do
   else
      meshXCoords(:) = cartMeshXCoords(:)
      meshYCoords(:) = cartMeshYCoords(:)
      ! meshZCoords(:) = cartMeshZCoords(:)  !  meshZCoords is never used and assignment causes runtime error
   endif

end function odu_get_xy_coordinates


!Calculate the start and the end nodes of the branches
! NOTE: This function assumes that the mesh nodes are numbered in the order of the branches, so
!       first all mesh nodes on branch 1, then those of branch 2, etc.
function odu_get_start_end_nodes_of_branches(branchidx, branchStartNode, branchEndNode) result(ierr)

   integer, dimension(:), intent(in)      :: branchidx

   integer, dimension(:), intent(inout)   :: branchStartNode
   integer, dimension(:), intent(inout)   :: branchEndNode
   integer                                :: ierr, i, ibran, numnode, nbranches 

   ! Get the starting and ending indexes of the grid points
   ierr  =  0
   ibran =  0
   numnode = size(branchidx)
   nbranches = size(branchStartNode)
   !initialize
   branchStartNode = -1
   branchEndNode = -1
   do i = 1, numnode
      if (branchidx(i) > ibran) then
         if (ibran > 0) then
            branchEndNode(ibran) = i - 1
         endif
         ibran = branchidx(i)
         branchStartNode(ibran) = i
      elseif (branchidx(i) < ibran) then
         ! Unsorted input, return error code
         ierr = -1
      endif
   enddo
   branchEndNode(ibran) = numnode
end function odu_get_start_end_nodes_of_branches

end module odugrid