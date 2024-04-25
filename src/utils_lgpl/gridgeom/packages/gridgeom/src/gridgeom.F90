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

!> Module for grid operations.

module gridgeom


implicit none

!
! Subroutines
!

   contains
   
function ggeo_get_xy_coordinates(branchids, branchoffsets, geopointsX, geopointsY, nbranchgeometrynodes, branchlengths, jsferic, meshXCoords, meshYCoords) result(ierr)
   
   use odugrid
   
   integer, intent(in)               :: branchids(:),nbranchgeometrynodes(:)
   double precision, intent(in)      :: branchoffsets(:), geopointsX(:), geopointsY(:), branchlengths(:)
   double precision, intent(inout)   :: meshXCoords(:), meshYCoords(:)
   integer, intent(in)               :: jsferic
   
   integer                           :: ierr

   ierr = odu_get_xy_coordinates(branchids, branchoffsets, geopointsX, geopointsY, nbranchgeometrynodes, branchlengths, jsferic, meshXCoords, meshYCoords)

end function ggeo_get_xy_coordinates


function ggeo_get_start_end_nodes_of_branches(branchidx, branchStartNode, branchEndNode) result(ierr)

   use odugrid
   
   integer, dimension(:), intent(in)      :: branchidx
   
   integer, dimension(:), intent(inout)   :: branchStartNode
   integer, dimension(:), intent(inout)   :: branchEndNode
   integer                                :: ierr
   
   ierr = odu_get_start_end_nodes_of_branches(branchidx, branchStartNode, branchEndNode)

end function ggeo_get_start_end_nodes_of_branches



function ggeo_make1D2Dinternalnetlinks(xplLinks, yplLinks, zplLinks, oneDmask, inNet, c_jsferic, c_jasfer3D, c_jglobe) result(ierr)

   use gridoperations
   use m_sferic
   
   integer :: ierr
   double precision, intent(in) :: xplLinks(:), yplLinks(:), zplLinks(:)
   integer, intent(in)          :: oneDmask(:)
   integer, intent(in)          :: inNet
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe

   jsferic  = c_jsferic
   jasfer3D = c_jasfer3D
   jglobe   = c_jglobe

   ierr = make1D2Dinternalnetlinks(xplLinks, yplLinks, zplLinks, oneDmask, inNet)

end function ggeo_make1D2Dinternalnetlinks


function ggeo_make1D2Droofgutterpipes(xplRoofs, yplRoofs, zplRoofs, oneDmask, c_jsferic, c_jasfer3D, c_jglobe) result(ierr)

   use gridoperations
   use m_sferic
   
   integer :: ierr
   double precision, intent(in) :: xplRoofs(:), yplRoofs(:), zplRoofs(:)
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe
   integer, intent(in)          :: oneDmask(:)

   jsferic  = c_jsferic
   jasfer3D = c_jasfer3D
   jglobe   = c_jglobe
   ierr     = 0

   call make1D2Droofgutterpipes(xplRoofs, yplRoofs, zplRoofs, oneDmask)

end function ggeo_make1D2Droofgutterpipes


function ggeo_make1D2Dstreetinletpipes(xsStreetInletPipes, ysStreetInletPipes, oneDmask, c_jsferic, c_jasfer3D, c_jglobe) result(ierr)

   use gridoperations
   use m_sferic
   
   integer                      :: ierr
   double precision, intent(in) :: xsStreetInletPipes(:), ysStreetInletPipes(:)
   integer, intent(in)          :: oneDmask(:)
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe

   jsferic  = c_jsferic
   jasfer3D = c_jasfer3D
   jglobe   = c_jglobe
   ierr     = 0

   call make1D2Dstreetinletpipes(xsStreetInletPipes, ysStreetInletPipes, oneDmask)

end function ggeo_make1D2Dstreetinletpipes



end module gridgeom