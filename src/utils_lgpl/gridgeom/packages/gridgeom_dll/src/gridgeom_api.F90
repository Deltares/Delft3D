!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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

!> \file
!! Basic API for gridgeom routines.

module gridgeom_api

use iso_c_binding

implicit none

!-------------------------------------------------------------------------------
   contains
!-------------------------------------------------------------------------------

!> Gets the x,y coordinates from UGRID
function ggeo_get_xy_coordinates_dll(c_branchids, c_branchoffsets, c_geopointsX, c_geopointsY, c_nbranchgeometrynodes, c_branchlengths, c_jsferic, c_meshXCoords, c_meshYCoords, nbranches, ngeopoints, nmeshnodes) result(ierr) bind(C, name="ggeo_get_xy_coordinates")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_xy_coordinates_dll
   
   use gridgeom
   use meshdata
   
   integer(kind=c_int), intent(in)   :: nbranches, ngeopoints, nmeshnodes, c_jsferic
   type(c_ptr), intent(in)           :: c_branchids
   type(c_ptr), intent(in)           :: c_branchoffsets, c_geopointsX, c_geopointsY, c_nbranchgeometrynodes, c_branchlengths
   type(c_ptr), intent(inout)        :: c_meshXCoords, c_meshYCoords
   
   !fortran pointers
   integer, pointer                  :: branchids(:), nbranchgeometrynodes(:)
   double precision, pointer         :: branchoffsets(:), geopointsX(:), geopointsY(:), branchlengths(:)
   double precision, pointer         :: meshXCoords(:), meshYCoords(:)
   
   integer                           :: ierr

   call c_f_pointer(c_branchids, branchids, (/ nmeshnodes /))
   call c_f_pointer(c_branchoffsets, branchoffsets, (/ nmeshnodes /))
   call c_f_pointer(c_geopointsX, geopointsX, (/ ngeopoints /))
   call c_f_pointer(c_geopointsY, geopointsY, (/ ngeopoints /))
   call c_f_pointer(c_nbranchgeometrynodes, nbranchgeometrynodes, (/ nbranches /))
   call c_f_pointer(c_branchlengths, branchlengths, (/ nbranches /))
   call c_f_pointer(c_meshXCoords, meshXCoords, (/ nmeshnodes /))
   call c_f_pointer(c_meshYCoords, meshYCoords, (/ nmeshnodes /))

   ierr = ggeo_get_xy_coordinates(branchids, branchoffsets, geopointsX, geopointsY, nbranchgeometrynodes, branchlengths, c_jsferic, meshXCoords, meshYCoords)
   
end function ggeo_get_xy_coordinates_dll

function ggeo_convert_dll(c_meshgeom, c_meshgeomdim, start_index) result(ierr) bind(C, name="ggeo_convert")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_convert_dll

   use gridoperations
   use gridgeom
   use meshdata
   
   type(c_t_ug_meshgeom), intent(in)      :: c_meshgeom
   type(c_t_ug_meshgeomdim), intent(in)   :: c_meshgeomdim
   type(t_ug_meshgeom)                    :: meshgeom
   integer, intent(in)                    :: start_index
   integer                                :: ierr
   
   ierr = convert_cptr_to_meshgeom(c_meshgeom, c_meshgeomdim, meshgeom)
   ierr = ggeo_initialize()
   ierr = ggeo_convert(meshgeom, start_index)
   
end function ggeo_convert_dll

function ggeo_make1D2Dinternalnetlinks_dll(c_nin, c_xpl, c_ypl, c_zpl, c_nOneDMask, c_oneDmask, c_inNet, c_jsferic, c_jasfer3D, c_jglobe) result(ierr) bind(C, name="ggeo_make1D2Dinternalnetlinks")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Dinternalnetlinks_dll
   
   use gridgeom
   
   integer, intent(in)          :: c_nin
   type(c_ptr), intent(in)      :: c_xpl
   type(c_ptr), intent(in)      :: c_ypl
   type(c_ptr), intent(in)      :: c_zpl
   integer, intent(in)          :: c_nOneDMask
   type(c_ptr), intent(in)      :: c_oneDmask  
   integer, intent(in)          :: c_inNet
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe
   integer                      :: ierr  
   double precision, pointer    :: xplLinks(:), yplLinks(:), zplLinks(:)   
   integer, pointer             :: oneDmask(:) 

   call c_f_pointer(c_xpl, xplLinks, (/c_nin/))
   call c_f_pointer(c_ypl, yplLinks, (/c_nin/))
   call c_f_pointer(c_zpl, zplLinks, (/c_nin/))
   call c_f_pointer(c_oneDmask, oneDmask, (/c_nOneDMask/))
   
   ierr = ggeo_make1D2Dinternalnetlinks(xplLinks, yplLinks, zplLinks, oneDmask, c_inNet, c_jsferic, c_jasfer3D, c_jglobe)
   
end function ggeo_make1D2Dinternalnetlinks_dll
!> Make 1d-2d roofs connections
!!
!! c_nin       :: size of the array containing the polygon's coordinates
!! c_xin       :: x coordinate of the polygon's points  
!! c_yin       :: y coordinate of the polygon's points
!! c_zpl       :: z coordinate of the polygon's points
!! c_nOneDMask :: size of the 1d mask for mesh 1d
!! c_oneDmask  :: mask for 1d mesh points (1 = potential connection, 0 = do not connect) 
!! c_jsferic   :: 2d sferic flag (1 = spheric / 0 = cartesian)
!! c_jasfer3D  :: 3d sferic flag (1 = spheric / 0 = cartesian)
!! c_jglobe    :: to be detailed
function ggeo_make1D2Droofgutterpipes_dll(c_nin, c_xpl, c_ypl, c_zpl, c_nOneDMask, c_oneDmask, c_jsferic, c_jasfer3D, c_jglobe) result(ierr) bind(C, name="ggeo_make1D2Droofgutterpipes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Droofgutterpipes_dll
   
   use gridgeom
   
   integer, intent(in)          :: c_nin, c_nOneDMask
   type(c_ptr), intent(in)      :: c_xpl
   type(c_ptr), intent(in)      :: c_ypl
   type(c_ptr), intent(in)      :: c_zpl
   type(c_ptr), intent(in)      :: c_oneDmask   
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe
   integer                      :: ierr  
   double precision, pointer    :: xplRoofs(:), yplRoofs(:), zplRoofs(:)   
   integer, pointer             :: oneDmask(:) 

   call c_f_pointer(c_xpl, xplRoofs, (/c_nin/))
   call c_f_pointer(c_ypl, yplRoofs, (/c_nin/))
   call c_f_pointer(c_zpl, zplRoofs, (/c_nin/))
   call c_f_pointer(c_oneDmask, oneDmask, (/c_nOneDMask/))

   ierr = ggeo_make1D2Droofgutterpipes(xplRoofs, yplRoofs, zplRoofs, oneDmask, c_jsferic, c_jasfer3D, c_jglobe)
   
end function ggeo_make1D2Droofgutterpipes_dll
!> Make 1d-2d gullies connections
!!
!! c_nin       :: size of the array containing the coordinates of the gullies
!! c_xin       :: x coordinate of the gullies  
!! c_yin       :: y coordinate of the gullies
!! c_nOneDMask :: size of the 1d mask for mesh 1d
!! c_oneDmask  :: mask for 1d mesh points (1 = potential connection, 0 = do not connect) 
!! c_jsferic   :: 2d sferic flag (1 = spheric / 0 = cartesian)
!! c_jasfer3D  :: 3d sferic flag (1 = advacent spheric algorithm / 0 = default spheric algorithm )
!! c_jglobe    :: to be detailed
function ggeo_make1D2Dstreetinletpipes_dll(c_nin, c_xin, c_yin, c_nOneDMask, c_oneDmask, c_jsferic, c_jasfer3D, c_jglobe) result(ierr) bind(C, name="ggeo_make1D2Dstreetinletpipes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Dstreetinletpipes_dll

   use gridgeom
   
   integer, intent(in)          :: c_nin, c_nOneDMask
   type(c_ptr), intent(in)      :: c_xin
   type(c_ptr), intent(in)      :: c_yin
   type(c_ptr), intent(in)      :: c_oneDmask   
   integer, intent(in)          :: c_jsferic
   integer, intent(in)          :: c_jasfer3D
   integer, intent(in)          :: c_jglobe
   integer                      :: ierr  
   double precision, pointer    :: xsStreetInletPipes(:), ysStreetInletPipes(:)
   integer, pointer             :: oneDmask(:) 

   call c_f_pointer(c_xin, xsStreetInletPipes, (/c_nin/))
   call c_f_pointer(c_yin, ysStreetInletPipes, (/c_nin/))
   call c_f_pointer(c_oneDmask, oneDmask, (/c_nOneDMask/))

   ierr = ggeo_make1D2Dstreetinletpipes(xsStreetInletPipes, ysStreetInletPipes, oneDmask, c_jsferic, c_jasfer3D, c_jglobe)

end function ggeo_make1D2Dstreetinletpipes_dll

!> Make 1d-2d internal connections. With this function multiple 2d cells can be connected to 1d mesh points. all the cell crossing the 1d links will be connected to the closest 1d point.
!> Please note that the gridgeom library has to be initialized before this function can be called.
!!
!! c_jsferic   :: 2d sferic flag (1 = spheric / 0 = cartesian)
!! c_jasfer3D  :: 3d sferic flag (1 = advanced spheric algorithm, 0 = default spheric algorithm )
!! c_nOneDMask :: size of the 1d mask for mesh 1d
!! c_oneDmask  :: mask for 1d mesh points (1 = potential connection, 0 = do not connect) 
function ggeo_make1D2Dembeddedlinks_dll(c_jsferic, c_jasfer3D, c_nOneDMask, c_oneDmask) result(ierr) bind(C, name="ggeo_make1D2Dembeddedlinks")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2Dembeddedlinks_dll

   use gridgeom
   use gridoperations
   
   integer, intent(in)     :: c_jsferic
   integer, intent(in)     :: c_jasfer3D
   integer, intent(in)     :: c_nOneDMask
   type(c_ptr), intent(in) :: c_oneDmask  
   integer, pointer        :: oneDmask(:) 
   integer                 :: ierr  
   
   call c_f_pointer(c_oneDmask, oneDmask, (/c_nOneDMask/))
   
   ierr = ggeo_make1D2Dembeddedlinks(c_jsferic, c_jasfer3D, oneDmask)
   
end function ggeo_make1D2Dembeddedlinks_dll

!> Make 1d-2d river connections connections. With this function multiple 2d boundary cells can be connected to 1d mesh points. 
!> Please note that the gridgeom library has to be initialized before this function can be called.
!!
!! c_npl       :: size of the array containing the polygon's coordinates
!! c_xpl       :: x coordinate of the polygon's points  
!! c_ypl       :: y coordinate of the polygon's points
!! c_zpl       :: z coordinate of the polygon's points
!! c_nOneDMask :: size of the 1d mask for mesh 1d
!! c_oneDmask  :: mask for 1d mesh points (1 = potential connection, 0 = do not connect) 
!! c_jsferic   :: 2d sferic flag (1 = spheric / 0 = cartesian)
!! c_jasfer3D  :: 3d sferic flag (1 = spheric / 0 = cartesian)
!! c_jglobe    :: to be detailed
function ggeo_make1D2DRiverLinks_dll(c_npl, c_xpl, c_ypl, c_zpl, c_nOneDMask, c_oneDmask, c_jsferic, c_jasfer3D, c_searchRadius) result(ierr) bind(C, name="ggeo_make1D2DRiverLinks")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_make1D2DRiverLinks_dll

   use gridgeom
   use gridoperations

   integer, intent(in)           :: c_npl, c_nOneDMask
   type(c_ptr), intent(in)       :: c_xpl
   type(c_ptr), intent(in)       :: c_ypl
   type(c_ptr), intent(in)       :: c_zpl
   type(c_ptr), intent(in)       :: c_oneDmask
   integer, intent(in)           :: c_jsferic
   integer, intent(in)           :: c_jasfer3D
   double precision, intent(in)  :: c_searchRadius
   
   double precision, pointer     :: xpl(:), ypl(:), zpl(:)   
   integer, pointer              :: oneDmask(:) 
   integer                       :: ierr  
   
   if(c_npl > 0) then
      call c_f_pointer(c_xpl, xpl, (/c_npl/))
      call c_f_pointer(c_ypl, ypl, (/c_npl/))
      call c_f_pointer(c_zpl, zpl, (/c_npl/))
   endif
   
   if (c_nOneDMask > 0) then
      call c_f_pointer(c_oneDmask, oneDmask, (/c_nOneDMask/))
   endif
   
   if (associated(xpl).and.associated(oneDMask)) then
      ierr = ggeo_make1D2DRiverLinks(c_jsferic, c_jasfer3D, c_searchRadius, xpl, ypl, zpl, oneDMask)
   else
      ierr = ggeo_make1D2DRiverLinks(c_jsferic, c_jasfer3D, c_searchRadius)
   endif
   
end function ggeo_make1D2DRiverLinks_dll

function ggeo_get_links_count_dll(nlinks, linkType) result(ierr) bind(C, name="ggeo_get_links_count")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_links_count_dll
   
   use gridoperations
   
   integer(kind=c_int), intent(inout):: nlinks, linkType
   integer :: ierr
   
   ierr =  ggeo_get_links_count(nlinks, linkType)
   
end function ggeo_get_links_count_dll


function ggeo_get_links_dll(c_arrayfrom, c_arrayto, nlinks, linkType, startIndex) result(ierr) bind(C, name="ggeo_get_links")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_get_links_dll
   use gridoperations
   
   type(c_ptr), intent(in)                  :: c_arrayfrom, c_arrayto
   integer(kind=c_int), intent(in)          :: startIndex
   integer(kind=c_int), intent(in)          :: nlinks, linkType
   integer, pointer                         :: arrayfrom(:), arrayto(:)
   integer                                  :: ierr
   
   call c_f_pointer(c_arrayfrom, arrayfrom, (/ nlinks /))
   call c_f_pointer(c_arrayto, arrayto, (/ nlinks /))
   
   ierr = ggeo_get_links(arrayfrom, arrayto, linkType, startIndex)
   
end function ggeo_get_links_dll

function ggeo_convert_1d_arrays_dll(c_nodex, c_nodey, c_branchoffset, c_branchlength, c_branchid, c_sourceNodeId, c_targetNodeId, nbranches, nmeshnodes, startIndex) result(ierr) bind(C, name="ggeo_convert_1d_arrays")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_convert_1d_arrays_dll

   use gridoperations
   use gridgeom
   use meshdata
   
   type(c_ptr), intent(in)                :: c_nodex, c_nodey, c_branchoffset, c_branchlength, c_branchid, c_sourceNodeId, c_targetNodeId   
   integer(kind=c_int), intent(in)        :: nmeshnodes, nBranches, startIndex
   !fortran pointers
   double precision, pointer              :: nodex(:), nodey(:), branchoffset(:), branchlength(:)
   integer, pointer                       :: branchid(:), sourceNodeId(:), targetNodeId(:)
   integer                                :: ierr
   type(t_ug_meshgeom)                    :: meshgeom
   
   call c_f_pointer(c_nodex, nodex, (/ nmeshnodes /))
   call c_f_pointer(c_nodey, nodey, (/ nmeshnodes /))
   call c_f_pointer(c_branchoffset, branchoffset, (/ nmeshnodes /))
   call c_f_pointer(c_branchid, branchid, (/ nmeshnodes /))
   
   call c_f_pointer(c_branchlength, branchlength, (/ nBranches /))
   call c_f_pointer(c_sourceNodeId, sourceNodeId, (/ nBranches /))
   call c_f_pointer(c_targetNodeId, targetNodeId, (/ nBranches /))
   
   !ggeo_convert_1d_arrays gives back 1d based arrays
   ierr =  ggeo_convert_1d_arrays(nodex, nodey, branchoffset, branchlength, branchid, sourcenodeid, targetnodeid, meshgeom, startIndex)
    
   !1d based arrays are provided
   ierr = ggeo_convert(meshgeom, 1)

end function ggeo_convert_1d_arrays_dll

function ggeo_count_edge_nodes_dll(c_branchoffset, c_branchlength, c_branchids, c_nedge_nodes, nBranches, nNodes, nEdgeNodes, startIndex) result(ierr) bind(C, name="ggeo_count_edge_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_count_edge_nodes_dll

   use gridoperations
   
   type(c_ptr), intent(in)   :: c_branchoffset, c_branchids, c_nedge_nodes, c_branchlength    
   integer, intent(in)       :: nBranches, nNodes, startIndex
   double precision, pointer :: branchoffset(:), branchlength(:)
   integer, pointer          :: branchids(:), nedge_nodes(:,:)
   integer, intent(inout)    :: nEdgeNodes
   integer                   :: ierr

   
   call c_f_pointer(c_branchoffset, branchoffset, (/ nNodes /))
   call c_f_pointer(c_branchlength, branchlength, (/ nBranches /))
   call c_f_pointer(c_branchids, branchids, (/ nNodes /))
   call c_f_pointer(c_nedge_nodes, nedge_nodes, (/ 2, nBranches /))

   ierr = ggeo_count_or_create_edge_nodes(branchids, branchoffset, nedge_nodes(1,:), nedge_nodes(2,:), branchlength, startIndex, nEdgeNodes)

end function ggeo_count_edge_nodes_dll


function ggeo_create_edge_nodes_dll(c_branchoffset, c_branchlength, c_branchids, c_nedge_nodes, c_edgenodes, nBranches, nNodes, nEdgeNodes, startIndex) result(ierr) bind(C, name="ggeo_create_edge_nodes")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_create_edge_nodes_dll

   use gridoperations
   
   type(c_ptr), intent(in)   :: c_branchoffset, c_branchids, c_edgenodes, c_nedge_nodes, c_branchlength    
   integer, intent(in)       :: nBranches, nNodes, startIndex
   double precision, pointer :: branchoffset(:), branchlength(:)
   integer, pointer          :: branchids(:), edgenodes(:,:), nedge_nodes(:,:)
   integer                   :: ierr
   integer, intent(inout)    :: nEdgeNodes

   call c_f_pointer(c_branchlength, branchlength, (/ nBranches /))
   call c_f_pointer(c_branchids, branchids, (/ nNodes /))
   call c_f_pointer(c_nedge_nodes, nedge_nodes, (/ 2, nBranches /))
   call c_f_pointer(c_edgenodes, edgenodes, (/ 2, nEdgeNodes /))
   call c_f_pointer(c_branchoffset, branchoffset, (/ nNodes /))

   ierr = ggeo_count_or_create_edge_nodes(branchids, branchoffset, nedge_nodes(1,:), nedge_nodes(2,:), branchlength, startIndex, nEdgeNodes, edgenodes)

end function ggeo_create_edge_nodes_dll

function ggeo_deallocate_dll() result(ierr) bind(C, name="ggeo_deallocate")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_deallocate_dll
   use gridoperations   
   
   integer                   :: ierr
   
   ierr = ggeo_deallocate()

end function ggeo_deallocate_dll


function ggeo_count_cells_dll(c_meshDimIn, c_meshIn, c_meshDimOut) result(ierr) bind(C, name="ggeo_count_cells")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_count_cells_dll

   use gridoperations   
   use m_cell_geometry
   use meshdata
   use network_data
   use m_missing
   use m_alloc

   type(c_t_ug_meshgeomdim), intent(in)       :: c_meshDimIn       !< input mesh dimensions, externally allocated
   type(c_t_ug_meshgeom), intent(in)          :: c_meshIn          !< input mesh, externally allocated 
   type(c_t_ug_meshgeomdim), intent(inout)    :: c_meshDimOut       !< input mesh dimensions
   !locals
   type(t_ug_meshgeom)                        :: meshgeomIn        !< fortran meshgeom
   integer                                    :: ierr, n, maxNumNodes
   
   ierr = 0
   
   ! destroy any previous state of the library
   ierr = network_data_destructor()   
   ! initialize local meshgeomIn
   ierr = t_ug_meshgeom_destructor(meshgeomIn)  
   !convert c to fortran pointers
   ierr = convert_cptr_to_meshgeom(c_meshIn, c_meshDimIn, meshgeomIn)
   !set library state
   ierr = ggeo_deallocate()
   ierr = ggeo_initialize() 
   ierr = ggeo_convert(meshgeomIn, c_meshIn%start_index)
   
   !find net cells
   call findcells(0)  
   
   !inquire dimension for outside allocation
   maxNumNodes = 0
   do n = 1, nump
      maxNumNodes = max(maxNumNodes, size(netcell(n)%nod))
   enddo
   c_meshDimOut%numface         = nump
   c_meshDimOut%maxnumfacenodes = maxNumNodes

end function ggeo_count_cells_dll


function ggeo_find_cells_dll(c_meshDimIn, c_meshInOut) result(ierr) bind(C, name="ggeo_find_cells")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_find_cells_dll
   use gridoperations   
   use m_cell_geometry
   use meshdata
   use network_data
   use m_missing
   use m_alloc

   type(c_t_ug_meshgeomdim), intent(in)       :: c_meshDimIn       !< input mesh dimensions, externally allocated
   type(c_t_ug_meshgeom), intent(inout)       :: c_meshInOut          !< input mesh, externally allocated 
   !locals
   type(t_ug_meshgeom)                        :: meshgeomInOut         !< fortran meshgeom
   integer                                    :: ierr, n, nn
   
   ierr = 0
   
   ! destroy any previous state of the library
   ierr = network_data_destructor()   
   ! initialize local meshgeomInOut
   ierr = t_ug_meshgeom_destructor(meshgeomInOut) 
   !convert c to fortran pointers
   ierr = convert_cptr_to_meshgeom(c_meshInOut, c_meshDimIn, meshgeomInOut)
   !set library state
   ierr = ggeo_deallocate()
   ierr = ggeo_initialize() 
   ierr = ggeo_convert(meshgeomInOut, meshgeomInOut%start_index)
   
   !find net cells
   call findcells(0)   
    
   if (meshgeomInOut%dim.eq.2) then
      do n = 1, nump
         !fill face nodes
         meshgeomInOut%face_nodes(:,n) = imiss;
         nn = size(netcell(n)%nod)
         meshgeomInOut%face_nodes(1:nn,n) = netcell(n)%nod(1:nn)
         !fill cell centers
         meshgeomInOut%facex(n) = xz(n)
         meshgeomInOut%facey(n) = yz(n)
      end do
   endif
   
   !convert back to the start index
   if (meshgeomInOut%start_index == 0) then
      where(meshgeomInOut%face_nodes.ne.imiss) meshgeomInOut%face_nodes = meshgeomInOut%face_nodes - 1;
   endif
 
   
end function ggeo_find_cells_dll

!> Destroys the memory allocated by fortran 
function ggeo_meshgeom_destructor_dll(c_meshDim, c_mesh) result(ierr) bind(C, name="ggeo_meshgeom_destructor")
!DEC$ ATTRIBUTES DLLEXPORT :: ggeo_meshgeom_destructor_dll
   use meshdata
   
   type(c_t_ug_meshgeomdim), intent(in)    :: c_meshDim     !< input mesh dimensions, externally allocated
   type(c_t_ug_meshgeom), intent(in)       :: c_mesh    !< input mesh dimensions, intenally allocated
   type(t_ug_meshgeom)                     :: meshgeom
   integer                                 :: ierr

   ierr = - 1
   !initialize 
   ierr = t_ug_meshgeom_destructor(meshgeom) 
   !convert
   ierr = convert_cptr_to_meshgeom(c_mesh, c_meshDim, meshgeom)
   !destory
   ierr = t_ug_meshgeom_destructor(meshgeom) 
   

end function ggeo_meshgeom_destructor_dll


end module gridgeom_api
