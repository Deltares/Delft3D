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

module meshdata

use iso_c_binding
use m_ug_constants
use m_ug_meshgeom

implicit none

type, bind(C) :: c_t_ug_meshgeomdim

   character(kind=c_char)   :: meshname(ug_nameLen)  !< Name of this mesh ! TODO: AvD: should this be in this data type?
   integer(kind=c_int)      :: dim                   !< Dimensionality of the mesh (1/2/3)
   integer(kind=c_int)      :: numnode               !< Number of mesh nodes.
   integer(kind=c_int)      :: numedge               !< Number of mesh edges.
   integer(kind=c_int)      :: numface               !< Number of mesh faces.
   integer(kind=c_int)      :: maxnumfacenodes       !< Maximum of number of face nodes.
   integer(kind=c_int)      :: num_layers            !< Number of mesh layers (num interfaces == num_layers + 1), num_layers = 0 means "no layers".
   ! integer(kind=c_int)      :: numtopsig             !< NOTE: UNST-5477: intentionally disabled, to avoid API-change.
   integer(kind=c_int)      :: layertype             !< Type of vertical layer definition (only if num_layers >= 1), one of LAYERTYPE_* parameters.
   integer(kind=c_int)      :: nnodes
   integer(kind=c_int)      :: nbranches             !< Number of branches
   integer(kind=c_int)      :: ngeometry             !< Number of geometry points
   integer(kind=c_int)      :: epsg                  !< epsg code that uniquely identifies the coordinate reference system
   integer(kind=c_int)      :: numlinks              !< the number of links

end type c_t_ug_meshgeomdim

type, bind(C) :: c_t_ug_meshgeom

   type(c_ptr) :: edge_nodes              !< Edge-to-node mapping array.
   type(c_ptr) :: face_nodes              !< Face-to-node mapping array.
   type(c_ptr) :: edge_faces              !< Edge-to-face mapping array (optional, can be null()).
   type(c_ptr) :: face_edges              !< Face-to-edge mapping array (optional, can be null()).
   type(c_ptr) :: face_links              !< Face-to-face mapping array (optional, can be null()).

   !Mesh 1d variables
   type(c_ptr)  :: nnodex                 !< x-coordinates of the network points.
   type(c_ptr)  :: nnodey                 !< y-coordinates of the network points.
   type(c_ptr)  :: nedge_nodes            !< Start-end node of each branch
   type(c_ptr)  :: nbranchlengths         !< The branch lenghts
   type(c_ptr)  :: nbranchgeometrynodes   !< Number of geometry points in each branch
   type(c_ptr)  :: ngeopointx             !< x-coordinates of geometry points.
   type(c_ptr)  :: ngeopointy             !< y-coordinates of geometry points.
   type(c_ptr)  :: nbranchorder           !< the branch order
   type(c_ptr)  :: nodebranchidx              !< The branch index of each 1d mesh point
   type(c_ptr)  :: nodeoffsets          !< The branch offset of each 1d mesh point

   type(c_ptr) :: nodex                   !< x-coordinates of the mesh nodes.
   type(c_ptr) :: nodey                   !< y-coordinates of the mesh nodes.
   type(c_ptr) :: nodez                   !< z-coordinates of the mesh nodes.
   type(c_ptr) :: edgex                   !< x-coordinates of the mesh edges.
   type(c_ptr) :: edgey                   !< y-coordinates of the mesh edges.
   type(c_ptr) :: edgez                   !< z-coordinates of the mesh edges.
   type(c_ptr) :: facex                   !< x-coordinates of the mesh faces.
   type(c_ptr) :: facey                   !< y-coordinates of the mesh faces.
   type(c_ptr) :: facez                   !< z-coordinates of the mesh faces.

   type(c_ptr) :: layer_zs                !< Vertical coordinates of the mesh layers' center (either z or sigma).
   type(c_ptr) :: interface_zs            !< Vertical coordinates of the mesh layers' interface (either z or sigma).
   type(c_ptr) :: nodeids                 !< The mesh node ids
   type(c_ptr) :: nodelongnames           !< The mesh node longnames

   type(c_ptr) :: nbranchids              !< The network branch ids
   type(c_ptr) :: nbranchlongnames        !< The network branch longnames
   type(c_ptr) :: nnodeids                !< The network node ids
   type(c_ptr) :: nnodelongnames          !< The network node longnames

   integer(kind=c_int)  :: start_index    !< Start index of the arrays

end type c_t_ug_meshgeom

   contains

function convert_meshgeom_to_cptr(meshgeom, c_meshgeom, c_meshgeomdim) result(ierr)

   type(t_ug_meshgeom), intent(in)         :: meshgeom
   type(c_t_ug_meshgeom), intent(inout)    :: c_meshgeom
   type(c_t_ug_meshgeomdim), intent(inout) :: c_meshgeomdim
   integer                                 :: ierr
   !support variables
   integer,          pointer  :: edge_nodes(:,:) => null()!< Edge-to-node mapping array.
   integer,          pointer  :: face_nodes(:,:) => null()!< Face-to-node mapping array.
   integer,          pointer  :: edge_faces(:,:) => null()!< Edge-to-face mapping array (optional, can be null()).
   integer,          pointer  :: face_edges(:,:) => null()!< Face-to-edge mapping array (optional, can be null()).
   integer,          pointer  :: face_links(:,:) => null()!< Face-to-face mapping array (optional, can be null()).

   !Network1d variables
   double precision, pointer :: nnodex(:) => null()
   double precision, pointer :: nnodey(:) => null()
   integer,          pointer :: nedge_nodes(:,:) => null()
   double precision, pointer :: nbranchlengths(:) => null()
   integer,          pointer :: nbranchgeometrynodes(:)=> null()
   double precision, pointer :: ngeopointx(:) => null()
   double precision, pointer :: ngeopointy(:)   => null()
   integer,          pointer :: nbranchorder(:) => null()
   !Mesh1d variables
   integer,          pointer :: nodebranchidx(:) => null()    !< Branch id of each mesh node
   double precision, pointer :: nodeoffsets(:)=> null() !< Branch offset of each mesh node

   double precision, pointer :: nodex(:) => null()       !< x-coordinates of the mesh nodes.
   double precision, pointer :: nodey(:) => null()      !< y-coordinates of the mesh nodes.
   double precision, pointer :: nodez(:) => null()      !< z-coordinates of the mesh nodes.
   double precision, pointer :: edgex(:) => null()      !< x-coordinates of the mesh edges.
   double precision, pointer :: edgey(:) => null()      !< y-coordinates of the mesh edges.
   double precision, pointer :: edgez(:) => null()      !< z-coordinates of the mesh edges.
   double precision, pointer :: facex(:) => null()      !< x-coordinates of the mesh faces.
   double precision, pointer :: facey(:) => null()      !< y-coordinates of the mesh faces.
   double precision, pointer :: facez(:) => null()      !< z-coordinates of the mesh faces.

   double precision, pointer :: layer_zs(:) => null()     !< Vertical coordinates of the mesh layers' center (either z or sigma).
   double precision, pointer :: interface_zs(:) => null() !< Vertical coordinates of the mesh layers' interface (either z or sigma).

   character(len=ug_idsLen),          pointer :: nodeids(:)         => null()
   character(len=ug_idsLongNamesLen), pointer :: nodelongnames(:)   => null()
   character(len=ug_idsLen),          pointer :: nbranchids(:)      => null()
   character(len=ug_idsLongNamesLen), pointer :: nbranchlongnames(:)=> null()
   character(len=ug_idsLen),          pointer :: nnodeids(:)        => null()
   character(len=ug_idsLongNamesLen), pointer :: nnodelongnames(:)  => null()

   integer :: i

   ierr = 0
   !dimension variables
   c_meshgeomdim%dim = meshgeom%dim
   c_meshgeomdim%numnode = meshgeom%numnode
   c_meshgeomdim%numedge = meshgeom%numedge
   c_meshgeomdim%numface = meshgeom%numface
   c_meshgeomdim%maxnumfacenodes = meshgeom%maxnumfacenodes
   c_meshgeomdim%num_layers = meshgeom%num_layers
   c_meshgeomdim%layertype = meshgeom%layertype

   c_meshgeomdim%nnodes = meshgeom%nnodes
   c_meshgeomdim%nbranches = meshgeom%nbranches
   c_meshgeomdim%ngeometry = meshgeom%ngeometry
   c_meshgeomdim%epsg = meshgeom%epsg
   do i = 1, ug_nameLen
      c_meshgeomdim%meshname(i) = meshgeom%meshname(i:i)
   end do

   !array variables
   if (associated(meshgeom%edge_nodes).and.c_associated(c_meshgeom%edge_nodes)) then
      call c_f_pointer(c_meshgeom%edge_nodes, edge_nodes, shape(meshgeom%edge_nodes))
      edge_nodes = meshgeom%edge_nodes
   endif

   if (associated(meshgeom%face_nodes).and.c_associated(c_meshgeom%face_nodes)) then
      call c_f_pointer(c_meshgeom%face_nodes, face_nodes, shape(meshgeom%face_nodes))
      face_nodes= meshgeom%face_nodes
   endif

   if (associated(meshgeom%edge_faces).and.c_associated(c_meshgeom%edge_faces)) then
      call c_f_pointer(c_meshgeom%edge_faces, edge_faces, shape(meshgeom%edge_faces))
      edge_faces= meshgeom%edge_faces
   endif

   if (associated(meshgeom%face_edges).and.c_associated(c_meshgeom%face_edges)) then
      call c_f_pointer(c_meshgeom%face_edges, face_edges, shape(meshgeom%face_edges))
      face_edges= meshgeom%face_edges
   endif

   if (associated(meshgeom%face_links).and.c_associated(c_meshgeom%face_links)) then
      call c_f_pointer(c_meshgeom%face_links, face_links, shape(face_links))
      face_links= meshgeom%face_links
   endif

   !Network1d variables
   if (associated(meshgeom%nnodex).and.c_associated(c_meshgeom%nnodex)) then
      call c_f_pointer(c_meshgeom%nnodex, nnodex, shape(meshgeom%nnodex))
      nnodex = meshgeom%nnodex
   endif

   if (associated(meshgeom%nnodey).and.c_associated(c_meshgeom%nnodey)) then
      call c_f_pointer(c_meshgeom%nnodey, nnodey,shape(meshgeom%nnodey))
      nnodey = meshgeom%nnodey
   endif

   if (associated(meshgeom%nedge_nodes).and.c_associated(c_meshgeom%nedge_nodes)) then
      call c_f_pointer(c_meshgeom%nedge_nodes,nedge_nodes , shape(meshgeom%nedge_nodes))
      nedge_nodes= meshgeom%nedge_nodes
   endif

   if (associated(meshgeom%nbranchlengths).and.c_associated(c_meshgeom%nbranchlengths)) then
      call c_f_pointer(c_meshgeom%nbranchlengths, nbranchlengths, shape(meshgeom%nbranchlengths))
      nbranchlengths= meshgeom%nbranchlengths
   endif

   if (associated(meshgeom%nbranchgeometrynodes).and.c_associated(c_meshgeom%nbranchgeometrynodes)) then
      call c_f_pointer(c_meshgeom%nbranchgeometrynodes, nbranchgeometrynodes , shape(meshgeom%nbranchgeometrynodes))
      nbranchgeometrynodes = meshgeom%nbranchgeometrynodes
   endif

   if (associated(meshgeom%ngeopointx).and.c_associated(c_meshgeom%ngeopointx)) then
      call c_f_pointer(c_meshgeom%ngeopointx, ngeopointx , shape(meshgeom%ngeopointx))
      ngeopointx = meshgeom%ngeopointx
   endif

   if (associated(meshgeom%ngeopointy).and.c_associated(c_meshgeom%ngeopointy)) then
      call c_f_pointer(c_meshgeom%ngeopointy, ngeopointy , shape(meshgeom%ngeopointy))
      ngeopointy = meshgeom%ngeopointy
   endif

   if (associated(meshgeom%nbranchorder).and.c_associated(c_meshgeom%nbranchorder)) then
      call c_f_pointer(c_meshgeom%nbranchorder, nbranchorder, shape(meshgeom%nbranchorder))
      nbranchorder = meshgeom%nbranchorder
   endif

   !Mesh1d
   if (associated(meshgeom%nodebranchidx).and.c_associated(c_meshgeom%nodebranchidx)) then
      call c_f_pointer(c_meshgeom%nodebranchidx, nodebranchidx, shape(meshgeom%nodebranchidx))
      nodebranchidx = meshgeom%nodebranchidx
   endif

   if (associated(meshgeom%nodeoffsets).and.c_associated(c_meshgeom%nodeoffsets)) then
      call c_f_pointer(c_meshgeom%nodeoffsets, nodeoffsets, shape(meshgeom%nodeoffsets))
      nodeoffsets = meshgeom%nodeoffsets
   endif

   !mesh nodes
   if (associated(meshgeom%nodex).and.c_associated(c_meshgeom%nodex)) then
      call c_f_pointer(c_meshgeom%nodex, nodex, shape(meshgeom%nodex))
      nodex = meshgeom%nodex
   endif

   if (associated(meshgeom%nodey).and.c_associated(c_meshgeom%nodey)) then
      call c_f_pointer(c_meshgeom%nodey, nodey, shape(meshgeom%nodey))
      nodey = meshgeom%nodey
   endif

   if (associated(meshgeom%nodez).and.c_associated(c_meshgeom%nodez)) then
      call c_f_pointer(c_meshgeom%nodez, nodez, shape(meshgeom%nodez))
      nodez = meshgeom%nodez
   endif

   !mesh edges
   if (associated(meshgeom%edgex).and.c_associated(c_meshgeom%edgex)) then
      call c_f_pointer(c_meshgeom%edgex, edgex, shape(meshgeom%edgex))
      edgex= meshgeom%edgex
   endif
   if (associated(meshgeom%edgey).and.c_associated(c_meshgeom%edgey)) then
      call c_f_pointer(c_meshgeom%edgey, edgey, shape(meshgeom%edgey))
      edgey= meshgeom%edgey
   endif
   if (associated(meshgeom%edgez).and.c_associated(c_meshgeom%edgez)) then
      call c_f_pointer(c_meshgeom%edgez, edgez, shape(meshgeom%edgez))
      edgez= meshgeom%edgez
   endif

   !mesh faces
   if (associated(meshgeom%facex).and.c_associated(c_meshgeom%facex)) then
      call c_f_pointer(c_meshgeom%facex, facex, shape(meshgeom%facex))
      facex= meshgeom%facex
   endif
   if (associated(meshgeom%facey).and.c_associated(c_meshgeom%facey)) then
      call c_f_pointer(c_meshgeom%facey, facey, shape(meshgeom%facey))
      facey= meshgeom%facey
   endif
   if (associated(meshgeom%facez).and.c_associated(c_meshgeom%facez)) then
      call c_f_pointer(c_meshgeom%facez, facez, shape(meshgeom%facez))
      facez= meshgeom%facez
   endif

   !layer
   if (associated(meshgeom%layer_zs).and.c_associated(c_meshgeom%layer_zs)) then
      call c_f_pointer(c_meshgeom%layer_zs, layer_zs, shape(meshgeom%layer_zs))
      layer_zs= meshgeom%layer_zs
   endif

   !interface
   if (associated(meshgeom%interface_zs).and.c_associated(c_meshgeom%interface_zs)) then
      call c_f_pointer(c_meshgeom%interface_zs, interface_zs, shape(meshgeom%interface_zs))
      interface_zs= meshgeom%interface_zs
   endif

   if (associated(meshgeom%nodeids).and.c_associated(c_meshgeom%nodeids)) then
      call c_f_pointer(c_meshgeom%nodeids, nodeids, shape(meshgeom%nodeids))
	  nodeids= meshgeom%nodeids
   endif

   if (associated(meshgeom%nodelongnames).and.c_associated(c_meshgeom%nodelongnames)) then
      call c_f_pointer(c_meshgeom%nodelongnames, nodelongnames, shape(meshgeom%nodelongnames))
	  nodelongnames= meshgeom%nodelongnames
   endif

   if (associated(meshgeom%nbranchids).and.c_associated(c_meshgeom%nbranchids)) then
      call c_f_pointer(c_meshgeom%nbranchids, nbranchids, shape(meshgeom%nbranchids))
	  nbranchids= meshgeom%nbranchids
   endif

   if (associated(meshgeom%nbranchlongnames).and.c_associated(c_meshgeom%nbranchlongnames)) then
      call c_f_pointer(c_meshgeom%nbranchlongnames, nbranchlongnames, shape(meshgeom%nbranchlongnames))
	  nbranchlongnames= meshgeom%nbranchlongnames
   endif

   if (associated(meshgeom%nnodeids).and.c_associated(c_meshgeom%nnodeids)) then
      call c_f_pointer(c_meshgeom%nnodeids, nnodeids, shape(meshgeom%nnodeids))
	  nnodeids= meshgeom%nnodeids
   endif

   if (associated(meshgeom%nnodelongnames).and.c_associated(c_meshgeom%nnodelongnames)) then
      call c_f_pointer(c_meshgeom%nnodelongnames, nnodelongnames, shape(meshgeom%nnodelongnames))
	  nnodelongnames= meshgeom%nnodelongnames
   endif

end function convert_meshgeom_to_cptr


function convert_cptr_to_meshgeom(c_meshgeom, c_meshgeomdim, meshgeom) result(ierr)

   type(c_t_ug_meshgeom), intent(in)      :: c_meshgeom
   type(c_t_ug_meshgeomdim), intent(in)   :: c_meshgeomdim
   type(t_ug_meshgeom), intent(inout)     :: meshgeom
   integer                                :: ierr
   integer                                :: i

   ! get the dimensions

   ierr = 0

   meshgeom%dim = c_meshgeomdim%dim
   meshgeom%numnode = c_meshgeomdim%numnode
   meshgeom%numedge = c_meshgeomdim%numedge
   meshgeom%numface = c_meshgeomdim%numface
   meshgeom%maxnumfacenodes = c_meshgeomdim%maxnumfacenodes
   meshgeom%num_layers = c_meshgeomdim%num_layers
   meshgeom%layertype = c_meshgeomdim%layertype

   meshgeom%nnodes = c_meshgeomdim%nnodes
   meshgeom%nbranches = c_meshgeomdim%nbranches
   meshgeom%ngeometry = c_meshgeomdim%ngeometry
   meshgeom%epsg = c_meshgeomdim%epsg
   do i = 1, ug_nameLen
      meshgeom%meshname(i:i) = c_meshgeomdim%meshname(i)
   end do

   meshgeom%start_index =  c_meshgeom%start_index
   if(c_associated(c_meshgeom%edge_nodes)) call c_f_pointer(c_meshgeom%edge_nodes, meshgeom%edge_nodes, (/ 2, c_meshgeomdim%numedge /))
   if(c_associated(c_meshgeom%face_nodes)) call c_f_pointer(c_meshgeom%face_nodes, meshgeom%face_nodes, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   if(c_associated(c_meshgeom%edge_faces)) call c_f_pointer(c_meshgeom%edge_faces, meshgeom%edge_faces, (/ 2, c_meshgeomdim%numedge /))
   if(c_associated(c_meshgeom%face_edges)) call c_f_pointer(c_meshgeom%face_edges, meshgeom%face_edges, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))
   if(c_associated(c_meshgeom%face_links)) call c_f_pointer(c_meshgeom%face_links, meshgeom%face_links, (/ c_meshgeomdim%maxnumfacenodes, c_meshgeomdim%numface /))

   !Network variables
   if(c_associated(c_meshgeom%nnodex)) call c_f_pointer(c_meshgeom%nnodex, meshgeom%nnodex,(/c_meshgeomdim%nnodes/))
   if(c_associated(c_meshgeom%nnodey)) call c_f_pointer(c_meshgeom%nnodey, meshgeom%nnodey,(/c_meshgeomdim%nnodes/))
   if(c_associated(c_meshgeom%nedge_nodes)) call c_f_pointer(c_meshgeom%nedge_nodes, meshgeom%nedge_nodes, (/ 2, c_meshgeomdim%nbranches /))
   if(c_associated(c_meshgeom%nbranchlengths)) call c_f_pointer(c_meshgeom%nbranchlengths, meshgeom%nbranchlengths, (/ c_meshgeomdim%nbranches /))
   if(c_associated(c_meshgeom%nbranchgeometrynodes)) call c_f_pointer(c_meshgeom%nbranchgeometrynodes, meshgeom%nbranchgeometrynodes, (/ c_meshgeomdim%nbranches /))
   if(c_associated(c_meshgeom%ngeopointx)) call c_f_pointer(c_meshgeom%ngeopointx, meshgeom%ngeopointx, (/ c_meshgeomdim%ngeometry/))
   if(c_associated(c_meshgeom%ngeopointy)) call c_f_pointer(c_meshgeom%ngeopointy, meshgeom%ngeopointy, (/ c_meshgeomdim%ngeometry/))
   if(c_associated(c_meshgeom%nbranchorder)) call c_f_pointer(c_meshgeom%nbranchorder, meshgeom%nbranchorder, (/ c_meshgeomdim%nbranches/))
   if(c_associated(c_meshgeom%nodebranchidx)) call c_f_pointer(c_meshgeom%nodebranchidx, meshgeom%nodebranchidx, (/ c_meshgeomdim%numnode/))
   if(c_associated(c_meshgeom%nodeoffsets)) call c_f_pointer(c_meshgeom%nodeoffsets, meshgeom%nodeoffsets, (/ c_meshgeomdim%numnode /))

   if(c_associated(c_meshgeom%nodex)) call c_f_pointer(c_meshgeom%nodex, meshgeom%nodex,(/c_meshgeomdim%numnode/))
   if(c_associated(c_meshgeom%nodey)) call c_f_pointer(c_meshgeom%nodey, meshgeom%nodey,(/c_meshgeomdim%numnode/))
   if(c_associated(c_meshgeom%nodez)) call c_f_pointer(c_meshgeom%nodez, meshgeom%nodez,(/c_meshgeomdim%numnode/))
   if(c_associated(c_meshgeom%edgex)) call c_f_pointer(c_meshgeom%edgex, meshgeom%edgex,(/c_meshgeomdim%numedge/))
   if(c_associated(c_meshgeom%edgey)) call c_f_pointer(c_meshgeom%edgey, meshgeom%edgey,(/c_meshgeomdim%numedge/))
   if(c_associated(c_meshgeom%edgez)) call c_f_pointer(c_meshgeom%edgez, meshgeom%edgez,(/c_meshgeomdim%numedge/))
   if(c_associated(c_meshgeom%facex)) call c_f_pointer(c_meshgeom%facex, meshgeom%facex,(/c_meshgeomdim%numface/))
   if(c_associated(c_meshgeom%facey)) call c_f_pointer(c_meshgeom%facey, meshgeom%facey,(/c_meshgeomdim%numface/))
   if(c_associated(c_meshgeom%facez)) call c_f_pointer(c_meshgeom%facez, meshgeom%facez,(/c_meshgeomdim%numface/))
   if(c_associated(c_meshgeom%layer_zs)) call c_f_pointer(c_meshgeom%layer_zs, meshgeom%layer_zs,(/c_meshgeomdim%num_layers/))
   if(c_associated(c_meshgeom%interface_zs)) call c_f_pointer(c_meshgeom%interface_zs, meshgeom%interface_zs,(/c_meshgeomdim%num_layers + 1/))

   if(c_associated(c_meshgeom%nodeids)) call c_f_pointer(c_meshgeom%nodeids, meshgeom%nodeids, (/c_meshgeomdim%numnode/))
   if(c_associated(c_meshgeom%nodelongnames)) call c_f_pointer(c_meshgeom%nodelongnames, meshgeom%nodelongnames, (/c_meshgeomdim%numnode/))

   if(c_associated(c_meshgeom%nbranchids)) call c_f_pointer(c_meshgeom%nbranchids, meshgeom%nbranchids,(/c_meshgeomdim%nbranches/))
   if(c_associated(c_meshgeom%nbranchlongnames)) call c_f_pointer(c_meshgeom%nbranchlongnames, meshgeom%nbranchlongnames,(/c_meshgeomdim%nbranches/))
   if(c_associated(c_meshgeom%nnodeids)) call c_f_pointer(c_meshgeom%nnodeids, meshgeom%nnodeids,(/c_meshgeomdim%nnodes/))
   if(c_associated(c_meshgeom%nnodelongnames)) call c_f_pointer(c_meshgeom%nnodelongnames, meshgeom%nnodelongnames,(/c_meshgeomdim%nnodes/))

end function convert_cptr_to_meshgeom

!> by deallocating what is associated, no memory leaks
function t_ug_meshgeom_destructor(meshgeom) result(ierr)

   type(t_ug_meshgeom), intent(inout) :: meshgeom
   integer  :: ierr

   if(associated(meshgeom%edge_nodes)) deallocate(meshgeom%edge_nodes)
   if(associated(meshgeom%face_nodes)) deallocate(meshgeom%face_nodes)
   if(associated(meshgeom%edge_faces)) deallocate(meshgeom%edge_faces)
   if(associated(meshgeom%face_edges)) deallocate(meshgeom%face_edges)
   if(associated(meshgeom%face_links)) deallocate(meshgeom%face_links)


   if(associated(meshgeom%nnodex)) deallocate(meshgeom%nnodex)
   if(associated(meshgeom%nnodey)) deallocate(meshgeom%nnodey)
   if(associated(meshgeom%nnodeids)) deallocate(meshgeom%nnodeids)
   if(associated(meshgeom%nnodelongnames)) deallocate(meshgeom%nnodelongnames)
   if(associated(meshgeom%nedge_nodes)) deallocate(meshgeom%nedge_nodes)
   if(associated(meshgeom%nbranchlengths)) deallocate(meshgeom%nbranchlengths)
   if(associated(meshgeom%nbranchgeometrynodes)) deallocate(meshgeom%nbranchgeometrynodes)
   if(associated(meshgeom%ngeopointx)) deallocate(meshgeom%ngeopointx)
   if(associated(meshgeom%ngeopointy)) deallocate(meshgeom%ngeopointy)
   if(associated(meshgeom%nbranchorder)) deallocate(meshgeom%nbranchorder)

   if(associated(meshgeom%nodebranchidx)) deallocate(meshgeom%nodebranchidx)
   if(associated(meshgeom%nodeoffsets)) deallocate(meshgeom%nodeoffsets)

   if(associated(meshgeom%nodex)) deallocate(meshgeom%nodex)
   if(associated(meshgeom%nodey)) deallocate(meshgeom%nodey)
   if(associated(meshgeom%nodez)) deallocate(meshgeom%nodez)
   if(associated(meshgeom%edgex)) deallocate(meshgeom%edgex)
   if(associated(meshgeom%edgey)) deallocate(meshgeom%edgey)

   if(associated(meshgeom%edgez)) deallocate(meshgeom%edgez)
   if(associated(meshgeom%facex)) deallocate(meshgeom%facex)
   if(associated(meshgeom%facey)) deallocate(meshgeom%facey)
   if(associated(meshgeom%facez)) deallocate(meshgeom%facez)
   if(associated(meshgeom%layer_zs)) deallocate(meshgeom%layer_zs)
   if(associated(meshgeom%interface_zs)) deallocate(meshgeom%interface_zs)

   if(associated(meshgeom%nodeids)) deallocate(meshgeom%nodeids)
   if(associated(meshgeom%nodelongnames)) deallocate(meshgeom%nodelongnames)
   if(associated(meshgeom%nbranchids)) deallocate(meshgeom%nbranchids)
   if(associated(meshgeom%nbranchlongnames)) deallocate(meshgeom%nbranchlongnames)
   if(associated(meshgeom%nnodeids)) deallocate(meshgeom%nnodeids)
   if(associated(meshgeom%nnodelongnames)) deallocate(meshgeom%nnodelongnames)

   meshgeom%dim             = -1
   meshgeom%numnode         = -1
   meshgeom%numedge         = -1
   meshgeom%numface         = -1
   meshgeom%maxnumfacenodes = -1
   meshgeom%num_layers      = -1
   meshgeom%layertype       = -1
   meshgeom%nnodes          = -1
   meshgeom%nbranches       = -1
   meshgeom%ngeometry       = -1
   meshgeom%start_index     = -1
   meshgeom%epsg            = -1
   meshgeom%meshname        = ''
   ierr = 0

   !nullify
   meshgeom%edge_nodes => null()
   meshgeom%face_nodes => null()
   meshgeom%edge_faces => null()
   meshgeom%face_edges => null()
   meshgeom%face_links => null()

   meshgeom%nnodex => null()
   meshgeom%nnodey => null()
   meshgeom%nnodeids => null()
   meshgeom%nnodelongnames       => null()
   meshgeom%nedge_nodes          => null()

   meshgeom%nbranchlengths       => null()
   meshgeom%nbranchgeometrynodes => null()
   meshgeom%ngeopointx => null()
   meshgeom%ngeopointy => null()
   meshgeom%nbranchorder => null()
   meshgeom%nodebranchidx => null()
   meshgeom%nodeoffsets => null()

   meshgeom%nodex => null()
   meshgeom%nodey => null()
   meshgeom%nodez => null()
   meshgeom%edgex => null()
   meshgeom%edgey => null()
   meshgeom%edgez => null()
   meshgeom%facex => null()
   meshgeom%facey => null()
   meshgeom%facez => null()

   meshgeom%layer_zs     => null()
   meshgeom%interface_zs => null()

   meshgeom%nodeids       => null()
   meshgeom%nodelongnames => null()
   meshgeom%nbranchids     => null()
   meshgeom%nbranchlongnames     => null()
   meshgeom%nnodeids         => null()
   meshgeom%nnodelongnames   => null()

   end function t_ug_meshgeom_destructor

end module meshdata
