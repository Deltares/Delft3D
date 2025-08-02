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

module m_ug_meshgeom

   use m_ug_constants
   implicit none

   !> Structure for storing an entire mesh geometry (topology and coordinates and more).
   !> This is general data structures shared also by gridgeom
   type t_ug_meshgeom
   ! TODO: AvD: extend this to 3D (volumes)
      character(len=ug_nameLen) :: meshname                !< Name of this mesh ! TODO: AvD: should this be in this data type?
      integer                   :: dim             = -1    !< Dimensionality of the mesh (1/2/3)
      integer                   :: numnode         = -1    !< Number of mesh nodes.
      integer                   :: numedge         = -1    !< Number of mesh edges (size of kn)
      integer                   :: numface         = -1    !< Number of mesh faces.
      integer                   :: maxnumfacenodes = -1    !< Maximum of number of face nodes.
      integer                   :: num_layers      = -1    !< Number of mesh layers (num interfaces == num_layers + 1), num_layers = 0 means "no layers".
      integer                   :: numtopsig       = -1    !< Number of top sigma layers in the case of z-sigma coordinates.
      integer                   :: layertype       = -1    !< Type of vertical layer definition (only if num_layers >= 1), one of LAYERTYPE_* parameters.
      integer                   :: nnodes          = -1    !< Number of branches
      integer                   :: nbranches       = -1    !< Number of branches
      integer                   :: ngeometry       = -1    !< Number of geometrical points
      integer                   :: start_index     = -1    !< The base index of the arrays
      integer                   :: epsg            = -1    !< epsg code that uniquely identifies the coordinate reference system

      integer, pointer   :: edge_nodes(:,:) => null()       !< Edge-to-node mapping array.
      integer, pointer   :: face_nodes(:,:) => null()       !< Face-to-node mapping array.
      integer, pointer   :: edge_faces(:,:) => null()       !< Edge-to-face mapping array (optional, can be null()).
      integer, pointer   :: face_edges(:,:) => null()       !< Face-to-edge mapping array (optional, can be null()).
      integer, pointer   :: face_links(:,:) => null()       !< Face-to-face mapping array (optional, can be null()).

      !Network1d variables
      double precision,                  pointer :: nnodex(:)               => null()   !< x-coordinates of the network points.
      double precision,                  pointer :: nnodey(:)               => null()   !< y-coordinates of the network points.
      character(len=ug_idsLen),          pointer :: nnodeids(:)             => null()   !< network nodes ids description
      character(len=ug_idsLongNamesLen), pointer :: nnodelongnames(:)       => null()   !< network nodes nnodelongnames description

      integer,                           pointer :: nedge_nodes(:,:)        => null()   !< Start-end node of each branch
      character(len=ug_idsLen),          pointer :: nbranchids(:)           => null()   !< Branch nodes ids
      character(len=ug_idsLongNamesLen), pointer :: nbranchlongnames(:)     => null()   !< Branch long names
      double precision,                  pointer :: nbranchlengths(:)       => null()   !< Branch lenghts
      integer,                           pointer :: nbranchgeometrynodes(:) => null()   !< Number of geometry points in each branch
      double precision,                  pointer :: ngeopointx(:)           => null()   !< x-coordinates of geometry points.
      double precision,                  pointer :: ngeopointy(:)           => null()   !< y-coordinates of geometry points.
      integer,                           pointer :: nbranchorder(:)         => null()   !< the branch order

      !Mesh1d variables
      integer,                           pointer :: nodebranchidx(:)  => null()           !< The branch index of each 1d mesh point
      integer,                           pointer :: nodeidx(:)        => null()           !< node indices in main netnodes array corresponding to branchid/nodeoffset array
      integer,                           pointer :: nodeidx_inverse(:)=> null()           !< node indices in main netnodes array corresponding to branchid/nodeoffset array
      double precision,                  pointer :: nodeoffsets(:)    => null()           !< The branch offset of each 1d mesh point
      integer,                           pointer :: edgebranchidx(:)  => null()           !< The branch index of each 1d mesh edge ! TODO: UNST-2716: also incorporate these two new fields in _c interface and C# wrappers.
      double precision,                  pointer :: edgeoffsets(:)    => null()           !< The branch offset of each 1d mesh edge
      character(len=ug_idsLen),          pointer :: nodeids(:)        => null()
      character(len=ug_idsLongNamesLen), pointer :: nodelongnames(:)  => null()
      character(len=ug_idsLen),          pointer :: branchids(:)      => null()
      character(len=ug_idsLongNamesLen), pointer :: branchlongnames(:)=> null()
      character(len=ug_idsLen),          pointer :: nnodesids(:)      => null()
      character(len=ug_idsLongNamesLen), pointer :: nnodesnames(:)    => null()

      double precision, pointer :: nodex(:)=> null()       !< x-coordinates of the mesh nodes.
      double precision, pointer :: nodey(:)=> null()       !< y-coordinates of the mesh nodes.
      double precision, pointer :: nodez(:)=> null()       !< z-coordinates of the mesh nodes.
      double precision, pointer :: edgex(:)=> null()       !< x-coordinates of the mesh edges.
      double precision, pointer :: edgey(:)=> null()       !< y-coordinates of the mesh edges.
      double precision, pointer :: edgez(:)=> null()       !< z-coordinates of the mesh edges.
      double precision, pointer :: facex(:)=> null()       !< x-coordinates of the mesh faces.
      double precision, pointer :: facey(:)=> null()       !< y-coordinates of the mesh faces.
      double precision, pointer :: facez(:)=> null()       !< z-coordinates of the mesh faces.

      double precision, pointer :: layer_zs(:) => null()    !< Vertical coordinates of the mesh layers' center (either z or sigma).
      double precision, pointer :: interface_zs(:)=> null() !< Vertical coordinates of the mesh layers' interface (either z or sigma).


   end type t_ug_meshgeom


end module m_ug_meshgeom
