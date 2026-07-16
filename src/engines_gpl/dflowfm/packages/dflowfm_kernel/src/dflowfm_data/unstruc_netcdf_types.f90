!TODO: move other unstruc netcdf types from unstruc_netcdf_data.f90 to this module.

module m_unstruc_netcdf_types

   use m_ug_meshgeom, only: t_ug_meshgeom

   implicit none(type, external)

!> D-Flow FM-specific flow geometry object, intended for two features:
!! * UGRID-compliant output must write 1D and 2D mesh parts as separate geometries.
!! * An optional masking polygon to restrict output to a particular region needs bookkeeping of
!!   the reduced geometry numbering in relation to the original global numbering.
!! The t_ug_meshgeom members hold the UGRID-compliant mesh data (nodes, edges, faces,
!! connectivity) for 1d and 2d separately, such that they can be passed directly to io_ugrid write routines.
!! The remaining members hold D-FlowFM-specific administration, amongst others mapping from 1D+2D
!! global numbering in D-Flow FM to the 1D/2D separate (possibly mask-reduced) UGRID numbering.
   type t_fm_flowgeom

      type(t_ug_meshgeom) :: mesh2D !< Node/edge/face topology and coordinates for the 2D mesh.
      type(t_ug_meshgeom) :: mesh1D !< Node/edge/face topology and coordinates for the 1D mesh.

      logical :: remapping_active = .false. !< True when polygon output reduction requires the mapping arrays below.
      integer, dimension(:), allocatable :: face_map_2D !< 2D: mapping from reduced output set UGRID face index to global flow cell number.
      integer, dimension(:), allocatable :: edge_map_2D !< 2D: mapping from reduced output set UGRID edge index to local 2D net link index.
      integer, dimension(:), allocatable :: node_map_2D !< 2D: mapping from reduced output set UGRID node index to global flow node number.
      integer, dimension(:), allocatable :: node_map_1D !< 1D: mapping from reduced output set UGRID node index to global flow node number.

      integer, dimension(:), allocatable :: edge_type !< Edge type array (size numl2d): encodes the flow-link relation for each 2D mesh edge.
      integer, dimension(:), allocatable :: edge_map_1D !< 1D: mapping from mesh1D UGRID edge index to flow link number.
      integer, dimension(:), allocatable :: contacts_map !< 1D2D: mapping from contact index to flow link number.
      integer, dimension(:, :), allocatable :: contacts !< 1D2D contact node pairs [2, n1d2dcontacts].
      integer, dimension(:), allocatable :: contacttype !< 1D2D contact type per contact entry.
      integer :: n1d2dcontacts = 0 !< Number of 1D2D contacts.

      !> fm-specific counters, difference between internal and boundary nodes/links, needed for output writing
      integer :: lnx2d_int = 0 !< Number of internal 2D flow links in the output set.
      integer :: lnx2d_bnd = 0 !< Number of boundary 2D flow links in the output set.
      integer :: numl2d_closed = 0 !< Number of closed 2D edges in the output set.
      integer :: ndx_out = 0 !< Total output nodes (3D work array loop bound).
      integer, allocatable :: netlink_perm(:) !< Pre-computed permutation for UNC_LOC_L writing.

   end type t_fm_flowgeom

end module m_unstruc_netcdf_types
