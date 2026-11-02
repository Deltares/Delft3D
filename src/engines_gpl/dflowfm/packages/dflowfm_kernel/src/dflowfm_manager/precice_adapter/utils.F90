module precice_adapter_utils
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double
contains

   !> Set the z-coordinates of a 3D cell-centered mesh based on the vertical coordinates of the cell faces (zws).
   !! Assumption: The 2D mesh is already constructed
   !! The code is copied from unstruc_netcdf, tracking id_flowelemzcc
   !! @return cell_center_mesh_coordinates_3d: A 1D array containing the x,y,z-coordinates of the 3D cell-centered mesh, ordered as x1,y1,z1,x2,y2,z2,...,xN,yN,zN
   !!         This is the format expected by preCICE for mesh coordinates.
   subroutine set_cell_center_mesh_zcoords(count_2d_cells, count_layers, zws, cell_center_mesh_coordinates_3d)
      use precision, only: dp
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_layer_indices, only: getlayerindices

      implicit none(type, external)

      integer(kind=c_int), intent(in) :: count_2d_cells
      integer(kind=c_int), intent(in) :: count_layers
      real(kind=c_double), dimension(:), intent(in) :: zws
      real(kind=c_double), dimension(:), intent(out) :: cell_center_mesh_coordinates_3d

      integer :: cell_index_2d
      integer :: cell_index_3d
      integer :: bottom_cell_index_3d
      integer :: top_cell_index_3d
      integer :: bottom_layer_index
      integer :: max_layer_count_from_bottom
      integer :: precice_index

      cell_center_mesh_coordinates_3d(3::3) = -999.0_dp
      do cell_index_2d = 1, count_2d_cells
         call getkbotktop(cell_index_2d, bottom_cell_index_3d, top_cell_index_3d)
         call getlayerindices(cell_index_2d, bottom_layer_index, max_layer_count_from_bottom)
         do cell_index_3d = bottom_cell_index_3d, top_cell_index_3d
            precice_index = (cell_index_2d - 1) * count_layers + cell_index_3d - bottom_cell_index_3d + bottom_layer_index
            cell_center_mesh_coordinates_3d(3 * precice_index) = 0.5_dp * (zws(cell_index_3d) + zws(cell_index_3d - 1))
         end do
      end do
   end subroutine set_cell_center_mesh_zcoords

   !> Pack a flow-node scalar field in the same cell/layer order as the preCICE 3D mesh.
   !! Excludes the 2D prefix and column ghost entries; inactive mesh layers receive zero.
   subroutine pack_cell_center_mesh_data(count_2d_cells, count_layers, flow_values, mesh_values)
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_layer_indices, only: getlayerindices

      implicit none(type, external)

      integer(kind=c_int), intent(in) :: count_2d_cells !< Number of horizontal mesh cells.
      integer(kind=c_int), intent(in) :: count_layers !< Number of mesh layer slots per cell.
      real(kind=c_double), dimension(:), intent(in) :: flow_values !< Field indexed by FM flow nodes.
      real(kind=c_double), dimension(:), intent(out) :: mesh_values !< Packed preCICE vertex values.

      integer :: cell_index_2d
      integer :: cell_index_3d
      integer :: bottom_cell_index_3d
      integer :: top_cell_index_3d
      integer :: bottom_layer_index
      integer :: max_layer_count_from_bottom
      integer :: precice_index

      mesh_values = 0.0_c_double
      do cell_index_2d = 1, count_2d_cells
         call getkbotktop(cell_index_2d, bottom_cell_index_3d, top_cell_index_3d)
         call getlayerindices(cell_index_2d, bottom_layer_index, max_layer_count_from_bottom)
         do cell_index_3d = bottom_cell_index_3d, top_cell_index_3d
            precice_index = (cell_index_2d - 1) * count_layers + cell_index_3d - bottom_cell_index_3d + bottom_layer_index
            mesh_values(precice_index) = flow_values(cell_index_3d)
         end do
      end do
   end subroutine pack_cell_center_mesh_data

end module precice_adapter_utils
