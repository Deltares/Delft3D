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
      ! Names of local variables are matching the original code
      integer :: i
      integer :: k
      integer :: id_3d
      integer :: kb
      integer :: kt
      integer :: nlayb
      integer :: nrlay

      do i = 1, count_2d_cells
         call getkbotktop(i, kb, kt)
         call getlayerindices(i, nlayb, nrlay)
         do k = 1, count_layers
            id_3d = (i - 1) * count_layers + k
            if (k < nlayb .OR. k > nrlay) then
               cell_center_mesh_coordinates_3d(3 * id_3d) = -999.0_dp ! or some other invalid value to indicate out-of-domain points
            else
               cell_center_mesh_coordinates_3d(3 * id_3d) = 0.5 * (zws(kb + k - 1) + zws(kb + k - 2))
            end if
         end do
      end do
   end subroutine set_cell_center_mesh_zcoords

end module precice_adapter_utils
