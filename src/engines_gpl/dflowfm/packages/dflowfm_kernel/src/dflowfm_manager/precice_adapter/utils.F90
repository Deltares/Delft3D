module precice_adapter_utils
   use, intrinsic :: iso_c_binding, only: c_int, c_char, c_double
   contains

   subroutine set_cell_center_mesh_zcoords(cell_center_mesh_2d_size, count_layers, zws, cell_center_mesh_coordinates_3d)
         ! Copied from unstruc_netcdf, tracking id_flowelemzcc:
         !work1 = dmiss ! For zcc, can start at index 1 (kmx   vertical values)
         !do kk = 1, ndxi
         !   call getkbotktop(kk, kb, kt)
         !   call getlayerindices(kk, nlayb, nrlay)
         !   do k = kb, kt
         !      work1(k - kb + nlayb, kk) = 0.5 * (zws(k) + zws(k - 1))
         !   end do
         !end do
         !ierr = nf90_put_var(imapfile, id_flowelemzcc(iid), work1(1:kmx, 1:ndxi), start=[1, 1, itim], count=[kmx, ndxi, 1])
      use precision, only: dp
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_layer_indices, only: getlayerindices

      integer(kind=c_int), intent(in) :: cell_center_mesh_2d_size
      integer(kind=c_int), intent(in) :: count_layers
      real(kind=c_double), dimension(:), intent(in) :: zws
      real(kind=c_double), dimension(:), intent(out) :: cell_center_mesh_coordinates_3d
      ! Local variables
      integer :: i, k, id_3d, kb, kt, nlayb, nrlay


      do i = 1, cell_center_mesh_2d_size
         call getkbotktop(i, kb, kt)
         call getlayerindices(i, nlayb, nrlay)
         do k = 1, count_layers
            id_3d = (i - 1) * count_layers + k
            if (k < nlayb .OR. k > nrlay) then
               cell_center_mesh_coordinates_3d(3 * id_3d) = -999.0_dp ! or some other invalid value to indicate out-of-domain points
            else
               cell_center_mesh_coordinates_3d(3 * id_3d) = 0.5 * (zws(kb + k - 1) + zws(kb + k - 2)) ! TODO: replace with actual z coordinate, currently just a placeholder
            end if
         end do
      end do

   end subroutine set_cell_center_mesh_zcoords
end module precice_adapter_utils
