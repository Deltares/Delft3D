module m_unc_put_var_map
use m_unstruc_netcdf_data, only: t_unc_timespace_id
use m_flowgeom, only: t_fm_flowgeom
use m_unc_build_flowgeom, only: flowgeom
use io_ugrid, only: UG_NOTIMPLEMENTED, UG_INVALID_DATALOCATION
use netcdf, only: nf90_put_var, nf90_inquire_variable, nf90_inquire_dimension

implicit none(type,external)

   interface unc_put_var_map
      module procedure unc_put_var_map_dble2
      module procedure unc_put_var_map_dble3
   end interface unc_put_var_map

contains

!> copy of unc_put_var_map_byte with buffered time
!! TODO: only implemented for UNC_LOC_S
   function unc_put_var_map_byte_timebuffer(ncid, id_tsp, id_var, iloc, values, t1, tl, jabndnd) result(ierr)
      
      use dfm_error, only: dfm_noerr
      use fm_location_types, only: unc_loc_s
      use m_unc_build_flowgeom, only: flowgeom

      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      integer(kind=1), intent(in) :: values(:, :) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      integer, intent(in) :: t1 !< first time in buffer to be written
      integer, intent(in) :: tl !< last time in buffer to be written
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: tstart !< time index of t1
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: ndx2d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      jabndnd_ = jabndnd
      ierr = DFM_NOERR

      ndx2d     = flowgeom%mesh2d%numFace
      n1d_write = flowgeom%mesh1D%numNode

      select case (iloc)

      case (UNC_LOC_S) ! Pressure point location
         tstart = id_tsp%idx_curtime - tl + t1
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:ndx2d + n1d_write, t1:tl), start=[1, tstart])
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d, t1:tl), start=[1, tstart])
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_byte_timebuffer
!
   function unc_put_var_map_dble2(ncid, id_tsp, id_var, iloc, values, default_value, locdim, jabndnd) result(ierr)
      use precision, only: dp
      use m_flowgeom, only: lnx1d, lnxi, lnx, lnx1db
      use dfm_error, only: dfm_noerr
      use fm_location_types, only: unc_loc_s, unc_loc_u
      use network_data, only: numl, numl1d
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      real(kind=dp), intent(in) :: values(:, :) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      real(kind=dp), optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: locdim !< Optional index of the location dimension (default = 1)
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: lnx2d, lnx2db, numl2d
      integer :: ilocdim
      integer :: lndim
      integer, dimension(3) :: dimids_var
      real(kind=dp), allocatable :: work(:, :)
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndx2d !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR
      jabndnd_ = jabndnd

      if (present(locdim)) then
         ilocdim = locdim
      else
         ilocdim = 1
      end if

      ndx2d     = flowgeom%mesh2d%numFace
      n1d_write = flowgeom%mesh1D%numNode
      last_1d = ndx2d + n1d_write
      select case (iloc)
      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(n1d_write, size(values, 2)))
               work = values(ndx2d + 1:last_1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), n1d_write))
               work = values(:, ndx2d + 1:last_1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(ndx2d, size(values, 2)))
               work = values(1:ndx2d, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), ndx2d))
               work = values(:, 1:ndx2d)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx1d, size(values, 2)))
               work = values(1:lnx1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx1d))
               work = values(:, 1:lnx1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnxi - lnx1d, size(values, 2)))
               work = values(lnx1d + 1:lnxi, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnxi - lnx1d))
               work = values(:, lnx1d + 1:lnxi)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx - lnx1db, size(values, 2)))
               work = values(lnx1db + 1:lnx, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[lnx2d + 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx - lnx1db))
               work = values(:, lnx1db + 1:lnx)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, lnx2d + 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_inquire_variable(ncid, id_var(2), dimids=dimids_var)
               ! Use map = 0 to write a single value on multiple edges in file.
               select case (ilocdim)
               case (1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(2), len=lndim)
                  ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[lnx2d + lnx2db + 1, 1, id_tsp%idx_curtime], count=[numl2d - lnx2d - lnx2db, lndim, 1], map=[0])
               case (2)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(1), len=lndim)
                  ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + lnx2db + 1, id_tsp%idx_curtime], count=[lndim, numl2d - lnx2d - lnx2db, 1], map=[0])
               end select
            end if
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_dble2

   function unc_put_var_map_dble3(ncid, id_tsp, id_var, iloc, values, default_value, locdim, jabndnd) result(ierr)
      use precision, only: dp
      use m_flowgeom, only: lnx1d, lnxi, lnx, lnx1db
      use dfm_error, only: dfm_noerr
      use fm_location_types, only: unc_loc_s, unc_loc_u
      use network_data, only: numl, numl1d
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      real(kind=dp), intent(in) :: values(:, :, :) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      real(kind=dp), optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: locdim !< Optional index of the location dimension (default = 1)
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: lnx2d, lnx2db, numl2d
      integer :: ilocdim
      integer :: lndim1, lndim2
      integer, dimension(4) :: dimids_var
      real(kind=dp), allocatable :: work(:, :, :)
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndx2d !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR
      if (present(locdim)) then
         ilocdim = locdim
      else
         ilocdim = 1
      end if
      jabndnd_ = jabndnd

      ndx2d     = flowgeom%mesh2d%numFace
      n1d_write = flowgeom%mesh1D%numNode
      last_1d = ndx2d + n1d_write

      select case (iloc)
      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(n1d_write, size(values, 2), size(values, 3)))
               work = values(ndx2d + 1:last_1d, :, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), n1d_write, size(values, 3)))
               work = values(:, ndx2d + 1:last_1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), n1d_write))
               work = values(:, :, ndx2d + 1:last_1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(ndx2d, size(values, 2), size(values, 3)))
               work = values(1:ndx2d, :, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), ndx2d, size(values, 3)))
               work = values(:, 1:ndx2d, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), ndx2d))
               work = values(:, :, 1:ndx2d)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx1d, size(values, 2), size(values, 3)))
               work = values(1:lnx1d, :, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx1d, size(values, 3)))
               work = values(:, 1:lnx1d, :)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), lnx1d))
               work = values(:, :, 1:lnx1d)
               ierr = nf90_put_var(ncid, id_var(1), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnxi - lnx1d, size(values, 2), size(values, 3)))
               work = values(lnx1d + 1:lnxi, :, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnxi - lnx1d, size(values, 3)))
               work = values(:, lnx1d + 1:lnxi, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), lnxi - lnx1d))
               work = values(:, :, lnx1d + 1:lnxi)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            select case (ilocdim)
            case (1)
               allocate (work(lnx - lnx1db, size(values, 2), size(values, 3)))
               work = values(lnx1db + 1:lnx, :, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[lnx2d + 1, 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (2)
               allocate (work(size(values, 1), lnx - lnx1db, size(values, 3)))
               work = values(:, lnx1db + 1:lnx, :)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, lnx2d + 1, 1, id_tsp%idx_curtime])
               deallocate (work)
            case (3)
               allocate (work(size(values, 1), size(values, 2), lnx - lnx1db))
               work = values(:, :, lnx1db + 1:lnx)
               ierr = nf90_put_var(ncid, id_var(2), work, start=[1, 1, lnx2d + 1, id_tsp%idx_curtime])
               deallocate (work)
            end select
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_inquire_variable(ncid, id_var(2), dimids=dimids_var)
               ! Use map = 0 to write a single value on multiple edges in file.
               select case (ilocdim)
               case (1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(2), len=lndim1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(3), len=lndim2)
                  ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[lnx2d + lnx2db + 1, 1, 1, id_tsp%idx_curtime], count=[numl2d - lnx2d - lnx2db, lndim1, lndim2, 1], map=[0])
               case (2)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(1), len=lndim1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(3), len=lndim2)
                  ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + lnx2db + 1, 1, id_tsp%idx_curtime], count=[lndim1, numl2d - lnx2d - lnx2db, lndim2, 1], map=[0])
               case (3)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(1), len=lndim1)
                  ierr = nf90_inquire_dimension(ncid, dimids_var(2), len=lndim2)
                  ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, 1, lnx2d + lnx2db + 1, id_tsp%idx_curtime], count=[lndim1, lndim2, numl2d - lnx2d - lnx2db, 1], map=[0])
               end select
            end if
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_dble3

end module m_unc_put_var_map
