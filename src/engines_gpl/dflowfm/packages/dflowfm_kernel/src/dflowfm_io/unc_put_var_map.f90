module m_unc_put_var_map
use unstruc_netcdf_data, only: t_unc_timespace_id
use m_flowgeom, only: t_fm_flowgeom
use unc_build_flowgeom, only: flowgeom
use io_ugrid, only: UG_NOTIMPLEMENTED, UG_INVALID_DATALOCATION
use netcdf, only: nf90_put_var, nf90_inquire_variable, nf90_inquire_dimension

implicit none(type,external)

   interface unc_put_var_map
      module procedure unc_put_var_map_int
      module procedure unc_put_var_map_real
      module procedure unc_put_var_map_dble
      module procedure unc_put_var_map_dble2
      module procedure unc_put_var_map_dble3
   end interface unc_put_var_map

contains
! TODO: AvD: support integer/other data types
! TODO: AvD: support in/exclude boundary points/links

!> Writes a map field of a flow variable to a NetCDF map file, taking care of 1D/2D/3D specifics and s/u/w-point specifics.
!! Only writes data for the current time. Assumes that the mapids%id_tsp%idx_curtime contains the new time index where to write to.
!! Produces a UGRID-compliant map file.
!! If there is a 1d and a 2d mesh, then values are written for both meshes in one call to this function.
!! Typical call: unc_put_var(mapids, mapids%id_s1(:), UNC_LOC_S, s1)

   function unc_put_var_map_int(ncid, id_tsp, id_var, iloc, integers, default_value, jabndnd) result(ierr)
      use precision, only: dp
      implicit none
      integer :: ierr
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable)
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_S, UNC_LOC_U, UNC_LOC_W).
      integer, dimension(:), intent(in) :: integers
      real(kind=dp), optional :: default_value
      integer, optional, intent(in) :: jabndnd

      real(kind=dp), dimension(:), allocatable :: values
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if

      allocate (values(size(integers)))
      values = integers ! casting an array of integers to an array of doubles
      if (present(default_value)) then
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd=jabndnd_)
      else
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, jabndnd=jabndnd_)
      end if
      deallocate (values)
   end function unc_put_var_map_int

   function unc_put_var_map_real(ncid, id_tsp, id_var, iloc, reals, default_value, jabndnd) result(ierr)
      use precision, only: dp
      implicit none
      integer :: ierr
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable)
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_S, UNC_LOC_U, UNC_LOC_W).
      real(kind=4), dimension(:), intent(in) :: reals
      real(kind=dp), optional :: default_value
      integer, optional, intent(in) :: jabndnd

      real(kind=dp), dimension(:), allocatable :: values
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if

      allocate (values(size(reals)))
      values = reals ! casting an array of reals to an array of doubles
      if (present(default_value)) then
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd=jabndnd_)
      else
         ierr = unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, jabndnd=jabndnd_)
      end if
      deallocate (values)
   end function unc_put_var_map_real

   function unc_put_var_map_dble(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd) result(ierr)
      use precision, only: dp
      use m_flowgeom, only: lnx1d, lnxi, lnx, lnx1db, ln2lne, lne2ln, t_fm_flowgeom
      use unc_build_flowgeom, only: flowgeom
      use dfm_error, only: dfm_noerr
      use m_alloc, only: realloc
      use m_missing, only: dmiss
      use fm_location_types, only: unc_loc_cn, unc_loc_s, unc_loc_u, unc_loc_l, unc_loc_s3d, unc_loc_u3d, unc_loc_w, unc_loc_wu
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_layer_indices, only: getlayerindices
      use m_get_layer_indices_l_max, only: getlayerindiceslmax
      use m_get_Lbot_Ltop_max, only: getlbotltopmax
      use network_data, only: numl, numl1d
      use m_flow, only: kmx

      implicit none

      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      real(kind=dp), intent(in) :: values(:) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      real(kind=dp), optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.

      !These two will be removed once 2d edge2ln and face2ln are properly built in flowgeom.
      integer :: ndx2d !< Number of 2D faces in the output set (from flowgeom).
      integer :: n1d_write !< Number of 1D nodes to write (from flowgeom).
      integer :: lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
!TODO remove save and deallocate?
      real(kind=dp), allocatable, save :: workL(:)
      real(kind=dp), allocatable, save :: workS3D(:, :), workU3D(:, :), workW(:, :), workWU(:, :)

      ierr = DFM_NOERR
      if (jabndnd == 1) then
      end if
      ! Output-set counts from the flowgeom object (already account for jabndnd via build_flowgeom).
      ndx2d = flowgeom%mesh2d%numFace
      n1d_write = flowgeom%mesh1D%numNode

      select case (iloc)
      case (UNC_LOC_CN) ! Corner point location
         ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then ! If there are 1d flownodes, then there are 1d netnodes.
            ierr = UG_NOTIMPLEMENTED ! TODO: AvD putting data on 1D corners not implemented yet.
            goto 888
         end if
         ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
            ierr = nf90_put_var(ncid, id_var(2), values(1:flowgeom%mesh2d%numNode), start=[1, id_tsp%idx_curtime])
         end if

      case (UNC_LOC_S) ! Pressure point location
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:ndx2d + n1d_write), start=[1, id_tsp%idx_curtime])
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d), start=[1, id_tsp%idx_curtime])
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
            ! 1d mesh
            if (size(flowgeom%edgetoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(1), values(flowgeom%edgetoln(:)), start=[1, id_tsp%idx_curtime])
            end if
         end if

         if (id_var(4) > 0 .and. flowgeom%n1d2dcontacts > 0) then
            ! 1d2d contacts
            if (size(flowgeom%contactstoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(4), values(flowgeom%contactstoln(:)), start=[1, id_tsp%idx_curtime])
            end if
         end if

         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1d + 1:lnxi), start=[1, id_tsp%idx_curtime])
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1db + 1:lnx), start=[lnx2d + 1, id_tsp%idx_curtime])
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[lnx2d + lnx2db + 1, id_tsp%idx_curtime], count=[numl2d - lnx2d - lnx2db, 1], map=[0]) ! Use map = 0 to write a single value on multiple edges in file.
            end if
         end if

      case (UNC_LOC_L) ! Horizontal net link location
         ! NOTE: In the ugrid geometry, edges have been order based on flow link order. All non-flowlink net links are at the end of the edge array.

         call realloc(workL, numl, keepExisting=.false.)

         ! Permute the input values(:) from netlink ordering to flow link ordering.
         ! TODO: AvD: cache this permutation for all future map writes in a flow() run.
         do Lf = 1, lnx1d
            L = abs(ln2lne(Lf))
            workL(Lf) = values(L)
         end do

         ! 1D: write all values on 1D flow links. ! TODO: AvD: for 1D I now assume that all net links are also a flow link. This is not always true (thin dams), so make code below equal to 2D code hereafter.
         if (id_var(1) > 0 .and. lnx1d > 0) then ! TODO: AvD: along with previous TODO, this should become numl1d
            ierr = nf90_put_var(ncid, id_var(1), workL(1:lnx1d), start=[1, id_tsp%idx_curtime])
         end if

         ! 2D: permute all values on net links such that flow links come first, followed by remaining non-flowlink net links.
         lnx2d = lnxi - lnx1d
         lnx2db = lnx - lnx1db
         i = lnx2d + lnx2db ! last position in permuted array of a written non-flowlink net link (none as a start, i.e., last 2d flow link)
         do L = numl1d + 1, numl ! Only 2D net links
            Lf = lne2ln(L) ! If negative, then no flow link

            if (Lf > lnx1db) then ! 2D open boundary flow link
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1db + lnx2d) = values(L)
            else if (Lf > lnx1d) then ! 2D internal flow link. This intentionally excludes 2D net links that are 1D2D flow links.
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1d) = values(L)
            else
               ! Values on netlinks that are no flowlinks come as a last block (in remaining net link order).
               i = i + 1
               workL(i) = values(L)
            end if
         end do
         if (id_var(2) > 0 .and. numl - numl1d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workL(1:(numl - numl1d)), start=[1, id_tsp%idx_curtime])
         end if

      case (UNC_LOC_S3D) ! Pressure point location in all layers.
         ! Fill work array.
         call realloc(workS3D, [kmx, flowgeom%ndx_out], keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, flowgeom%ndx_out
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workS3D(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layers.
            do k = kb, kt
               workS3D(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workS3D(1:kmx, ndx2d + 1:ndx2d + n1d_write), start=[1, 1, id_tsp%idx_curtime], count=[kmx, n1d_write, 1])
         end if
         ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workS3D(1:kmx, 1:ndx2d), start=[1, 1, id_tsp%idx_curtime], count=[kmx, ndx2d, 1])
         end if

         ! TODO: AvD: include flow link bug fix (Feb 15, 2017) from 1d/2D above also in U3D and WU code below.
      case (UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
         ! Fill work array.
         call realloc(workU3D, [kmx, lnx], keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workU3D(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layers.
            do L = Lb, Ltx
               workU3D(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
            if (size(flowgeom%edgetoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(1), workU3D(1:kmx, flowgeom%edgetoln(:)), start=[1, 1, id_tsp%idx_curtime], count=[kmx, size(flowgeom%edgetoln, 1), 1])
            end if
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d horizontal flowlinks (and 2dv external flowlinks). Horizontal position: edges in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workU3D(1:kmx, lnx1d + 1:lnx), start=[1, 1, id_tsp%idx_curtime], count=[kmx, lnx2d, 1])
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], count=[kmx, numl2d - lnx2d, 1], map=[0, 0, 0]) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case (UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workW, [kmx, flowgeom%ndx_out], lindex=[0, 1], keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, flowgeom%ndx_out
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workW(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do k = kb - 1, kt
               workW(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. n1d_write > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(1), workW(0:kmx, ndx2d + 1:ndx2d + n1d_write), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, n1d_write, 1])
         end if
         ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(2), workW(0:kmx, 1:ndx2d), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, ndx2d, 1])
         end if

      case (UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workWU, [kmx, lnx], lindex=[0, 1], keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workWU(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do L = Lb - 1, Ltx
               workWU(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workWU(0:kmx, 1:lnx1d), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx1d, 1])
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d vertical viscosity points (and 2dv external viscosity points). Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workWU(0:kmx, lnx1d + 1:lnx), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx2d, 1])
         end if
         ! Default value is different from a fill value, use for example for zero values on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], count=[kmx + 1, numl2d - lnx2d, 1], map=[0]) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_dble

!> copy of unc_put_var_map_dble for writing bytes
!! TODO: use templating
   function unc_put_var_map_byte(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd) result(ierr)
      use precision, only: dp
      use m_flowgeom, only: ndx, ndx1db, ndxi, ndx2d, lnx1d, lnxi, lnx, lnx1db, ln2lne, lne2ln
      use dfm_error, only: dfm_noerr
      use m_alloc, only: realloc
      use m_missing, only: dmiss
      use fm_location_types, only: unc_loc_cn, unc_loc_s, unc_loc_u, unc_loc_l, unc_loc_s3d, unc_loc_u3d, unc_loc_w, unc_loc_wu
      use m_get_kbot_ktop, only: getkbotktop
      use m_get_layer_indices, only: getlayerindices
      use m_get_layer_indices_l_max, only: getlayerindiceslmax
      use m_get_Lbot_Ltop_max, only: getlbotltopmax
      use network_data, only: numk, numl, numl1d
      use m_flow, only: kmx
      implicit none
      integer, intent(in) :: ncid
      type(t_unc_timespace_id), intent(in) :: id_tsp !< Map file and other NetCDF ids.
      integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
      integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
      integer(kind=1), intent(in) :: values(:) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
      integer(kind=1), optional, intent(in) :: default_value !< Optional default value, used for writing dummy data on closed edges (i.e. netlinks with no flowlink). NOTE: is not a _FillValue!
      integer, optional, intent(in) :: jabndnd

      integer :: ierr !< Result status, DFM_NOERR if successful.
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last 2d/3d node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
!TODO remove save and deallocate?
      real(kind=dp), allocatable, save :: workL(:)
      real(kind=dp), allocatable, save :: workS3D(:, :), workU3D(:, :), workW(:, :), workWU(:, :)

      ierr = DFM_NOERR

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      select case (iloc)
      case (UNC_LOC_CN) ! Corner point location
         ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. ndxi > ndx2d) then ! If there are 1d flownodes, then there are 1d netnodes.
            ierr = UG_NOTIMPLEMENTED ! TODO: AvD putting data on 1D corners not implemented yet.
            goto 888
         end if
         ! Internal 2d netnodes. Horizontal position: nodes in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
            ierr = nf90_put_var(ncid, id_var(2), values(1:numk), start=[1, id_tsp%idx_curtime])
         end if

      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:last_1d), start=[1, id_tsp%idx_curtime])
         end if
         ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d), start=[1, id_tsp%idx_curtime])
         end if

      case (UNC_LOC_U) ! Horizontal velocity point location
         ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ! 1d mesh
            if (size(id_tsp%edgetoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(1), values(id_tsp%edgetoln(:)), start=[1, id_tsp%idx_curtime])
            end if
         end if

         if (id_var(4) > 0 .and. lnx1d > 0) then
            ! 1d2d contacts
            if (size(id_tsp%contactstoln, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(4), values(id_tsp%contactstoln(:)), start=[1, id_tsp%idx_curtime])
            end if
         end if

         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1d + 1:lnxi), start=[1, id_tsp%idx_curtime])
         end if
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         lnx2db = lnx - lnx1db
         if (id_var(2) > 0 .and. lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1db + 1:lnx), start=[lnx2d + 1, id_tsp%idx_curtime])
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[lnx2d + lnx2db + 1, id_tsp%idx_curtime], count=[numl2d - lnx2d - lnx2db, 1], map=[0]) ! Use map = 0 to write a single value on multiple edges in file.
            end if
         end if

      case (UNC_LOC_L) ! Horizontal net link location
         ! NOTE: In the ugrid geometry, edges have been order based on flow link order. All non-flowlink net links are at the end of the edge array.

         call realloc(workL, numl, keepExisting=.false.)

         ! Permute the input values(:) from netlink ordering to flow link ordering.
         ! TODO: AvD: cache this permutation for all future map writes in a flow() run.
         do Lf = 1, lnx1d
            L = abs(ln2lne(Lf))
            workL(Lf) = values(L)
         end do

         ! 1D: write all values on 1D flow links. ! TODO: AvD: for 1D I now assume that all net links are also a flow link. This is not always true (thin dams), so make code below equal to 2D code hereafter.
         if (id_var(1) > 0 .and. lnx1d > 0) then ! TODO: AvD: along with previous TODO, this should become numl1d
            ierr = nf90_put_var(ncid, id_var(1), workL(1:lnx1d), start=[1, id_tsp%idx_curtime])
         end if

         ! 2D: permute all values on net links such that flow links come first, followed by remaining non-flowlink net links.
         lnx2d = lnxi - lnx1d
         lnx2db = lnx - lnx1db
         i = lnx2d + lnx2db ! last position in permuted array of a written non-flowlink net link (none as a start, i.e., last 2d flow link)
         do L = numl1d + 1, numl ! Only 2D net links
            Lf = lne2ln(L) ! If negative, then no flow link

            if (Lf > lnx1db) then ! 2D open boundary flow link
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1db + lnx2d) = values(L)
            else if (Lf > lnx1d) then ! 2D internal flow link. This intentionally excludes 2D net links that are 1D2D flow links.
               ! Values on netlinks that are also flowlinks come first.
               workL(Lf - lnx1d) = values(L)
            else
               ! Values on netlinks that are no flowlinks come as a last block (in remaining net link order).
               i = i + 1
               workL(i) = values(L)
            end if
         end do
         if (id_var(2) > 0 .and. numl - numl1d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workL(1:(numl - numl1d)), start=[1, id_tsp%idx_curtime])
         end if

      case (UNC_LOC_S3D) ! Pressure point location in all layers.
         ! Fill work array.
         call realloc(workS3D, [kmx, ndxi], keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, ndxi
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workS3D(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layers.
            do k = kb, kt
               workS3D(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         n1d_write = last_1d - ndx2d
         ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workS3D(1:kmx, ndx2d + 1:last_1d), start=[1, 1, id_tsp%idx_curtime], count=[kmx, n1d_write, 1])
         end if
         ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. ndx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workS3D(1:kmx, 1:ndx2d), start=[1, 1, id_tsp%idx_curtime], count=[kmx, ndx2d, 1])
         end if

         ! TODO: AvD: include flow link bug fix (Feb 15, 2017) from 1d/2D above also in U3D and WU code below.
      case (UNC_LOC_U3D) ! Horizontal velocity point location in all layers.
         ! Fill work array.
         call realloc(workU3D, [kmx, lnx], keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workU3D(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layers.
            do L = Lb, Ltx
               workU3D(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workU3D(1:kmx, 1:lnx1d), start=[1, 1, id_tsp%idx_curtime], count=[kmx, lnx1d, 1])
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d horizontal flowlinks (and 2dv external flowlinks). Horizontal position: edges in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workU3D(1:kmx, lnx1d + 1:lnx), start=[1, 1, id_tsp%idx_curtime], count=[kmx, lnx2d, 1])
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], count=[kmx, numl2d - lnx2d, 1], map=[0]) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case (UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workW, [kmx, ndxi], lindex=[0, 1], keepExisting=.false.)
         ! Loop over horizontal flownodes.
         do n = 1, ndxi
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode n).
            workW(:, n) = dmiss
            ! The current horizontal flownode n has active layers nlayb:nlayb+nrlay-1.
            call getlayerindices(n, nlayb, nrlay)
            ! The current horizontal flownode n has indices kb:kt in values array (one value per active layer).
            call getkbotktop(n, kb, kt)
            ! The range kb:kt can have a different length for each flownode due to inactive layers.
            ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do k = kb - 1, kt
               workW(k - kb + nlayb, n) = values(k)
            end do
         end do

         ! Write work array.
         n1d_write = last_1d - ndx2d
         ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. n1d_write > 0) then ! If there are 1d flownodes and layers, then there are 2dv vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(1), workW(0:kmx, ndx2d + 1:last_1d), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, n1d_write, 1])
         end if
         ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes and layers, then there are 3d vertical flowlinks.
            ierr = nf90_put_var(ncid, id_var(2), workW(0:kmx, 1:ndx2d), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, ndx2d, 1])
         end if

      case (UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
         ! Fill work array.
         call realloc(workWU, [kmx, lnx], lindex=[0, 1], keepExisting=.false.)
         ! Loop over horizontal flowlinks.
         do LL = 1, lnx
            ! Store missing values for inactive layer interfaces (i.e. z layers below bottomlevel or above waterlevel for current horizontal flowlink LL).
            workWU(:, LL) = dmiss
            ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
            call getlayerindicesLmax(LL, nlaybL, nrlayLx)
            ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
            call getLbotLtopmax(LL, Lb, Ltx)
            ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
            ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
            ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
            do L = Lb - 1, Ltx
               workWU(L - Lb + nlaybL, LL) = values(L)
            end do
         end do

         ! Write work array.
         ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workWU(0:kmx, 1:lnx1d), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx1d, 1])
         end if
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d vertical viscosity points (and 2dv external viscosity points). Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), workWU(0:kmx, lnx1d + 1:lnx), start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx2d, 1])
         end if
         ! Default value is different from a fill value, use for example for zero values on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], count=[kmx + 1, numl2d - lnx2d, 1], map=[0]) ! Use map = 0 to write a single value on multiple edges in file.
         end if

      case default
         ierr = UG_INVALID_DATALOCATION
         goto 888
      end select

      return ! Successful return.

888   continue
      ! Some error occurred
   end function unc_put_var_map_byte

!> copy of unc_put_var_map_byte with buffered time
!! TODO: only implemented for UNC_LOC_S
   function unc_put_var_map_byte_timebuffer(ncid, id_tsp, id_var, iloc, values, t1, tl, jabndnd) result(ierr)
      use m_flowgeom, only: ndx, ndx1db, ndxi, ndx2d
      use dfm_error, only: dfm_noerr
      use fm_location_types, only: unc_loc_s
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
      integer :: n1d_write !< Number of 1D nodes to write.
      integer :: jabndnd_ !< Flag specifying whether boundary nodes are to be written.
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

      select case (iloc)

      case (UNC_LOC_S) ! Pressure point location
         n1d_write = last_1d - ndx2d
         tstart = id_tsp%idx_curtime - tl + t1
         ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
         if (id_var(1) > 0 .and. n1d_write > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:last_1d, t1:tl), start=[1, tstart])
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

   function unc_put_var_map_dble2(ncid, id_tsp, id_var, iloc, values, default_value, locdim, jabndnd) result(ierr)
      use precision, only: dp
      use m_flowgeom, only: ndx, ndx1db, ndxi, ndx2d, lnx1d, lnxi, lnx, lnx1db
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
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR
      if (present(locdim)) then
         ilocdim = locdim
      else
         ilocdim = 1
      end if

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

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
      use m_flowgeom, only: ndx, ndx1db, ndxi, ndx2d, lnx1d, lnxi, lnx, lnx1db
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
      integer :: ndxndxi !< Last node to be saved. Equals ndx when boundary nodes are written, or ndxi otherwise.
      integer :: last_1d !< Last 1d node to be saved. Equals ndx1db when boundary nodes are written, or ndxi otherwise.

      ierr = DFM_NOERR
      if (present(locdim)) then
         ilocdim = locdim
      else
         ilocdim = 1
      end if

      if (present(jabndnd)) then
         jabndnd_ = jabndnd
      else
         jabndnd_ = 0
      end if
      if (jabndnd_ == 1) then
         ndxndxi = ndx
         last_1d = ndx1db
      else
         ndxndxi = ndxi
         last_1d = ndxi
      end if

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
