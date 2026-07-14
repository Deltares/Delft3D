"""
Generates m_unc_put_var_map_generated.f90 — all unc_put_var_map overloads for all dtype/rank combinations.
Usage: python generate_unc_put_var_map.py <output_file>
"""

import sys
from pathlib import Path

# --- Type definitions --------------------------------------------------------


class FortranType:
    def __init__(self, dtype: str, name: str):
        self.dtype = dtype  # e.g. "real(kind=dp)"
        self.name = name  # suffix for procedure name, e.g. "dble"

    @staticmethod
    def all_rank1():
        """Types that get the full rank-1 body (all iloc cases including 3D)."""
        return [
            FortranType("real(kind=dp)", "dble"),
            FortranType("integer", "int"),
            FortranType("real(kind=4)", "real"),
            FortranType("integer(kind=1)", "byte"),
        ]


# --- Rank-1 body (full select case with all iloc) ---------------------------


def generate_rank1(ftype: FortranType) -> str:
    proc = f"unc_put_var_map_{ftype.name}"
    T = ftype.dtype
    return f"""\
   !> Write variable specified by id_var and values to netcdf file ncid on the location specified by iloc. 
   function {proc}(ncid, id_tsp, id_var, iloc_in, values, default_value, jabndnd) result(ierr)

   implicit none

   integer, intent(in) :: ncid !< file ID of open netcdf file.
   type(t_unc_timespace_id), intent(in) :: id_tsp !> unc_timespace_id, only the index for current time is needed.
   integer, intent(in), dimension(:) :: id_var !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
   integer, intent(in) :: iloc_in !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
   {T}, intent(in), target, dimension(:) :: values !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
   {T}, optional, intent(in) :: default_value !< Optional default value to be written when no value is available.
   integer, optional, intent(in) :: jabndnd !< flag specifying whether boundary nodes are written (1) or not (0).

   integer :: ierr

   integer :: ndx2d, n1d_write
   integer :: lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
   integer :: iloc, n_open, g
   logical :: masked_edge_2d, special_output
   {T}, allocatable, save, target :: work(:)
   {T}, pointer :: output_values(:)
   {T}, allocatable, save :: work_layers(:,:), work_interfaces(:,:)

   ierr = DFM_NOERR
   nullify(output_values)

   if (present(jabndnd)) then
      associate (dummy => jabndnd)
      end associate
   end if

   ndx2d = flowgeom%mesh2d%numFace
   n1d_write = flowgeom%mesh1D%numNode
   iloc = iloc_in

   ! Keep the common write paths zero-copy. Only the niche reduced-output and
   ! surface-from-3D modes prepare a temporary in output order.
   masked_edge_2d = allocated(flowgeom%edge_flowlink_map_2D)
   output_values => values
   special_output = .false.

   if (iloc == UNC_LOC_CN) then
      special_output = allocated(flowgeom%node_map_2D)
   else if (iloc == UNC_LOC_S) then
      special_output = allocated(flowgeom%face_map_2D) .or. allocated(flowgeom%node_map_1D)
   else if (iloc == UNC_LOC_S3D) then
      special_output = write_surface_data_to_map_file
   end if

   if (special_output) then
      select case (iloc)
      case (UNC_LOC_CN)
         call realloc(work, flowgeom%mesh2d%numNode, keepExisting=.false.)
         do i = 1, flowgeom%mesh2d%numNode
            work(i) = values(flowgeom%node_map_2D(i))
         end do

      case (UNC_LOC_S)
         call realloc(work, flowgeom%ndx_out, keepExisting=.false.)
         if (allocated(flowgeom%face_map_2D)) then
            do i = 1, ndx2d
               work(i) = values(flowgeom%face_map_2D(i))
            end do
         else
            work(1:ndx2d) = values(1:ndx2d)
         end if
         if (allocated(flowgeom%node_map_1D)) then
            do i = 1, n1d_write
               work(ndx2d + i) = values(flowgeom%node_map_1D(i))
            end do
         else if (n1d_write > 0) then
            work(ndx2d + 1:ndx2d + n1d_write) = values(ndx2d + 1:ndx2d + n1d_write)
         end if

      case (UNC_LOC_S3D)
         ! A map-file surface is a 2D pressure-point field derived from the top
         ! active layer. Prepare it in output-face order before the normal write.
         iloc = UNC_LOC_S
         n1d_write = 0
         call realloc(work, ndx2d, keepExisting=.false.)
         do n = 1, ndx2d
            if (allocated(flowgeom%face_map_2D)) then
               g = flowgeom%face_map_2D(n)
            else
               g = n
            end if
            work(n) = values(ktop(g))
         end do
      end select

      output_values => work
   end if

   select case (iloc)
   case (UNC_LOC_CN) ! Corner point location
      ! Internal 1d netnodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. n1d_write > 0) then ! If there are 1d flownodes, then there are 1d netnodes.
         ierr = UG_NOTIMPLEMENTED
         return
      end if
      if (id_var(2) > 0 .and. ndx2d > 0) then ! If there are 2d flownodes, then there are 2d netnodes.
         ierr = nf90_put_var(ncid, id_var(2), output_values(1:flowgeom%mesh2d%numNode), start=[1, id_tsp%idx_curtime])
      end if

   case (UNC_LOC_S) ! Pressure point location
      ! Internal 1d flownodes. Horizontal position: nodes in 1d mesh.
      if (id_var(1) > 0 .and. n1d_write > 0) then
         ierr = nf90_put_var(ncid, id_var(1), output_values(ndx2d + 1:ndx2d + n1d_write), start=[1, id_tsp%idx_curtime])
      end if
      ! Internal 2d flownodes. Horizontal position: faces in 2d mesh.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), output_values(1:ndx2d), start=[1, id_tsp%idx_curtime])
      end if

   case (UNC_LOC_U) ! Horizontal velocity point location
      ! Internal 1d flowlinks. Horizontal position: edges in 1d mesh.
      if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
         if (size(flowgeom%edge_map_1D, 1) > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(flowgeom%edge_map_1D(:)), start=[1, id_tsp%idx_curtime])
         end if
      end if
      if (id_var(4) > 0 .and. flowgeom%n1d2dcontacts > 0) then
         ! 1d2d contacts
         if (size(flowgeom%contacts_map, 1) > 0) then
            ierr = nf90_put_var(ncid, id_var(4), values(flowgeom%contacts_map(:)), start=[1, id_tsp%idx_curtime])
         end if
      end if
      if (masked_edge_2d) then
         ! Reduced output: gather open 2d edges (internal+boundary) in output order, then default on closed edges.
         n_open = flowgeom%lnx2d_int + flowgeom%lnx2d_bnd
         if (id_var(2) > 0 .and. n_open > 0) then
            call realloc(work, n_open, keepExisting=.false.)
            do i = 1, n_open
               work(i) = values(flowgeom%edge_flowlink_map_2D(i))
            end do
            ierr = nf90_put_var(ncid, id_var(2), work(1:n_open), start=[1, id_tsp%idx_curtime])
         end if
         if (present(default_value) .and. id_var(2) > 0 .and. flowgeom%numl2d_closed > 0) then
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[n_open + 1, id_tsp%idx_curtime], &
                                count=[flowgeom%numl2d_closed, 1], map=[0])
         end if
      else
         lnx2d = lnxi - lnx1d
         ! Internal 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1d + 1:lnxi), start=[1, id_tsp%idx_curtime])
         end if
         lnx2db = lnx - lnx1db
         ! External 2d flowlinks. Horizontal position: edges in 2d mesh.
         if (id_var(2) > 0 .and. lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), values(lnx1db + 1:lnx), start=[lnx2d + 1, id_tsp%idx_curtime])
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all closed edges.
            if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
               ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[lnx2d + lnx2db + 1, id_tsp%idx_curtime], &
                                   count=[numl2d - lnx2d - lnx2db, 1], map=[0])
            end if
         end if
      end if

   case (UNC_LOC_L) ! Horizontal net link location
      if (masked_edge_2d) then
         ! Reduced output: gather directly on output edges (all edges, including closed, carry net link data).
         if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
            call realloc(work, flowgeom%mesh1D%numEdge, keepExisting=.false.)
            do i = 1, flowgeom%mesh1D%numEdge
               work(i) = values(abs(ln2lne(flowgeom%edge_map_1D(i))))
            end do
            ierr = nf90_put_var(ncid, id_var(1), work(1:flowgeom%mesh1D%numEdge), start=[1, id_tsp%idx_curtime])
         end if
         if (id_var(2) > 0 .and. flowgeom%mesh2d%numEdge > 0) then
            call realloc(work, flowgeom%mesh2d%numEdge, keepExisting=.false.)
            do i = 1, flowgeom%mesh2d%numEdge
               work(i) = values(numl1d + flowgeom%edge_map_2D(i))
            end do
            ierr = nf90_put_var(ncid, id_var(2), work(1:flowgeom%mesh2d%numEdge), start=[1, id_tsp%idx_curtime])
         end if
      else
         ! NOTE: In the ugrid geometry, edges have been order based on flow link order. All non-flowlink net links are at the end of the edge array.
         call realloc(work, numl, keepExisting=.false.)
         ! Permute the input values(:) from netlink ordering to flow link ordering. TODO: cache
         do Lf = 1, lnx1d
            L = abs(ln2lne(Lf))
            work(Lf) = values(L)
         end do
         ! 1D: write all values on 1D flow links. ! TODO: AvD: for 1D I now assume that all net links are also a flow link. This is not always true (thin dams), so make code below equal to 2D code hereafter.
         if (id_var(1) > 0 .and. lnx1d > 0) then  ! TODO: AvD: along with previous TODO, this should become numl1d
            ierr = nf90_put_var(ncid, id_var(1), work(1:lnx1d), start=[1, id_tsp%idx_curtime])
         end if
         ! 2D: permute all values on net links such that flow links come first, followed by remaining non-flowlink net links.
         lnx2d = lnxi - lnx1d
         lnx2db = lnx - lnx1db
         i = lnx2d + lnx2db ! last position in permuted array of a written non-flowlink net link (none as a start, i.e., last 2d flow link)
         do L = numl1d + 1, numl ! Only 2D net links
            Lf = lne2ln(L) ! If negative, then no flow link
            if (Lf > lnx1db) then ! 2D open boundary flow link
               ! Values on netlinks that are also flowlinks come first.
               work(Lf - lnx1db + lnx2d) = values(L)
            else if (Lf > lnx1d) then ! 2D internal flow link. This intentionally excludes 2D net links that are 1D2D flow links.
               ! Values on netlinks that are also flowlinks come first.
               work(Lf - lnx1d) = values(L) 
            else
               ! Values on netlinks that are no flowlinks come as a last block (in remaining net link order).
               i = i + 1
               work(i) = values(L)
            end if
         end do
         if (id_var(2) > 0 .and. numl - numl1d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), work(1:(numl - numl1d)), start=[1, id_tsp%idx_curtime])
         end if
      end if

   case (UNC_LOC_S3D) ! Pressure point location in all layers.
      call realloc(work_layers, [kmx, flowgeom%ndx_out], keepExisting=.false.)
      do n = 1, flowgeom%ndx_out ! Loop over horizontal flownodes (output order: 2d faces first, then 1d nodes).
         if (n <= ndx2d) then
            if (allocated(flowgeom%face_map_2D)) then
               g = flowgeom%face_map_2D(n)
            else
               g = n
            end if
         else
            if (allocated(flowgeom%node_map_1D)) then
               g = flowgeom%node_map_1D(n - ndx2d)
            else
               g = n
            end if
         end if
         work_layers(:, n) = dmiss ! Store missing values for inactive layers (i.e. z layers below bottomlevel or above waterlevel for current horizontal flownode g).
         call getlayerindices(g, nlayb, nrlay) ! The current horizontal flownode g has active layers nlayb:nlayb+nrlay-1.
         call getkbotktop(g, kb, kt) ! The current horizontal flownode g has indices kb:kt in values array (one value per active layer).
         ! The range kb:kt can have a different length for each flownode due to inactive layers.
         ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
         ! Loop over active layers.         
         do k = kb, kt
            work_layers(k - kb + nlayb, n) = values(k)
         end do
      end do
      ! Internal 2dv flownodes. Horizontal position: nodes in 1d mesh. Vertical position: layer centers.
      if (id_var(1) > 0 .and. n1d_write > 0) then
         ierr = nf90_put_var(ncid, id_var(1), work_layers(1:kmx, ndx2d + 1:ndx2d + n1d_write), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx, n1d_write, 1])
      end if
      ! Internal 3d flownodes. Horizontal position: faces in 2d mesh. Vertical position: layer centers.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), work_layers(1:kmx, 1:ndx2d), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx, ndx2d, 1])
      end if

   ! TODO: AvD: include flow link bug fix (Feb 15, 2017) from 1d/2D above also in U3D and WU code below.
   case (UNC_LOC_U3D)
      n_open = 0
      if (masked_edge_2d) n_open = flowgeom%lnx2d_int + flowgeom%lnx2d_bnd
      call realloc(work_layers, [kmx, lnx + n_open], keepExisting=.false.)
      do LL = 1, lnx ! Loop over horizontal flowlinks.
         work_layers(:, LL) = dmiss
         call getlayerindicesLmax(LL, nlaybL, nrlayLx) ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
         call getLbotLtopmax(LL, Lb, Ltx) ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
         ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
         ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
         ! Loop over active layers.
         do L = Lb, Ltx
            work_layers(L - Lb + nlaybL, LL) = values(L)
         end do
      end do
      ! Internal 2dv horizontal flowlinks. Horizontal position: edges in 1d mesh. Vertical position: layer centers.
      if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
         if (size(flowgeom%edge_map_1D, 1) > 0) then
            ierr = nf90_put_var(ncid, id_var(1), work_layers(1:kmx, flowgeom%edge_map_1D(:)), &
                                start=[1, 1, id_tsp%idx_curtime], count=[kmx, size(flowgeom%edge_map_1D, 1), 1])
         end if
      end if
      if (masked_edge_2d) then
         ! Reduced output: gather open 2d edges (internal+boundary) in output order, then default on closed edges.
         n_open = flowgeom%lnx2d_int + flowgeom%lnx2d_bnd
         if (id_var(2) > 0 .and. n_open > 0) then
            do i = 1, n_open
               work_layers(1:kmx, lnx + i) = work_layers(1:kmx, flowgeom%edge_flowlink_map_2D(i))
            end do
            ierr = nf90_put_var(ncid, id_var(2), work_layers(1:kmx, lnx + 1:lnx + n_open), &
                                start=[1, 1, id_tsp%idx_curtime], count=[kmx, n_open, 1])
         end if
         if (present(default_value) .and. id_var(2) > 0 .and. flowgeom%numl2d_closed > 0) then
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, n_open + 1, id_tsp%idx_curtime], &
                                count=[kmx, flowgeom%numl2d_closed, 1], map=[0, 0, 0])
         end if
      else
         lnx2d = lnx - lnx1d ! TODO: AvD: now also includes 1D bnds, dont want that.
         ! Internal and external 3d horizontal flowlinks (and 2dv external flowlinks). Horizontal position: edges in 2d mesh. Vertical position: layer centers.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), work_layers(1:kmx, lnx1d + 1:lnx), &
                                start=[1, 1, id_tsp%idx_curtime], count=[kmx, lnx2d, 1])
         end if
         ! Default value is different from a fill value, use for example for zero velocities on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            numl2d = numl - numl1d
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], &
                                count=[kmx, numl2d - lnx2d, 1], map=[0, 0, 0])
         end if
      end if

   case (UNC_LOC_W) ! Vertical velocity point location on all layer interfaces.
      call realloc(work_interfaces, [kmx, flowgeom%ndx_out], lindex=[0, 1], keepExisting=.false.)
      ! Loop over horizontal flownodes (output order: 2d faces first, then 1d nodes).
      do n = 1, flowgeom%ndx_out
         if (n <= ndx2d) then
            if (allocated(flowgeom%face_map_2D)) then
               g = flowgeom%face_map_2D(n)
            else
               g = n
            end if
         else
            if (allocated(flowgeom%node_map_1D)) then
               g = flowgeom%node_map_1D(n - ndx2d)
            else
               g = n
            end if
         end if
         work_interfaces(:, n) = dmiss
         call getlayerindices(g, nlayb, nrlay) ! The current horizontal flownode g has active layers nlayb:nlayb+nrlay-1.
         call getkbotktop(g, kb, kt) ! The current horizontal flownode g has indices kb:kt in values array (one value per active layer).
         ! The range kb:kt can have a different length for each flownode due to inactive layers.
         ! Here kb corresponds to nlayb and kt corresponds to nlayb+nrlay-1
         ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
         do k = kb - 1, kt
            work_interfaces(k - kb + nlayb, n) = values(k)
         end do
      end do
      ! Internal 2dv vertical flowlinks. Horizontal position: nodes in 1d mesh. Vertical position: layer interfaces.
      if (id_var(1) > 0 .and. n1d_write > 0) then
         ierr = nf90_put_var(ncid, id_var(1), work_interfaces(0:kmx, ndx2d + 1:ndx2d + n1d_write), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, n1d_write, 1])
      end if
      ! Internal 3d vertical flowlinks. Horizontal position: faces in 2d mesh. Vertical position: layer interfaces.
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), work_interfaces(0:kmx, 1:ndx2d), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, ndx2d, 1])
      end if

   case (UNC_LOC_WU) ! Vertical viscosity point location on all layer interfaces.
      n_open = 0
      if (masked_edge_2d) n_open = flowgeom%lnx2d_int + flowgeom%lnx2d_bnd
      call realloc(work_interfaces, [kmx, lnx + n_open], lindex=[0, 1], keepExisting=.false.)
      do LL = 1, lnx ! Loop over horizontal flowlinks.
         work_interfaces(:, LL) = dmiss
         call getlayerindicesLmax(LL, nlaybL, nrlayLx) ! The current horizontal flowlink LL has active layers nlaybL:nlaybL+nrlayLx-1.
         call getLbotLtopmax(LL, Lb, Ltx) ! The current horizontal flowlink LL has indices Lb:Ltx in values array (one value per active layer).
         ! The range Lb:Ltx can have a different length for each flowlink due to inactive layers.
         ! Here Lb corresponds to nlaybL and Ltx corresponds to nlaybL+nrlayLx-1
         ! Loop over active layer interfaces. First active layer interface has index of first active layer - 1.
         do L = Lb - 1, Ltx
            work_interfaces(L - Lb + nlaybL, LL) = values(L)
         end do
      end do
      if (masked_edge_2d) then
         ! Reduced output: 1d edges via edge_map_1D, 2d open edges gathered, default on closed edges.
         if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
            if (size(flowgeom%edge_map_1D, 1) > 0) then
               ierr = nf90_put_var(ncid, id_var(1), work_interfaces(0:kmx, flowgeom%edge_map_1D(:)), &
                                   start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, size(flowgeom%edge_map_1D, 1), 1])
            end if
         end if
         n_open = flowgeom%lnx2d_int + flowgeom%lnx2d_bnd
         if (id_var(2) > 0 .and. n_open > 0) then
            do i = 1, n_open
               work_interfaces(0:kmx, lnx + i) = work_interfaces(0:kmx, flowgeom%edge_flowlink_map_2D(i))
            end do
            ierr = nf90_put_var(ncid, id_var(2), work_interfaces(0:kmx, lnx + 1:lnx + n_open), &
                                start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, n_open, 1])
         end if
         if (present(default_value) .and. id_var(2) > 0 .and. flowgeom%numl2d_closed > 0) then
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, n_open + 1, id_tsp%idx_curtime], &
                                count=[kmx + 1, flowgeom%numl2d_closed, 1], map=[0])
         end if
      else
         ! Internal 2dv vertical viscosity points. Horizontal position: edges in 1d mesh. Vertical position: layer interfaces.
         if (id_var(1) > 0 .and. lnx1d > 0) then
            ierr = nf90_put_var(ncid, id_var(1), work_interfaces(0:kmx, 1:lnx1d), &
                                start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx1d, 1])
         end if
         lnx2d = lnx - lnx1d
         ! Internal and external 3d vertical viscosity points (and 2dv external viscosity points). Horizontal position: edges in 2d mesh. Vertical position: layer interfaces.
         if (id_var(2) > 0 .and. lnx2d > 0) then
            ierr = nf90_put_var(ncid, id_var(2), work_interfaces(0:kmx, lnx1d + 1:lnx), &
                                start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx2d, 1])
         end if
         ! Default value is different from a fill value, use for example for zero values on closed edges.
         if (id_var(2) > 0 .and. present(default_value)) then
            numl2d = numl - numl1d ! Number of netlinks can be > number of flowlinks, if there are closed edges.
            ! Write default_value on all remaining edges in 2d mesh (i.e. closed edges).
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], &
                                count=[kmx + 1, numl2d - lnx2d, 1], map=[0])
         end if
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      return
   end select

   nullify(output_values)

   end function {proc}"""


# --- Top-level generation ----------------------------------------------------


def generate(output_file: Path) -> None:
    rank1_types = FortranType.all_rank1()

    interface_procs = []
    subroutines = []

    # Rank-1 overloads
    for ftype in rank1_types:
        proc = f"unc_put_var_map_{ftype.name}"
        interface_procs.append(f"      module procedure {proc}")
        subroutines.append(generate_rank1(ftype))

    interface_block = "\n".join(interface_procs)
    all_bodies = "\n\n".join(subroutines)

    content = f"""\
! This file is generated by generate_unc_put_var_map.py. do not edit manually.
module m_unc_put_var_map_generated
   use m_unstruc_netcdf_data, only: t_unc_timespace_id, flowgeom
   use m_unstruc_netcdf_types, only: t_fm_flowgeom
   use io_ugrid, only: UG_NOTIMPLEMENTED, UG_INVALID_DATALOCATION
   use netcdf, only: nf90_put_var, nf90_inquire_variable, nf90_inquire_dimension
   use precision, only: dp
   use m_flowgeom, only: lnx1d, lnxi, lnx, lnx1db, ln2lne, lne2ln
   use dfm_error, only: dfm_noerr
   use m_alloc, only: realloc
   use m_missing, only: dmiss
   use fm_location_types, only: unc_loc_cn, unc_loc_s, unc_loc_u, unc_loc_l, unc_loc_s3d, unc_loc_u3d, unc_loc_w, unc_loc_wu
   use m_get_kbot_ktop, only: getkbotktop
   use m_get_layer_indices, only: getlayerindices
   use m_get_layer_indices_l_max, only: getlayerindiceslmax
   use m_get_Lbot_Ltop_max, only: getlbotltopmax
   use m_flowparameters, only: write_surface_data_to_map_file
   use network_data, only: numl, numl1d
   use m_flow, only: kmx, ktop

   implicit none(type, external)

   interface unc_put_var_map
      {interface_block}
   end interface unc_put_var_map

contains

{all_bodies}

end module m_unc_put_var_map_generated
"""
    output_file.parent.mkdir(parents=True, exist_ok=True)
    output_file.write_text(content, encoding="utf-8")
    print(f"Generated {output_file}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {{sys.argv[0]}} <output_file>")
        sys.exit(1)

    generate(Path(sys.argv[1]))
