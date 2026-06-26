"""
Generates m_unc_put_var_map_generated.f90 — all unc_put_var_map overloads for all dtype/rank combinations.
Usage: python generate_unc_put_var_map.py <output_file>
"""

import sys
from pathlib import Path


# --- Type definitions --------------------------------------------------------

class FortranType:
    def __init__(self, dtype: str, name: str):
        self.dtype = dtype                       # e.g. "real(kind=dp)"
        self.name = name                         # suffix for procedure name, e.g. "dble"

    @staticmethod
    def all_rank1():
        """Types that get the full rank-1 body (all iloc cases including 3D)."""
        return [
            FortranType("real(kind=dp)", "dble"),
            FortranType("integer",       "int"),
            FortranType("real(kind=4)",   "real"),
            FortranType("integer(kind=1)", "byte"),
        ]

# --- Rank-1 body (full select case with all iloc) ---------------------------

def generate_rank1(ftype: FortranType) -> str:
    proc = f"unc_put_var_map_{ftype.name}"
    T = ftype.dtype
    return f"""\
   !> Write variable specified by id_var and values to netcdf file ncid on the location specified by iloc. 
   function {proc}(ncid, id_tsp, id_var, iloc, values, default_value, jabndnd) result(ierr)

   implicit none

   integer, intent(in) :: ncid !< file ID of open netcdf file.
   type(t_unc_timespace_id), intent(in) :: id_tsp !> unc_timespace_id, only the index for current time is needed.
   integer, intent(in) :: id_var(:) !< Ids of variable to write values into, one for each submesh (1d/2d/3d if applicable).
   integer, intent(in) :: iloc !< Stagger location for this variable (one of UNC_LOC_CN, UNC_LOC_S, UNC_LOC_U, UNC_LOC_L, UNC_LOC_S3D, UNC_LOC_U3D, UNC_LOC_W).
   {T}, intent(in) :: values(:) !< The data values to be written. Should in standard FM order (1d/2d/3d node/link conventions, @see m_flow).
   {T}, optional, intent(in) :: default_value !< Optional default value to be written when no value is available.
   integer, optional, intent(in) :: jabndnd !< flag specifying whether boundary nodes are written (1) or not (0).

   integer :: ierr

   integer :: ndx2d, n1d_write
   integer :: lnx2d, lnx2db, numl2d, Lf, L, i, n, k, kb, kt, nlayb, nrlay, LL, Lb, Ltx, nlaybL, nrlayLx
   {T}, allocatable, save :: workL(:)
   {T}, allocatable, save :: workS3D(:, :), workU3D(:, :), workW(:, :), workWU(:, :)

   ierr = DFM_NOERR

   if (present(jabndnd)) then
      associate (dummy => jabndnd)
      end associate
   end if

   ndx2d = flowgeom%mesh2d%numFace
   n1d_write = flowgeom%mesh1D%numNode

   select case (iloc)
   case (UNC_LOC_CN)
      if (id_var(1) > 0 .and. n1d_write > 0) then
         ierr = UG_NOTIMPLEMENTED
         goto 888
      end if
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(1:flowgeom%mesh2d%numNode), start=[1, id_tsp%idx_curtime])
      end if

   case (UNC_LOC_S)
      if (id_var(1) > 0 .and. n1d_write > 0) then
         ierr = nf90_put_var(ncid, id_var(1), values(ndx2d + 1:ndx2d + n1d_write), start=[1, id_tsp%idx_curtime])
      end if
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(1:ndx2d), start=[1, id_tsp%idx_curtime])
      end if

   case (UNC_LOC_U)
      if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
         if (size(flowgeom%edge_map_1D, 1) > 0) then
            ierr = nf90_put_var(ncid, id_var(1), values(flowgeom%edge_map_1D(:)), start=[1, id_tsp%idx_curtime])
         end if
      end if
      if (id_var(4) > 0 .and. flowgeom%n1d2dcontacts > 0) then
         if (size(flowgeom%contacts_map, 1) > 0) then
            ierr = nf90_put_var(ncid, id_var(4), values(flowgeom%contacts_map(:)), start=[1, id_tsp%idx_curtime])
         end if
      end if
      lnx2d = lnxi - lnx1d
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(lnx1d + 1:lnxi), start=[1, id_tsp%idx_curtime])
      end if
      lnx2db = lnx - lnx1db
      if (id_var(2) > 0 .and. lnx2db > 0) then
         ierr = nf90_put_var(ncid, id_var(2), values(lnx1db + 1:lnx), start=[lnx2d + 1, id_tsp%idx_curtime])
      end if
      if (present(default_value)) then
         numl2d = numl - numl1d
         if (id_var(2) > 0 .and. numl2d - lnx2d - lnx2db > 0) then
            ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[lnx2d + lnx2db + 1, id_tsp%idx_curtime], &
                                count=[numl2d - lnx2d - lnx2db, 1], map=[0])
         end if
      end if

   case (UNC_LOC_L)
      call realloc(workL, numl, keepExisting=.false.)
      do Lf = 1, lnx1d
         L = abs(ln2lne(Lf))
         workL(Lf) = values(L)
      end do
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workL(1:lnx1d), start=[1, id_tsp%idx_curtime])
      end if
      lnx2d = lnxi - lnx1d
      lnx2db = lnx - lnx1db
      i = lnx2d + lnx2db
      do L = numl1d + 1, numl
         Lf = lne2ln(L)
         if (Lf > lnx1db) then
            workL(Lf - lnx1db + lnx2d) = values(L)
         else if (Lf > lnx1d) then
            workL(Lf - lnx1d) = values(L)
         else
            i = i + 1
            workL(i) = values(L)
         end if
      end do
      if (id_var(2) > 0 .and. numl - numl1d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workL(1:(numl - numl1d)), start=[1, id_tsp%idx_curtime])
      end if

   case (UNC_LOC_S3D)
      call realloc(workS3D, [kmx, flowgeom%ndx_out], keepExisting=.false.)
      do n = 1, flowgeom%ndx_out
         workS3D(:, n) = dmiss
         call getlayerindices(n, nlayb, nrlay)
         call getkbotktop(n, kb, kt)
         do k = kb, kt
            workS3D(k - kb + nlayb, n) = values(k)
         end do
      end do
      if (id_var(1) > 0 .and. n1d_write > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workS3D(1:kmx, ndx2d + 1:ndx2d + n1d_write), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx, n1d_write, 1])
      end if
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workS3D(1:kmx, 1:ndx2d), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx, ndx2d, 1])
      end if

   case (UNC_LOC_U3D)
      call realloc(workU3D, [kmx, lnx], keepExisting=.false.)
      do LL = 1, lnx
         workU3D(:, LL) = dmiss
         call getlayerindicesLmax(LL, nlaybL, nrlayLx)
         call getLbotLtopmax(LL, Lb, Ltx)
         do L = Lb, Ltx
            workU3D(L - Lb + nlaybL, LL) = values(L)
         end do
      end do
      if (id_var(1) > 0 .and. flowgeom%mesh1D%numEdge > 0) then
         if (size(flowgeom%edge_map_1D, 1) > 0) then
            ierr = nf90_put_var(ncid, id_var(1), workU3D(1:kmx, flowgeom%edge_map_1D(:)), &
                                start=[1, 1, id_tsp%idx_curtime], count=[kmx, size(flowgeom%edge_map_1D, 1), 1])
         end if
      end if
      lnx2d = lnx - lnx1d
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workU3D(1:kmx, lnx1d + 1:lnx), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx, lnx2d, 1])
      end if
      if (id_var(2) > 0 .and. present(default_value)) then
         numl2d = numl - numl1d
         ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], &
                             count=[kmx, numl2d - lnx2d, 1], map=[0, 0, 0])
      end if

   case (UNC_LOC_W)
      call realloc(workW, [kmx, flowgeom%ndx_out], lindex=[0, 1], keepExisting=.false.)
      do n = 1, flowgeom%ndx_out
         workW(:, n) = dmiss
         call getlayerindices(n, nlayb, nrlay)
         call getkbotktop(n, kb, kt)
         do k = kb - 1, kt
            workW(k - kb + nlayb, n) = values(k)
         end do
      end do
      if (id_var(1) > 0 .and. n1d_write > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workW(0:kmx, ndx2d + 1:ndx2d + n1d_write), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, n1d_write, 1])
      end if
      if (id_var(2) > 0 .and. ndx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workW(0:kmx, 1:ndx2d), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, ndx2d, 1])
      end if

   case (UNC_LOC_WU)
      call realloc(workWU, [kmx, lnx], lindex=[0, 1], keepExisting=.false.)
      do LL = 1, lnx
         workWU(:, LL) = dmiss
         call getlayerindicesLmax(LL, nlaybL, nrlayLx)
         call getLbotLtopmax(LL, Lb, Ltx)
         do L = Lb - 1, Ltx
            workWU(L - Lb + nlaybL, LL) = values(L)
         end do
      end do
      if (id_var(1) > 0 .and. lnx1d > 0) then
         ierr = nf90_put_var(ncid, id_var(1), workWU(0:kmx, 1:lnx1d), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx1d, 1])
      end if
      lnx2d = lnx - lnx1d
      if (id_var(2) > 0 .and. lnx2d > 0) then
         ierr = nf90_put_var(ncid, id_var(2), workWU(0:kmx, lnx1d + 1:lnx), &
                             start=[1, 1, id_tsp%idx_curtime], count=[kmx + 1, lnx2d, 1])
      end if
      if (id_var(2) > 0 .and. present(default_value)) then
         numl2d = numl - numl1d
         ierr = nf90_put_var(ncid, id_var(2), [default_value], start=[1, lnx2d + 1, id_tsp%idx_curtime], &
                             count=[kmx + 1, numl2d - lnx2d, 1], map=[0])
      end if

   case default
      ierr = UG_INVALID_DATALOCATION
      goto 888
   end select

   return
888 continue
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
   use m_unstruc_netcdf_data, only: t_unc_timespace_id, t_fm_flowgeom
   use io_ugrid, only: UG_NOTIMPLEMENTED, UG_INVALID_DATALOCATION
   use netcdf, only: nf90_put_var, nf90_inquire_variable, nf90_inquire_dimension
   use precision, only: dp
   use m_flowgeom, only: lnx1d, lnxi, lnx, lnx1db, ln2lne, lne2ln
   use m_unc_flowgeom, only: flowgeom
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