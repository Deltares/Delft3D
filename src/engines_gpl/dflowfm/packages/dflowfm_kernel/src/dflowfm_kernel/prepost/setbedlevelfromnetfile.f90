!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> Reads and sets cell-centered bed levels directly from the net file (when present).
!! _net.nc file should contain a variable with standard_name=altitude
subroutine setbedlevelfromnetfile()
   use m_flowgeom
   use m_flowparameters
   use M_samples
   use m_missing
   use netcdf
   use unstruc_netcdf
   use unstruc_model
   use io_netcdf
   use m_ec_interpolationsettings
   use m_delsam
   implicit none

   integer :: ierr, ioncid, iconvtype, ncid, nmesh, im, id_bl, networkid
   double precision :: bl_fillvalue
   double precision :: convversion
   integer :: nflownode
   integer :: k
   type(t_ug_meshgeom) :: meshgeom
   logical :: jawel

   inquire (file=md_netfile, exist=jawel)
   jawel = jawel .and. (len_trim(md_netfile) > 0) ! strange behavior on some Linux systems if file name is empty, but reported exist=.true.

   if (.not. jawel) then ! only set tile depth data if bl data is present in net file
      return
   end if

   ! Try and read bed level values on cells (flow nodes) from NetFile (only for UGRID >= 1.0)
   if (ibedlevtyp == 1) then
      ierr = ionc_open(trim(md_netfile), NF90_NOWRITE, ioncid, iconvtype, convversion) ! md_netfile
      if (ierr == ionc_noerr .and. iconvtype == IONC_CONV_UGRID .and. convversion >= 1.0) then
         ierr = ionc_get_ncid(ioncid, ncid)

         !bl = dmiss

         ierr = ionc_get_mesh_count(ioncid, nmesh)
         do im = 1, nmesh
            networkid = 0
            ierr = ionc_get_meshgeom(ioncid, im, networkid, meshgeom) !This call is only used to get the dimension (later on when we use UGrid format we might use other fields of the meshgeom structure)
            if (ierr /= ionc_noerr) then
               cycle
            end if

            if (meshgeom%dim /= 1 .and. meshgeom%dim /= 2) then ! Only support 1D network and 2D grid
               cycle
            end if

            ierr = ionc_inq_varid_by_standard_name(ioncid, im, UG_LOC_FACE, 'altitude', id_bl) ! Searches for var with standard_name='altitude' on cell centres.
            if (ierr /= ionc_noerr) then
               ierr = ionc_inq_varid(ioncid, im, 'flowelem_bl', id_bl) ! Fallback: searches for var with name 'mesh2d_flowelem_bl'
            end if
            if (ierr /= ionc_noerr) then
               cycle
            end if

            if (meshgeom%dim == 1) then
               nflownode = meshgeom%numnode
               call increasesam(nflownode)
               ierr = ionc_get_node_coordinates(ioncid, im, xs(1:nflownode), ys(1:nflownode))
            else if (meshgeom%dim == 2) then
               nflownode = meshgeom%numface
               call increasesam(nflownode)
               ierr = ionc_get_face_coordinates(ioncid, im, xs(1:nflownode), ys(1:nflownode))
            end if

            ierr = nf90_get_var(ncid, id_bl, zs, count=(/nflownode/))
            if (ierr /= nf90_noerr) then
               cycle
            end if

            ierr = nf90_get_att(ncid, id_bl, '_FillValue', bl_fillvalue)
            if (ierr /= nf90_noerr) then
               cycle
            end if

            do k = 1, nflownode
               if (zs(k) == bl_fillvalue) then
                  zs(k) = dmiss
               end if
            end do

            ! NOTE: associate cell bl's from file with our model's flowgeom cells via nearest neighbour matching.
            NS = nflownode
            INTERPOLATIONTYPE = INTP_AVG
            IAV = AVGTP_NEARESTNB
            call interpdivers(1)

         end do ! nummesh
      end if ! UGRID >= 1.0
   end if ! ibedlevtyp == 1

   call delsam(-1) ! deallocate

end subroutine setbedlevelfromnetfile
