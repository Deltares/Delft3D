!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
!
!

module m_read_waqgeom
    use m_alloc
    use MessageHandling
    use m_utils_waqgeom

    private

    public read_waqgeom_file

    contains

    !> Reads an unstructured waqgeom grid from UGRID file format.
    !! Reads netnode coordinates, edges (netlinks), net boundary and elements (netelems).
    function read_waqgeom_file(filename, meta, crs, waqgeom, edge_type, idomain, iglobal, conv_type, conv_version) result (success)
        use netcdf
        use io_netcdf
        use io_ugrid

        implicit none

        character(len=*)                       :: filename
        type(t_ug_meta)    , intent(out)       :: meta
        type(t_crs)        , intent(out)       :: crs
        type(t_ug_meshgeom), intent(out)       :: waqgeom
        integer, pointer   , intent(out)       :: edge_type(:)
        integer, pointer   , intent(out)       :: idomain(:)
        integer, pointer   , intent(out)       :: iglobal(:)
        integer                                :: conv_type
        real(8)                                :: conv_version

        logical                                :: success !< Result status, true if successful.

        ! NetCDF variables
        !> Dimensions   Node variables Link variables Link type Boundary variables Element variables Computational boundaries
        integer :: ierr
        integer :: ioncid
        integer :: ncid
        integer :: nmesh
        integer :: id_edgetypes
        integer :: id_idomain
        integer :: id_iglobal
        integer :: file_size

        integer :: i_mesh, i_netw, ifill

        success = .false.

        inquire(FILE=filename, SIZE=file_size)
        if (file_size == 0) then
            return
        end if

        ierr = ionc_open(trim(filename) , nf90_nowrite, ioncid)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'File ' // trim(filename) // ' could not be opened.')
            return
        end if
        !
        ierr = ionc_get_meta_data(ioncid, meta)
        ierr = ionc_get_coordinate_reference_system(ioncid, crs)

        ierr = ionc_inq_conventions(ioncid, conv_type, conv_version)
        if ( (ierr == nf90_noerr .and. conv_type /= IONC_CONV_UGRID) .or. &
             (ierr == nf90_noerr .and. conv_type == IONC_CONV_UGRID .and. conv_version<1.0)) then
            ! read old format grid file
            ierr = ionc_close(ioncid)
!            call read_grid_file_unstruc_netcdf(filename, waqgeom)
            return
        end if
        ! It is a valid UGRID file format
        ierr = ionc_get_mesh_count(ioncid, nmesh) ! UGRID: required
        if (nmesh==0) then
            call mess(LEVEL_ERROR,'No mesh found in UGRID file: ' // trim(filename) )
            return
        end if
        if (nmesh>1) then
            call mess(LEVEL_ERROR,'More than one mesh found in UGRID file: ' // trim(filename) )
            return
        end if
        i_mesh = 1
        i_netw = -1

        ! Read the mesh
        ierr = ionc_get_meshgeom(ioncid, i_mesh, i_netw, waqgeom, includeArrays=.true.)

        call reallocP(waqgeom%face_nodes, (/waqgeom%maxnumfacenodes, waqgeom%numface/), keepExisting = .false.)
        ierr = ionc_get_face_nodes(ioncid, i_mesh, waqgeom%face_nodes, ifill, startindex=1)

        call reallocP(waqgeom%face_edges, (/waqgeom%maxnumfacenodes, waqgeom%numface/), keepExisting = .false.)
        ierr = ionc_get_face_edges(ioncid, i_mesh, waqgeom%face_edges, ifill, startindex=1)
        success = ierr == 0
        if ( .not. success ) then
            call mess(LEVEL_ERROR,'Unable to read face-edges table in UGRID file: ' // trim(filename) )
            return
        endif

        call reallocP(waqgeom%edge_faces,(/2, waqgeom%numedge/), keepExisting = .false.)
        ierr = ionc_get_edge_faces(ioncid, i_mesh, waqgeom%edge_faces, ifill, startindex=1)

        call reallocP(edge_type, waqgeom%numedge, keepExisting = .false.)
        ierr = ionc_get_ncid(ioncid, ncid)
        ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_edge_type", id_edgetypes)  ! instead of variable name, we should look at the mesh attributes (but these are not set yet)
        ierr = nf90_get_var(ncid, id_edgetypes, edge_type, count=(/ waqgeom%numedge /))

        ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_face_domain_number", id_idomain)  ! instead of variable name, we should look at the mesh attributes (but these are not set yet)
        if (ierr .eq. 0) then
           call reallocP(idomain, waqgeom%numface, keepExisting = .false.)
           ierr = nf90_get_var(ncid, id_idomain, idomain, count=(/ waqgeom%numface /))
        endif
        ierr = nf90_inq_varid(ncid, trim(waqgeom%meshname)//"_face_global_number", id_iglobal)  ! instead of variable name, we should look at the mesh attributes (but these are not set yet)
        if (ierr .eq. 0) then
           call reallocP(iglobal, waqgeom%numface, keepExisting = .false.)
           ierr = nf90_get_var(ncid, id_iglobal, iglobal, count=(/ waqgeom%numface /))
        endif

        call add_facexy_waqgeom(waqgeom)
        call add_edgexy_waqgeom(waqgeom)
        call add_facelinks_waqgeom(waqgeom)

        success = .true.

    end function read_waqgeom_file

    end module
