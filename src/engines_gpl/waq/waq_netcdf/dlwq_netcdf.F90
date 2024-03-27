!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

! dlwq_netcdf --
!     Module of utility routines for dealing with NetCDF output
!
module dlwq_netcdf
    use netcdf
    use results, only : ncopt
    use ISO_FORTRAN_ENV, only : int64

    implicit none

    interface
        subroutine getuuid(guid_string) bind(c, name = 'getuuid')
            character(len = 1), dimension(*) :: guid_string
        end subroutine getuuid
    end interface

    interface dlwqnc_write_wqvariable
        module procedure dlwqnc_write_wqvariable_3d
        module procedure dlwqnc_write_wqvariable_2d
    end interface

    logical :: dlwqnc_debug = .false.
    integer, parameter :: dlwqnc_deflate = 5
    integer, parameter :: dlwqnc_type2d = -999
    integer, parameter :: dlwqnc_type1d = -111

    integer, parameter :: type_ugrid_face_crds = 1 ! Names used by UNTRIM
    integer, parameter :: type_ugrid_node_crds = 2 ! Names used by D-Flow-FM for 2d meshes
    integer, parameter :: type_ugrid_mesh1d = 3 ! Names used by D-Flow-FM for 1d meshes
    integer, parameter :: type_ugrid_network = 4 ! Names used by D-Flow-FM for networks
    integer, parameter :: type_ugrid_network_geometry = 5 ! Names used by D-Flow-FM for network geometries

    logical, save, private :: warning_message = .false. ! Because of optional attributes (UGRID standard)

contains

    ! dlwqnc_debug_status --
    !     Set the debug status of these routines
    !
    ! Arguments:
    !     set_dlwqnc_debug   True or False
    !
    ! Returns:
    !     nf90_noerr if variable found, otherwise an error code
    !
    integer function dlwqnc_debug_status (set_dlwqnc_debug)

        implicit none

        logical :: set_dlwqnc_debug

        dlwqnc_debug = set_dlwqnc_debug
        dlwqnc_debug_status = 0
    end function dlwqnc_debug_status

    function dlwqnc_lowercase(name) result(name_lower)
        character(len = *) :: name
        character(len = len(name)) :: name_lower

        integer, parameter :: offset = iachar('a') - iachar('A')
        integer :: i, asc

        do i = 1, len(name)
            asc = iachar(name(i:i))
            if (asc >= iachar('A') .and. asc <= iachar('Z')) then
                asc = asc - offset
            endif
            name_lower(i:i) = achar(asc)
        enddo
    end function dlwqnc_lowercase

    ! dlwqnc_find_meshes_by_att --
    !     Find the grid(s) by their particular attribute(s)
    !
    ! Arguments:
    !     ncid                  ID of the NetCDF file
    !     varid2d               varaiable ID of 2D mesh (if found)
    !     type_ugrid            type of 2D mesh
    !     varid1d               varaiable ID of 1D mesh (if found)
    !     varidnetwork          varaiable ID of 1D network (if found)
    !     varidnetwork_geometry varaiable ID of 1D network geometry (if found)
    !
    ! Returns:
    !     nf90_noerr if variable found, otherwise an error code
    !
    ! - detect UNTRIM 2D by cf_role = "delwaq_role"
    ! - detect 2D by cf_role = "mesh_topology" and topology_dimension = 2 ;
    ! - detect 1D by cf_role = "mesh_topology" and topology_dimension = 1 ;
    ! - detect network by edge_geometry = "network_geometry" ;

    integer function dlwqnc_find_meshes_by_att(ncid, varid2d, type_ugrid, varid1d, varidnetwork, varidnetwork_geometry)
        use io_ugrid
        use netcdf_utils, only : ncu_get_att

        implicit none

        integer, intent(in) :: ncid
        integer, intent(out) :: varid2d
        integer, intent(out) :: type_ugrid
        integer, intent(out) :: varid1d
        integer, intent(out) :: varidnetwork
        integer, intent(out) :: varidnetwork_geometry

        character(len = 256) :: attribute
        character(len = 256) :: expected_value

        integer :: nvars
        integer :: ierror
        integer :: ivar
        integer :: ivar2

        character(len = nf90_max_name) :: varname
        logical :: delwaq_role
        character(len = :), allocatable :: cf_role
        character(len = :), allocatable :: edge_geometry
        logical :: mesh_topology
        integer :: topology_dimension
        logical :: network

        allocate(character(len = 0) :: cf_role)
        allocate(character(len = 0) :: edge_geometry)
        dlwqnc_find_meshes_by_att = -1
        varid2d = -1
        type_ugrid = -1
        varid1d = -1
        varidnetwork = -1

        ierror = nf90_inquire(ncid, nVariables = nvars)
        if (ierror /= nf90_noerr) then
            return
        endif

        do ivar = 1, nvars
            ierror = nf90_inquire_variable(ncid, ivar, name = varname)

            delwaq_role = .false.
            ierror = nf90_inquire_attribute(ncid, ivar, 'delwaq_role')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            else if (ierror == nf90_noerr) then
                delwaq_role = .true.
            endif

            cf_role = ''
            mesh_topology = .false.
            ierror = nf90_inquire_attribute(ncid, ivar, 'cf_role')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            elseif (ierror == nf90_noerr) then
                ierror = ncu_get_att(ncid, ivar, 'cf_role', cf_role)
                if (ierror /= nf90_noerr) then
                    return
                else if (cf_role == 'mesh_topology') then
                    mesh_topology = .true.
                endif
            else
                cycle
            endif

            topology_dimension = 0
            ierror = nf90_inquire_attribute(ncid, ivar, 'topology_dimension')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            elseif (ierror == nf90_noerr) then !
                ierror = nf90_get_att(ncid, ivar, 'topology_dimension', topology_dimension)
                if (ierror /= nf90_noerr) then
                    return
                endif
            else
                cycle
            endif

            network = .false.
            ierror = nf90_inquire_attribute(ncid, ivar, 'edge_geometry')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            else if (ierror == nf90_noerr) then
                ierror = ncu_get_att(ncid, ivar, 'edge_geometry', edge_geometry)
                if (ierror /= nf90_noerr) then
                    return
                endif
                ierror = nf90_inq_varid(ncid, edge_geometry, ivar2)
                if (ierror /= nf90_noerr) then
                    return
                endif
                network = .true.
            endif

            if (delwaq_role) then ! UNTRIM
                if (varid2d < 0 .and. ivar > 0) then
                    varid2d = ivar
                    type_ugrid = type_ugrid_face_crds
                else
                    if (dlwqnc_debug) write(*, *) 'Error: found multiple grids with ''delwaq_role'''
                    return
                endif
            else if (mesh_topology) then ! D-Flow-FM UGRID
                if (topology_dimension == 2) then ! 2D
                    if (varid2d < 0 .and. ivar > 0) then
                        varid2d = ivar
                        type_ugrid = type_ugrid_node_crds
                    else
                        if (dlwqnc_debug) write(*, *) 'Error: found multiple 2D meshes'
                        return
                    endif
                else if (topology_dimension == 1) then ! 1D
                    if (network) then
                        if (varidnetwork < 0 .and. ivar > 0 .and. ivar2 > 0) then
                            varidnetwork = ivar
                            varidnetwork_geometry = ivar2
                        else
                            if (dlwqnc_debug) write(*, *) 'Error: found multiple networks'
                            return
                        endif
                    else
                        if (varid1d < 0 .and. ivar > 0) then
                            varid1d = ivar
                        else
                            if (dlwqnc_debug) write(*, *) 'Error: found multiple 1D meshes'
                            return
                        endif
                    endif
                else
                    if (dlwqnc_debug) write(*, *) 'Error: topology_dimension has a value that is not supported (not equal to 1 or 2).'
                endif
            endif
        enddo

        if (varid2d /= -1 .or. varid1d /= -1) then
            dlwqnc_find_meshes_by_att = nf90_noerr
        else
            dlwqnc_find_meshes_by_att = nf90_enotatt
        endif

    end function dlwqnc_find_meshes_by_att

    ! dlwqnc_copy_var_atts --
    !     Copy the attributes for a variable (convenience function)
    !
    ! Arguments:
    !     ncidin             ID of the input NetCDF file
    !     ncidout            ID of the output NetCDF file
    !     varidin            ID of the variable in the input file
    !     varidout           ID of the variable in the output file
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    ! Note:
    !     The variable in the output file must already exist
    !
    integer function dlwqnc_copy_var_atts(ncidin, ncidout, varidin, varidout)
        integer, intent(in) :: ncidin, ncidout, varidin, varidout

        integer :: ierror
        integer :: i

        character(len = nf90_max_name) :: attname
        integer :: natts

        dlwqnc_copy_var_atts = -1

        ierror = nf90_inquire_variable(ncidin, varidin, nAtts = natts)
        if (ierror == nf90_enotvar) then
            ierror = nf90_inquire(ncidin, nAttributes = natts)
        endif
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_var_atts = ierror
            return
        endif

        do i = 1, natts
            ierror = nf90_inq_attname(ncidin, varidin, i, attname)
            if (ierror /= nf90_noerr) then
                dlwqnc_copy_var_atts = ierror
                return
            endif

            ierror = nf90_copy_att(ncidin, varidin, attname, ncidout, varidout)
            if (ierror /= nf90_noerr) then
                dlwqnc_copy_var_atts = ierror
                return
            endif
        enddo

        !
        ! Remove the attribute "parent_mesh" however - we do not need it and it would
        ! require copying much more information to be consistent. If it does not exist,
        ! there will probably be an error, but ignore that.
        !
        ierror = nf90_del_att(ncidout, varidout, "parent_mesh")

        dlwqnc_copy_var_atts = nf90_noerr
    end function dlwqnc_copy_var_atts

    ! dlwqnc_copy_mesh --
    !     Copy the mesh data to the output file
    !
    ! Arguments:
    !     ncidin             ID of the input NetCDF file
    !     ncidout            ID of the output NetCDF file
    !     meshid             ID of the mesh variable in the input file
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    integer function dlwqnc_copy_mesh(ncidin, ncidout, meshidin, mesh_name, type_ugrid)
        integer, intent(in) :: ncidin, ncidout, meshidin
        character(len = *), intent(in) :: mesh_name
        integer, intent(in) :: type_ugrid

        integer :: meshidout, varidin, varidout
        integer :: ierror
        integer, dimension(10) :: ierrorn
        integer :: meshvalue
        integer :: i, k
        integer :: xtype, length, attnum, crs_value

        character(len = nf90_max_name) :: varname
        integer :: natts
        integer, dimension(nf90_max_dims) :: dimsizes

        dlwqnc_copy_mesh = -1

        !
        ! First create the variable in the output file
        !
        ierror = nf90_inquire_variable(ncidin, meshidin, name = varname)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        ierror = nf90_redef(ncidout)
        if (ierror /= nf90_noerr .and. ierror /= nf90_eindefine) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        ierror = nf90_def_var(ncidout, trim(varname), nf90_int, varid = meshidout)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        ierror = nf90_enddef(ncidout)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        ierror = nf90_get_var(ncidin, meshidin, meshvalue)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        ierror = nf90_put_var(ncidout, meshidout, meshvalue)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        ierror = nf90_redef(ncidout)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        !
        ! Copy the attributes
        !
        ierror = dlwqnc_copy_var_atts(ncidin, ncidout, meshidin, meshidout)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        !
        ! Copy the dimensions
        !
        ierror = dlwqnc_copy_dims(ncidin, ncidout, dimsizes)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_mesh = ierror
            return
        endif

        !
        ! Now the associated data
        !
        ierror = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "node_coordinates", dimsizes)
        if (ierror /= nf90_noerr) then
            if (dlwqnc_debug) write(*, *) 'Copy associated failed (copying mesh)  - ', ierror
            dlwqnc_copy_mesh = ierror
            return
        endif

        ierrorn = 0
        select case (type_ugrid)
        case (type_ugrid_face_crds)
            ierrorn(1) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_coordinates", dimsizes)
            ierrorn(2) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "face_coordinates", dimsizes)
            ierrorn(3) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_node_connectivity", dimsizes)
            ierrorn(4) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_face_connectivity", dimsizes)
            ierrorn(5) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "face_node_connectivity", dimsizes)
            ierrorn(6) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "face_edge_connectivity", dimsizes)
            ierrorn(7) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, &
                    trim(mesh_name) // "_edge_bc", dimsizes, use_attrib = .false.)
            ierrorn(8) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "projected_coordinate_system", &
                    dimsizes, use_attrib = .false.)
        case (type_ugrid_node_crds)
            warning_message = .true.
            ierrorn(1) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_coordinates", dimsizes)
            ierrorn(2) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "face_coordinates", dimsizes)
            ierrorn(3) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_node_connectivity", dimsizes)
            ierrorn(4) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_face_connectivity", dimsizes)
            ierrorn(5) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "face_node_connectivity", dimsizes)
            ierrorn(6) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "projected_coordinate_system", &
                    dimsizes, use_attrib = .false.)
            warning_message = .false.
        case (type_ugrid_mesh1d)
            warning_message = .true.
            ierrorn(1) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_node_connectivity", dimsizes)
            ierrorn(2) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_coordinates", dimsizes)
            ierrorn(3) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_node_connectivity", dimsizes)
            ierrorn(4) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "node_id", dimsizes)
            ierrorn(5) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "node_long_name", dimsizes)
            ierrorn(6) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "projected_coordinate_system", &
                    dimsizes, use_attrib = .false.)
            warning_message = .false.
        case (type_ugrid_network)
            warning_message = .true.
            ierrorn(1) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_node_connectivity", dimsizes)
            ierrorn(2) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "branch_id", dimsizes)
            ierrorn(3) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "branch_long_name", dimsizes)
            ierrorn(4) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "edge_length", dimsizes)
            ierrorn(5) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "node_id", dimsizes)
            ierrorn(6) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "node_long_name", dimsizes)
            ierrorn(7) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "projected_coordinate_system", &
                    dimsizes, use_attrib = .false.)
            warning_message = .false.
        case (type_ugrid_network_geometry)
            warning_message = .true.
            ierrorn(1) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "node_count", dimsizes)
            ierrorn(2) = dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, "node_coordinates", dimsizes)
            warning_message = .false.
        end select

        if (any(ierrorn /= nf90_noerr .and. ierrorn /= -999)) then
            if (type_ugrid == type_ugrid_face_crds) then
                if (dlwqnc_debug) write(*, *) 'Copy associated failed (copying mesh) - ', maxval(ierrorn)
            endif
            !!      dlwqnc_copy_mesh = ierror
            !!      return
        endif
        if (any(ierrorn == -999)) then
            if (dlwqnc_debug) then
                write(*, *) 'One or more mesh attributes missing - '
                write(*, *) 'this may cause problems in some postprocessing programs'
            endif
        endif

        !
        ! Now copy the coordinate information variable - if it is required
        !
        if (type_ugrid == type_ugrid_face_crds) then
            k = index(varname, char(0))
            if (k == 0) then
                k = len_trim(varname)
            else
                k = k - 1
            endif
            varname = varname(1:k) // "_crs"

            ierror = nf90_inq_varid(ncidin, varname, varidin)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Failed to find the coordinate information (' // trim(varname) // ') - ', ierror
                dlwqnc_copy_mesh = ierror
                return
            endif

            ierror = nf90_redef(ncidout)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Preparing to define ' // trim(varname) // ' failed - ', ierror
                dlwqnc_copy_mesh = ierror
                !return
            endif
            ierror = nf90_def_var(ncidout, trim(varname), nf90_int, varid = varidout)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Defining ' // trim(varname) // ' failed - ', ierror
                dlwqnc_copy_mesh = ierror
                return
            endif
            ierror = dlwqnc_copy_var_atts(ncidin, ncidout, varidin, varidout)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Copying attributes (for ' // trim(varname) // ') failed - ', ierror
                dlwqnc_copy_mesh = ierror
                return
            endif
            crs_value = -999
            ierror = nf90_get_var(ncidin, varidin, values = crs_value)
            ierror = nf90_enddef(ncidout)
            ierror = nf90_put_var(ncidout, varidout, values = crs_value)
        else
            ierror = nf90_enddef(ncidout)
            if (ierror == nf90_enotindefine) then
                ierror = 0 ! TODO: For some reason we may be stuck in data mode - possibly if optional attributes are missing
            endif
        endif

        dlwqnc_copy_mesh = nf90_noerr
    end function dlwqnc_copy_mesh

    ! dlwqnc_read_dims --
    !     Read all dimensions from the input file
    !
    ! Arguments:
    !     ncidin             ID of the input NetCDF file
    !     dimsizes           Size of each dimension (convenience; output)
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    integer function dlwqnc_read_dims(ncidin, dimsizes)
        integer, intent(in) :: ncidin
        integer, intent(out), dimension(:) :: dimsizes

        integer :: ierror
        integer :: dimid, dimidnew

        character(len = nf90_max_name) :: dimname
        integer :: dimvalue

        dlwqnc_read_dims = -1

        dimid = 0
        do
            dimid = dimid + 1
            ierror = nf90_inquire_dimension(ncidin, dimid, dimname, dimvalue)
            if (ierror == nf90_ebaddim) then
                exit
            elseif (ierror /= nf90_noerr) then
                dlwqnc_read_dims = ierror
                return
            endif

            dimsizes(dimid) = dimvalue
        enddo

        dlwqnc_read_dims = nf90_noerr
    end function dlwqnc_read_dims

    ! dlwqnc_copy_dims --
    !     Copy all dimensions to the output file
    !
    ! Arguments:
    !     ncidin             ID of the input NetCDF file
    !     ncidout            ID of the output NetCDF file
    !     dimsizes           Size of each dimension (convenience; output)
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    integer function dlwqnc_copy_dims(ncidin, ncidout, dimsizes)
        integer, intent(in) :: ncidin, ncidout
        integer, intent(out), dimension(:) :: dimsizes

        integer :: ierror
        integer :: dimid, dimidnew

        character(len = nf90_max_name) :: dimname
        integer :: dimvalue

        dlwqnc_copy_dims = -1

        dimid = 0
        do
            dimid = dimid + 1
            ierror = nf90_inquire_dimension(ncidin, dimid, dimname, dimvalue)
            if (ierror == nf90_ebaddim) then
                exit
            elseif (ierror /= nf90_noerr) then
                dlwqnc_copy_dims = ierror
                return
            endif

            dimsizes(dimid) = dimvalue

            ierror = nf90_def_dim(ncidout, dimname, dimvalue, dimidnew)
            if (dimidnew /= dimid .and. ierror /= nf90_enameinuse) then
                if (dlwqnc_debug) write(*, *) 'WARNING: different dimension IDs - ', trim(dimname), ' - ', dimid, dimidnew, ierror
            endif
        enddo

        dlwqnc_copy_dims = nf90_noerr
    end function dlwqnc_copy_dims

    ! dlwqnc_copy_associated --
    !     Copy the data of an associated variable to the output file
    !
    ! Arguments:
    !     ncidin             ID of the input NetCDF file
    !     ncidout            ID of the output NetCDF file
    !     meshidin           ID of the mesh variable in the input file
    !     meshidout          ID of the mesh variable in the output file
    !     attribute          Attribute holding the names of the associated variables
    !     dimsizes           Array with actual dimension sizes
    !     use_attrib         Use the attribute name (default), otherwise assume the attribute argument
    !                        is actually the name of the variable to be copied (bit of a hack)
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    recursive function dlwqnc_copy_associated(ncidin, ncidout, meshidin, meshidout, attribute, &
            dimsizes, use_attrib) result(dlwqnc_result)
        use io_ugrid
        use netcdf_utils, only : ncu_get_att

        integer, intent(in) :: ncidin, ncidout, meshidin, meshidout
        character(len = *), intent(in) :: attribute
        integer, intent(in), dimension(:) :: dimsizes
        logical, intent(in), optional :: use_attrib

        integer :: dlwqnc_result

        integer :: ierror
        integer :: ierr
        integer :: i, j
        integer :: xtype, length, attnum
        integer :: oldvarid, newvarid
        integer :: ndims
        integer, dimension(nf90_max_var_dims) :: dimids, newdimids

        character(len = :), allocatable :: att_value
        character(len = nf90_max_name) :: varname
        character(len = 1) :: dummy

        logical :: use_names_in_attrib

        logical, save :: suppress_message = .false. ! Because of the recursive call

        allocate(character(len = 0) :: att_value)
        dlwqnc_result = -1

        use_names_in_attrib = .true.
        if (present(use_attrib)) then
            use_names_in_attrib = use_attrib
        endif

        !
        ! First retrieve the names of the associated variables and
        ! re-create them in the output file
        !
        ierror = nf90_enddef(ncidout)
        if (ierror /= nf90_noerr .and. ierror /= nf90_enotindefine) then
            dlwqnc_result = ierror
            return
        endif

        if (use_names_in_attrib) then
            att_value = ''
            ierror = ncu_get_att(ncidin, meshidin, attribute, att_value)
            if (ierror /= nf90_noerr .and. .not. suppress_message) then
                if (warning_message) then
                    if (dlwqnc_debug) write(*, *) 'Warning: retrieving attribute ', trim(attribute), ' failed -- ', ierror
                    dlwqnc_result = -999
                else
                    if (dlwqnc_debug) write(*, *) 'Error retrieving attribute ', trim(attribute), ' -- ', ierror
                    dlwqnc_result = ierror
                endif
                return
            endif
        else
            att_value = attribute
        endif

        do i = 1, 100  ! JanM: Is att_value lang genoeg
            read(att_value, *, iostat = ierr) (dummy, j = 1, i - 1), varname

            if (ierr /= 0) then
                exit
            endif

            ierror = nf90_inq_varid(ncidin, trim(varname), varid = oldvarid)
            if (ierror /= nf90_noerr) then
                dlwqnc_result = ierror
                return
            endif

            !
            ! Create the variable
            !
            ierror = nf90_inquire_variable(ncidin, oldvarid, xtype = xtype, ndims = ndims, dimids = dimids)

            if (ierror /= nf90_noerr) then
                dlwqnc_result = ierror
                return
            endif

            ierror = nf90_redef(ncidout)
            if (ierror /= nf90_noerr) then
                dlwqnc_result = ierror
                return
            endif

            !
            ! Note: the deflate_level option is the only API difference between NetCDF 4 and
            !       NetCDF 3 of interest to the code.
            !       Should you require NetCDF 3, then use the commented call to nf90_def_var
            !       instead.
            !
            if (ncopt(4) == 1 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(ncidout, trim(varname), xtype, dimids(1:ndims), newvarid, &
                        deflate_level = dlwqnc_deflate)
            else
                ierror = nf90_def_var(ncidout, trim(varname), xtype, dimids(1:ndims), newvarid)
            endif
            if (ierror /= nf90_noerr) then
                dlwqnc_result = ierror
                return
            endif

            ierror = dlwqnc_copy_var_atts(ncidin, ncidout, oldvarid, newvarid)
            if (ierror /= nf90_noerr) then
                dlwqnc_result = ierror
                return
            endif

            ierror = nf90_enddef(ncidout)
            if (ierror /= nf90_noerr) then
                dlwqnc_result = ierror
                return
            endif

            select case (xtype)
            case(nf90_int)
                ierror = dlwqnc_copy_int_var(ncidin, ncidout, oldvarid, newvarid, ndims, dimids, dimsizes)
            case(nf90_int64)
                ierror = dlwqnc_copy_int64_var(ncidin, ncidout, oldvarid, newvarid, ndims, dimids, dimsizes)
            case(nf90_real)
                ierror = dlwqnc_copy_real_var(ncidin, ncidout, oldvarid, newvarid, ndims, dimids, dimsizes)
            case(nf90_double)
                ierror = dlwqnc_copy_double_var(ncidin, ncidout, oldvarid, newvarid, ndims, dimids, dimsizes)
            case (nf90_char)
                ierror = dlwqnc_copy_char_var(ncidin, ncidout, oldvarid, newvarid, ndims, dimids, dimsizes)
            case default
                ierror = -1
            end select
            if (ierror /= nf90_noerr) then
                dlwqnc_result = ierror
                return
            endif

            !
            ! The variable may in turn have variables that require copying: the bounds attribute
            ! (Ignore any errors though)
            !
            if (attribute /= 'bounds') then
                suppress_message = .true.
                ierror = dlwqnc_copy_associated(ncidin, ncidout, oldvarid, newvarid, 'bounds', dimsizes)
                suppress_message = .false.
            endif
        enddo
        deallocate(att_value)
        dlwqnc_result = nf90_noerr
    end function dlwqnc_copy_associated

    ! dlwqnc_copy_int_var, ... --
    !     Copy the data of an integer/.../ variable to the output file
    !
    ! Arguments:
    !     ncidin             ID of the input NetCDF file
    !     ncidout            ID of the output NetCDF file
    !     meshidin           ID of the mesh variable in the input file
    !     meshidout          ID of the mesh variable in the output file
    !     ndims              Number of dimensions
    !     dimids             IDs of the dimensions
    !     dimsizes           Actual size of each dimension
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    ! Note:
    !     For some reason we need to distinguish between one and two dimensions,
    !     otherwise we get an error "nf90_eedge"
    !
    integer function dlwqnc_copy_int_var(ncidin, ncidout, varin, varout, ndims, dimids, dimsizes)
        !   use ISO_C_BINDING

        integer, intent(in) :: ncidin, ncidout, varin, varout, ndims
        integer, intent(in), dimension(:) :: dimids, dimsizes

        integer :: sz, sz1
        integer :: ierror
        integer :: ierr
        integer :: i

        integer, dimension(:), allocatable :: value
        integer, dimension(:, :), allocatable :: value2d

        dlwqnc_copy_int_var = -1

        if (ndims /= 2) then
            sz = 1
            do i = 1, ndims
                sz = sz * dimsizes(dimids(i))
            enddo
            allocate(value(sz), stat = ierr)
            ierror = nf90_get_var(ncidin, varin, value)
        else
            allocate(value2d(dimsizes(dimids(1)), dimsizes(dimids(2))), stat = ierr)
            ierror = nf90_get_var(ncidin, varin, value2d)
        endif

        if (ndims /= 2) then
            ierror = nf90_put_var(ncidout, varout, value)
        else
            ierror = nf90_put_var(ncidout, varout, value2d)
        endif

        if (ierror /= nf90_noerr) then
            dlwqnc_copy_int_var = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving int values: ', ierror, ' -- size: ', sz
            return
        endif

        dlwqnc_copy_int_var = nf90_noerr
    end function dlwqnc_copy_int_var

    integer function dlwqnc_copy_int64_var(ncidin, ncidout, varin, varout, ndims, dimids, dimsizes)
        !   use ISO_C_BINDING

        integer, intent(in) :: ncidin, ncidout, varin, varout, ndims
        integer, intent(in), dimension(:) :: dimids, dimsizes

        integer :: sz, sz1
        integer :: ierror
        integer :: ierr
        integer :: i

        integer(kind = int64), dimension(:), allocatable :: value
        integer(kind = int64), dimension(:, :), allocatable :: value2d

        dlwqnc_copy_int64_var = -1

        if (ndims /= 2) then
            sz = 1
            do i = 1, ndims
                sz = sz * dimsizes(dimids(i))
            enddo
            allocate(value(sz), stat = ierr)
            ierror = nf90_get_var(ncidin, varin, value)
        else
            allocate(value2d(dimsizes(dimids(1)), dimsizes(dimids(2))), stat = ierr)
            ierror = nf90_get_var(ncidin, varin, value2d)
        endif

        if (ndims /= 2) then
            ierror = nf90_put_var(ncidout, varout, value)
        else
            ierror = nf90_put_var(ncidout, varout, value2d)
        endif

        if (ierror /= nf90_noerr) then
            dlwqnc_copy_int64_var = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving int64 values: ', ierror, ' -- size: ', sz
            return
        endif

        dlwqnc_copy_int64_var = nf90_noerr
    end function dlwqnc_copy_int64_var

    integer function dlwqnc_copy_real_var(ncidin, ncidout, varin, varout, ndims, dimids, dimsizes)
        integer, intent(in) :: ncidin, ncidout, varin, varout, ndims
        integer, intent(in), dimension(:) :: dimids, dimsizes

        integer :: sz
        integer :: ierror
        integer :: ierr
        integer :: i

        real, dimension(:), allocatable :: value
        real, dimension(:, :), allocatable :: value2d

        dlwqnc_copy_real_var = -1

        if (ndims /= 2) then
            sz = 1
            do i = 1, ndims
                sz = sz * dimsizes(dimids(i))
            enddo
            allocate(value(sz))
            ierror = nf90_get_var(ncidin, varin, value)
        else
            allocate(value2d(dimsizes(dimids(1)), dimsizes(dimids(2))))
            ierror = nf90_get_var(ncidin, varin, value2d)
        endif

        if (ierror /= nf90_noerr) then
            dlwqnc_copy_real_var = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving real values: ', ierror, ' -- size: ', sz
            return
        endif

        if (ndims /= 2) then
            ierror = nf90_put_var(ncidout, varout, value)
        else
            ierror = nf90_put_var(ncidout, varout, value2d)
        endif

        if (ierror /= nf90_noerr) then
            dlwqnc_copy_real_var = ierror
            return
        endif

        dlwqnc_copy_real_var = nf90_noerr
    end function dlwqnc_copy_real_var

    integer function dlwqnc_copy_double_var(ncidin, ncidout, varin, varout, ndims, dimids, dimsizes)
        integer, intent(in) :: ncidin, ncidout, varin, varout, ndims
        integer, intent(in), dimension(:) :: dimids, dimsizes

        integer :: sz
        integer :: ierror
        integer :: ierr
        integer :: i

        real(kind = kind(1.0d0)), dimension(:), allocatable :: value
        real(kind = kind(1.0d0)), dimension(:, :), allocatable :: value2d

        dlwqnc_copy_double_var = -1

        if (ndims /= 2) then
            sz = 1
            do i = 1, ndims
                sz = sz * dimsizes(dimids(i))
            enddo
            allocate(value(sz))
            ierror = nf90_get_var(ncidin, varin, value)
        else
            allocate(value2d(dimsizes(dimids(1)), dimsizes(dimids(2))))
            ierror = nf90_get_var(ncidin, varin, value2d)
        endif

        if (ierror /= nf90_noerr) then
            dlwqnc_copy_double_var = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving double values: ', ierror, ' -- size: ', sz
            return
        endif

        if (ndims /= 2) then
            ierror = nf90_put_var(ncidout, varout, value)
        else
            ierror = nf90_put_var(ncidout, varout, value2d)
        endif

        if (ierror /= nf90_noerr) then
            dlwqnc_copy_double_var = ierror
            return
        endif

        dlwqnc_copy_double_var = nf90_noerr
    end function dlwqnc_copy_double_var

    integer function dlwqnc_copy_char_var(ncidin, ncidout, varin, varout, ndims, dimids, dimsizes)
        !   use ISO_C_BINDING

        integer, intent(in) :: ncidin, ncidout, varin, varout, ndims
        integer, intent(in), dimension(:) :: dimids, dimsizes

        integer :: dim1, dim2
        integer :: ierror
        integer :: ierr

        character(len = :), dimension(:), allocatable :: value

        dlwqnc_copy_char_var = -1

        dim1 = dimsizes(dimids(1))
        dim2 = dimsizes(dimids(2))

        allocate(character(len = dim1) :: value(dim2), stat = ierr)
        if (ierr /= 0) then
            return
        endif

        ierror = nf90_get_var(ncidin, varin, value)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_char_var = ierror
            return
        endif

        ierror = nf90_put_var(ncidout, varout, value)
        if (ierror /= nf90_noerr) then
            dlwqnc_copy_char_var = ierror
            return
        endif

        dlwqnc_copy_char_var = nf90_noerr
    end function dlwqnc_copy_char_var

    ! dlwqnc_write_wqtime --
    !     Write the time to the output file
    !
    ! Arguments:
    !     ncidout            ID of the output NetCDF file
    !     timeid             ID of the time variable
    !     bndtimeid          ID of the time boundaries variable
    !     record             Index of the record in the output file
    !     time               Actual time value (seconds)
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    integer function dlwqnc_write_wqtime(ncidout, timeid, bndtimeid, record, time)
        integer, intent(in) :: ncidout
        integer, intent(in) :: timeid
        integer, intent(in) :: bndtimeid
        integer, intent(in) :: record
        integer, intent(in) :: time

        integer :: ierror
        integer, dimension(1) :: values
        integer, dimension(2) :: start
        integer, dimension(2) :: count
        integer, dimension(2) :: bndtime

        dlwqnc_write_wqtime = nf90_noerr

        values = time
        start = record
        count = 1
        ierror = nf90_put_var(ncidout, timeid, values, start = start(1:1), count = count(1:1))
        if (ierror /= nf90_noerr) then
            dlwqnc_write_wqtime = ierror
            return
        endif

        count = (/ 2, 1 /)
        if (record == 1) then
            bndtime = time
        else
            start = (/ 1, record - 1 /)
            ierror = nf90_get_var(ncidout, bndtimeid, bndtime, start = start, count = count)
            if (ierror /= nf90_noerr) then
                dlwqnc_write_wqtime = ierror
                return
            endif
        endif
        bndtime(1) = bndtime(2)
        bndtime(2) = time
        start(2) = record
        ierror = nf90_put_var(ncidout, bndtimeid, bndtime, start = start, count = count)
        if (ierror /= nf90_noerr) then
            dlwqnc_write_wqtime = ierror
            return
        endif

    end function dlwqnc_write_wqtime

    ! dlwqnc_write_wqvariable --
    !     Write the current values of a WQ variable to the output file
    !
    ! Arguments:
    !     ncidout            ID of the output NetCDF file
    !     wqid               ID of the WQ variable in the output file
    !     record             Index of the record in the output file
    !     values             Array of values
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    integer function dlwqnc_write_wqvariable_3d(ncidout, wqid, record, values)
        integer, intent(in) :: ncidout
        integer, intent(in) :: wqid
        integer, intent(in) :: record
        real, dimension(:, :), intent(in) :: values

        integer :: ierror
        integer, dimension(3) :: start
        integer, dimension(3) :: count

        dlwqnc_write_wqvariable_3d = nf90_noerr

        start = (/1, 1, record /)
        count = (/ size(values, 1), size(values, 2), 1 /)
        ierror = nf90_put_var(ncidout, wqid, values, start = start, count = count)
        if (ierror /= nf90_noerr) then
            dlwqnc_write_wqvariable_3d = ierror
            return
        endif
    end function dlwqnc_write_wqvariable_3d

    integer function dlwqnc_write_wqvariable_2d(ncidout, wqid, record, values)
        integer, intent(in) :: ncidout
        integer, intent(in) :: wqid
        integer, intent(in) :: record
        real, dimension(:), intent(in) :: values

        integer :: ierror
        integer, dimension(2) :: start
        integer, dimension(2) :: count

        dlwqnc_write_wqvariable_2d = nf90_noerr

        start = (/1, record /)
        count = (/ size(values), 1 /)
        ierror = nf90_put_var(ncidout, wqid, values, start = start, count = count)
        if (ierror /= nf90_noerr) then
            dlwqnc_write_wqvariable_2d = ierror
            return
        endif
    end function dlwqnc_write_wqvariable_2d

    ! dlwqnc_create_wqtime --
    !     Create the time coordinate variable
    !
    ! Arguments:
    !     ncidout            ID of the output NetCDF file
    !     t0string           The so-called T0-string
    !     timeid             ID of the time variable
    !     bndtimeid          ID of the time bounds variable
    !     ntimeid            ID of the time dimension
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    integer function dlwqnc_create_wqtime(ncidout, t0string, timeid, bndtimeid, ntimeid)
        integer, intent(in) :: ncidout
        character(len = *), intent(in) :: t0string
        integer, intent(out) :: timeid
        integer, intent(out) :: bndtimeid

        integer :: ierror
        integer :: ntimeid
        integer :: twoid
        character(len = 30) :: t0_date_time
        character(len = nf90_max_name) :: name
        integer :: dimvalue
        integer :: k

        dlwqnc_create_wqtime = nf90_noerr

        timeid = 0

        name = 'nTimesDlwq'
        dimvalue = nf90_unlimited

        ierror = nf90_redef(ncidout)
        if (ierror /= 0 .and. ierror /= nf90_eindefine) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_def_dim(ncidout, name, dimvalue, ntimeid)
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_def_var(ncidout, name, nf90_int, (/ ntimeid /), timeid)
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_inq_dimid(ncidout, "Two", twoid)
        if (ierror == nf90_ebaddim) then
            ierror = nf90_def_dim(ncidout, "Two", 2, twoid)
        endif
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        name = 'nTimesDlwqBnd'
        ierror = nf90_def_var(ncidout, name, nf90_int, (/ twoid, ntimeid /), bndtimeid)
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, timeid, "bounds", name) ! Must be the same as variable above, of course
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, timeid, "long_name", "time")
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, timeid, "standard_name", "time")
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, timeid, "calendar", "gregorian")
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, timeid, "axis", "T")
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        t0_date_time = t0string(5:23) // " +00:00" ! DELWAQ has no timezone information!
        t0_date_time(5:5) = '-'
        t0_date_time(8:8) = '-'

        ierror = nf90_put_att(ncidout, timeid, "units", "seconds since " // t0_date_time)
        if (ierror /= 0) then
            dlwqnc_create_wqtime = ierror
            return
        endif

        ierror = nf90_enddef(ncidout)

    end function dlwqnc_create_wqtime


    ! dlwqnc_create_wqvariable --
    !     Create a WQ variable in the output file
    !
    ! Arguments:
    !     ncidout            ID of the output NetCDF file
    !     mesh_name          Name of the mesh
    !     wqname             DELWAQ name of the WQ variable
    !     longname           Long descriptive name of the WQ variable
    !     stdname            Standard name according to CF convention (or identical to wqname)
    !     unit               String defining the unit of the WQ variable
    !     ntimeid            Dimension: number of times
    !     noseglid           Dimension: number of cells per layer
    !     nolayid            DimensionL number of layers (or -999 if 2D variable - use dlwqnc_type2d, or -111 if 1D variable - use dlwqnc_type1d)
    !     wqid               ID of the WQ variable in the output file
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    ! Notes:
    !     time ID? mesh ID? Roles?
    !
    integer function dlwqnc_create_wqvariable(ncidout, mesh_name, wqname, longname, stdname, unit, ntimeid, noseglid, nolayid, wqid)
        use netcdf_utils, only : ncu_get_att

        integer, intent(in) :: ncidout
        character(len = *), intent(in) :: mesh_name
        character(len = *), intent(in) :: wqname
        character(len = *), intent(in) :: longname
        character(len = *), intent(in) :: stdname
        character(len = *), intent(in) :: unit
        integer, intent(in) :: ntimeid
        integer, intent(in) :: noseglid
        integer, intent(in) :: nolayid
        integer, intent(out) :: wqid

        integer :: i
        integer :: k, k2
        integer :: ierror
        integer :: meshid
        integer :: dimvalue
        character(len = nf90_max_name) :: name
        character(len = nf90_max_name) :: name2D
        character(len = nf90_max_name), dimension(5) :: dimname
        character(len = 3 * nf90_max_name) :: methods
        character(len = :), allocatable :: coords

        allocate(character(len = 0) :: coords)

        dlwqnc_create_wqvariable = nf90_noerr

        ierror = nf90_redef(ncidout)

        k = index(mesh_name, char(0))
        if (k == 0) then
            k = len_trim(mesh_name)
        else
            k = k - 1
        endif
        write(name, '(a,a,a)') mesh_name(1:k), '_', trim(wqname)
        write(name2d, '(a,a,a)') mesh_name(1:k), '_2d_', trim(wqname)

        do i = 1, len_trim(name)
            if (name(i:i) == ' ') then
                name(i:i) = '_'
            endif
        enddo

        do i = 1, len_trim(name2d)
            if (name2d(i:i) == ' ') then
                name2d(i:i) = '_'
            endif
        enddo

        !
        ! TODO: support for chunking - this requires an array of chunksizes per dimension
        !
        if (nolayid /= dlwqnc_type2d .and. nolayid /= dlwqnc_type1d) then
            if (ncopt(1) == 4 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(ncidout, name, nf90_float, (/ noseglid, nolayid, ntimeid /), wqid, &
                        deflate_level = ncopt(2))
            else
                ierror = nf90_def_var(ncidout, name, nf90_float, (/ noseglid, nolayid, ntimeid /), wqid)
            endif
        else if (nolayid == dlwqnc_type2d) then
            if (ncopt(1) == 4 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(ncidout, name2d, nf90_float, (/ noseglid, ntimeid /), wqid, &
                        deflate_level = ncopt(2))
            else
                ierror = nf90_def_var(ncidout, name2d, nf90_float, (/ noseglid, ntimeid /), wqid)
            endif
        else
            if (ncopt(1) == 4 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(ncidout, name, nf90_float, (/ noseglid, ntimeid /), wqid, &
                        deflate_level = ncopt(2))
            else
                ierror = nf90_def_var(ncidout, name, nf90_float, (/ noseglid, ntimeid /), wqid)
            endif
        endif
        !    ierror = nf90_def_var( ncidout, name, nf90_float, (/ noseglid, nolayid, ntimeid /), wqid )
        if (ierror /= 0 .and. ierror /= nf90_enameinuse) then
            if (dlwqnc_debug) write(*, *) 'Error creating WQ variable: ', ierror
            dlwqnc_create_wqvariable = ierror
            return
        else if (ierror == nf90_enameinuse) then
            if (nolayid /= dlwqnc_type2d) then
                ierror = nf90_inq_varid(ncidout, name, wqid)
            else
                ierror = nf90_inq_varid(ncidout, name2d, wqid)
            endif
        endif

        ierror = nf90_put_att(ncidout, wqid, "long_name", longname)
        if (ierror /= 0) then
            dlwqnc_create_wqvariable = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, wqid, "units", unit)
        if (ierror /= 0) then
            dlwqnc_create_wqvariable = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, wqid, "_FillValue", -999.0)
        if (ierror /= 0 .and. ierror /= nf90_elatefill) then
            dlwqnc_create_wqvariable = ierror
            return
        endif

        if (nolayid /= dlwqnc_type2d .and. nolayid /= dlwqnc_type1d) then
            ierror = nf90_inquire_dimension(ncidout, ntimeid, dimname(1), dimvalue)   &
                    + nf90_inquire_dimension(ncidout, noseglid, dimname(2), dimvalue) &
                    + nf90_inquire_dimension(ncidout, nolayid, dimname(3), dimvalue)
        else
            ierror = nf90_inquire_dimension(ncidout, ntimeid, dimname(1), dimvalue)   &
                    + nf90_inquire_dimension(ncidout, noseglid, dimname(2), dimvalue)
        endif
        if (ierror /= 0) then
            dlwqnc_create_wqvariable = ierror
            return
        endif

        do i = 1, 3
            k2 = index(dimname(i), char(0))
            if (k2 > 0) then
                dimname(i)(k2:) = ' '
            endif
        enddo

        if (dlwqnc_lowercase(wqname) /= 'volume') then
            write(name, '(a,a,a)') 'volume: ', mesh_name(1:k), '_volume'
            ierror = nf90_put_att(ncidout, wqid, "cell_measures", name)
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
            if (nolayid /= dlwqnc_type2d .and. nolayid /= dlwqnc_type1d) then
                write(methods, '(20a)') trim(dimname(1)), ': point ', trim(dimname(2)), &
                        ': mean ', trim(dimname(3)), ': mean'
            else
                write(methods, '(20a)') trim(dimname(1)), ': point ', trim(dimname(2)), ': mean '
            endif

            ierror = nf90_put_att(ncidout, wqid, "cell_methods", trim(methods))
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
        endif

        ierror = nf90_inq_varid(ncidout, mesh_name, meshid)
        coords = ''
        ierror = ncu_get_att(ncidout, meshid, "face_coordinates", coords)
        if (ierror /= 0) then
            coords = ''
            ierror = ncu_get_att(ncidout, meshid, "node_coordinates", coords)
        endif

        ierror = nf90_put_att(ncidout, wqid, "coordinates", coords)
        if (ierror /= 0) then
            dlwqnc_create_wqvariable = ierror
            return
        endif

        ierror = nf90_put_att(ncidout, wqid, "grid_mapping", mesh_name(1:k) // "_crs")
        if (ierror /= 0) then
            dlwqnc_create_wqvariable = ierror
            return
        endif

        if (len_trim(stdname) /= 0) then
            ierror = nf90_put_att(ncidout, wqid, "standard_name", stdname)
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
        endif

        if (nolayid /= dlwqnc_type2d .and. nolayid /= dlwqnc_type1d) then
            ierror = nf90_put_att(ncidout, wqid, "delwaq_name", wqname)
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
        else if (nolayid == dlwqnc_type2d) then
            ierror = nf90_put_att(ncidout, wqid, "delwaq_name", trim(wqname) // "_avg")
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
        else
            ierror = nf90_put_att(ncidout, wqid, "delwaq_name", trim(wqname) // "_1d")
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
        endif

        ierror = nf90_put_att(ncidout, wqid, "mesh", mesh_name(1:k))
        if (ierror /= 0) then
            dlwqnc_create_wqvariable = ierror
            return
        endif

        if (nolayid /= dlwqnc_type1d) then
            ierror = nf90_put_att(ncidout, wqid, "location", "face")
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
        else
            ierror = nf90_put_att(ncidout, wqid, "location", "node")
            if (ierror /= 0) then
                dlwqnc_create_wqvariable = ierror
                return
            endif
        endif

        ierror = nf90_enddef(ncidout)

    end function dlwqnc_create_wqvariable

    ! dlwqnc_create_layer_dim --
    !     Create the dimension for the layers
    !
    ! Arguments:
    !     ncidout            ID of the output NetCDF file
    !     mesh_name          Name of the mesh
    !     nolay              Number of layers
    !     thickness          Array with thicknesses
    !     nolayid            ID for number of layers (dimension)
    !
    ! Returns:
    !     nf90_noerr if all okay, otherwise an error code
    !
    integer function dlwqnc_create_layer_dim(ncidout, mesh_name, nolay, thickness, nolayid)
        integer, intent(in) :: ncidout
        character(len = *), intent(in) :: mesh_name
        integer, intent(in) :: nolay
        real, dimension(:), intent(in) :: thickness
        integer, intent(out) :: nolayid

        integer :: i, k
        integer :: ierror
        integer :: varlayid, cumlayid
        character(len = nf90_max_name) :: name
        real, dimension(size(thickness)) :: z_centre
        real :: z_sum

        character(len = 20), dimension(5) :: attname = &
                (/ 'long_name    ', 'units        ', 'axis         ', 'positive     ', 'standard_name' /)
        character(len = 20), dimension(5) :: attvalue = &
                (/ 'depth of layer', 'm             ', 'Z             ', 'down          ', 'depth         '/)
        character(len = 40), dimension(5) :: z_attvalue = &
                (/ 'sigma layer coordinate at element center', &
                        '                                        ', &
                        'Z                                       ', &
                        'up                                      ', &
                        'ocean_sigma_coordinate                  '  /)

        dlwqnc_create_layer_dim = nf90_noerr

        k = index(mesh_name, char(0))
        if (k == 0) then
            k = len_trim(mesh_name)
        else
            k = k - 1
        endif

        ierror = nf90_redef(ncidout)
        if (ierror /= nf90_noerr .and. ierror /= nf90_eindefine) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (redef): ', ierror
            dlwqnc_create_layer_dim = ierror
            return
        endif

        write(name, '(2a)') mesh_name(1:k), '_nLayersDlwq'
        ierror = nf90_def_dim(ncidout, name, nolay, nolayid)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (def_dim): ', ierror
            if (dlwqnc_debug) write(*, *) 'Note: Name: ', trim(name), ' number: ', nolay
            dlwqnc_create_layer_dim = ierror
            return
        endif

        !
        ! Cumulative sigma coordinate
        !
        write(name, '(3a)') mesh_name(1:k), '_layer_dlwq'
        ierror = nf90_def_var(ncidout, name, nf90_float, (/ nolayid /), cumlayid)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (def_var): ', ierror
            dlwqnc_create_layer_dim = ierror
            return
        endif

        do i = 1, 5
            ierror = nf90_put_att(ncidout, cumlayid, attname(i), z_attvalue(i))
            if (ierror /= 0) then
                if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (put_att): ', ierror
                dlwqnc_create_layer_dim = ierror
                return
            endif
        enddo

        ierror = nf90_enddef(ncidout)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (enddef): ', ierror
            dlwqnc_create_layer_dim = ierror
            return
        endif

        !
        ! Construct the cumulative sigma coordinate and write it to the file
        ! Note: following the D-FLOW-FM convention, sigma = 0 is the bottom
        !
        z_sum = 0.0
        do i = nolay, 1, -1
            z_centre(i) = z_sum + 0.5 * thickness(i)
            z_sum = z_sum + thickness(i)
        enddo

        ierror = nf90_put_var(ncidout, cumlayid, z_centre)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (put_var): ', ierror
            dlwqnc_create_layer_dim = ierror
            return
        endif

        ierror = nf90_redef(ncidout)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (redef): ', ierror
            dlwqnc_create_layer_dim = ierror
            return
        endif

        dlwqnc_create_layer_dim = ierror
    end function dlwqnc_create_layer_dim

    ! dlwqnc_create_delwaq_dims --
    !     Create the missing dimensions based on the selected mesh
    !
    ! Arguments:
    !     ncidout            ID of the output NetCDF file
    !     nosegl             Number of segments per layer according to DELWAQ
    !     nolay              Number of layers according to DELWAQ
    !     dimids             Array of dimension IDs for the corresponding DELWAQ mesh
    !     dimsizes           Array of dimensions (with new dimensions added)
    !
    ! Remark:
    !     This is only necessary if the geometry file does not define an aggregation table
    !
    integer function dlwqnc_create_delwaq_dims(ncidout, nosegl, nolay, dimids, dimsizes)
        integer, intent(in) :: ncidout, nosegl, nolay
        integer, intent(inout), dimension(:) :: dimids, dimsizes

        integer :: inc_error
        integer :: varid
        character(len = 200) :: coordinate_names
        character(len = 100) :: name

        dlwqnc_create_delwaq_dims = nf90_noerr

        inc_error = nf90_redef(ncidout)
        if (inc_error /= nf90_noerr) then
            dlwqnc_create_delwaq_dims = inc_error
            return
        endif

        inc_error = nf90_def_dim(ncidout, "nSegmentsPerLayerDlwq", nosegl, dimids(1))

        if (inc_error /= nf90_noerr) then
            dlwqnc_create_delwaq_dims = inc_error
            return
        endif

        inc_error = nf90_enddef(ncidout)
        if (inc_error /= nf90_noerr) then
            dlwqnc_create_delwaq_dims = inc_error
            return
        endif

        dlwqnc_create_delwaq_dims = dlwqnc_read_dims(ncidout, dimsizes)

    end function dlwqnc_create_delwaq_dims

end module dlwq_netcdf
