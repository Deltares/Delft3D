!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

! waq_netcdf_utils --
!     Module of utility routines for dealing with NetCDF output
module waq_netcdf_utils
    use netcdf, only: nf90_max_dims, nf90_max_name, nf90_inquire, nf90_inquire_attribute, nf90_redef, nf90_noerr, &
            nf90_def_var, nf90_enameinuse, nf90_inquire_dimension, nf90_ebaddim, nf90_enotvar, nf90_enotatt, &
            nf90_eindefine, nf90_enddef, nf90_get_var, nf90_put_var, nf90_inq_varid, nf90_inq_attname, nf90_get_att, &
            nf90_copy_att, nf90_del_att, nf90_inq_dimid, nf90_float, nf90_def_dim, nf90_put_att, nf90_def_dim, &
            nf90_elatefill, nf90_inquire_variable, nf90_int, nf90_enotindefine, nf90_unlimited, &
            ! imported from waq_netcdf_utils, but not used in this module (should be abstractec in a derived data type)
            nf90_inq_libvers, nf90_nowrite, nf90_open, nf90_strerror, nf90_create, nf90_netcdf4, nf90_clobber, &
            nf90_format_classic, nf90_global, nf90_char, nf90_max_var_dims, nf90_sync, nf90_double

    use results, only: ncopt
    use ISO_FORTRAN_ENV, only: int64

    implicit none

    interface
        subroutine getuuid(guid_string) bind(c, name = 'getuuid')
            character(len = 1), dimension(*) :: guid_string
        end subroutine getuuid
    end interface

    interface dlwqnc_write_wqvariable
        module procedure write_3d_variable
        module procedure write_2d_variable
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

    private
    public :: nf90_max_name, nf90_inq_libvers, nf90_nowrite, nf90_noerr, nf90_get_att, nf90_open, nf90_strerror, &
            nf90_inquire_variable, nf90_max_var_dims, nf90_max_dims, nf90_create, nf90_netcdf4, nf90_clobber, &
            nf90_format_classic, nf90_global, nf90_put_att, nf90_def_dim, nf90_char, nf90_sync, nf90_enddef, &
            nf90_redef, nf90_get_var, nf90_def_var, nf90_double, nf90_put_var, nf90_float, nf90_enameinuse, &
            nf90_inq_dimid
    public :: set_dlwqnc_debug_status, find_meshes_by_attributes, write_time, dlwqnc_write_wqvariable, &
            create_variable, create_time_variable, create_layer_dimension, create_dimension, &
            copy_mesh, copy_variable_attributes, read_dimensions
    public :: type_ugrid_face_crds, type_ugrid_node_crds, dlwqnc_type1d, dlwqnc_type2d, type_ugrid_mesh1d, &
            type_ugrid_network_geometry, type_ugrid_network

contains

    integer function set_dlwqnc_debug_status (debug_status)
        !! Set the debug status of these routines
        !! Returns:
        !!     nf90_noerr if variable found, otherwise an error code
        logical :: debug_status         !! True or False

        dlwqnc_debug = debug_status
        set_dlwqnc_debug_status = 0
    end function set_dlwqnc_debug_status

    function lowercase(input_string) result(lowercase_string)
        !! Convert a string to lowercase
        character(len = *), intent(in) :: input_string
        character(len = len(input_string)) :: lowercase_string

        integer, parameter :: ASCII_a = iachar('a')
        integer, parameter :: ASCII__A = iachar('A')
        integer, parameter :: ASCII_Z = iachar('Z')
        integer, parameter :: case_offset = ASCII_a - ASCII__A
        integer :: char_index, ascii_value

        do char_index = 1, len(input_string)
            ascii_value = iachar(input_string(char_index:char_index))
            if (ascii_value >= ASCII__A .and. ascii_value <= ASCII_Z) then
                ascii_value = ascii_value - case_offset
            endif
            lowercase_string(char_index:char_index) = achar(ascii_value)
        enddo
    end function lowercase

    integer function find_meshes_by_attributes(netcdf_id, var_id_2d, ugrid_type, var_id_1d, var_id_network, &
            var_id_network_geometry)
        !! Find the grid(s) by their particular attribute(s)
        !!
        !! Returns:
        !!     nf90_noerr if variable found, otherwise an error code
        !!
        !! - detect UNTRIM 2D by cf_role = "delwaq_role"
        !! - detect 2D by cf_role = "mesh_topology" and topology_dimension = 2 ;
        !! - detect 1D by cf_role = "mesh_topology" and topology_dimension = 1 ;
        !! - detect network by edge_geometry = "network_geometry" ;
        use io_ugrid
        use netcdf_utils, only : ncu_get_att

        integer, intent(in) :: netcdf_id         !! ID of the NetCDF file
        integer, intent(out) :: var_id_2d     !! varaiable ID of 2D mesh (if found)
        integer, intent(out) :: ugrid_type  !! type of 2D mesh
        integer, intent(out) :: var_id_1d     !! varaiable ID of 1D mesh (if found)
        integer, intent(out) :: var_id_network!! varaiable ID of 1D network (if found)
        integer, intent(out) :: var_id_network_geometry   !! varaiable ID of 1D network geometry (if found)

        character(len = 256) :: attributem, expected_value
        integer :: num_vars, ierror, var_index, var_index_2
        character(len = nf90_max_dims) :: var_name
        logical :: is_delwaq_role, is_network, is_mesh_topology
        character(len = :), allocatable :: cf_role, edge_geometry
        integer :: topology_dimension

        allocate(character(len = 0) :: cf_role)
        allocate(character(len = 0) :: edge_geometry)
        find_meshes_by_attributes = -1
        var_id_2d = -1
        ugrid_type = -1
        var_id_1d = -1
        var_id_network = -1

        ierror = nf90_inquire(netcdf_id, nVariables = num_vars)
        if (ierror /= nf90_noerr) then
            return
        endif

        ! Iterate through variables
        do var_index = 1, num_vars
            ierror = nf90_inquire_variable(netcdf_id, var_index, name = var_name)

            ! Check for UNTRIM 2D
            is_delwaq_role = .false.
            ierror = nf90_inquire_attribute(netcdf_id, var_index, 'delwaq_role')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            else if (ierror == nf90_noerr) then
                is_delwaq_role = .true.
            endif

            ! Check for mesh topology
            cf_role = ''
            is_mesh_topology = .false.
            ierror = nf90_inquire_attribute(netcdf_id, var_index, 'cf_role')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            elseif (ierror == nf90_noerr) then
                ierror = ncu_get_att(netcdf_id, var_index, 'cf_role', cf_role)
                if (ierror /= nf90_noerr) then
                    return
                else if (cf_role == 'mesh_topology') then
                    is_mesh_topology = .true.
                endif
            else
                cycle
            endif

            ! Check topology dimension
            topology_dimension = 0
            ierror = nf90_inquire_attribute(netcdf_id, var_index, 'topology_dimension')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            elseif (ierror == nf90_noerr) then !
                ierror = nf90_get_att(netcdf_id, var_index, 'topology_dimension', topology_dimension)
                if (ierror /= nf90_noerr) then
                    return
                endif
            else
                cycle
            endif

            ! Check for network
            is_network = .false.
            ierror = nf90_inquire_attribute(netcdf_id, var_index, 'edge_geometry')
            if (ierror /= nf90_noerr .and. ierror /= nf90_enotatt) then
                return
            else if (ierror == nf90_noerr) then
                ierror = ncu_get_att(netcdf_id, var_index, 'edge_geometry', edge_geometry)
                if (ierror /= nf90_noerr) then
                    return
                endif
                ierror = nf90_inq_varid(netcdf_id, edge_geometry, var_index_2)
                if (ierror /= nf90_noerr) then
                    return
                endif
                is_network = .true.
            endif

            if (is_delwaq_role) then ! UNTRIM
                if (var_id_2d < 0 .and. var_index > 0) then
                    var_id_2d = var_index
                    ugrid_type = type_ugrid_face_crds
                else
                    if (dlwqnc_debug) write(*, *) 'Error: found multiple grids with ''delwaq_role'''
                    return
                endif
            else if (is_mesh_topology) then ! D-Flow-FM UGRID
                if (topology_dimension == 2) then ! 2D
                    if (var_id_2d < 0 .and. var_index > 0) then
                        var_id_2d = var_index
                        ugrid_type = type_ugrid_node_crds
                    else
                        if (dlwqnc_debug) write(*, *) 'Error: found multiple 2D meshes'
                        return
                    endif
                else if (topology_dimension == 1) then ! 1D
                    if (is_network) then
                        if (var_id_network < 0 .and. var_index > 0 .and. var_index_2 > 0) then
                            var_id_network = var_index
                            var_id_network_geometry = var_index_2
                        else
                            if (dlwqnc_debug) write(*, *) 'Error: found multiple networks'
                            return
                        endif
                    else
                        if (var_id_1d < 0 .and. var_index > 0) then
                            var_id_1d = var_index
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

        if (var_id_2d /= -1 .or. var_id_1d /= -1) then
            find_meshes_by_attributes = nf90_noerr
        else
            find_meshes_by_attributes = nf90_enotatt
        endif

    end function find_meshes_by_attributes

    integer function copy_variable_attributes(source_nc_id, destination_nc_id, source_var_id, destination_var_id)
        !! Copy the attributes for a variable (convenience function)
        !! Returns:
        !!     nf90_noerr if all okay, otherwise an error code
        !!
        !! Note:
        !!     The variable in the output file must already exist
        integer, intent(in) :: source_nc_id           !! ID of the input NetCDF file
        integer, intent(in) :: destination_nc_id      !! ID of the output NetCDF file
        integer, intent(in) :: source_var_id          !! ID of the variable in the input file
        integer, intent(in) :: destination_var_id     !! ID of the variable in the output file

        integer :: ierror
        integer :: i
        character(len = nf90_max_dims) :: attname
        integer :: num_atts

        copy_variable_attributes = -1

        ! Get the number of attributes for the variable
        ierror = nf90_inquire_variable(source_nc_id, source_var_id, nAtts = num_atts)
        if (ierror == nf90_enotvar) then
            ierror = nf90_inquire(source_nc_id, nAttributes = num_atts)
        endif
        if (ierror /= nf90_noerr) then
            copy_variable_attributes = ierror
            return
        endif

        ! Copy each attribute
        do i = 1, num_atts
            ierror = nf90_inq_attname(source_nc_id, source_var_id, i, attname)
            if (ierror /= nf90_noerr) then
                copy_variable_attributes = ierror
                return
            endif

            ierror = nf90_copy_att(source_nc_id, source_var_id, attname, destination_nc_id, destination_var_id)
            if (ierror /= nf90_noerr) then
                copy_variable_attributes = ierror
                return
            endif
        enddo

        ! Remove the attribute "parent_mesh" however - we do not need it and it would
        ! require copying much more information to be consistent. If it does not exist,
        ! there will probably be an error, but ignore that.
        ierror = nf90_del_att(destination_nc_id, destination_var_id, "parent_mesh")

        copy_variable_attributes = nf90_noerr
    end function copy_variable_attributes

    integer function copy_mesh(source_nc_id, destination_nc_id, source_mesh_id, mesh_name, ugrid_type)
        !! Copy the mesh data to the output file
        !! Returns nf90_noerr if all okay, otherwise an error code
        integer, intent(in) :: source_nc_id       !! ID of the input NetCDF file
        integer, intent(in) :: destination_nc_id      !! ID of the output NetCDF file
        integer, intent(in) :: source_mesh_id     !! ID of the mesh variable in the input file
        character(len = *), intent(in) :: mesh_name
        integer, intent(in) :: ugrid_type

        integer :: mesh_id_out, source_var_id, destination_var_id
        integer :: ierror, i, k
        integer, dimension(10) :: ierrorn
        integer :: mesh_value, xtype, length, crs_value, num_atts

        character(len = nf90_max_name) :: var_name
        integer, dimension(nf90_max_dims) :: dimsizes

        copy_mesh = -1

        ! First create the variable in the output file
        ierror = nf90_inquire_variable(source_nc_id, source_mesh_id, name = var_name)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ! Define the variable in the destination file
        ierror = nf90_redef(destination_nc_id)
        if (ierror /= nf90_noerr .and. ierror /= nf90_eindefine) then
            copy_mesh = ierror
            return
        endif

        ! Copy associated data based on ugrid_type
        ierror = nf90_def_var(destination_nc_id, trim(var_name), nf90_int, varid = mesh_id_out)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ierror = nf90_enddef(destination_nc_id)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ierror = nf90_get_var(source_nc_id, source_mesh_id, mesh_value)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ierror = nf90_put_var(destination_nc_id, mesh_id_out, mesh_value)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ierror = nf90_redef(destination_nc_id)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ! Copy the attributes
        ierror = copy_variable_attributes(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ! Copy the dimensions
        ierror = copy_dimensions(source_nc_id, destination_nc_id, dimsizes)
        if (ierror /= nf90_noerr) then
            copy_mesh = ierror
            return
        endif

        ! Copy the associated data
        ierror = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                "node_coordinates", dimsizes)
        if (ierror /= nf90_noerr) then
            if (dlwqnc_debug) write(*, *) 'Copy associated failed (copying mesh)  - ', ierror
            copy_mesh = ierror
            return
        endif

        ierrorn = 0
        select case (ugrid_type)
        case (type_ugrid_face_crds)
            ierrorn(1) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_coordinates", dimsizes)
            ierrorn(2) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "face_coordinates", dimsizes)
            ierrorn(3) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_node_connectivity", dimsizes)
            ierrorn(4) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_face_connectivity", dimsizes)
            ierrorn(5) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "face_node_connectivity", dimsizes)
            ierrorn(6) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "face_edge_connectivity", dimsizes)
            ierrorn(7) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    trim(mesh_name) // "_edge_bc", dimsizes, use_attribute = .false.)
            ierrorn(8) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "projected_coordinate_system", dimsizes, use_attribute = .false.)
        case (type_ugrid_node_crds)
            warning_message = .true.
            ierrorn(1) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_coordinates", dimsizes)
            ierrorn(2) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "face_coordinates", dimsizes)
            ierrorn(3) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_node_connectivity", dimsizes)
            ierrorn(4) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_face_connectivity", dimsizes)
            ierrorn(5) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "face_node_connectivity", dimsizes)
            ierrorn(6) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "projected_coordinate_system", dimsizes, use_attribute = .false.)
            warning_message = .false.
        case (type_ugrid_mesh1d)
            warning_message = .true.
            ierrorn(1) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_node_connectivity", dimsizes)
            ierrorn(2) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_coordinates", dimsizes)
            ierrorn(3) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_node_connectivity", dimsizes)
            ierrorn(4) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "node_id", dimsizes)
            ierrorn(5) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "node_long_name", dimsizes)
            ierrorn(6) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "projected_coordinate_system", &
                    dimsizes, use_attribute = .false.)
            warning_message = .false.
        case (type_ugrid_network)
            warning_message = .true.
            ierrorn(1) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_node_connectivity", dimsizes)
            ierrorn(2) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "branch_id", dimsizes)
            ierrorn(3) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "branch_long_name", dimsizes)
            ierrorn(4) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "edge_length", dimsizes)
            ierrorn(5) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "node_id", dimsizes)
            ierrorn(6) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "node_long_name", dimsizes)
            ierrorn(7) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "projected_coordinate_system", dimsizes, use_attribute = .false.)
            warning_message = .false.
        case (type_ugrid_network_geometry)
            warning_message = .true.
            ierrorn(1) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "node_count", dimsizes)
            ierrorn(2) = copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, mesh_id_out, &
                    "node_coordinates", dimsizes)
            warning_message = .false.
        end select

        if (any(ierrorn /= nf90_noerr .and. ierrorn /= -999)) then
            if (ugrid_type == type_ugrid_face_crds) then
                if (dlwqnc_debug) write(*, *) 'Copy associated failed (copying mesh) - ', maxval(ierrorn)
            endif
            !! copy_mesh = ierror
            !! return
        endif
        if (any(ierrorn == -999)) then
            if (dlwqnc_debug) then
                write(*, *) 'One or more mesh attributes missing - '
                write(*, *) 'this may cause problems in some postprocessing programs'
            endif
        endif

        ! copy the coordinate information variable - if it is required
        if (ugrid_type == type_ugrid_face_crds) then
            k = index(var_name, char(0))
            if (k == 0) then
                k = len_trim(var_name)
            else
                k = k - 1
            endif
            var_name = var_name(1:k) // "_crs"

            ierror = nf90_inq_varid(source_nc_id, var_name, source_var_id)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Failed to find the coordinate information ('  &
                        // trim(var_name) // ') - ', ierror
                copy_mesh = ierror
                return
            endif

            ierror = nf90_redef(destination_nc_id)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Preparing to define ' // trim(var_name) // ' failed - ', ierror
                copy_mesh = ierror
                !return
            endif
            ierror = nf90_def_var(destination_nc_id, trim(var_name), nf90_int, varid = destination_var_id)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Defining ' // trim(var_name) // ' failed - ', ierror
                copy_mesh = ierror
                return
            endif
            ierror = copy_variable_attributes(source_nc_id, destination_nc_id, source_var_id, destination_var_id)
            if (ierror /= nf90_noerr) then
                if (dlwqnc_debug) write(*, *) 'Copying attributes (for ' // trim(var_name) // ') failed - ', ierror
                copy_mesh = ierror
                return
            endif
            crs_value = -999
            ierror = nf90_get_var(source_nc_id, source_var_id, values = crs_value)
            ierror = nf90_enddef(destination_nc_id)
            ierror = nf90_put_var(destination_nc_id, destination_var_id, values = crs_value)
        else
            ierror = nf90_enddef(destination_nc_id)
            if (ierror == nf90_enotindefine) then
                ierror = 0 ! TODO: For some reason we may be stuck in data mode - possibly if optional attributes are missing
            endif
        endif

        copy_mesh = nf90_noerr
    end function copy_mesh

    integer function read_dimensions(nc_id, dimsizes)
        !! Read all dimensions from the input NetCDF file
        !! Returns nf90_noerr if all okay, otherwise an error code
        integer, intent(in) :: nc_id           !!ID of the input NetCDF file
        integer, intent(out), dimension(:) :: dimsizes      !! Size of each dimension (convenience; output)

        integer :: ierror
        integer :: dim_id, dim_value
        character(len = nf90_max_name) :: dim_name

        read_dimensions = -1
        dim_id = 0

        ! Read each dimension
        do
            dim_id = dim_id + 1
            ierror = nf90_inquire_dimension(nc_id, dim_id, dim_name, dim_value)
            if (ierror == nf90_ebaddim) then
                ! No more dimensions to read
                exit
            elseif (ierror /= nf90_noerr) then
                read_dimensions = ierror
                return
            endif

            dimsizes(dim_id) = dim_value
        enddo

        read_dimensions = nf90_noerr
    end function read_dimensions

    integer function copy_dimensions(source_nc_id, destination_nc_id, dimension_sizes)
        ! Copy all dimensions to the output NetCDF file
        ! Returns nf90_noerr if all okay, otherwise an error code
        integer, intent(in) :: source_nc_id                        !! ID of the input NetCDF file
        integer, intent(in) :: destination_nc_id                   !! ID of the output NetCDF file
        integer, intent(out), dimension(:) :: dimension_sizes      !! Size of each dimension (convenience; output)

        integer :: ierror
        integer :: dim_id, new_dim, dim_value
        character(len = nf90_max_name) :: dim_name

        copy_dimensions = -1
        dim_id = 0

        ! Read and copy each dimension
        do
            dim_id = dim_id + 1
            ierror = nf90_inquire_dimension(source_nc_id, dim_id, dim_name, dim_value)
            if (ierror == nf90_ebaddim) then
                ! No more dimensions to read
                exit
            elseif (ierror /= nf90_noerr) then
                copy_dimensions = ierror
                return
            endif

            dimension_sizes(dim_id) = dim_value

            ! Define the dimension in the destination file
            ierror = nf90_def_dim(destination_nc_id, dim_name, dim_value, new_dim)
            if (new_dim /= dim_id .and. ierror /= nf90_enameinuse) then
                if (dlwqnc_debug) write(*, *) 'WARNING: different dimension IDs - ', trim(dim_name), ' - ', &
                        dim_id, new_dim, ierror
            endif
        enddo

        copy_dimensions = nf90_noerr
    end function copy_dimensions

    recursive function copy_associated_variables(source_nc_id, destination_nc_id, source_mesh_id, destination_mesh_id, &
            attribute, dimension_sizes, use_attribute) result(copy_result)
        !! Copy the data of an associated variable to the output file
        !! Returns:
        !!     nf90_noerr if all okay, otherwise an error code
        use io_ugrid
        use netcdf_utils, only : ncu_get_att

        integer, intent(in) :: source_nc_id                 !! ID of the input NetCDF file
        integer, intent(in) :: destination_nc_id            !! ID of the output NetCDF file
        integer, intent(in) :: source_mesh_id               !! ID of the mesh variable in the input file
        integer, intent(in) :: destination_mesh_id          !! ID of the mesh variable in the output file
        character(len = *), intent(in) :: attribute         !! Attribute holding the names of the associated variables
        integer, intent(in), dimension(:) :: dimension_sizes!! Array with actual dimension sizes
        logical, intent(in), optional :: use_attribute         !! Use the attribute name (default), otherwise assume
        !! the attribute argument is actually the name of the variable to be copied (bit of a hack)

        integer :: copy_result
        integer :: ierror
        integer :: ierr
        integer :: i, j, xtype, length
        integer :: variable_id, new_variable_id
        integer :: num_dimensions
        integer, dimension(nf90_max_var_dims) :: dimension_ids

        character(len = :), allocatable :: attribute_value
        character(len = nf90_max_name) :: var_name
        character(len = 1) :: dummy
        logical :: use_names_from_attribute
        logical, save :: suppress_message = .false. ! Because of the recursive call

        allocate(character(len = 0) :: attribute_value)
        copy_result = -1
        use_names_from_attribute = .true.
        if (present(use_attribute)) then
            use_names_from_attribute = use_attribute
        endif

        ! First retrieve the names of the associated variables and
        ! re-create them in the output file
        ierror = nf90_enddef(destination_nc_id)
        if (ierror /= nf90_noerr .and. ierror /= nf90_enotindefine) then
            copy_result = ierror
            return
        endif

        if (use_names_from_attribute) then
            attribute_value = ''
            ierror = ncu_get_att(source_nc_id, source_mesh_id, attribute, attribute_value)
            if (ierror /= nf90_noerr .and. .not. suppress_message) then
                if (warning_message) then
                    if (dlwqnc_debug) write(*, *) 'Warning: retrieving attribute ', trim(attribute), &
                            ' failed -- ', ierror
                    copy_result = -999
                else
                    if (dlwqnc_debug) write(*, *) 'Error retrieving attribute ', trim(attribute), ' -- ', ierror
                    copy_result = ierror
                endif
                return
            endif
        else
            attribute_value = attribute
        endif

        do i = 1, 100  ! JanM: Is attribute_value lang genoeg
            read(attribute_value, *, iostat = ierr) (dummy, j = 1, i - 1), var_name

            if (ierr /= 0) then
                exit
            endif

            ierror = nf90_inq_varid(source_nc_id, trim(var_name), varid = variable_id)
            if (ierror /= nf90_noerr) then
                copy_result = ierror
                return
            endif

            ! Create the variable
            ierror = nf90_inquire_variable(source_nc_id, variable_id, xtype = xtype, ndims = num_dimensions, &
                    dimids = dimension_ids)

            if (ierror /= nf90_noerr) then
                copy_result = ierror
                return
            endif

            ierror = nf90_redef(destination_nc_id)
            if (ierror /= nf90_noerr) then
                copy_result = ierror
                return
            endif


            ! Note: the deflate_level option is the only API difference between NetCDF 4 and
            !       NetCDF 3 of interest to the code.
            !       Should you require NetCDF 3, then use the commented call to nf90_def_var
            !       instead.
            if (ncopt(4) == 1 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(destination_nc_id, trim(var_name), xtype, dimension_ids(1:num_dimensions), &
                        new_variable_id, deflate_level = dlwqnc_deflate)
            else
                ierror = nf90_def_var(destination_nc_id, trim(var_name), xtype, dimension_ids(1:num_dimensions), &
                        new_variable_id)
            endif
            if (ierror /= nf90_noerr) then
                copy_result = ierror
                return
            endif

            ierror = copy_variable_attributes(source_nc_id, destination_nc_id, variable_id, new_variable_id)
            if (ierror /= nf90_noerr) then
                copy_result = ierror
                return
            endif

            ierror = nf90_enddef(destination_nc_id)
            if (ierror /= nf90_noerr) then
                copy_result = ierror
                return
            endif

            select case (xtype)
            case(nf90_int)
                ierror = copy_integer_variable(source_nc_id, destination_nc_id, variable_id, new_variable_id, &
                        num_dimensions, dimension_ids, dimension_sizes)
            case(nf90_int64)
                ierror = copy_integer64_variable(source_nc_id, destination_nc_id, variable_id, new_variable_id, &
                        num_dimensions, dimension_ids, dimension_sizes)
            case(nf90_real)
                ierror = copy_real_variable(source_nc_id, destination_nc_id, variable_id, new_variable_id, &
                        num_dimensions, dimension_ids, dimension_sizes)
            case(nf90_double)
                ierror = copy_double_variable(source_nc_id, destination_nc_id, variable_id, new_variable_id, &
                        num_dimensions, dimension_ids, dimension_sizes)
            case (nf90_char)
                ierror = copy_character_variable(source_nc_id, destination_nc_id, variable_id, new_variable_id, &
                        num_dimensions, dimension_ids, dimension_sizes)
            case default
                ierror = -1
            end select
            if (ierror /= nf90_noerr) then
                copy_result = ierror
                return
            endif

            ! The variable may in turn have variables that require copying: the bounds attribute
            ! (Ignore any errors though)
            if (attribute /= 'bounds') then
                suppress_message = .true.
                ierror = copy_associated_variables(source_nc_id, destination_nc_id, variable_id, new_variable_id, &
                        'bounds', dimension_sizes)
                suppress_message = .false.
            endif
        enddo
        deallocate(attribute_value)
        copy_result = nf90_noerr
    end function copy_associated_variables

    integer function copy_integer_variable(source_nc_id, destination_nc_id, source_variable_id, &
            destination_variable_id, num_dimensions, dimension_ids, dimension_sizes)
        !! Copy integer data of a variable to the output NetCDF file
        !! Returns nf90_noerr if all okay, otherwise an error code
        !! Note:
        !!     For some reason we need to distinguish between one and two dimensions,
        !!     otherwise we get an error "nf90_eedge"
        integer, intent(in) :: source_nc_id                  !! ID of the input NetCDF file
        integer, intent(in) :: destination_nc_id             !! ID of the output NetCDF file
        integer, intent(in) :: source_variable_id            !! ID of the mesh variable in the input file
        integer, intent(in) :: destination_variable_id       !! ID of the mesh variable in the output file
        integer, intent(in) :: num_dimensions                !! Number of dimensions
        integer, intent(in), dimension(:) :: dimension_ids   !! IDs of the dimensions
        integer, intent(in), dimension(:) :: dimension_sizes !! Actual size of each dimension

        integer :: total_size
        integer :: ierror
        integer :: ierr
        integer :: i
        integer, dimension(:), allocatable :: int_data_1d
        integer, dimension(:, :), allocatable :: int_data_2d

        copy_integer_variable = -1

        ! Determine total size for allocation
        if (num_dimensions /= 2) then
            total_size = 1
            do i = 1, num_dimensions
                total_size = total_size * dimension_sizes(dimension_ids(i))
            enddo
            ! Allocate based on number of dimensions and copy data
            allocate(int_data_1d(total_size), stat = ierr)
            ierror = nf90_get_var(source_nc_id, source_variable_id, int_data_1d)
        else
            ! Allocate based on number of dimensions and copy data
            allocate(int_data_2d(dimension_sizes(dimension_ids(1)), dimension_sizes(dimension_ids(2))), stat = ierr)
            ierror = nf90_get_var(source_nc_id, source_variable_id, int_data_2d)
        endif

        if (num_dimensions /= 2) then
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, int_data_1d)
        else
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, int_data_2d)
        endif

        if (ierror /= nf90_noerr) then
            copy_integer_variable = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving int values: ', ierror, ' -- size: ', total_size
            return
        endif

        copy_integer_variable = nf90_noerr
    end function copy_integer_variable

    integer function copy_integer64_variable(source_nc_id, destination_nc_id, source_variable_id, &
            destination_variable_id, num_dimensions, dimension_ids, dimension_sizes)

        integer, intent(in) :: source_nc_id, destination_nc_id, source_variable_id, destination_variable_id, &
                num_dimensions
        integer, intent(in), dimension(:) :: dimension_ids, dimension_sizes

        integer :: total_size
        integer :: ierror
        integer :: ierr
        integer :: i

        integer(kind = int64), dimension(:), allocatable :: int_data_1d
        integer(kind = int64), dimension(:, :), allocatable :: int_data_2d

        copy_integer64_variable = -1

        if (num_dimensions /= 2) then
            total_size = 1
            do i = 1, num_dimensions
                total_size = total_size * dimension_sizes(dimension_ids(i))
            enddo
            allocate(int_data_1d(total_size), stat = ierr)
            ierror = nf90_get_var(source_nc_id, source_variable_id, int_data_1d)
        else
            allocate(int_data_2d(dimension_sizes(dimension_ids(1)), dimension_sizes(dimension_ids(2))), stat = ierr)
            ierror = nf90_get_var(source_nc_id, source_variable_id, int_data_2d)
        endif

        if (num_dimensions /= 2) then
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, int_data_1d)
        else
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, int_data_2d)
        endif

        if (ierror /= nf90_noerr) then
            copy_integer64_variable = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving int64 values: ', ierror, ' -- size: ', total_size
            return
        endif

        copy_integer64_variable = nf90_noerr
    end function copy_integer64_variable

    integer function copy_real_variable(source_nc_id, destination_nc_id, source_variable_id, destination_variable_id, &
            num_dimensions, dimension_ids, dimension_sizes)
        integer, intent(in) :: source_nc_id, destination_nc_id, source_variable_id, destination_variable_id, &
                num_dimensions
        integer, intent(in), dimension(:) :: dimension_ids, dimension_sizes

        integer :: total_size
        integer :: ierror
        integer :: ierr
        integer :: i

        real, dimension(:), allocatable :: real_data_1d
        real, dimension(:, :), allocatable :: real_data_2d

        copy_real_variable = -1

        if (num_dimensions /= 2) then
            total_size = 1
            do i = 1, num_dimensions
                total_size = total_size * dimension_sizes(dimension_ids(i))
            enddo
            allocate(real_data_1d(total_size))
            ierror = nf90_get_var(source_nc_id, source_variable_id, real_data_1d)
        else
            allocate(real_data_2d(dimension_sizes(dimension_ids(1)), dimension_sizes(dimension_ids(2))))
            ierror = nf90_get_var(source_nc_id, source_variable_id, real_data_2d)
        endif

        if (ierror /= nf90_noerr) then
            copy_real_variable = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving real values: ', ierror, ' -- size: ', total_size
            return
        endif

        if (num_dimensions /= 2) then
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, real_data_1d)
        else
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, real_data_2d)
        endif

        if (ierror /= nf90_noerr) then
            copy_real_variable = ierror
            return
        endif

        copy_real_variable = nf90_noerr
    end function copy_real_variable

    integer function copy_double_variable(source_nc_id, destination_nc_id, source_variable_id, &
            destination_variable_id, num_dimensions, dimension_ids, dimension_sizes)
        integer, intent(in) :: source_nc_id, destination_nc_id, source_variable_id, destination_variable_id, &
                num_dimensions
        integer, intent(in), dimension(:) :: dimension_ids, dimension_sizes

        integer :: total_size
        integer :: ierror
        integer :: ierr
        integer :: i

        real(kind = kind(1.0d0)), dimension(:), allocatable :: real_data_1d
        real(kind = kind(1.0d0)), dimension(:, :), allocatable :: real_data_2d

        copy_double_variable = -1

        if (num_dimensions /= 2) then
            total_size = 1
            do i = 1, num_dimensions
                total_size = total_size * dimension_sizes(dimension_ids(i))
            enddo
            allocate(real_data_1d(total_size))
            ierror = nf90_get_var(source_nc_id, source_variable_id, real_data_1d)
        else
            allocate(real_data_2d(dimension_sizes(dimension_ids(1)), dimension_sizes(dimension_ids(2))))
            ierror = nf90_get_var(source_nc_id, source_variable_id, real_data_2d)
        endif

        if (ierror /= nf90_noerr) then
            copy_double_variable = ierror
            if (dlwqnc_debug) write(*, *) 'Error retrieving double values: ', ierror, ' -- size: ', total_size
            return
        endif

        if (num_dimensions /= 2) then
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, real_data_1d)
        else
            ierror = nf90_put_var(destination_nc_id, destination_variable_id, real_data_2d)
        endif

        if (ierror /= nf90_noerr) then
            copy_double_variable = ierror
            return
        endif

        copy_double_variable = nf90_noerr
    end function copy_double_variable

    integer function copy_character_variable(source_nc_id, destination_nc_id, source_variable_id, &
            destination_variable_id, num_dimensions, dimension_ids, dimension_sizes)

        integer, intent(in) :: source_nc_id, destination_nc_id, source_variable_id, destination_variable_id, &
                num_dimensions
        integer, intent(in), dimension(:) :: dimension_ids, dimension_sizes

        integer :: dim1, dim2
        integer :: ierror
        integer :: ierr

        character(len = :), dimension(:), allocatable :: value

        copy_character_variable = -1

        dim1 = dimension_sizes(dimension_ids(1))
        dim2 = dimension_sizes(dimension_ids(2))

        allocate(character(len = dim1) :: value(dim2), stat = ierr)
        if (ierr /= 0) then
            return
        endif

        ierror = nf90_get_var(source_nc_id, source_variable_id, value)
        if (ierror /= nf90_noerr) then
            copy_character_variable = ierror
            return
        endif

        ierror = nf90_put_var(destination_nc_id, destination_variable_id, value)
        if (ierror /= nf90_noerr) then
            copy_character_variable = ierror
            return
        endif

        copy_character_variable = nf90_noerr
    end function copy_character_variable

    integer function write_time(nc_id, time_variable_id, time_boundary_var_id, record_index, time_value)
        !! Write the time and time boundaries to the output NetCDF file
        !! Returns nf90_noerr if all okay, otherwise an error code
        integer, intent(in) :: nc_id                  !! ID of the output NetCDF file
        integer, intent(in) :: time_variable_id       !! ID of the time variable
        integer, intent(in) :: time_boundary_var_id   !! ID of the time boundaries variable
        integer, intent(in) :: record_index           !! Index of the record in the output file
        integer, intent(in) :: time_value             !! Actual time value (seconds)

        integer :: ierror
        integer, dimension(1) :: time_data
        integer, dimension(2) :: start, count
        integer, dimension(2) :: time_bounds

        ! Initialization
        write_time = nf90_noerr

        ! Write the time variable
        time_data = time_value
        start = record_index
        count = 1
        ierror = nf90_put_var(nc_id, time_variable_id, time_data, start = start(1:1), count = count(1:1))
        if (ierror /= nf90_noerr) then
            write_time = ierror
            return
        endif

        count = (/ 2, 1 /)
        if (record_index == 1) then
            time_bounds = time_value
        else
            start = (/ 1, record_index - 1 /)
            ierror = nf90_get_var(nc_id, time_boundary_var_id, time_bounds, start = start, count = count)
            if (ierror /= nf90_noerr) then
                write_time = ierror
                return
            endif
        endif
        time_bounds(1) = time_bounds(2)
        time_bounds(2) = time_value
        start(2) = record_index
        ierror = nf90_put_var(nc_id, time_boundary_var_id, time_bounds, start = start, count = count)
        if (ierror /= nf90_noerr) then
            write_time = ierror
            return
        endif

    end function write_time

    integer function write_3d_variable(nc_id, variable_id, record_index, real_data_3d)
        !! Write a 3D variable to the output NetCDF file
        !! Returns nf90_noerr if all okay, otherwise an error code
        integer, intent(in) :: nc_id      !! ID of the output NetCDF file
        integer, intent(in) :: variable_id         !! ID of the WQ variable in the output file
        integer, intent(in) :: record_index       !! Index of the record in the output file
        real, dimension(:, :), intent(in) :: real_data_3d     !! Array of values

        integer :: ierror
        integer, dimension(3) :: start
        integer, dimension(3) :: count

        write_3d_variable = nf90_noerr

        start = (/1, 1, record_index /)
        count = (/ size(real_data_3d, 1), size(real_data_3d, 2), 1 /)
        ierror = nf90_put_var(nc_id, variable_id, real_data_3d, start = start, count = count)
        if (ierror /= nf90_noerr) then
            write_3d_variable = ierror
            return
        endif
    end function write_3d_variable

    integer function write_2d_variable(nc_id, variable_id, record_index, real_data_2d)
        integer, intent(in) :: nc_id
        integer, intent(in) :: variable_id
        integer, intent(in) :: record_index
        real, dimension(:), intent(in) :: real_data_2d

        integer :: ierror
        integer, dimension(2) :: start
        integer, dimension(2) :: count

        write_2d_variable = nf90_noerr

        start = (/1, record_index /)
        count = (/ size(real_data_2d), 1 /)
        ierror = nf90_put_var(nc_id, variable_id, real_data_2d, start = start, count = count)
        if (ierror /= nf90_noerr) then
            write_2d_variable = ierror
            return
        endif
    end function write_2d_variable

    integer function create_time_variable(nc_id, t0_string, time_variable_id, time_boundary_var_id, time_dimension_id)
        !! Create the time coordinate variable in the NetCDF file
        !! Returns nf90_noerr if all okay, otherwise an error code
        integer, intent(in) :: nc_id                    !! ID of the output NetCDF file
        character(len = *), intent(in) :: t0_string     !! The so-called T0-string
        integer, intent(out) :: time_variable_id        !! ID of the time variable
        integer, intent(out) :: time_boundary_var_id    !! ID of the time bounds variable
        integer :: time_dimension_id      !! ID of the time dimension
        integer :: twoid
        integer :: ierror
        character(len = 30) :: t0_date_time
        character(len = nf90_max_name) :: name
        integer :: dim_value
        integer :: k

        create_time_variable = nf90_noerr

        time_variable_id = 0

        name = 'nTimesDlwq'
        dim_value = nf90_unlimited

        ierror = nf90_redef(nc_id)
        if (ierror /= 0 .and. ierror /= nf90_eindefine) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_def_dim(nc_id, name, dim_value, time_dimension_id)
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_def_var(nc_id, name, nf90_int, (/ time_dimension_id /), time_variable_id)
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_inq_dimid(nc_id, "Two", twoid)
        if (ierror == nf90_ebaddim) then
            ierror = nf90_def_dim(nc_id, "Two", 2, twoid)
        endif
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        name = 'nTimesDlwqBnd'
        ierror = nf90_def_var(nc_id, name, nf90_int, (/ twoid, time_dimension_id /), time_boundary_var_id)
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, time_variable_id, "bounds", name) ! Must be the same as variable above, of course
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, time_variable_id, "long_name", "time")
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, time_variable_id, "standard_name", "time")
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, time_variable_id, "calendar", "gregorian")
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, time_variable_id, "axis", "T")
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        t0_date_time = t0_string(5:23) // " +00:00" ! DELWAQ has no timezone information!
        t0_date_time(5:5) = '-'
        t0_date_time(8:8) = '-'

        ierror = nf90_put_att(nc_id, time_variable_id, "units", "seconds since " // t0_date_time)
        if (ierror /= 0) then
            create_time_variable = ierror
            return
        endif

        ierror = nf90_enddef(nc_id)

    end function create_time_variable

    integer function create_variable(nc_id, mesh_name, delwaq_variable_name, long_name, standard_name, unit, &
            time_dimension_id, segments_num, layers_num, variable_id)
        !! Create a WQ variable in the output file
        !!
        !! Returns:
        !!     nf90_noerr if all okay, otherwise an error code
        !!
        !! Notes:
        !!     time ID? mesh ID? Roles?
        use netcdf_utils, only : ncu_get_att

        integer, intent(in) :: nc_id              !! ID of the output NetCDF file
        character(len = *), intent(in) :: mesh_name !! Name of the mesh
        character(len = *), intent(in) :: delwaq_variable_name    !! DELWAQ name of the WQ variable
        character(len = *), intent(in) :: long_name  !! Long descriptive name of the WQ variable
        character(len = *), intent(in) :: standard_name
        !! Standard name according to CF convention (or identical to wqname)
        character(len = *), intent(in) :: unit      !! String defining the unit of the WQ variable
        integer, intent(in) :: time_dimension_id              !! Dimension: number of times
        integer, intent(in) :: segments_num             !! Dimension: number of cells per layer
        integer, intent(in) :: layers_num              !! DimensionL number of layers (or -999 if 2D variable - use
        !! dlwqnc_type2d, or -111 if 1D variable - use dlwqnc_type1d)
        integer, intent(out) :: variable_id                !! ID of the WQ variable in the output file

        integer :: i
        integer :: k, k2
        integer :: ierror
        integer :: meshid
        integer :: dim_value
        character(len = nf90_max_name) :: name
        character(len = nf90_max_name) :: name2D
        character(len = nf90_max_name), dimension(5) :: dim_name
        character(len = 3 * nf90_max_name) :: methods
        character(len = :), allocatable :: coords

        allocate(character(len = 0) :: coords)

        create_variable = nf90_noerr

        ierror = nf90_redef(nc_id)

        k = index(mesh_name, char(0))
        if (k == 0) then
            k = len_trim(mesh_name)
        else
            k = k - 1
        endif
        write(name, '(a,a,a)') mesh_name(1:k), '_', trim(delwaq_variable_name)
        write(name2d, '(a,a,a)') mesh_name(1:k), '_2d_', trim(delwaq_variable_name)

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

        ! TODO: support for chunking - this requires an array of chunksizes per dimension
        if (layers_num /= dlwqnc_type2d .and. layers_num /= dlwqnc_type1d) then
            if (ncopt(1) == 4 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(nc_id, name, nf90_float, (/ segments_num, layers_num, time_dimension_id /), &
                        variable_id, deflate_level = ncopt(2))
            else
                ierror = nf90_def_var(nc_id, name, nf90_float, (/ segments_num, layers_num, time_dimension_id /), &
                        variable_id)
            endif
        else if (layers_num == dlwqnc_type2d) then
            if (ncopt(1) == 4 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(nc_id, name2d, nf90_float, (/ segments_num, time_dimension_id /), variable_id, &
                        deflate_level = ncopt(2))
            else
                ierror = nf90_def_var(nc_id, name2d, nf90_float, (/ segments_num, time_dimension_id /), variable_id)
            endif
        else
            if (ncopt(1) == 4 .and. ncopt(2) /= 0) then
                ierror = nf90_def_var(nc_id, name, nf90_float, (/ segments_num, time_dimension_id /), variable_id, &
                        deflate_level = ncopt(2))
            else
                ierror = nf90_def_var(nc_id, name, nf90_float, (/ segments_num, time_dimension_id /), variable_id)
            endif
        endif
        !ierror = nf90_def_var( nc_id, name, nf90_float, (/ segments_num, layers_num, time_dimension_id /), variable_id)
        if (ierror /= 0 .and. ierror /= nf90_enameinuse) then
            if (dlwqnc_debug) write(*, *) 'Error creating WQ variable: ', ierror
            create_variable = ierror
            return
        else if (ierror == nf90_enameinuse) then
            if (layers_num /= dlwqnc_type2d) then
                ierror = nf90_inq_varid(nc_id, name, variable_id)
            else
                ierror = nf90_inq_varid(nc_id, name2d, variable_id)
            endif
        endif

        ierror = nf90_put_att(nc_id, variable_id, "long_name", long_name)
        if (ierror /= 0) then
            create_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, variable_id, "units", unit)
        if (ierror /= 0) then
            create_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, variable_id, "_FillValue", -999.0)
        if (ierror /= 0 .and. ierror /= nf90_elatefill) then
            create_variable = ierror
            return
        endif

        if (layers_num /= dlwqnc_type2d .and. layers_num /= dlwqnc_type1d) then
            ierror = nf90_inquire_dimension(nc_id, time_dimension_id, dim_name(1), dim_value)   &
                    + nf90_inquire_dimension(nc_id, segments_num, dim_name(2), dim_value) &
                    + nf90_inquire_dimension(nc_id, layers_num, dim_name(3), dim_value)
        else
            ierror = nf90_inquire_dimension(nc_id, time_dimension_id, dim_name(1), dim_value)   &
                    + nf90_inquire_dimension(nc_id, segments_num, dim_name(2), dim_value)
        endif
        if (ierror /= 0) then
            create_variable = ierror
            return
        endif

        do i = 1, 3
            k2 = index(dim_name(i), char(0))
            if (k2 > 0) then
                dim_name(i)(k2:) = ' '
            endif
        enddo

        if (lowercase(delwaq_variable_name) /= 'volume') then
            write(name, '(a,a,a)') 'volume: ', mesh_name(1:k), '_volume'
            ierror = nf90_put_att(nc_id, variable_id, "cell_measures", name)
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
            if (layers_num /= dlwqnc_type2d .and. layers_num /= dlwqnc_type1d) then
                write(methods, '(20a)') trim(dim_name(1)), ': point ', trim(dim_name(2)), &
                        ': mean ', trim(dim_name(3)), ': mean'
            else
                write(methods, '(20a)') trim(dim_name(1)), ': point ', trim(dim_name(2)), ': mean '
            endif

            ierror = nf90_put_att(nc_id, variable_id, "cell_methods", trim(methods))
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
        endif

        ierror = nf90_inq_varid(nc_id, mesh_name, meshid)
        coords = ''
        ierror = ncu_get_att(nc_id, meshid, "face_coordinates", coords)
        if (ierror /= 0) then
            coords = ''
            ierror = ncu_get_att(nc_id, meshid, "node_coordinates", coords)
        endif

        ierror = nf90_put_att(nc_id, variable_id, "coordinates", coords)
        if (ierror /= 0) then
            create_variable = ierror
            return
        endif

        ierror = nf90_put_att(nc_id, variable_id, "grid_mapping", mesh_name(1:k) // "_crs")
        if (ierror /= 0) then
            create_variable = ierror
            return
        endif

        if (len_trim(standard_name) /= 0) then
            ierror = nf90_put_att(nc_id, variable_id, "standard_name", standard_name)
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
        endif

        if (layers_num /= dlwqnc_type2d .and. layers_num /= dlwqnc_type1d) then
            ierror = nf90_put_att(nc_id, variable_id, "delwaq_name", delwaq_variable_name)
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
        else if (layers_num == dlwqnc_type2d) then
            ierror = nf90_put_att(nc_id, variable_id, "delwaq_name", trim(delwaq_variable_name) // "_avg")
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
        else
            ierror = nf90_put_att(nc_id, variable_id, "delwaq_name", trim(delwaq_variable_name) // "_1d")
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
        endif

        ierror = nf90_put_att(nc_id, variable_id, "mesh", mesh_name(1:k))
        if (ierror /= 0) then
            create_variable = ierror
            return
        endif

        if (layers_num /= dlwqnc_type1d) then
            ierror = nf90_put_att(nc_id, variable_id, "location", "face")
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
        else
            ierror = nf90_put_att(nc_id, variable_id, "location", "node")
            if (ierror /= 0) then
                create_variable = ierror
                return
            endif
        endif

        ierror = nf90_enddef(nc_id)

    end function create_variable

    integer function create_layer_dimension(nc_id, mesh_name, num_layers, thickness, layers_dim_id)
        !! Create the dimension and variables for the layers in the NetCDF file
        !! Returns nf90_noerr if all okay, otherwise an error code
        integer, intent(in) :: nc_id                    !! ID of the output NetCDF file
        character(len = *), intent(in) :: mesh_name     !! Name of the mesh
        integer, intent(in) :: num_layers               !! Number of layers
        real, dimension(:), intent(in) :: thickness     !! Array with thicknesses
        integer, intent(out) :: layers_dim_id           !! ID for number of layers (dimension)

        integer :: i, k
        integer :: ierror
        integer :: cum_layer_var_id
        character(len = nf90_max_name) :: layer_dim_name
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

        create_layer_dimension = nf90_noerr

        k = index(mesh_name, char(0))
        if (k == 0) then
            k = len_trim(mesh_name)
        else
            k = k - 1
        endif

        ierror = nf90_redef(nc_id)
        if (ierror /= nf90_noerr .and. ierror /= nf90_eindefine) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (redef): ', ierror
            create_layer_dimension = ierror
            return
        endif

        write(layer_dim_name, '(2a)') mesh_name(1:k), '_nLayersDlwq'
        ierror = nf90_def_dim(nc_id, layer_dim_name, num_layers, layers_dim_id)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (def_dim): ', ierror
            if (dlwqnc_debug) write(*, *) 'Note: Name: ', trim(layer_dim_name), ' number: ', num_layers
            create_layer_dimension = ierror
            return
        endif

        ! Cumulative sigma coordinate
        write(layer_dim_name, '(3a)') mesh_name(1:k), '_layer_dlwq'
        ierror = nf90_def_var(nc_id, layer_dim_name, nf90_float, (/ layers_dim_id /), cum_layer_var_id)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (def_var): ', ierror
            create_layer_dimension = ierror
            return
        endif

        do i = 1, 5
            ierror = nf90_put_att(nc_id, cum_layer_var_id, attname(i), z_attvalue(i))
            if (ierror /= 0) then
                if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (put_att): ', ierror
                create_layer_dimension = ierror
                return
            endif
        enddo

        ierror = nf90_enddef(nc_id)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (enddef): ', ierror
            create_layer_dimension = ierror
            return
        endif

        ! Construct the cumulative sigma coordinate and write it to the file
        ! Note: following the D-FLOW-FM convention, sigma = 0 is the bottom
        z_sum = 0.0
        do i = num_layers, 1, -1
            z_centre(i) = z_sum + 0.5 * thickness(i)
            z_sum = z_sum + thickness(i)
        enddo

        ierror = nf90_put_var(nc_id, cum_layer_var_id, z_centre)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (put_var): ', ierror
            create_layer_dimension = ierror
            return
        endif

        ierror = nf90_redef(nc_id)
        if (ierror /= 0) then
            if (dlwqnc_debug) write(*, *) 'Note: Creating layer dimension failed (redef): ', ierror
            create_layer_dimension = ierror
            return
        endif

        create_layer_dimension = ierror
    end function create_layer_dimension

    integer function create_dimension(nc_id, num_cells, num_layers, dimension_ids, dimension_sizes)
        !! Create the missing dimensions based on the selected mesh
        !! This is only necessary if the geometry file does not define an aggregation table
        integer, intent(in) :: nc_id          !! ID of the output NetCDF file
        integer, intent(in) :: num_cells   !! Number of segments per layer according to DELWAQ
        integer, intent(in) :: num_layers     !! Number of layers according to DELWAQ
        integer, intent(inout), dimension(:) :: dimension_ids !!Array of dimension IDs for the corresponding DELWAQ mesh
        integer, intent(inout), dimension(:) :: dimension_sizes !! Array of dimensions (with new dimensions added)

        integer :: inc_error
        integer :: varid
        character(len = 200) :: coordinate_names
        character(len = 100) :: name

        create_dimension = nf90_noerr

        inc_error = nf90_redef(nc_id)
        if (inc_error /= nf90_noerr) then
            create_dimension = inc_error
            return
        endif

        inc_error = nf90_def_dim(nc_id, "nSegmentsPerLayerDlwq", num_cells, dimension_ids(1))

        if (inc_error /= nf90_noerr) then
            create_dimension = inc_error
            return
        endif

        inc_error = nf90_enddef(nc_id)
        if (inc_error /= nf90_noerr) then
            create_dimension = inc_error
            return
        endif

        create_dimension = read_dimensions(nc_id, dimension_sizes)

    end function create_dimension

end module waq_netcdf_utils
