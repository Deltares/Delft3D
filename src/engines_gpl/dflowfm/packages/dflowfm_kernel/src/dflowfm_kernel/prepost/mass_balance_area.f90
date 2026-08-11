!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.6
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

!> Handles mass balance areas
module m_mass_balance_area
    use messageHandling, only: warn_flush, err_flush, msgbuf
    use m_unstruc_model_data, only: md_mbafile
    use precision_basics, only: dp
    use tree_structures, only: tree_data

    implicit none(type, external)

    private

    public :: read_and_initialize_mass_balance_area

contains

    !> Reads and initializes the user-specified mass balance areas
    subroutine read_and_initialize_mass_balance_area()
        use tree_structures, only: tree_destroy

        ! Local variables
        type(tree_data), pointer :: mba_ptr !< Pointer to the mass balance area tree structure.

        if (len_trim(md_mbafile) == 0) then

            ! When nog mass balance area file is specified, return without reading
            return

        elseif (index(md_mbafile, '.ini') > 0) then

            call open_mass_balance_area_file(mba_ptr)

            call read_mass_balance_area_file(mba_ptr)

            call tree_destroy(mba_ptr)

        else

            write (msgbuf, '(A)') 'Error while reading mass balance area file '''//trim(md_mbafile)//''': must be an .ini file.'
            call err_flush()

        end if

    end subroutine read_and_initialize_mass_balance_area

    !> Opens the mass balance area file and creates a tree structure from it.
    subroutine open_mass_balance_area_file(mba_ptr)
        use m_flowtimes, only: ti_mba
        use m_unstruc_model_data, only: MBA_MAJOR_FILE_VERSION, MBA_MINOR_FILE_VERSION
        use properties, only: get_version_number, prop_file
        use tree_structures, only: tree_create

        ! Arguments
        type(tree_data), pointer, intent(out) :: mba_ptr !< Pointer to the mass balance area tree structure.

        ! Local variables
        integer :: istat !< Status code for file reading.
        integer :: major !< Major version number of the mass balance area file.
        integer :: minor !< Minor version number of the mass balance area file.
        logical :: success !< Flag indicating whether the mass balance area file was read successfully.

        ! Initialization
        major = 0
        minor = 0

        ! Read the mass balance area file and create the tree structure
        call tree_create(md_mbafile, mba_ptr)
        call prop_file('ini', md_mbafile, mba_ptr, istat)

        if (istat /= 0) then
            write (msgbuf, '(A)') 'Error reading mass balance area file '''//trim(md_mbafile)//'''.'
            call err_flush()
            return
        end if

        ! Check if the mbaInterval is specified in the .mdu file, if not raise an error
        if (.not. (ti_mba > 0)) then
            write (msgbuf, '(A)') 'A mass balance area file has been specified in the .mdu, but no mbaInterval was specified. Please specify a mbaInterval in the .mdu file.'
            call err_flush()
            return
        end if

        ! Check the version number of the mass balance area file
        call get_version_number(mba_ptr, major=major, minor=minor, success=success)

        if (.not. success) then
            write (msgbuf, '(A)') 'File version number not found in mass balance area file '''//trim(md_mbafile)//'''.'
            call warn_flush()
            return
        else if (major > MBA_MAJOR_FILE_VERSION .or. (major == MBA_MAJOR_FILE_VERSION .and. minor > MBA_MINOR_FILE_VERSION)) then
            write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported mass balance area file version in file '''//trim(md_mbafile)//'''. v', &
                major, minor, 'Current format: v', MBA_MAJOR_FILE_VERSION, MBA_MINOR_FILE_VERSION, '.'
            call err_flush()
            return
        end if

    end subroutine open_mass_balance_area_file

    !> Reads the mass balance area file and processes its blocks.
    subroutine read_mass_balance_area_file(mba_ptr)
        use string_module, only: str_tolower
        use tree_structures, only: tree_num_nodes, tree_get_name

        ! Arguments
        type(tree_data), pointer, intent(in) :: mba_ptr !< Pointer to the mass balance area tree structure.

        ! Local variables
        type(tree_data), pointer :: block_ptr !< Pointer to the current block in the mass balance area tree structure.
        integer :: i !< Loop index for iterating over the blocks in the mass balance area tree structure.
        integer :: num_blocks !< Number of blocks in the mass balance area tree structure.
        character(len=:), allocatable :: block_name !< Name of the current block in the mass balance area tree structure.

        num_blocks = tree_num_nodes(mba_ptr)

        do i = 1, num_blocks
            block_ptr => mba_ptr%child_nodes(i)%node_ptr
            block_name = trim(tree_get_name(block_ptr))

            select case (str_tolower(block_name))

                case ('general')

                    ! Skip since it is already read in the open_mass_balance_area_file subroutine

                case ('massbalancearea')

                    ! Read the mass balance area block
                    call read_mass_balance_area_block(block_ptr)

                case default

                    write (msgbuf, '(A)') 'Unknown block '''//trim(block_name)//''' in mass balance area file '''//trim(md_mbafile)//'''.'
                    call warn_flush()

            end select
        end do

    end subroutine read_mass_balance_area_file

    !> Reads a mass balance area block and updates the mass balance area definitions.
    subroutine read_mass_balance_area_block(block_ptr)
        use m_alloc, only: realloc
        use m_cell_geometry, only: xz, yz
        use m_find_name, only: find_name
        use m_flow, only: kmxn
        use m_flowgeom, only: ndxi, kcs
        use m_get_kbot_ktop, only: getkbotktop
        use m_mass_balance_area_data, only: mbaname, nomba, mbadef, nammbalen
        use m_read_location_info, only: read_polyline_coordinates
        use properties, only: prop_get, max_prop_length
        use timespace, only: selectelset_internal_nodes, LOCTP_POLYGON_XY

        ! Arguments
        type(tree_data), pointer, intent(in) :: block_ptr !< Pointer to the mass balance area block in the tree structure.

        ! Local variables
        character(len=max_prop_length) :: name !< Name of the mass balance area.
        character(len=max_prop_length) :: location_file !< Location file for the mass balance area
        integer :: num_columns !< Number of columns in the location file (2D or 3D).
        integer :: num_coordinates !< Number of coordinates defining the polygon of the mass balance area.
        real(kind=dp), allocatable :: x_coordinates(:) !< X coordinates of the polygon defining the mass balance area.
        real(kind=dp), allocatable :: y_coordinates(:) !< Y coordinates of the polygon defining the mass balance area.
        real(kind=dp), allocatable :: z_coordinates(:) !< Z coordinates of the polygon defining the mass balance area.

        logical :: success
        integer :: k, kt, kb, node
        integer :: imba !< Index of the mass balance area in the list of mass balance areas.
        integer :: nselected !< Number of selected internal nodes within the polygon defined in the location file.
        integer, allocatable :: selected_nodes(:) !< Array of selected internal nodes within the polygon defined in the location file.

        ! Initialization
        imba = 0
        name = ''
        location_file = ''

        ! Get name and locationFile of mass balance area block
        call prop_get(block_ptr, '', 'name', name)

        call read_polyline_coordinates(block_ptr, name, md_mbafile, '', 'massbalancearea', x_coordinates, y_coordinates, z_coordinates, num_columns, success)

        if (success) then
            num_coordinates = size(x_coordinates)
        else
            write (msgbuf, '(A)') 'Error reading location file for mass balance area '''//trim(name)//''' in mass balance area file '''//trim(md_mbafile)//'''.'
            return
        end if

        ! Check if the mass balance area name array is allocated, if not allocate it with size 0
        if (.not. allocated(mbaname)) then
            allocate (mbaname(0))
        end if

        ! Check if the mass balance area name already exists in the list of mass balance areas
        imba = find_name(mbaname, name)

        ! If the mass balance area name does not exist, add it to the list of mass balance areas
        if (imba == 0) then
            nomba = nomba + 1
            imba = nomba
            call realloc(mbaname, nomba, keepExisting=.true., fill=name)
        end if

        ! Read the location file and select the internal nodes within the polygon defined in the location file
        allocate (selected_nodes(ndxi))

        call selectelset_internal_nodes(xz, yz, kcs, ndxi, selected_nodes, nselected, &
                                        LOCTP_POLYGON_XY, numcoord=num_coordinates, xpin=x_coordinates, ypin=y_coordinates)

        do k = 1, nselected
            node = selected_nodes(k)

            mbadef(node) = imba
            call getkbotktop(node, kb, kt)
            mbadef(kb:kb + kmxn(node) - 1) = imba
        end do

        deallocate (selected_nodes)

    end subroutine read_mass_balance_area_block

end module m_mass_balance_area
