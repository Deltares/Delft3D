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
module m_grid_utils_external
    !!  module contains everything for specification of multiple grids
    !!
    !!  contains the fiollowing derived types:
    !!          t_grid            ! a set of information with respect to one grid pointer
    !!          GridPointerColl        ! a collection of these grid pointers
    !!
    !!  contains the following functions:
    !!      GridPointerCollFind    ! to search a Grid in the GridPointerColl ; returns the index or zero if not found
    !!      GridPointerCollAdd     ! to add a t_grid to the collection ; returns the current size

    use m_srstop

    integer, parameter :: NAME_SIZE = 20                ! size of descriptive names
    integer, parameter :: MAX_NUM = 5                ! allocated per bunch

    integer, parameter :: BaseGrid = 1                ! implementation of an enumeration
    integer, parameter :: ProcessGrid = 2                !               type in Fortran
    integer, parameter :: BottomGrid = 3
    integer, parameter :: AggregationFile = 4
    integer, parameter :: NolayGrid = 5
    integer, parameter :: NrGridTypes = 5
    character*20        GridTypes(NrGridTypes)
    DATA GridTypes / 'BASEGRID', 'PROCESSGRID', 'BOTTOMGRID', 'AGGREGATIONFILE', 'NOLAY' /

    ! this is the grid pointer itself
    type t_grid
        character(len = NAME_SIZE) :: name      ! name of the grid
        integer :: noseg              ! number of segments
        integer :: noseg_lay          ! number of segments per layer / 2D
        integer :: iref               ! grid reference nr
        character(len = NAME_SIZE) :: name_ref           ! name of the reference grid
        integer :: itype              ! type of grid
        integer, pointer :: iarray(:)          ! the pointer to reference the reference grid
        integer, pointer :: finalpointer(:)    ! the pointer to the final grid
        logical :: space_var_nolay    ! switch for space varying nr of layers
        integer :: nolay              ! nr of expandable layers
        integer, pointer :: nolay_var(:)       ! space varying nr of layers if any
    contains
        procedure :: write => write_grid
        procedure :: read => read_grid
    end type t_grid

    ! this is the collection of the grid pointers
    type GridPointerColl
        type(t_grid), pointer :: Pointers(:)        ! array with gridpointers
        integer :: maxsize            ! maximum size of the current array
        integer :: current_size            ! filled up to this size
        integer :: base_grid          ! index base grid in collection
        integer :: bottom_grid        ! index bottom grid in collection
    contains
        procedure :: find_column => GridPointerCollFind
        procedure :: add => GridPointerCollAdd
    end type GridPointerColl

    private
    public :: GridPointerColl, t_grid
    public :: BASEGRID, BOTTOMGRID, PROCESSGRID

contains

    function GridPointerCollFind(self, name) result (iret)
        ! function to find a grid name in a collection of GridPointers
        class(GridPointerColl) :: self
        character(LEN = *) :: name
        integer :: iret

        iret = 0
        do i = 1, self%current_size         ! search by name
            if (self%Pointers(i)%name == name) then
                iret = i
                return
            endif
        end do
    end function GridPointerCollFind

    function GridPointerCollAdd(self, aGridPointer) result (current_size)
        ! function to add to a collection of fileproperties
        class(GridPointerColl) :: self      ! the collection of GridPointers
        type(t_grid) :: aGridPointer          ! the t_grid to add to the collection
        integer :: current_size               ! return value the new current collection size
        ! and the index of the added t_grid

        type(t_grid), pointer :: aGridPointerPnts(:)   ! should be a pointer for the resize operation

        if (self%current_size == self%maxsize) then
            allocate (aGridPointerPnts (self%maxsize + MAX_NUM))
            do i = 1, self%maxsize
                aGridPointerPnts(i) = self%Pointers(i)        ! copies the pointers
            enddo
            if (self%maxsize /= 0) deallocate (self%Pointers)
            self%Pointers => aGridPointerPnts                   ! attaches this new array of pointers
            self%maxsize = self%maxsize + MAX_NUM
        endif

        self%current_size = self%current_size + 1
        self%Pointers(self%current_size) = aGridPointer
        current_size = self%current_size
    end function GridPointerCollAdd

    function write_grid(self, file_unit) result (ierror)

        class(t_grid), intent(in) :: self        ! datastructure to be written
        integer, intent(in) :: file_unit         ! unit number binary file with data
        integer :: ierror

        ierror = 0

        write(file_unit, err = 100) self%name
        write(file_unit, err = 100) self%noseg
        write(file_unit, err = 100) self%noseg_lay
        write(file_unit, err = 100) self%iref
        write(file_unit, err = 100) self%name_ref
        write(file_unit, err = 100) self%itype
        write(file_unit, err = 100) self%finalpointer
        write(file_unit, err = 100) self%space_var_nolay
        write(file_unit, err = 100) self%nolay
        if (self%space_var_nolay) then
            write(file_unit, err = 100) self%nolay_var
        endif

        return
        100    continue
        ierror = 1

    end function write_grid

    function read_grid(self, file_unit, noseg) result (ierror)

        class(t_grid), intent(out) :: self        ! datastructure to be filled
        integer, intent(in) :: file_unit         ! unit number binary file with data
        ! number of segments in base grid (perhaps remove this dependency by adding the length of the pointer to
        ! the structure)
        integer, intent(in) :: noseg
        integer :: ierror

        ierror = 0

        read(file_unit, err = 100) self%name
        read(file_unit, err = 100) self%noseg
        read(file_unit, err = 100) self%noseg_lay
        read(file_unit, err = 100) self%iref
        read(file_unit, err = 100) self%name_ref
        read(file_unit, err = 100) self%itype
        allocate (self%finalpointer(noseg), stat = ierr_alloc)
        if (ierr_alloc /= 0) then
            write(*, *) 'ERROR : allocating array in read_grid'
            call srstop(1)
        endif
        read(file_unit, err = 100) self%finalpointer
        read(file_unit, err = 100) self%space_var_nolay
        read(file_unit, err = 100) self%nolay
        if (self%space_var_nolay) then
            allocate (self%nolay_var(self%noseg_lay), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(*, *) 'ERROR : allocating array in read_grid'
                call srstop(1)
            endif
            read(file_unit, err = 100) self%nolay_var
        endif

        return
        100    continue

        ierror = 1

    end function read_grid

end module m_grid_utils_external
