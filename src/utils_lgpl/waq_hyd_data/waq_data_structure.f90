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

module m_waq_data_structure
    !! module contains everything for model data input and storage
    !!
    !! contains the following derived types:
    !!
    !!   t_data_block!
    !!          poperties with respect to a data item
    !!
    !!   t_data_column!
    !!       a collection of dlwqdata, for instance all wasteloads

    use m_logger_helper, only : stop_with_error, get_log_unit_number
    use waq_file_utils_external, only : create_new_file_unit_number
    use m_open_waq_files
    use m_string_utils

    implicit none

    integer, parameter :: ITEM_NAME_SIZE = 20          ! length all names
    integer, parameter :: NAME_SIZE = 20          ! size of descriptive names
    integer, parameter :: FILE_NAME_SIZE = 256          ! length all names
    integer, parameter :: MAX_NUM = 5          ! allocated per bunch

    integer, parameter :: SUBJECT_UNKNOWN = 0            ! unknown
    integer, parameter :: SUBJECT_IDT = 1            ! timestep related input
    integer, parameter :: SUBJECT_VOLUME = 2            ! volume related input
    integer, parameter :: SUBJECT_DISPERSION = 3            ! dispersion related input
    integer, parameter :: SUBJECT_AREA = 4            ! area related input
    integer, parameter :: SUBJECT_FLOW = 5            ! flow related input
    integer, parameter :: SUBJECT_VELOC = 6            ! velocity related input
    integer, parameter :: SUBJECT_DSPLEN = 7            ! dispersion length related input
    integer, parameter :: SUBJECT_BOUNDARY = 8            ! boundary related input
    integer, parameter :: SUBJECT_WASTE = 9            ! discharge related input
    integer, parameter :: SUBJECT_CONSTANT = 10            ! constant process parameter
    integer, parameter :: SUBJECT_PARAMETER = 11            ! parameter process parameter
    integer, parameter :: SUBJECT_FUNCTION = 12            ! function process parameter
    integer, parameter :: SUBJECT_SEGFUNC = 13            ! segment-function process parameter
    integer, parameter :: SUBJECT_INITIAL = 14            ! initial condition
    integer, parameter :: SUBJECT_FEATURE = 15            ! feature (kenmerk)

    integer, parameter :: ORDER_UNKNOWN = 0            ! data ordering unknown
    integer, parameter :: ORDER_PARAM_LOC = 1            ! data ordered parameters inners loop, locations outer loop
    integer, parameter :: ORDER_LOC_PARAM = 2            ! data ordered locations inners loop, parametrs outer loop

    integer, parameter :: FUNCTYPE_CONSTANT = 0            ! constant in time
    integer, parameter :: FUNCTYPE_BLOCK = 1            ! block funtion
    integer, parameter :: FUNCTYPE_LINEAR = 2            ! linear function
    integer, parameter :: FUNCTYPE_HARMONIC = 3            ! harmonic function
    integer, parameter :: FUNCTYPE_FOURIER = 4            ! fourier function
    integer, parameter :: FUNCTYPE_ALLDT = 5            ! every timestep one record (file option 0)

    integer, parameter :: FILE_NONE = 0            ! data not in file but in memory
    integer, parameter :: FILE_BINARY = 1            ! data in (delwaq) binary file
    integer, parameter :: FILE_ODS = 2            ! data in ODS file
    integer, parameter :: FILE_OMS = 3            ! data in OMS dataspace
    integer, parameter :: FILE_DIO = 4            ! data in DIO coupling
    integer, parameter :: FILE_UNFORMATTED = 5            ! real unformatted file (so not binary)
    integer, parameter :: FILE_BIG_ENDIAN = 10            ! big endian pattern (Telemac)

    type t_data_block
        integer :: subject           ! subject for this data
        integer :: num_parameters    ! number of paramters in this block of data
        integer :: num_locations     ! number of locations
        integer :: num_breakpoints   ! number of breakpoints or harmonics
        integer :: function_type     ! constant, block, linear, harmonics, fourier
        integer :: igrid             ! grid number of input
        logical :: is_external       ! is data in file or online coupling
        integer :: filetype          ! type of external data source
        character(len = FILE_NAME_SIZE) :: filename          ! name of file or dataset in coupling
        integer :: lun               ! unit number external file
        integer :: iorder            ! ordering of the data matrix, param-loc or loc-param
        logical :: is_parameter_named         ! are the parameters named
        character(len = ITEM_NAME_SIZE), pointer :: param_name(:)     ! parameter names
        logical :: are_location_named         ! are the locations named
        character(len = ITEM_NAME_SIZE), pointer :: loc_name(:)       ! location names
        logical :: is_parameter_pointered     ! are the parameters pointered
        integer, pointer :: param_pointers(:) ! index of the parameters in the waq substance/constants/etc arrays
        logical :: are_locations_default      ! data is default for all locations
        logical :: are_locations_pointered    ! are the locations pointered
        integer, pointer :: location_pointers(:)   ! segment number of the locations in the specific grid
        logical :: is_scaled               ! overall scaling applied?
        real :: scale_factor               ! overall scaling factor
        logical :: need_parameters_scaling ! need the parameters scaling
        real, pointer :: parameter_scale_factor(:)   ! scale factors for parameters if any
        logical :: need_location_scaling   ! need the locations scaling
        real, pointer :: location_scale_factor(:) ! scale factors for locations if any
        integer, pointer :: times(:)         ! time at breakpoints
        real, pointer :: phase(:)            ! phase in case of harmonics
        real, pointer :: values(:, :, :)     ! the data itself either(no_loc,no_param,no_brk)

    contains
        procedure :: write => write_data_block
        procedure :: read => read_data_block
        procedure :: evaluate => evaluate_data_block
        procedure :: read_external => read_external_data_block
        procedure :: copy => copy_data_block
    end type t_data_block

    type t_data_column
        type(t_data_block), pointer :: data_block(:)     ! pointer
        integer :: maxsize         ! maximum size of the current array
        integer :: current_size         ! filled up to this size
    contains
        procedure :: add => add_data_column
    end type t_data_column

    type t_fdata
        integer :: ndim1           ! first dimension
        integer :: ndim2           ! second dimension
        integer :: nobrk           ! third dimension, number of times
        integer, pointer :: times(:)        ! times
        real, pointer :: values(:, :, :)   ! the data itself either(no_loc,no_param,no_brk)
    end type t_fdata

    ! this is a collection of items
    type t_waq_item
        character(len = NAME_SIZE), pointer :: name(:)            ! names of item
        integer, pointer :: ipnt(:)            ! index pointer of item (in waq list, etc )
        integer, pointer :: sequence(:)        ! sequence index of item in input
        real, pointer :: constant(:)        ! constant value of item
        integer :: no_item            ! filled up to this size
        integer :: maxsize            ! allocated up to this size
    contains
        procedure :: initialize => initialize_item
        procedure :: cleanup => cleanup_item
        procedure :: resize => resize_item
        procedure :: find => find_item
    end type t_waq_item

    ! this is a collection of data_items
    type t_waq_data_items
        type(t_waq_item), pointer :: for_item(:) ! pointer
        character(len = NAME_SIZE), pointer :: name(:)         ! names of item
        logical, pointer :: used(:)         ! flag
        integer :: maxsize         ! maximum size of the current array
        integer :: current_size         ! filled up to this size
    contains
        procedure :: initialize => initialize_data_items
        procedure :: add => add_data_items
    end type t_waq_data_items

    integer, parameter :: TYPE_CHAR = 1                 ! character
    integer, parameter :: TYPE_INT = 2                 ! integer
    integer, parameter :: TYPE_REAL = 3                 ! real
    integer, parameter :: TYPE_ALL = 0                 ! all types allowed
    integer, parameter :: TYPE_NOCHAR = -1                 ! no character allowed
    integer, parameter :: TYPE_NOINT = -2                 ! no integer allowed
    integer, parameter :: TYPE_NOREAL = -3                 ! no real allowed

    ! the remnant of the old implementation
    type t_input_file
        logical :: is_date_format          ! is_date_format
        logical :: is_ddhhmmss_format      ! is_ddhhmmss_format
        logical :: is_yyddhh_format        ! is_yyddhh_format
        integer :: itfact          ! itfact
        integer :: iblock          ! input block
        integer :: ierr            ! error on inputfile
    endtype t_input_file

    private
    public :: max_num, t_data_block
    public :: TYPE_INT, TYPE_REAL, TYPE_CHAR, TYPE_ALL, TYPE_NOCHAR, TYPE_NOINT, TYPE_NOREAL
    public :: SUBJECT_WASTE, FUNCTYPE_BLOCK, ORDER_PARAM_LOC

    ! for waq_io
    public :: t_waq_item, t_fdata, t_data_column, t_waq_data_items, t_input_file, SUBJECT_UNKNOWN, FUNCTYPE_CONSTANT, &
            FILE_NONE, ORDER_UNKNOWN, ORDER_LOC_PARAM, SUBJECT_PARAMETER, SUBJECT_FUNCTION, SUBJECT_SEGFUNC, &
            SUBJECT_CONSTANT, SUBJECT_INITIAL, FILE_BIG_ENDIAN, FILE_UNFORMATTED, FILE_BINARY, FILE_ODS, &
            FUNCTYPE_FOURIER, FUNCTYPE_HARMONIC, FUNCTYPE_LINEAR

contains

    function add_data_column(self, dlwqdata) result (current_size)

        class(t_data_column) :: self
        type(t_data_block) :: dlwqdata
        integer :: current_size

        type(t_data_block), pointer :: dlwqdatas(:)   ! should be a pointer for the resize operation
        integer :: ierr_alloc
        integer :: i

        if (self%current_size == self%maxsize) then
            allocate (dlwqdatas (self%maxsize + MAX_NUM), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif
            do i = 1, self%maxsize
                dlwqdatas(i) = self%data_block(i)            ! copies the contents
            enddo
            if (self%maxsize /= 0) deallocate (self%data_block)
            self%data_block => dlwqdatas                    ! attaches this new array of pointers
            self%maxsize = self%maxsize + MAX_NUM
        endif
        self%current_size = self%current_size + 1
        self%data_block(self%current_size) = dlwqdata
        current_size = self%current_size
    end function add_data_column

    function write_data_block(self, file_unit) result (ierror)

        class(t_data_block), intent(in) :: self     ! datastructure to be written
        integer, intent(in) :: file_unit         ! unit number binary file with data
        integer :: ierror

        integer :: nopar        ! local copy number of parameters
        integer :: noloc        ! local copy number of locations
        integer :: nobrk        ! local copy number of breakpoints
        integer :: ipar         ! index paramaters
        integer :: iloc         ! index locations
        integer :: ibrk         ! index breakpoints

        ierror = 0
        nopar = self%num_parameters
        noloc = self%num_locations
        nobrk = self%num_breakpoints

        write(file_unit, err = 100) self%subject
        write(file_unit, err = 100) self%num_parameters
        write(file_unit, err = 100) self%num_locations
        write(file_unit, err = 100) self%num_breakpoints
        write(file_unit, err = 100) self%function_type
        write(file_unit, err = 100) self%igrid
        write(file_unit, err = 100) self%is_external
        write(file_unit, err = 100) self%filetype
        write(file_unit, err = 100) self%filename
        write(file_unit, err = 100) self%iorder
        write(file_unit, err = 100) self%is_parameter_named
        if (self%is_parameter_named) then
            write(file_unit, err = 100) (self%param_name(ipar), ipar = 1, nopar)
        endif
        write(file_unit, err = 100) self%are_location_named
        if (self%are_location_named) then
            write(file_unit, err = 100) (self%loc_name(iloc), iloc = 1, noloc)
        endif
        write(file_unit, err = 100) self%is_parameter_pointered
        if (self%is_parameter_pointered) then
            write(file_unit, err = 100) (self%param_pointers(ipar), ipar = 1, nopar)
        endif
        write(file_unit, err = 100) self%are_locations_default
        write(file_unit, err = 100) self%are_locations_pointered
        if (self%are_locations_pointered) then
            write(file_unit, err = 100) (self%location_pointers(iloc), iloc = 1, noloc)
        endif
        write(file_unit, err = 100) self%is_scaled
        write(file_unit, err = 100) self%scale_factor
        write(file_unit, err = 100) self%need_parameters_scaling
        if (self%need_parameters_scaling) then
            write(file_unit, err = 100) (self%parameter_scale_factor(ipar), ipar = 1, nopar)
        endif
        write(file_unit, err = 100) self%need_location_scaling
        if (self%need_location_scaling) then
            write(file_unit, err = 100) (self%location_scale_factor(iloc), iloc = 1, noloc)
        endif
        if (self%function_type /= FUNCTYPE_CONSTANT .and. self%num_breakpoints > 0) then
            write(file_unit, err = 100) (self%times(ibrk), ibrk = 1, nobrk)
        endif
        if (self%function_type == FUNCTYPE_HARMONIC .or. self%function_type == FUNCTYPE_FOURIER) then
            write(file_unit, err = 100) (self%phase(ibrk), ibrk = 1, nobrk)
        endif
        if (self%iorder == ORDER_PARAM_LOC) then
            write(file_unit, err = 100) (((self%values(ipar, iloc, ibrk), ipar = 1, nopar), iloc = 1, noloc), &
                    ibrk = 1, nobrk)
        else
            write(file_unit, err = 100) (((self%values(iloc, ipar, ibrk), iloc = 1, noloc), ipar = 1, nopar), &
                    ibrk = 1, nobrk)
        endif

        return

        100    continue

        ierror = 1

    end function write_data_block

    function read_data_block(self, lunrep, file_unit) result (ierror)

        class(t_data_block), intent(out) :: self     ! datastructure to be filled
        integer, intent(in) :: lunrep       ! unit number report file
        integer, intent(in) :: file_unit         ! unit number binary file with data
        integer :: ierror       ! return value

        integer :: ierr2        ! local error
        integer :: i

        ierror = 0

        read(file_unit, err = 100) self%subject
        read(file_unit, err = 100) self%num_parameters
        read(file_unit, err = 100) self%num_locations
        read(file_unit, err = 100) self%num_breakpoints
        read(file_unit, err = 100) self%function_type
        read(file_unit, err = 100) self%igrid
        read(file_unit, err = 100) self%is_external
        read(file_unit, err = 100) self%filetype
        read(file_unit, err = 100) self%filename
        read(file_unit, err = 100) self%iorder
        read(file_unit, err = 100) self%is_parameter_named
        if (self%is_parameter_named) then
            allocate(self%param_name(self%num_parameters))
            read(file_unit, err = 100) self%param_name
        endif
        read(file_unit, err = 100) self%are_location_named
        if (self%are_location_named) then
            allocate(self%loc_name(self%num_locations))
            read(file_unit, err = 100) (self%loc_name(i), i = 1, self%num_locations)
        endif
        read(file_unit, err = 100) self%is_parameter_pointered
        if (self%is_parameter_pointered) then
            allocate(self%param_pointers(self%num_parameters))
            read(file_unit, err = 100) self%param_pointers
        endif
        read(file_unit, err = 100) self%are_locations_default
        read(file_unit, err = 100) self%are_locations_pointered
        if (self%are_locations_pointered) then
            allocate(self%location_pointers(self%num_locations))
            read(file_unit, err = 100) self%location_pointers
        endif
        read(file_unit, err = 100) self%is_scaled
        read(file_unit, err = 100) self%scale_factor
        read(file_unit, err = 100) self%need_parameters_scaling
        if (self%need_parameters_scaling) then
            allocate(self%parameter_scale_factor(self%num_parameters))
            read(file_unit, err = 100) self%parameter_scale_factor
        endif
        read(file_unit, err = 100) self%need_location_scaling
        if (self%need_location_scaling) then
            allocate(self%location_scale_factor(self%num_locations))
            read(file_unit, err = 100) self%location_scale_factor
        endif
        if (self%function_type /= FUNCTYPE_CONSTANT .and. self%num_breakpoints > 0) then
            allocate(self%times(self%num_breakpoints))
            read(file_unit, err = 100) self%times
        endif
        if (self%function_type == FUNCTYPE_HARMONIC .or. self%function_type == FUNCTYPE_FOURIER) then
            allocate(self%phase(self%num_breakpoints))
            read(file_unit, err = 100) self%phase
        endif
        if (self%iorder == ORDER_PARAM_LOC) then
            allocate(self%values(self%num_parameters, self%num_locations, max(self%num_breakpoints, 1)))
        else
            allocate(self%values(self%num_locations, self%num_parameters, max(self%num_breakpoints, 1)))
        endif
        if (.not. self%is_external) then
            read(file_unit, err = 100) self%values
        else
            if (self%function_type == FUNCTYPE_CONSTANT) then
                ierr2 = self%read_external(lunrep)
                if (ierr2 /= 0) goto 100
                self%is_external = .false.
            endif
        endif

        return
        100    continue

        ierror = 1

    end function read_data_block

    function evaluate_data_block(self, GridPs, itime, ndim1, ndim2, conc) result (ierror)

        use m_grid_utils_external
        use timers

        class(t_data_block), intent(in) :: self             ! data block to be used
        type(GridPointerColl), intent(in) :: GridPs               ! collection off all grid definitions
        integer, intent(in) :: itime                ! system timer
        integer, intent(in) :: ndim1                ! number of substances
        integer, intent(in) :: ndim2                ! number of segments
        real, intent(inout) :: conc(ndim1, ndim2)    ! concentrations to be set
        integer :: ierror               !

        ! local
        real :: aa                   ! value at first breakpoint and final value
        real :: ab                   ! value at second breakpoint
        real :: factor               ! overall scale factor
        real :: loc_factor           ! location scale factor
        real :: param_factor         ! parameter scale factor
        integer :: notot                ! number of parameters in output array
        integer :: noseg                ! number of segments in output array
        integer :: iloc                 ! index locations
        integer :: ipar                 ! index parameters
        integer :: ibrk                 ! index breakpoints
        integer :: iseg, iseg2          ! index segments
        integer :: isys                 ! index substances
        integer :: itim1                ! first time
        integer :: itim2                ! second time
        integer :: itimf                ! time offset
        integer :: idt                  ! step between times
        integer :: it1c                 ! first time copy
        integer :: it2c                 ! second time copy
        integer :: idtc                 ! step copy
        integer :: i                    ! loop counter
        real :: amiss                ! missing value
        real, allocatable :: tmp_conc(:, :)        ! store result on different grid in temp array
        logical, allocatable :: iseg_set(:)          ! indicates if segment is set in temporary array
        integer(4) ithandl /0/
        if (timon) call timstrt ("evaluate_data_block", ithandl)

        ierror = 0
        amiss = -999.0

        if (self%subject == SUBJECT_SEGFUNC) then
            notot = ndim2
            noseg = ndim1
        else
            notot = ndim1
            noseg = ndim2
        endif

        ! Get the right time in the block
        if (self%num_breakpoints > 1) then
            itim1 = self%times(1)
            itim2 = self%times(self%num_breakpoints)
            idt = itim2 - itim1
            if (itime < itim1) then
                ibrk = 1
                itim1 = 0
                itim2 = 1
                idt = itim1 + itim2
            else
                itimf = itime
                if (itime >= itim2) itimf = itime - ((itime - itim2) / idt + 1) * idt

                ! make interpolation constants if iopt = 2
                do i = 2, self%num_breakpoints
                    if (self%times(i) > itimf) then
                        if (self%function_type == FUNCTYPE_LINEAR) then
                            itim1 = itimf - self%times(i - 1)
                            itim2 = self%times(i) - itimf
                        else
                            itim1 = 0
                            itim2 = 1
                        endif
                        idt = itim1 + itim2
                        ibrk = i - 1

                        exit

                    endif
                enddo
            endif
        else
            ibrk = 1
            itim2 = 1
            itim1 = 0
            idt = 1
        endif

        ! to-do find out if isys or iseg can become zero and the data must not be used, in that case exit the
        ! relevant loop
        if (self%is_scaled) then
            factor = self%scale_factor
        else
            factor = 1.0
        endif

        if (self%are_locations_default) then ! default, also in case of igrid .ne. 1 ?
            iloc = 1
            if (self%need_location_scaling) then
                loc_factor = self%location_scale_factor(iloc) * factor
            else
                loc_factor = factor
            endif
            do ipar = 1, self%num_parameters

                if (self%is_parameter_pointered) then
                    isys = self%param_pointers(ipar)
                    if (isys <= 0) cycle
                else
                    isys = ipar
                endif

                if (self%need_parameters_scaling) then
                    param_factor = self%parameter_scale_factor(ipar) * factor
                else
                    param_factor = 1.0
                endif

                if (self%iorder == ORDER_PARAM_LOC) then
                    aa = self%values(ipar, iloc, ibrk)
                else
                    aa = self%values(iloc, ipar, ibrk)
                endif
                if (ibrk < self%num_breakpoints) then ! self%nobrk can be 0 so use .lt. instead of .eq.
                    if (self%iorder == ORDER_PARAM_LOC) then
                        ab = self%values(ipar, iloc, ibrk + 1)
                    else
                        ab = self%values(iloc, ipar, ibrk + 1)
                    endif
                else
                    ab = 0.0
                endif

                ! Dealing with missing values
                it1c = itim1
                it2c = itim2
                idtc = idt
                if (aa == amiss .or. ab == amiss) then
                    call dlwqdataGetValueMiss (self, ipar, iloc, ibrk, amiss, &
                            itimf, it1c, it2c, idtc, aa, &
                            ab)
                endif

                ! Make the wanted value
                aa = (it2c * aa + it1c * ab) / idtc
                aa = aa * param_factor * loc_factor

                if (self%subject == SUBJECT_SEGFUNC) then
                    conc(:, isys) = aa
                else
                    conc(isys, :) = aa
                endif

            enddo

        else
            if (self%igrid > 1) then
                allocate(tmp_conc(self%num_parameters, self%num_locations), iseg_set(self%num_locations))
                iseg_set = .false.
            endif
            do iloc = 1, self%num_locations

                if (self%need_location_scaling) then
                    loc_factor = self%location_scale_factor(iloc) * factor
                else
                    loc_factor = factor
                endif

                if (self%are_locations_pointered) then
                    iseg = self%location_pointers(iloc)
                    if (iseg <= 0) cycle
                else
                    iseg = iloc
                endif

                do ipar = 1, self%num_parameters

                    if (self%is_parameter_pointered) then
                        isys = self%param_pointers(ipar)
                        if (isys <= 0) cycle
                    else
                        isys = ipar
                    endif

                    if (self%need_parameters_scaling) then
                        param_factor = self%parameter_scale_factor(ipar) * factor
                    else
                        param_factor = 1.0
                    endif

                    if (self%iorder == ORDER_PARAM_LOC) then
                        aa = self%values(ipar, iloc, ibrk)
                    else
                        aa = self%values(iloc, ipar, ibrk)
                    endif
                    if (ibrk < self%num_breakpoints) then ! self%nobrk can be 0 so use .lt. instead of .eq.
                        if (self%iorder == ORDER_PARAM_LOC) then
                            ab = self%values(ipar, iloc, ibrk + 1)
                        else
                            ab = self%values(iloc, ipar, ibrk + 1)
                        endif
                    else
                        ab = 0.0
                    endif

                    ! Dealing with missing values
                    it1c = itim1
                    it2c = itim2
                    idtc = idt
                    if (aa == amiss .or. ab == amiss) then
                        call dlwqdataGetValueMiss (self, ipar, iloc, ibrk, amiss, &
                                itimf, it1c, it2c, idtc, aa, &
                                ab)
                    endif

                    ! Make the wanted value
                    aa = (it2c * aa + it1c * ab) / idtc
                    aa = aa * param_factor * loc_factor

                    if (self%igrid <= 1) then
                        if (self%subject == SUBJECT_SEGFUNC) then
                            conc(iseg, isys) = aa
                        else
                            conc(isys, iseg) = aa
                        endif
                    else
                        iseg_set(iseg) = .true.
                        tmp_conc(ipar, iseg) = aa
                    endif

                enddo
            enddo
            if (self%igrid > 1) then
                do iseg2 = 1, noseg
                    iseg = GridPs%Pointers(self%igrid)%finalpointer(iseg2)
                    if (iseg > 0) then
                        if (iseg_set(iseg)) then
                            do ipar = 1, self%num_parameters
                                if (self%is_parameter_pointered) then
                                    isys = self%param_pointers(ipar)
                                    if (isys <= 0) cycle
                                else
                                    isys = ipar
                                endif
                                if (self%subject == SUBJECT_SEGFUNC) then
                                    conc(iseg2, isys) = tmp_conc(ipar, iseg)
                                else
                                    conc(isys, iseg2) = tmp_conc(ipar, iseg)
                                endif
                            enddo
                        endif
                    endif
                enddo
                deallocate(tmp_conc, iseg_set)
            endif
        endif

        if (timon) call timstop (ithandl)
    end function evaluate_data_block

    function initialize_item(self) result (iret)
        !! function to initialise an item structure

        class(t_waq_item), intent(inout) :: self
        integer :: iret

        iret = 0
        self%no_item = 0
        self%maxsize = 0
        self%name => null()
        self%ipnt => null()
        self%sequence => null()
        self%constant => null()

    end function initialize_item

    function cleanup_item(self) result (iret)
        !! function to clean up an item structure

        class(t_waq_item), intent(inout) :: self
        integer :: iret
        logical :: l_alloc

        iret = 0
        self%no_item = 0
        self%maxsize = 0
        l_alloc = associated(self%name)
        if (l_alloc) deallocate(self%name)
        l_alloc = associated(self%ipnt)
        if (l_alloc) deallocate(self%ipnt)
        l_alloc = associated(self%sequence)
        if (l_alloc) deallocate(self%sequence)
        l_alloc = associated(self%constant)
        if (l_alloc) deallocate(self%constant)
        self%name => null()
        self%ipnt => null()
        self%sequence => null()
        self%constant => null()

    end function cleanup_item

    function find_item(self, name) result (iret)
        !! function to find a grid name in a collection of GridPointers

        class(t_waq_item) :: self
        character(LEN = *) :: name
        integer :: iret

        integer :: i
        integer :: iaindx

        iret = 0
        do i = 1, self%no_item
            if (string_equals(name(1:NAME_SIZE), self%name(i))) then
                iret = i
                exit
            endif
        enddo
    end function find_item

    function resize_item(self, newsize) result (iret)
        !! function to resize a dlwq_item (if needed)
        class(t_waq_item), intent(inout) :: self
        integer :: newsize
        integer :: iret

        integer, pointer :: iarray(:)
        real, pointer :: rarray(:)
        character(LEN = NAME_SIZE), pointer :: carray(:)
        integer :: newsize_extra

        iret = 0
        if (newsize > self%maxsize) then
            newsize_extra = newsize + MAX_NUM
            if (self%maxsize > 0) then

                allocate(carray(newsize_extra))
                carray(1:self%maxsize) = self%name
                deallocate(self%name)
                self%name => carray

                allocate(iarray(newsize_extra))
                iarray(1:self%maxsize) = self%ipnt
                deallocate(self%ipnt)
                self%ipnt => iarray

                allocate(iarray(newsize_extra))
                iarray(1:self%maxsize) = self%sequence
                deallocate(self%sequence)
                self%sequence => iarray

                allocate(rarray(newsize_extra))
                rarray(1:self%maxsize) = self%constant
                deallocate(self%constant)
                self%constant => rarray

            else
                allocate(self%name(newsize_extra))
                allocate(self%ipnt(newsize_extra))
                allocate(self%sequence(newsize_extra))
                allocate(self%constant(newsize_extra))
            endif
            self%maxsize = newsize_extra
        endif

    end function resize_item

    function initialize_data_items(self) result (iret)
        !! function to initialise an data_items structure

        class(t_waq_data_items) :: self
        integer :: iret

        self%for_item => null()
        self%name => null()
        self%used => null()
        self%current_size = 0
        self%maxsize = 0
        iret = 0

    end function initialize_data_items

    function add_data_items(self, data_item_name, dlwq_foritem) result (current_size)

        class(t_waq_data_items) :: self
        character(LEN = NAME_SIZE) :: data_item_name         ! name of item to add
        type(t_waq_item) :: dlwq_foritem
        integer :: current_size

        type(t_waq_item), pointer :: dlwq_foritems(:)         ! should be a pointer for the resize operation
        character(LEN = NAME_SIZE), pointer :: data_item_names(:)       ! names of data_items
        logical, pointer :: data_item_used(:)       ! use status of data_items
        integer :: ierr_alloc1
        integer :: ierr_alloc2
        integer :: ierr_alloc3
        integer :: i

        if (self%current_size == self%maxsize) then
            allocate (dlwq_foritems (self%maxsize + MAX_NUM), stat = ierr_alloc1)
            allocate (data_item_names (self%maxsize + MAX_NUM), stat = ierr_alloc2)
            allocate (data_item_used (self%maxsize + MAX_NUM), stat = ierr_alloc3)
            if (ierr_alloc1 /= 0 .or. ierr_alloc2 /= 0 .or. ierr_alloc3 /= 0) then
                write(*, *) 'ERROR : ALLOCATING WORK ARRAY'
                call stop_with_error()
            endif
            do i = 1, self%maxsize
                dlwq_foritems(i) = self%for_item(i)                ! copies the contents
                data_item_names(i) = self%name(i)                      ! copies the contents
                data_item_used(i) = self%used(i)                       ! copies the contents
            enddo
            if (self%maxsize /= 0) deallocate (self%for_item)
            self%for_item => dlwq_foritems                        ! attaches this new array of pointers
            self%name => data_item_names                              ! attaches this new array of pointers
            self%used => data_item_used                              ! attaches this new array of pointers
            self%maxsize = self%maxsize + MAX_NUM
        endif
        self%current_size = self%current_size + 1
        self%for_item(self%current_size) = dlwq_foritem
        self%name(self%current_size) = data_item_name
        self%used(self%current_size) = .false.
        current_size = self%current_size
    end function add_data_items

    function read_external_data_block(self, lunrep) result (ierror)

        use waq_file_utils_external, only: create_new_file_unit_number

        class(t_data_block), intent(inout) :: self             ! data block to be used
        integer, intent(in) :: lunrep               ! unit number report file
        integer :: ierror               ! return value

        integer :: lun                  ! unit number
        integer :: itime                ! time from file
        integer :: nopar                ! local copy number of parameters
        integer :: noloc                ! local copy number of locations
        integer :: nobrk                ! local copy number of breakpoints
        integer :: ipar                 ! index paramaters
        integer :: iloc                 ! index locations
        integer :: ibrk                 ! index breakpoints
        integer                                    ftype                ! the equivalent of the ftype array elsewhere

        nopar = self%num_parameters
        noloc = self%num_locations
        nobrk = max(self%num_breakpoints, 1)

        call create_new_file_unit_number(701, lun)

        ftype = 2
        if (mod(self%filetype, 10) == FILE_UNFORMATTED) ftype = ftype + 10
        if (self%filetype / 10 == 1) ftype = ftype + 20       ! I am in for a better solution (lp)

        call open_waq_files(lun, self%filename, 3, ftype, ierror)

        if (ierror /= 0) then
            write(lunrep, 1000) trim(self%filename)
            write(lunrep, 1010) ierror
        else
            if (self%iorder == ORDER_PARAM_LOC) then
                if (.not. associated(self%values)) allocate(self%values(nopar, noloc, nobrk))
                read(lun, iostat = ierror) itime, (((self%values(ipar, iloc, ibrk), ipar = 1, nopar), &
                        iloc = 1, noloc), ibrk = 1, nobrk)
            else
                if (.not. associated(self%values)) allocate(self%values(noloc, nopar, nobrk))
                read(lun, iostat = ierror) itime, (((self%values(iloc, ipar, ibrk), iloc = 1, noloc), &
                        ipar = 1, nopar), ibrk = 1, nobrk)
            endif
            if (ierror /= 0) then
                write(lunrep, 1020) trim(self%filename)
                write(lunrep, 1010) ierror
            endif
        endif
        close(lun)

        1000 format(' ERROR opening external data file:', A)
        1010 format(' error number:', I10)
        1020 format(' ERROR reading external data file:', A)

    end function read_external_data_block

    subroutine dlwqdataGetValueMiss(dlwqdata, ipar, iloc, ibrk, amiss, &
            itimf, it1c, it2c, idtc, aa, &
            ab)

        !! make function value in case of missing values
        type(t_data_block), intent(in) :: dlwqdata             ! data block to be used
        integer, intent(in) :: ipar                 ! index parameter to get
        integer, intent(in) :: iloc                 ! index location to get
        integer, intent(in) :: ibrk                 ! index current breakpoint
        real, intent(in) :: amiss                ! missing value
        integer, intent(in) :: itimf                ! time offset
        integer, intent(out) :: it1c                 ! first time interpolation factor
        integer, intent(out) :: it2c                 ! second time interpolation factor
        integer, intent(out) :: idtc                 ! dt in interpolation
        real, intent(out) :: aa                   ! first value in interpolation
        real, intent(out) :: ab                   ! second value in interpolation

        integer :: jj, kk

        ! search backward for the first valid point
        do jj = ibrk, 1, -1
            if (dlwqdata%iorder == ORDER_PARAM_LOC) then
                aa = dlwqdata%values(ipar, iloc, jj)
            else
                aa = dlwqdata%values(iloc, ipar, jj)
            endif
            if (aa /= amiss) goto 20
        end do
        jj = 0
        aa = 0.0

        ! search forward for the first valid point

        20 continue
        do kk = ibrk + 1, dlwqdata%num_breakpoints
            if (dlwqdata%iorder == ORDER_PARAM_LOC) then
                ab = dlwqdata%values(ipar, iloc, kk)
            else
                ab = dlwqdata%values(iloc, ipar, kk)
            endif
            if (ab /= amiss) goto 40
        end do
        kk = 0
        ab = 0.0

        40 continue
        it1c = 0
        it2c = 0

        ! there was a backward valid point
        if (jj /= 0) then
            if (dlwqdata%function_type == FUNCTYPE_BLOCK) it2c = 1
            if (dlwqdata%function_type == FUNCTYPE_LINEAR) then
                if (kk /= 0) then
                    it1c = itimf - dlwqdata%times(jj)
                else
                    it2c = 1
                endif
            endif
        endif

        ! there was a forward valid point
        if (kk /= 0) then
            if (dlwqdata%function_type == FUNCTYPE_BLOCK .and. jj == 0) it1c = 1
            if (dlwqdata%function_type == FUNCTYPE_LINEAR) then
                if (jj /= 0) then
                    it2c = dlwqdata%times(kk) - itimf
                else
                    it1c = 1
                endif
            endif
        endif
        idtc = it1c + it2c
        if (idtc == 0) idtc = 1
    end subroutine dlwqdataGetValueMiss

    function copy_data_block(self, data2) result (ierror)

        class(t_data_block), intent(in) :: self        ! data to be copied
        type(t_data_block), intent(out) :: data2        ! copy of the data
        integer :: ierror       !

        ! local decalaration
        integer :: nopar        ! local copy number of parameters
        integer :: noloc        ! local copy number of locations
        integer :: nobrk        ! local copy number of breakpoints
        integer :: ipar         ! index paramaters
        integer :: iloc         ! index locations
        integer :: ibrk         ! index breakpoints
        integer :: lunrep       ! unit number report file
        integer :: ierr_alloc

        call get_log_unit_number(lunrep)

        ierror = 0
        nopar = self%num_parameters
        noloc = self%num_locations
        nobrk = self%num_breakpoints

        data2%subject = self%subject
        data2%num_parameters = self%num_parameters
        data2%num_locations = self%num_locations
        data2%num_breakpoints = self%num_breakpoints
        data2%function_type = self%function_type
        data2%igrid = self%igrid
        data2%is_external = self%is_external
        data2%filetype = self%filetype
        data2%filename = self%filename
        data2%iorder = self%iorder
        data2%is_parameter_named = self%is_parameter_named
        if (data2%is_parameter_named) then
            allocate(data2%param_name(nopar), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%param_name = self%param_name
        endif
        data2%are_location_named = self%are_location_named
        if (data2%are_location_named) then
            allocate(data2%loc_name(noloc), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%loc_name = self%loc_name
        endif
        data2%is_parameter_pointered = self%is_parameter_pointered
        if (data2%is_parameter_pointered) then
            allocate(data2%param_pointers(nopar), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%param_pointers = self%param_pointers
        endif
        data2%are_locations_default = self%are_locations_default
        data2%are_locations_pointered = self%are_locations_pointered
        if (data2%are_locations_pointered) then
            allocate(data2%location_pointers(noloc), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%location_pointers = self%location_pointers
        endif
        data2%is_scaled = self%is_scaled
        data2%scale_factor = self%scale_factor
        data2%need_parameters_scaling = self%need_parameters_scaling
        if (data2%need_parameters_scaling) then
            allocate(data2%parameter_scale_factor(nopar), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%parameter_scale_factor = self%parameter_scale_factor
        endif
        data2%need_location_scaling = self%need_location_scaling
        if (data2%need_location_scaling) then
            allocate(data2%location_scale_factor(noloc), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%location_scale_factor = self%location_scale_factor
        endif
        if (data2%function_type /= FUNCTYPE_CONSTANT .and. data2%num_breakpoints > 0) then
            allocate(data2%times(nobrk), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%times = self%times
        endif
        if (data2%function_type == FUNCTYPE_HARMONIC .or. data2%function_type == FUNCTYPE_FOURIER) then
            allocate(data2%phase(nobrk), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            data2%phase = self%phase
        endif
        if (data2%iorder == ORDER_PARAM_LOC) then
            allocate(data2%values(nopar, noloc, nobrk), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            do ibrk = 1, nobrk
                do iloc = 1, noloc
                    do ipar = 1, nopar
                        data2%values(ipar, iloc, ibrk) = self%values(ipar, iloc, ibrk)
                    enddo
                enddo
            enddo
        else
            allocate(data2%values(noloc, nopar, nobrk), stat = ierr_alloc)
            if (ierr_alloc /= 0) then ; write(lunrep, *) ' error allocating memory' ; ierror = 1 ; return ;
            endif
            do ibrk = 1, nobrk
                do ipar = 1, nopar
                    do iloc = 1, noloc
                        data2%values(iloc, ipar, ibrk) = self%values(iloc, ipar, ibrk)
                    enddo
                enddo
            enddo
        endif

    end function copy_data_block

end module m_waq_data_structure
