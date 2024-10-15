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
module inputs_block_4
    use m_waq_precision
    use m_string_utils, only : string_equals, index_in_array
    use matrix_utils, only : scale_array, compute_matrix_size
    use exchange_pointers, only : read_exchange_pointers_irregular_grid, create_boundary_pointers
    use simulation_input_options, only : process_simulation_input_options, validate_simulation_time_steps, &
            read_constant_data, read_constants_time_variables
    use monitoring_areas, only : create_write_monitoring_area_array

    implicit none

    private
    public :: read_block_4_flow_dims_pointers

contains

    subroutine read_block_4_flow_dims_pointers (file_unit_list, file_name_list, filtype, nrftot, nrharm, &
            ilflag, is_date_format, iwidth, intsrt, is_yyddhh_format, &
            output_verbose_level, nsegdmp, isegdmp, nexcraai, &
            iexcraai, ioptraai, gridps, status, &
            has_hydfile, nexch)

        !! Reads flow dimensions and pointers and all transport information
        !>
        !>            This routine reads:
        !>               - the 3 dimensions of exchange surfaces in 3 directions
        !>               - the 4th dimension of exchanges in the water bed
        !>               - the number of additional diffusion arrays
        !>               - per substance the array entry that applies (0 is none)
        !>               - the number of additional velocity arrays
        !>               - per substance the array entry that applies
        !>               - the exchange surfaces table (from-to table)
        !>               - information on the time series of dispersions
        !>               - information on the time series of areas
        !>               - information on the time series of flows
        !>               - information on the time series of additional velocities
        !>               - information on the time series of from- and to lengthes

        !   Logical units      : file_unit_list(27) = unit stripped DELWAQ input file
        !                        file_unit_list(29) = unit formatted output file
        !                        file_unit_list( 2) = unit intermediate file (system)
        !                        file_unit_list( 3) = unit intermediate file (harmonics)
        !                        file_unit_list( 4) = unit intermediate file (pointers)
        !                        file_unit_list( 7) = unit intermediate file (volumes)
        !                        file_unit_list( 8) = unit intermediate file ('to-from')
        !                        file_unit_list( 9) = unit intermediate file (dispersions
        !                        file_unit_list(10) = unit intermediate file (areas)
        !                        file_unit_list(11) = unit intermediate file (flows)
        !                        file_unit_list(12) = unit intermediate file (velocities)
        !                        file_unit_list(13) = unit intermediate file (lengths)

        use error_handling, only : check_error
        use m_logger_helper, only : stop_with_error
        use m_open_waq_files
        use m_grid_utils_external        !   for the storage of contraction grids
        use rd_token     !   for the reading of tokens
        use exchange_pointers, only : read_exchange_pointers_regular_grid
        use partmem
        use timers       !   performance timers
        use m_waq_memory_dimensions          ! System characteristics
        use m_error_status

        integer(kind = int_wp), intent(inout) :: file_unit_list    (*)         !< array with unit numbers
        character(*), intent(inout) :: file_name_list  (*)        !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype(*)         !< type of binary file
        integer(kind = int_wp), intent(inout) :: nrftot (*)         !< number of function items
        integer(kind = int_wp), intent(inout) :: nrharm (*)         !< number of harmonic items
        integer(kind = int_wp), intent(out) :: ilflag             !< length flag
        logical, intent(in) :: is_date_format            !< 'date'-format 1st timescale
        integer(kind = int_wp), intent(in) :: iwidth             !< width of the output file
        integer(kind = int_wp), intent(in) :: intsrt             !< integration option
        logical, intent(in) :: is_yyddhh_format            !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< flag for more or less output
        integer(kind = int_wp), intent(in) :: nsegdmp (*)        !< number of volumes in this monitoring area
        integer(kind = int_wp), intent(inout) :: isegdmp (*)        !< computational volume numbers
        integer(kind = int_wp), intent(in) :: nexcraai(*)        !< number of exchanges in this monitoring transect
        integer(kind = int_wp), intent(in) :: iexcraai(*)        !< exchange area numbers of the transect
        integer(kind = int_wp), intent(in) :: ioptraai(*)        !< option for the transects
        type(GridPointerColl) :: GridPs            !< Collection of grid pointers
        logical, intent(in) :: has_hydfile       !< if true, much information comes from the hyd-file
        integer(kind = int_wp), dimension(*), intent(in) :: nexch   !< nmber of exchanges from the hyd-file

        type(error_status) :: status

        !     COMMON BLOCK  :

        !     integer :: num_exchanges_u_dir     !  number of exch. 1st direction
        !     integer :: num_exchanges_v_dir     !  number of exch. 2nd direction
        !     integer :: num_exchanges_z_dir     !  number of exch. 3rd direction
        !     integer :: ndmpar   !  Number of dump area's for balance output
        !     integer :: ndmpq    !  Number of exchanges of interest for balance
        !     integer :: num_monitoring_cells    !  Number of segments of interest for balance
        !     integer :: ntdmpq   !  Number of times exchanges contribute to balance
        !     integer :: ntdmps   !  Number of times segments contribute to balance

        !     Locals

        integer(kind = int_wp) :: nosss      !  number of volumes inclusive of bed volumes
        integer(kind = int_wp) :: volume     !  if true, computed volumes
        logical         disper    !  if true, dispersion
        real(kind = real_wp) :: adummy     !  real zero
        integer(kind = int_wp) :: idummy     !  integer zero
        character(255)  cdummy    !  dummy character space
        integer(kind = int_wp) :: idum       !  multi purpose dummy variable
        integer(kind = int_wp) :: ifact      !  factor between clocks ( 1 in the case of transport )
        integer(kind = int_wp) :: ierr2      !  local error count
        integer(kind = int_wp) :: nosegl     !  number of volumes per layer
        integer(kind = int_wp) :: noq12      !  num_exchanges_u_dir + num_exchanges_v_dir (number of horizontal exchanges)
        integer(kind = int_wp) :: noq34      !  num_exchanges_z_dir + num_exchanges_bottom_dir (number of vertical exchanges)
        integer(kind = int_wp) :: noqt       !  total number of exchanges (water and bed)
        integer(kind = int_wp) :: i, j, k    !  loop counters
        integer(kind = int_wp) :: ifound     !  help variable to find things
        integer(kind = int_wp) :: integration_id       !  option for type of input (2 = tabular input)
        integer(kind = int_wp) :: iopt1      !  option for file type (1 = this file etc.)
        integer(kind = int_wp) :: itype      !  the type of token that was presented
        logical         regular   !  if .true. indicates presence of a regular grid

        !     Local arrays

        character(20), allocatable :: dispnam(:)       !  dispersion names
        integer(kind = int_wp), pointer :: ipnt   (:, :)      !  room for the 'from-to' pointer table
        real(kind = real_wp), allocatable :: rwork  (:, :)      !  room for tabular input option
        integer(kind = int_wp) :: idisp  (num_substances_transported)    !  dispersion number per substance
        integer(kind = int_wp) :: ivelo  (num_substances_transported)    !  velocity number per substance
        real(kind = real_wp) :: factor (5)      !  scale factor tabular input
        integer(kind = int_wp), pointer :: cellpnt(:)        !  backpointer num_cells to mnmaxk
        integer(kind = int_wp), pointer :: flowpnt(:)        !  backpointer num_exchanges to 3*mnmaxk-mnmax
        real(kind = real_wp) :: length (3, 1)      !  lengthes per direction
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_block_4_flow_dims_pointers", ithndl)

        nosss = num_cells + num_cells_bottom
        iposr = 0
        volume = 0
        adummy = 0.0
        idummy = 0
        ifact = 1
        ierr2 = 0

        !        Read exchange dimensions of the system (num_exchanges_u_dir,num_exchanges_v_dir,num_exchanges_z_dir)

        if (has_hydfile) then
            num_exchanges_u_dir = nexch(1)
            num_exchanges_v_dir = nexch(2)
            num_exchanges_z_dir = nexch(3)
        else
            regular = .false.
            if (gettoken(cdummy, num_exchanges_u_dir, itype, ierr2) > 0) goto 100
            if (itype == 1) then
                if (cdummy(1:12) == 'REGULAR_GRID') then
                    regular = .true.
                    if (gettoken(num_exchanges_u_dir, ierr2) > 0) goto 100
                else
                    ierr2 = 1
                    goto 100
                endif
            endif
            if (gettoken(num_exchanges_v_dir, ierr2) > 0) goto 100
            if (gettoken(num_exchanges_z_dir, ierr2) > 0) goto 100
        endif

        num_exchanges = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

        !        These 2 options use a regular grid with full matrix.

        if (regular) then
            num_rows = num_exchanges_u_dir
            num_columns = num_exchanges_v_dir
            num_layers_grid = num_exchanges_z_dir
            write(file_unit, 2000) num_rows, num_columns, num_layers_grid
            GridPs%Pointers(1)%num_layers = num_layers_grid
            num_layers = num_layers_grid
        else
            num_rows = 0
            num_columns = 0
            num_layers_grid = 0
            write (file_unit, 2010) num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges

            !        detect number of layers (only structured sigma or z model, otherwise num_layers_grid=0)

            if (num_layers <= 1) then
                if (num_exchanges_z_dir > 0) then
                    num_layers_grid = 0
                    nosegl = num_cells - num_exchanges_z_dir
                    if (nosegl > 0) then
                        num_layers = num_cells / nosegl
                        if (num_layers * nosegl == num_cells) then
                            num_layers_grid = num_layers
                        else
                            num_layers = 1
                        endif
                    endif
                else
                    num_layers_grid = 1
                    num_layers = 1
                    nosegl = num_cells
                endif
            else
                num_layers_grid = num_layers
            endif
        endif
        if (.not. alone) then
            if (num_exchanges /= noqp) then
                write (file_unit, 2020) noqp
                call status%increase_error_count()
            endif
        endif
        num_exchanges_bottom_dir = 0
        if (num_cells_bottom /= 0) then
            num_exchanges_bottom_dir = num_cells_bottom + num_cells / num_layers
            num_exchanges_bottom_dir = num_exchanges_bottom_dir * 2
            write (file_unit, 2040) num_exchanges_bottom_dir
        endif

        !        Read number of additional dispersion arrays num_dispersion_arrays

        if (gettoken(num_dispersion_arrays, ierr2) > 0) goto 100
        write (file_unit, 2050) num_dispersion_arrays
        idisp = 0
        if (num_dispersion_arrays > 0) then
            allocate (dispnam(num_dispersion_arrays))   !    'Dispersion nnnn'
            do i = 1, num_dispersion_arrays
                if (gettoken(dispnam(i), ierr2) > 0) goto 100
                if (dispnam(i) == ' ') write (dispnam(i), 2060) i
                ifound = index_in_array(dispnam(i), dispnam(1:i - 1))
                if (ifound > 0) then
                    write(file_unit, 2070) dispnam(i)
                    call status%increase_error_count()
                endif
            enddo
            if (output_verbose_level >= 2) then
                write (file_unit, 2080) (i, dispnam(i), i = 1, num_dispersion_arrays)
            else
                write (file_unit, 2090)
            endif
            write (file_unit, *)
            write (file_unit_list(2)) (dispnam(i), i = 1, num_dispersion_arrays)
            deallocate (dispnam)

            do i = 1, num_substances_transported     !   read which dispersion array applies (0=none) for each subst.
                if (gettoken(idisp(i), ierr2) > 0) goto 100
                if (idisp(i) > num_dispersion_arrays) then
                    write (file_unit, 2100) idisp(i), num_dispersion_arrays
                    call status%increase_error_count()
                endif
            enddo
        endif

        !        Read number of additional velocity arrays num_velocity_arrays in exactly
        !                   the same way (could probably be better 1 code)

        if (gettoken(num_velocity_arrays, ierr2) > 0) goto 100
        write (file_unit, 2110) num_velocity_arrays
        ivelo = 0
        if (num_velocity_arrays > 0) then
            allocate (dispnam(num_velocity_arrays))   !
            do i = 1, num_velocity_arrays
                if (gettoken(dispnam(i), ierr2) > 0) goto 100
                if (dispnam(i) == ' ') write (dispnam(i), 2120) i
                ifound = index_in_array(dispnam(i), dispnam(1:i - 1))
                if (ifound > 0) then
                    write(file_unit, 2130) dispnam(i)
                    call status%increase_error_count()
                endif
            enddo
            if (output_verbose_level >= 2) then
                write (file_unit, 2080) (i, dispnam(i), i = 1, num_velocity_arrays)
            else
                write (file_unit, 2090)
            endif
            write (file_unit, *)
            write (file_unit_list(2)) (dispnam(i), i = 1, num_velocity_arrays)
            deallocate (dispnam)

            do i = 1, num_substances_transported     !   read which dispersion array applies (0=none) for each subst.
                if (gettoken(ivelo(i), ierr2) > 0) goto 100
                if (ivelo(i) > num_velocity_arrays) then
                    write (file_unit, 2100) ivelo(i), num_velocity_arrays
                    call status%increase_error_count()
                endif
            enddo
        endif
        !           write a report if sensible and write binary file
        if ((num_dispersion_arrays > 0 .or. num_velocity_arrays > 0) .and. output_verbose_level >= 2) &
                write (file_unit, 2140) (i, idisp(i), ivelo(i), i = 1, num_substances_transported)
        write (file_unit_list(2)) idisp
        write (file_unit_list(2)) ivelo
        !           a very obvious (and rude) check on correctness
        if (num_exchanges_u_dir < 0 .or. num_exchanges_v_dir   < 0 .or. num_exchanges_z_dir   < 0 .or. &
                num_exchanges  == 0 .or. num_dispersion_arrays < 0 .or. num_velocity_arrays < 0) then
            write (file_unit, 2150)
            call status%increase_error_count()
        endif

        !        Read option variable for input mode

        if (has_hydfile) then
            iopt1 = 0
        else
            if (gettoken(integration_id, ierr2) > 0) goto 100
            write (file_unit, 2170) integration_id
            noqt = num_exchanges
            if (integration_id == 2) goto 10

            !***************  first type of input ******************

            !        Read exchange pointers

            if (gettoken(iopt1, ierr2) > 0) goto 100
            write (file_unit, 2180)  iopt1

            if (regular) then  !        Regular grid
                call process_simulation_input_options (iopt1, file_unit_list, 8, file_name_list, filtype, &
                        is_date_format, is_yyddhh_format, 0, ierr2, status, &
                        .false.)
                if (ierr2  > 0) goto 100
                noqt = num_exchanges_bottom_dir
                call read_exchange_pointers_regular_grid (file_unit_list, file_name_list, num_cells, num_rows, num_columns, &
                        num_layers_grid, num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                        noqt, num_boundary_conditions, ipnt, intsrt, iopt1, &
                        num_codiagonals, output_verbose_level, iwidth, GridPs, cellpnt, &
                        flowpnt, status)
            endif
        endif
        if (has_hydfile .or. .not. (regular)) then  ! Irregular grid/hyd-file
            call process_simulation_input_options (iopt1, file_unit_list, 44, file_name_list, filtype, &
                    is_date_format, is_yyddhh_format, 0, ierr2, status, &
                    has_hydfile)
            if (ierr2  > 0) goto 100
            noqt = num_exchanges + num_exchanges_bottom_dir
            allocate (ipnt(4, noqt), stat = ierr2)
            if (ierr2 /= 0) then
                write (file_unit, 2160) ierr2, 4 * noqt
                goto 100
            endif
            ipnt = 0
            call read_exchange_pointers_irregular_grid (file_unit_list, file_name_list, num_cells, num_exchanges, num_exchanges_u_dir, &
                    num_exchanges_v_dir, num_exchanges_z_dir, noqt, num_boundary_conditions, ipnt, &
                    intsrt, iopt1, num_codiagonals, filtype(44), output_verbose_level, &
                    GridPs, status)
        endif
        noq12 = num_exchanges_u_dir + num_exchanges_v_dir
        noq34 = num_exchanges_z_dir + num_exchanges_bottom_dir

        !        set dump area structure

        call create_write_monitoring_area_array (file_unit_list, ndmpar, ntdmps, noqt, nosss, &
                num_boundary_conditions, ipnt, ntdmpq, ndmpq, num_monitoring_cells, &
                num_transects, num_transect_exchanges, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, status)

        !        calculate size of the fast solvers matrix

        if (intsrt == 15 .or. intsrt == 16 .or. &
                intsrt == 17 .or. intsrt == 18 .or. &
                intsrt == 21 .or. intsrt == 22) then
            call compute_matrix_size (num_exchanges_u_dir, num_exchanges_v_dir, noq34, nosss, ipnt, &
                    fast_solver_arr_size)
            write (file_unit, 2190) fast_solver_arr_size
        endif
        if (associated(ipnt)) then
            deallocate (ipnt)
        endif

        !        Read dispersions

        write (file_unit, 2200)
        disper = .true.
        ierr2 = 0
        call read_constants_time_variables   (file_unit_list, 9, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                num_dispersion_arrays, 1, nrftot(3), nrharm(3), ifact, &
                is_date_format, disper, volume, iwidth, file_name_list, &
                filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                status, .false.)
        call status%increase_error_count_with(ierr2)
        disper = .false.

        !        Read areas

        write (file_unit, 2210)
        ierr2 = 0
        call read_constants_time_variables   (file_unit_list, 10, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                1, 1, nrftot(4), nrharm(4), ifact, &
                is_date_format, disper, volume, iwidth, file_name_list, &
                filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                status, has_hydfile)
        call status%increase_error_count_with(ierr2)

        !        Read flows

        write (file_unit, 2220)
        ierr2 = 0
        call read_constants_time_variables   (file_unit_list, 11, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                1, 1, nrftot(5), nrharm(5), ifact, &
                is_date_format, disper, volume, iwidth, file_name_list, &
                filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                status, has_hydfile)
        call status%increase_error_count_with(ierr2)
        if (.not. alone) then
            if (file_name_list(11) /= fnamep(7)) then
                write (file_unit, 2225) fnamep(7)
                call status%increase_error_count()
            endif
        endif

        !        Read velos

        if (num_velocity_arrays > 0) then
            write (file_unit, 2230)
            ierr2 = 0
            call read_constants_time_variables   (file_unit_list, 12, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                    num_velocity_arrays, 1, nrftot(6), nrharm(6), ifact, &
                    is_date_format, disper, volume, iwidth, file_name_list, &
                    filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                    status, .false.)
            call status%increase_error_count_with(ierr2)
        endif

        !        Read length "to" and "from" surfaces

        write (file_unit, 2240)

        if (has_hydfile) then
            ilflag = 1
        else
            if (gettoken(ilflag, ierr2) > 0) goto 100
            write (file_unit, 2250) ilflag
        endif
        select case (ilflag)
        case (0)
            write (file_unit, 2260)
            idum = 4
            write (file_unit_list(2)) idummy
            call read_constant_data (1, length, 1, 3, 1, &
                    iwidth, file_unit_list(2), idum, ierr2)

        case (1)
            write (file_unit, 2270)
            write (file_unit_list(2)) idummy, (adummy, k = 1, 3)
            ierr2 = 0
            call read_constants_time_variables   (file_unit_list, 13, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                    2, 1, nrftot(7), nrharm(7), ifact, &
                    is_date_format, disper, volume, iwidth, file_name_list, &
                    filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                    status, has_hydfile)

        case default
            write (file_unit, 2280)
            call status%increase_error_count()

        end select
        goto 100

        !***************  second type of input ******************

        10 continue
        allocate (ipnt(4, noqt), stat = ierr2)
        if (ierr2 /= 0) then
            write (file_unit, 2160) ierr2, 4 * noqt
            goto 100
        endif
        ipnt = 0

        ilflag = 1
        if (num_dispersion_arrays < 1) then
            write (file_unit, 2290) num_dispersion_arrays
            ierr2 = 1
            goto 100
        endif

        allocate (rwork(5, num_exchanges), stat = ierr2)
        if (ierr2 /= 0) then
            write (file_unit, 2310) ierr2, 5 * num_exchanges
            goto 100
        endif

        if (gettoken(iopt1, ierr2) > 0) goto 100
        write (file_unit, 2320) iopt1
        if (iopt1 == 0) then
            write (file_unit, 2280)
            ierr2 = 1
            goto 100
        endif
        idum = 0

        call process_simulation_input_options (iopt1, file_unit_list, idum, file_name_list, filtype, &
                is_date_format, is_yyddhh_format, 0, ierr2, status, &
                .false.)
        if (ierr2  > 0) goto 100

        do k = 1, 4
            if (gettoken(factor(k), ierr2) > 0) goto 100
        enddo

        do j = 1, num_exchanges
            do i = 1, 4
                if (gettoken(ipnt (i, j), ierr2) > 0) goto 100
            enddo
            do i = 1, 5
                if (gettoken(rwork(i, j), ierr2) > 0) goto 100
            enddo
        enddo

        write (file_unit, 2330) (factor(i), i = 1, 4)
        write (file_unit, 2340)
        write (file_unit, 2350) ((ipnt (i, j), i = 1, 4), &
                (rwork(i, j), i = 1, 5), j = 1, num_exchanges)

        !       calculate number of boundaries and bandwith of matrix

        call create_boundary_pointers  (file_unit_list, num_cells, num_exchanges, noqt, intsrt, &
                output_verbose_level, GridPs, num_boundary_conditions, num_codiagonals, ipnt, &
                status)

        !        set dump area structure

        call create_write_monitoring_area_array (file_unit_list, ndmpar, ntdmps, num_exchanges, num_cells, &
                num_boundary_conditions, ipnt, ntdmpq, ndmpq, num_monitoring_cells, &
                num_transects, num_transect_exchanges, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, status)

        !        calculate size of the fast solvers matrix

        if (intsrt == 15 .or. intsrt == 16 .or. &
                intsrt == 17 .or. intsrt == 18 .or. &
                intsrt == 21 .or. intsrt == 22) then
            call compute_matrix_size (num_exchanges_u_dir, num_exchanges_v_dir, noq34, nosss, ipnt, &
                    fast_solver_arr_size)
            write (file_unit, 2190) fast_solver_arr_size
        endif

        factor(5) = factor(4)
        call scale_array (rwork, factor)

        write (file_unit_list(2)) idummy, (adummy, k = 1, 3)
        write (file_unit_list(2)) idummy, (adummy, k = 1, 3)

        call open_waq_files  (file_unit_list(8), file_name_list(8), 8, 1, ierr2)
        if (ierr2 /= 0) goto 100
        if (num_exchanges_u_dir > 0) write(file_unit_list(8))(ipnt(:, i), i = 1, num_exchanges_u_dir)
        if (num_exchanges_v_dir > 0) write(file_unit_list(8))(ipnt(:, i), i = num_exchanges_u_dir + 1, noq12)
        if (num_exchanges_z_dir > 0) write(file_unit_list(8))(ipnt(:, i), i = noq12 + 1, num_exchanges)
        close (file_unit_list(8))

        call open_waq_files  (file_unit_list(9), file_name_list(9), 9, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(9)) idummy, (rwork(1, i), (adummy, k = 1, num_dispersion_arrays - 1), i = 1, num_exchanges)
        close (file_unit_list(9))

        call open_waq_files  (file_unit_list(10), file_name_list(10), 10, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(10)) idummy, (rwork(2, i), i = 1, num_exchanges)
        close (file_unit_list(10))

        call open_waq_files (file_unit_list(11), file_name_list(11), 11, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(11)) idummy, (rwork(3, i), i = 1, num_exchanges)
        close (file_unit_list(11))

        if (num_velocity_arrays > 0) then
            call open_waq_files  (file_unit_list(12), file_name_list(12), 12, 1, ierr2)
            if (ierr2 /= 0) goto 100
            write (file_unit_list(12)) idummy, ((adummy, k = 1, num_velocity_arrays), i = 1, num_exchanges)
            close (file_unit_list(12))
        endif

        call open_waq_files  (file_unit_list(13), file_name_list(13), 13, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(13)) idummy, (rwork(4, i), rwork(5, i), i = 1, num_exchanges)
        close (file_unit_list(13))

        deallocate(ipnt, rwork)
        ierr2 = 0

        !       here ends the alternative input

        100 continue

        !       check the layers/3D model information:
        !       - is it a 3D model?
        !       - do we have consistency?

        if (num_exchanges_z_dir /= 0) then
            if (num_layers == 1) then
                call status%increase_warning_count()
                write(file_unit, 3000) num_cells, num_exchanges_z_dir, num_cells - num_exchanges_z_dir
                write(file_unit, 3005)
                write(*, '(1x,a)') 'WARNING: inconsistency if 3D model', &
                        '         check .lst file'
            else
                write(file_unit, 3010) num_layers
            endif
        endif

        if (ierr2 > 0) call status%increase_error_count()
        if (ierr2 == 3) call stop_with_error()
        call check_error(cdummy, iwidth, 4, ierr2, status)
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (//, ' Dimensions of the system :', &
                /, ' Number of gridcells 1st direction : ', I7, &
                /, ' Number of gridcells 2nd direction : ', I7, &
                /, ' Number of gridcells 3rd direction : ', I7)
        2010 format (//, ' Dimensions of the system :', &
                /, ' Number of exchanges 1st direction : ', I7, &
                /, ' Number of exchanges 2nd direction : ', I7, &
                /, ' Number of exchanges 3rd direction : ', I7, &
                /, ' Total number of exchanges         : ', I7)
        2020 format (/, ' ERROR. Total number of exchanges in Delpar differs: ', I10)
        2040 format (' Nr of added bottom layer exchanges: ', I7)
        2050 format (/, ' Number of dispersion arrays       : ', I7)
        2060 format ('Dispersion ', I4)
        2070 format (/, ' ERROR. dispersion ID not unique:', A)
        2080 format (/, ' Item nr:       names:', / &
                (I6, 10X, A20))
        2090 format (/, ' Names and assignments for substances are printed', &
                ' for output option 2 and higher !')
        2100 format (/, ' ERROR. Item number : ', I4, ' larger than maximum (', &
                I4, ') !')
        2110 format (/, ' Number of velocity arrays         : ', I7)
        2120 format ('Velocity ', I4)
        2130 format (/, ' ERROR. velocity ID not unique:', A)
        2140 format (' System dispersion velocity', /, &
                '   nr.      nr.      nr.   ', /, &
                (I6, I9, I9))
        2150 format (/, ' ERROR. One or more settings are invalid.')
        2160 format (/, ' ERROR. allocating memory for pointers:', I4, i10)
        2170 format (/, ' Input option                      : ', I7)
        2180 format (/, ' Option selected for pointers      : ', I7)
        2190 format (/, ' Size of the big matrix for fast solvers is: ', I10)
        2200 format (/, ' Dispersion:')
        2210 format (/, ' Area:')
        2220 format (/, ' Flows:')
        2225 format (/, ' ERROR: Flows for Delpar come from different file: ', A)
        2230 format (/, ' Velocities:')
        2240 format (/, ' Lengths:')
        2250 format (' Option for lengths          :', I7)
        2260 format (' Lengths constant per direction all over the area.')
        2270 format (' Lengths variable over the area.')
        2280 format (/, ' ERROR, option not implemented')
        2290 format (/, ' ERROR. Option incompatible with num_dispersion_arrays=', I4)
        2310 format (/, ' ERROR. allocating memory for input table:', I4)
        2320 format (/, ' Option selected for input    :', I7)
        2330 format (/, ' Scale factor for dispersions :', E13.6, &
                /, ' Scale factor for areas       :', E13.6, &
                /, ' Scale factor for flows       :', E13.6, &
                /, ' Scale factor for lengths     :', E13.6)
        2340 format (/, ' from   to fr-1 to+1  dispersion', &
                '     surface        flow from-length   to-length')
        2350 format (4I5, 1P, 5E12.4)
        3000 format (//, ' WARNING: The model seems to be a 3D model, but:' &
                , /, '          Number of segments:           ', i10 &
                , /, '          Number of vertical exchanges: ', i10 &
                , /, '          Difference gives expected number of' &
                , /, '          segments per layer:           ', i10)
        3005 format (/, '          - this is inconsistent' &
                , /, '          Note that the program will now assume one', &
                ' (1) layer!' &
                , //, '          You can specify the number of layers via these' &
                , /, '          keywords in block #3, just after the number of segments:' &
                , //, '          MULTIGRID ZMODEL NOLAY <num_layers> ... END_MULTIGRID')
        3010 format (//, ' Number of layers in the model:', I5)

    end subroutine read_block_4_flow_dims_pointers

end module inputs_block_4
