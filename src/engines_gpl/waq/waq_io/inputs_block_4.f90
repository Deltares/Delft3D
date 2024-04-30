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
        use m_srstop
        use m_open_waq_files
        use m_grid_utils_external        !   for the storage of contraction grids
        use rd_token     !   for the reading of tokens
        use exchange_pointers, only : read_exchange_pointers_regular_grid
        use partmem
        use timers       !   performance timers
        use m_sysn          ! System characteristics
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

        !     integer :: noseg    !  number of elements
        !     integer :: nolay    !  number of layers in the water
        !     integer :: nseg2    !  number of bottom elements
        !     integer :: nosys    !  number of active systems
        !     integer :: nodisp   !  number of dispersion arrays
        !     integer :: novelo   !  number of velocity arrays
        !     integer :: noq1     !  number of exch. 1st direction
        !     integer :: noq2     !  number of exch. 2nd direction
        !     integer :: noq3     !  number of exch. 3rd direction
        !     integer :: noq4     !  number of exch. bottom direction
        !     integer :: noq      !  number of exchanges
        !     integer :: nobnd    !  number of boundaries
        !     integer :: jtrack   !  number of codiagonals
        !     integer :: ndmpar   !  Number of dump area's for balance output
        !     integer :: ndmpq    !  Number of exchanges of interest for balance
        !     integer :: ndmps    !  Number of segments of interest for balance
        !     integer :: ntdmpq   !  Number of times exchanges contribute to balance
        !     integer :: ntdmps   !  Number of times segments contribute to balance
        !     integer :: noraai   !  Number of raaien
        !     integer :: ntraaq   !  Total number of times exchanges contribute to raai
        !     integer :: nomat    !  Size of the fast solvers matrix
        !     integer :: mmax     !  Number of columns in regular grid
        !     integer :: nmax     !  Number of rows in regular grid
        !     integer :: kmax     !  Number of layers in regular grid

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
        integer(kind = int_wp) :: noq12      !  noq1 + noq2 (number of horizontal exchanges)
        integer(kind = int_wp) :: noq34      !  noq3 + noq4 (number of vertical exchanges)
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
        integer(kind = int_wp) :: idisp  (nosys)    !  dispersion number per substance
        integer(kind = int_wp) :: ivelo  (nosys)    !  velocity number per substance
        real(kind = real_wp) :: factor (5)      !  scale factor tabular input
        integer(kind = int_wp), pointer :: cellpnt(:)        !  backpointer noseg to mnmaxk
        integer(kind = int_wp), pointer :: flowpnt(:)        !  backpointer noq to 3*mnmaxk-mnmax
        real(kind = real_wp) :: length (3, 1)      !  lengthes per direction
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_block_4_flow_dims_pointers", ithndl)

        nosss = noseg + nseg2
        iposr = 0
        volume = 0
        adummy = 0.0
        idummy = 0
        ifact = 1
        ierr2 = 0

        !        Read exchange dimensions of the system (NOQ1,NOQ2,NOQ3)

        if (has_hydfile) then
            noq1 = nexch(1)
            noq2 = nexch(2)
            noq3 = nexch(3)
        else
            regular = .false.
            if (gettoken(cdummy, noq1, itype, ierr2) > 0) goto 100
            if (itype == 1) then
                if (cdummy(1:12) == 'REGULAR_GRID') then
                    regular = .true.
                    if (gettoken(noq1, ierr2) > 0) goto 100
                else
                    ierr2 = 1
                    goto 100
                endif
            endif
            if (gettoken(noq2, ierr2) > 0) goto 100
            if (gettoken(noq3, ierr2) > 0) goto 100
        endif

        noq = noq1 + noq2 + noq3

        !        These 2 options use a regular grid with full matrix.

        if (regular) then
            nmax = noq1
            mmax = noq2
            kmax = noq3
            write (lunut, 2000) nmax, mmax, kmax
            GridPs%Pointers(1)%nolay = kmax
            nolay = kmax
        else
            nmax = 0
            mmax = 0
            kmax = 0
            write (lunut, 2010) noq1, noq2, noq3, noq

            !        detect number of layers (only structured sigma or z model, otherwise KMAX=0)

            if (nolay <= 1) then
                if (noq3 > 0) then
                    kmax = 0
                    nosegl = noseg - noq3
                    if (nosegl > 0) then
                        nolay = noseg / nosegl
                        if (nolay * nosegl == noseg) then
                            kmax = nolay
                        else
                            nolay = 1
                        endif
                    endif
                else
                    kmax = 1
                    nolay = 1
                    nosegl = noseg
                endif
            else
                kmax = nolay
            endif
        endif
        if (.not. alone) then
            if (noq /= noqp) then
                write (lunut, 2020) noqp
                call status%increase_error_count()
            endif
        endif
        noq4 = 0
        if (nseg2 /= 0) then
            noq4 = nseg2 + noseg / nolay
            noq4 = noq4 * 2
            write (lunut, 2040) noq4
        endif

        !        Read number of additional dispersion arrays NODISP

        if (gettoken(nodisp, ierr2) > 0) goto 100
        write (lunut, 2050) nodisp
        idisp = 0
        if (nodisp > 0) then
            allocate (dispnam(nodisp))   !    'Dispersion nnnn'
            do i = 1, nodisp
                if (gettoken(dispnam(i), ierr2) > 0) goto 100
                if (dispnam(i) == ' ') write (dispnam(i), 2060) i
                ifound = index_in_array(dispnam(i), dispnam(1:i - 1))
                if (ifound > 0) then
                    write(lunut, 2070) dispnam(i)
                    call status%increase_error_count()
                endif
            enddo
            if (output_verbose_level >= 2) then
                write (lunut, 2080) (i, dispnam(i), i = 1, nodisp)
            else
                write (lunut, 2090)
            endif
            write (lunut, *)
            write (file_unit_list(2)) (dispnam(i), i = 1, nodisp)
            deallocate (dispnam)

            do i = 1, nosys     !   read which dispersion array applies (0=none) for each subst.
                if (gettoken(idisp(i), ierr2) > 0) goto 100
                if (idisp(i) > nodisp) then
                    write (lunut, 2100) idisp(i), nodisp
                    call status%increase_error_count()
                endif
            enddo
        endif

        !        Read number of additional velocity arrays NOVELO in exactly
        !                   the same way (could probably be better 1 code)

        if (gettoken(novelo, ierr2) > 0) goto 100
        write (lunut, 2110) novelo
        ivelo = 0
        if (novelo > 0) then
            allocate (dispnam(novelo))   !
            do i = 1, novelo
                if (gettoken(dispnam(i), ierr2) > 0) goto 100
                if (dispnam(i) == ' ') write (dispnam(i), 2120) i
                ifound = index_in_array(dispnam(i), dispnam(1:i - 1))
                if (ifound > 0) then
                    write(lunut, 2130) dispnam(i)
                    call status%increase_error_count()
                endif
            enddo
            if (output_verbose_level >= 2) then
                write (lunut, 2080) (i, dispnam(i), i = 1, novelo)
            else
                write (lunut, 2090)
            endif
            write (lunut, *)
            write (file_unit_list(2)) (dispnam(i), i = 1, novelo)
            deallocate (dispnam)

            do i = 1, nosys     !   read which dispersion array applies (0=none) for each subst.
                if (gettoken(ivelo(i), ierr2) > 0) goto 100
                if (ivelo(i) > novelo) then
                    write (lunut, 2100) ivelo(i), novelo
                    call status%increase_error_count()
                endif
            enddo
        endif
        !           write a report if sensible and write binary file
        if ((nodisp > 0 .or. novelo > 0) .and. output_verbose_level >= 2) &
                write (lunut, 2140) (i, idisp(i), ivelo(i), i = 1, nosys)
        write (file_unit_list(2)) idisp
        write (file_unit_list(2)) ivelo
        !           a very obvious (and rude) check on correctness
        if (noq1 < 0 .or. noq2   < 0 .or. noq3   < 0 .or. &
                noq  == 0 .or. nodisp < 0 .or. novelo < 0) then
            write (lunut, 2150)
            call status%increase_error_count()
        endif

        !        Read option variable for input mode

        if (has_hydfile) then
            iopt1 = 0
        else
            if (gettoken(integration_id, ierr2) > 0) goto 100
            write (lunut, 2170) integration_id
            noqt = noq
            if (integration_id == 2) goto 10

            !***************  first type of input ******************

            !        Read exchange pointers

            if (gettoken(iopt1, ierr2) > 0) goto 100
            write (lunut, 2180)  iopt1

            if (regular) then  !        Regular grid
                call process_simulation_input_options (iopt1, file_unit_list, 8, file_name_list, filtype, &
                        is_date_format, is_yyddhh_format, 0, ierr2, status, &
                        .false.)
                if (ierr2  > 0) goto 100
                noqt = noq4
                call read_exchange_pointers_regular_grid (file_unit_list, file_name_list, noseg, nmax, mmax, &
                        kmax, noq, noq1, noq2, noq3, &
                        noqt, nobnd, ipnt, intsrt, iopt1, &
                        jtrack, output_verbose_level, iwidth, GridPs, cellpnt, &
                        flowpnt, status)
            endif
        endif
        if (has_hydfile .or. .not. (regular)) then  ! Irregular grid/hyd-file
            call process_simulation_input_options (iopt1, file_unit_list, 44, file_name_list, filtype, &
                    is_date_format, is_yyddhh_format, 0, ierr2, status, &
                    has_hydfile)
            if (ierr2  > 0) goto 100
            noqt = noq + noq4
            allocate (ipnt(4, noqt), stat = ierr2)
            if (ierr2 /= 0) then
                write (lunut, 2160) ierr2, 4 * noqt
                goto 100
            endif
            ipnt = 0
            call read_exchange_pointers_irregular_grid (file_unit_list, file_name_list, noseg, noq, noq1, &
                    noq2, noq3, noqt, nobnd, ipnt, &
                    intsrt, iopt1, jtrack, filtype(44), output_verbose_level, &
                    GridPs, status)
        endif
        noq12 = noq1 + noq2
        noq34 = noq3 + noq4

        !        set dump area structure

        call create_write_monitoring_area_array (file_unit_list, ndmpar, ntdmps, noqt, nosss, &
                nobnd, ipnt, ntdmpq, ndmpq, ndmps, &
                noraai, ntraaq, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, status)

        !        calculate size of the fast solvers matrix

        if (intsrt == 15 .or. intsrt == 16 .or. &
                intsrt == 17 .or. intsrt == 18 .or. &
                intsrt == 21 .or. intsrt == 22) then
            call compute_matrix_size (noq1, noq2, noq34, nosss, ipnt, &
                    nomat)
            write (lunut, 2190) nomat
        endif
        if (associated(ipnt)) then
            deallocate (ipnt)
        endif

        !        Read dispersions

        write (lunut, 2200)
        disper = .true.
        ierr2 = 0
        call read_constants_time_variables   (file_unit_list, 9, noq1, noq2, noq3, &
                nodisp, 1, nrftot(3), nrharm(3), ifact, &
                is_date_format, disper, volume, iwidth, file_name_list, &
                filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                status, .false.)
        call status%increase_error_count_with(ierr2)
        disper = .false.

        !        Read areas

        write (lunut, 2210)
        ierr2 = 0
        call read_constants_time_variables   (file_unit_list, 10, noq1, noq2, noq3, &
                1, 1, nrftot(4), nrharm(4), ifact, &
                is_date_format, disper, volume, iwidth, file_name_list, &
                filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                status, has_hydfile)
        call status%increase_error_count_with(ierr2)

        !        Read flows

        write (lunut, 2220)
        ierr2 = 0
        call read_constants_time_variables   (file_unit_list, 11, noq1, noq2, noq3, &
                1, 1, nrftot(5), nrharm(5), ifact, &
                is_date_format, disper, volume, iwidth, file_name_list, &
                filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                status, has_hydfile)
        call status%increase_error_count_with(ierr2)
        if (.not. alone) then
            if (file_name_list(11) /= fnamep(7)) then
                write (lunut, 2225) fnamep(7)
                call status%increase_error_count()
            endif
        endif

        !        Read velos

        if (novelo > 0) then
            write (lunut, 2230)
            ierr2 = 0
            call read_constants_time_variables   (file_unit_list, 12, noq1, noq2, noq3, &
                    novelo, 1, nrftot(6), nrharm(6), ifact, &
                    is_date_format, disper, volume, iwidth, file_name_list, &
                    filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                    status, .false.)
            call status%increase_error_count_with(ierr2)
        endif

        !        Read length "to" and "from" surfaces

        write (lunut, 2240)

        if (has_hydfile) then
            ilflag = 1
        else
            if (gettoken(ilflag, ierr2) > 0) goto 100
            write (lunut, 2250) ilflag
        endif
        select case (ilflag)
        case (0)
            write (lunut, 2260)
            idum = 4
            write (file_unit_list(2)) idummy
            call read_constant_data (1, length, 1, 3, 1, &
                    iwidth, file_unit_list(2), idum, ierr2)

        case (1)
            write (lunut, 2270)
            write (file_unit_list(2)) idummy, (adummy, k = 1, 3)
            ierr2 = 0
            call read_constants_time_variables   (file_unit_list, 13, noq1, noq2, noq3, &
                    2, 1, nrftot(7), nrharm(7), ifact, &
                    is_date_format, disper, volume, iwidth, file_name_list, &
                    filtype, is_yyddhh_format, output_verbose_level, ierr2, &
                    status, has_hydfile)

        case default
            write (lunut, 2280)
            call status%increase_error_count()

        end select
        goto 100

        !***************  second type of input ******************

        10 continue
        allocate (ipnt(4, noqt), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2160) ierr2, 4 * noqt
            goto 100
        endif
        ipnt = 0

        ilflag = 1
        if (nodisp < 1) then
            write (lunut, 2290) nodisp
            ierr2 = 1
            goto 100
        endif

        allocate (rwork(5, noq), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2310) ierr2, 5 * noq
            goto 100
        endif

        if (gettoken(iopt1, ierr2) > 0) goto 100
        write (lunut, 2320) iopt1
        if (iopt1 == 0) then
            write (lunut, 2280)
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

        do j = 1, noq
            do i = 1, 4
                if (gettoken(ipnt (i, j), ierr2) > 0) goto 100
            enddo
            do i = 1, 5
                if (gettoken(rwork(i, j), ierr2) > 0) goto 100
            enddo
        enddo

        write (lunut, 2330) (factor(i), i = 1, 4)
        write (lunut, 2340)
        write (lunut, 2350) ((ipnt (i, j), i = 1, 4), &
                (rwork(i, j), i = 1, 5), j = 1, noq)

        !       calculate number of boundaries and bandwith of matrix

        call create_boundary_pointers  (file_unit_list, noseg, noq, noqt, intsrt, &
                output_verbose_level, GridPs, nobnd, jtrack, ipnt, &
                status)

        !        set dump area structure

        call create_write_monitoring_area_array (file_unit_list, ndmpar, ntdmps, noq, noseg, &
                nobnd, ipnt, ntdmpq, ndmpq, ndmps, &
                noraai, ntraaq, nsegdmp, isegdmp, nexcraai, &
                iexcraai, ioptraai, status)

        !        calculate size of the fast solvers matrix

        if (intsrt == 15 .or. intsrt == 16 .or. &
                intsrt == 17 .or. intsrt == 18 .or. &
                intsrt == 21 .or. intsrt == 22) then
            call compute_matrix_size (noq1, noq2, noq34, nosss, ipnt, &
                    nomat)
            write (lunut, 2190) nomat
        endif

        factor(5) = factor(4)
        call scale_array (rwork, factor)

        write (file_unit_list(2)) idummy, (adummy, k = 1, 3)
        write (file_unit_list(2)) idummy, (adummy, k = 1, 3)

        call open_waq_files  (file_unit_list(8), file_name_list(8), 8, 1, ierr2)
        if (ierr2 /= 0) goto 100
        if (noq1 > 0) write(file_unit_list(8))(ipnt(:, i), i = 1, noq1)
        if (noq2 > 0) write(file_unit_list(8))(ipnt(:, i), i = noq1 + 1, noq12)
        if (noq3 > 0) write(file_unit_list(8))(ipnt(:, i), i = noq12 + 1, noq)
        close (file_unit_list(8))

        call open_waq_files  (file_unit_list(9), file_name_list(9), 9, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(9)) idummy, (rwork(1, i), (adummy, k = 1, nodisp - 1), i = 1, noq)
        close (file_unit_list(9))

        call open_waq_files  (file_unit_list(10), file_name_list(10), 10, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(10)) idummy, (rwork(2, i), i = 1, noq)
        close (file_unit_list(10))

        call open_waq_files (file_unit_list(11), file_name_list(11), 11, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(11)) idummy, (rwork(3, i), i = 1, noq)
        close (file_unit_list(11))

        if (novelo > 0) then
            call open_waq_files  (file_unit_list(12), file_name_list(12), 12, 1, ierr2)
            if (ierr2 /= 0) goto 100
            write (file_unit_list(12)) idummy, ((adummy, k = 1, novelo), i = 1, noq)
            close (file_unit_list(12))
        endif

        call open_waq_files  (file_unit_list(13), file_name_list(13), 13, 1, ierr2)
        if (ierr2 /= 0) goto 100
        write (file_unit_list(13)) idummy, (rwork(4, i), rwork(5, i), i = 1, noq)
        close (file_unit_list(13))

        deallocate(ipnt, rwork)
        ierr2 = 0

        !       here ends the alternative input

        100 continue

        !       check the layers/3D model information:
        !       - is it a 3D model?
        !       - do we have consistency?

        if (noq3 /= 0) then
            if (nolay == 1) then
                call status%increase_warning_count()
                write(lunut, 3000) noseg, noq3, noseg - noq3
                write(lunut, 3005)
                write(*, '(1x,a)') 'WARNING: inconsistency if 3D model', &
                        '         check .lst file'
            else
                write(lunut, 3010) nolay
            endif
        endif

        if (ierr2 > 0) call status%increase_error_count()
        if (ierr2 == 3) call srstop(1)
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
        2290 format (/, ' ERROR. Option incompatible with NODISP=', I4)
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
                , //, '          You can specify the number of layers via' &
                , /, '          these keywords:' &
                , //, '          MULTIGRID ZMODEL NOLAY ... END_MULTIGRID')
        3010 format (//, ' Number of layers in the model:', I5)

    end subroutine read_block_4_flow_dims_pointers

end module inputs_block_4
