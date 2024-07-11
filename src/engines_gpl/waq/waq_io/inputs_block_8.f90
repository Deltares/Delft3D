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
module inputs_block_8
    use m_waq_precision
    use initial_conditions, only : read_initial_conditions
    use simulation_input_options, only : process_simulation_input_options, validate_simulation_time_steps, &
            read_constant_data
    use m_error_status

    implicit none

    private
    public :: read_block_8_initial_conditions

contains


    subroutine read_block_8_initial_conditions(file_unit_list, file_name_list, filtype, num_cells, num_substances_total, &
            syname, iwidth, output_verbose_level, inpfil, &
            gridps, status)

        !!  Reads initial conditions
        !! This routine reads the initial conditions
        !!      - MASS/M2 is an allowed keyword to indicate that ASCII input
        !!          of passive substances is expressed in mass/m2
        !!       - ASCII with defaults and overridings is the same since 1988
        !!       - ASCII without defaults requires 1 keyword and values for
        !!          all volumes per substance, so 12345*2.27 ; substance 1 etc.
        !!          and 11522*0.0 823*3.14  ; for a passive substance in 15 layers.
        !!          The simulation system (set_dry_cells_to_zero_and_update_volumes.f) migrates automatically the
        !!          lowest value to the first active cell in a Z-layer model
        !!       - Binary files not being a .map file are assumed to be in mass/gridcell
        !!       - .map files are scanned for the presence of the mass/m2 token at
        !!          the end of title string 3 by the simulation system (initialize_all_conditions.f)
        !!       - The simulation system only produces mass/m2 .map files for restart
        !!          purposes any more.

        !!  Subroutines called : process_simulation_input_options    ( which file is it ? )
        !!                       read_constant_data    ( read the data from an ASCII file )
        !!                       rdtok1  ( tokenized data reading )
        !!                       open_waq_files  ( to open the binary intermediate file )
        !!                       check   ( to see of the group was read correctly )

        !! Logical units : file_unit_list(27) = unit DELWAQ input file
        !!                 file_unit_list(29) = unit formatted output file
        !!                 file_unit_list(18) = unit intermediate file (initials)

        use error_handling, only : check_error
        use m_logger_helper, only : stop_with_error
        use m_open_waq_files
        use m_grid_utils_external   ! for the storage of contraction grids
        use m_waq_data_structure  ! for definition and storage of data
        use rd_token
        use timers         ! performance timers
        use string_module  ! string manipulation tools

        integer(kind = int_wp), intent(inout) :: file_unit_list  (*)       !< array with unit numbers
        character       (*), intent(inout) :: file_name_list(*)       !< filenames
        integer(kind = int_wp), intent(inout) :: filtype(*)     !< type of binary file
        integer(kind = int_wp), intent(in) :: num_cells          !< nr of computational volumes
        integer(kind = int_wp), intent(in) :: num_substances_total          !< nr of delwaq + delpar state variables
        character       (20), intent(in) :: syname(num_substances_total)  !< names of the substances
        integer(kind = int_wp), intent(in) :: iwidth         !< width of the output file
        integer(kind = int_wp), intent(in) :: output_verbose_level         !< option for extent of output
        type(t_input_file), intent(inout) :: inpfil         !< input file structure with include stack and flags
        type(gridpointercoll), intent(in) :: gridps         !< collection off all grid definitions

        type(error_status), intent(inout) :: status !< current error status

        ! local
        integer(kind = int_wp), parameter :: STRING = 1
        integer, parameter :: EXTASCII = -1, BINARY = 0, THISFILE = 1
        integer(kind = int_wp), parameter :: NODEFAUL = 1, DEFAULTS = 2

        real(kind = real_wp), allocatable :: scales(:)              ! real workspace scale factors
        real(kind = real_wp), allocatable :: values(:, :)            ! real workspace values
        integer(kind = int_wp), allocatable :: iover (:)              ! integer space for overridings

        integer(kind = int_wp) :: ierr2      ! local error indicator
        integer(kind = int_wp) :: itype      ! 0 = all, 1 = string, 2 = integer, 3 = real
        character(255)  cdummy             ! workspace for reading
        character(4)  cext               ! inital conditions file extention
        integer(kind = int_wp) :: icopt1     ! first file option (ASCII/Binary/external etc)
        integer(kind = int_wp) :: icopt2     ! constants with or without defaults
        logical         ldummy             ! dummy variable
        integer(kind = int_wp) :: ip         ! location of the period in the file name
        integer(kind = int_wp) :: i          ! loop variable and dummy integer
        integer(kind = int_wp) :: isys, iseg ! substances and volumes loop variables
        logical         masspm2            ! is it mass per m2 ?
        logical         transp             ! input with a transposed matrix (per substance) ?
        logical         old_input          ! old or new input
        integer(kind = int_wp) :: itime      ! time in map file
        integer(kind = int_wp) :: ithndl = 0

        if (timon) call timstrt("read_block_8_initial_conditions", ithndl)

        !        Initialisations

        iposr = 0                                  ! start at begin of the input line with reading
        file_unit = file_unit_list(29)
        ierr2 = 0
        masspm2 = .false.
        transp = .false.

        !        Let's see what comes, file option or a token

        if (gettoken(cdummy, icopt1, itype, ierr2) > 0) goto 10
        if (itype == STRING) then
            if (cdummy == 'mass/m2' .or. &
                    cdummy == 'MASS/M2') then
                masspm2 = .true.
                write (file_unit, 2030)
                if (gettoken(cdummy, icopt1, itype, ierr2) > 0) goto 10
            elseif (cdummy /= 'INITIALS') then
                write (file_unit, 2040) trim(cdummy)
                ierr2 = 3
                goto 10
            endif
        endif

        if (itype == STRING) then
            if (cdummy == 'INITIALS') then
                icopt1 = THISFILE
                old_input = .false.
            else
                write (file_unit, 2040) trim(cdummy)
                ierr2 = 3
                goto 10
            endif
        else
            old_input = .true.
        endif

        !        The file option

        write (file_unit, 2000) icopt1
        if (icopt1 /= EXTASCII .and. icopt1 /= BINARY   .and. &
                icopt1 /= THISFILE) then
            write (file_unit, 2020)
            goto 10
        endif

        !        Get the input file name

        call process_simulation_input_options   (icopt1, file_unit_list, 18, file_name_list, filtype, &
                ldummy, ldummy, 0, ierr2, status, &
                .false.)
        if (ierr2  > 0) goto 10
        if (icopt1 == BINARY) then
            ip = scan (file_name_list(18), '.', back = .true.)              ! look for the file type
            cext = file_name_list(18)(ip:ip + 3)
            call str_lower(cext)
            if (cext == '.map' .or. cext == '.rmp' .or. &
                    cext == '.rm2') then                             ! if .rmp or .rm2 (Sobek) or .map, it is a map-file
                call open_waq_files(file_unit_list(18), file_name_list(18), 18, 2, ierr2)
                read (file_unit_list(18)) cdummy(1:160)                        ! read title of simulation
                close (file_unit_list(18))
                if (cdummy(114:120) == 'mass/m2' .or. &
                        cdummy(114:120) == 'MASS/M2') then            !  at end of third line ...
                    write (file_unit, 2070)
                else if (masspm2) then
                    write (file_unit, 2080)
                    call status%increase_error_count()
                else
                    write (file_unit, 2090)
                    call status%increase_warning_count()
                endif
            else if (masspm2) then
                write (file_unit, 2100)
                call status%increase_error_count()
            else
                write (file_unit, 2110)
                call status%increase_warning_count()
            endif
            goto 10
        endif

        !        Make the file a .map file instead of the previous .wrk file

        ip = scan (file_name_list(18), '.', back = .true.)
        if (ip == 0) then
            file_name_list(18) = trim(file_name_list(18)) // '.map'
        else
            file_name_list(18)(ip:ip + 3) = '.map'
        endif
        call open_waq_files(file_unit_list(18), file_name_list(18), 18, 1, ierr2)
        if (ierr2 > 0) goto 10

        !        Write the .map header

        if (masspm2) then
            cdummy(1:40) = 'Initial conditions file                 '
            cdummy(41:80) = 'inactive substances are in mass/m2      '
            cdummy(81:120) = 'this is the deciding keyword ==> mass/m2'
            cdummy(121:160) = 'there is no time string in this file    '
        else
            cdummy(1:40) = 'Initial conditions file                 '
            cdummy(41:80) = 'inactive substances are in mass/gridcell'
            cdummy(81:120) = '                                        '
            cdummy(121:160) = 'there is no time string in this file    '
        endif
        write (file_unit_list(18)) cdummy(1:160), num_substances_total, num_cells
        write (file_unit_list(18)) (syname(i), i = 1, num_substances_total)

        if (old_input) then
            !                see how the data comes

            if (gettoken(cdummy, icopt2, itype, ierr2) > 0) goto 10
            if (itype == STRING) then
                if (cdummy == 'TRANSPOSE') then
                    transp = .true.
                    write (file_unit, 2060)
                    if (gettoken(icopt2, ierr2) > 0) goto 10
                else
                    write (file_unit, 2040) trim(cdummy)
                    ierr2 = 3
                    goto 10
                endif
            endif
            write (file_unit, 2010) icopt2
            if (icopt2 /= NODEFAUL .and. icopt2 /= DEFAULTS) then
                write (file_unit, 2020)
                goto 10
            endif

            !           Get the data and write it to unit 18

            write (file_unit_list(18)) 0
            if (transp) then
                allocate (values(num_cells, num_substances_total))
                call read_constant_data  (icopt2, values, num_substances_total, num_cells, 1, &
                        iwidth, 0, output_verbose_level, ierr2)
                write (file_unit_list(18)) (values(i, :), i = 1, num_cells)
            else
                allocate (values(num_substances_total, num_cells))
                call read_constant_data  (icopt2, values, num_cells, num_substances_total, num_substances_total, &
                        iwidth, 0, output_verbose_level, ierr2)
                write (file_unit_list(18)) values
            endif
            close (file_unit_list(18))
            if (ierr2 > 0) goto 10

        else

            allocate (values(num_substances_total, num_cells), stat = ierr2)
            if (ierr2 /= 0) then
                write(file_unit, *) 'ERROR allocating memory for initials'
                ierr2 = 3
                goto 10
            endif
            push = .true.
            call read_initial_conditions (file_unit_list, file_name_list, filtype, inpfil, num_substances_total, &
                    syname, iwidth, output_verbose_level, gridps, num_cells, &
                    values, ierr2, status)
            itime = 0
            write(file_unit_list(18)) itime, values
            close (file_unit_list(18))
            if (ierr2 > 0) goto 10

        endif

        ierr2 = 0
        10 if (ierr2 > 0) call status%increase_error_count()
        if (ierr2 > 0) write (file_unit, 2050)
        if (ierr2 == 3) call stop_with_error()
        if (old_input) then
            call check_error(cdummy, iwidth, 8, ierr2, status)
        endif
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (/, ' Option selected for initials    :', I4)
        2010 format (' Second option for initials      :', I4)
        2020 format (/, ' ERROR, option not implemented')
        2030 format (/, ' Initials for passive substances are in mass/m2')
        2040 format (/, ' ERROR, keyword not supported: ', A)
        2050 format (' ERROR reading input!')
        2060 format (/, ' Block of input data is ordered per substance')
        2070 format (/, ' Binary initials file is .map file with bed substances in mass/m2!')
        2080 format (/, ' ERROR: initials file is .map file with bed substances in mass/gridcell rather than mass/m2!')
        2090 format (/, ' WARNING: Binary initials file is .map file with bed substances in mass/gridcell!')
        2100 format (/, ' ERROR: Binary initials file is assumed to have bed substances in mass/gridcell rather than mass/m2!')
        2110 format (/, ' WARNING: Binary initials file is assumed to have bed substances in mass/gridcell!')

    end subroutine read_block_8_initial_conditions

end module inputs_block_8
