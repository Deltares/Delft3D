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
module m_block_2_input_reader
    use m_waq_precision

    implicit none

    private
    public :: read_block_2_from_input

contains

    subroutine read_block_2_from_input (file_unit_list, file_name_list, filtype, nrftot, nlines, &
            npoins, is_date_format, is_ddhhmmss_format, nodump, integration_id_list, &
            num_integration_options, iwidth, is_yyddhh_format, ndmpar, ntdmps, &
            noraai, ntraaq, nosys, notot, nototp, &
            output_verbose_level, nsegdmp, isegdmp, nexcraai, &
            iexcraai, ioptraai, status)

        !> Reads integration method; monitoring areas/transects and timers
        !>
        !> This routine reads:
        !>    - the options for time strings (none, ddhhmmss or yydddhh)
        !>    - integration method and type
        !>    - start, stop and step of integration
        !>    - nr and names and cell nr's of monitoring areas
        !>    - nr and names and exchange surface nr's of monitoring transects
        !>    - start, step and stop time of the monitoring file
        !>    - start, step and stop time of the history file
        !>    - start, step and stop time of the map file

        use error_handling, only : check_error
        use m_report_date_time
        use m_rdpart
        use m_rdlgri
        use m_rdfnam
        use m_rdccol
        use m_logger, only : terminate_execution
        use m_open_waq_files
        use rd_token     !   for the reading of tokens
        use partmem      !   for PARTicle tracking
        use fileinfo     !   a filename array in PART
        use alloc_mod
        use timers       !   performance timers
        use m_sysi          ! Timer characteristics
        use date_time_utils, only : convert_string_to_time_offset, simulation_start_time_scu, &
                simulation_stop_time_scu, convert_relative_time, convert_time_format
        use waq_timers, only : timer
        use monitoring_areas, only : read_monitoring_transects, read_monitoring_areas
        use integration_options, only : check_integration_option
        use m_time_validation

        character(*), intent(inout), dimension(*) :: file_name_list !< array with file names of the files

        integer(kind = int_wp), intent(inout), dimension(*) :: file_unit_list      !< array with unit numbers
        integer(kind = int_wp), intent(inout), dimension(*) :: filtype  !< type of binary file
        integer(kind = int_wp), intent(inout), dimension(*) :: nrftot   !< number of function items
        integer(kind = int_wp), intent(in), dimension(*) :: integration_id_list     !< array with valid integration options
        integer(kind = int_wp), dimension(:), pointer :: nsegdmp  !< number of volumes in this monitoring area
        integer(kind = int_wp), dimension(:), pointer :: isegdmp  !< computational volume numbers
        integer(kind = int_wp), dimension(:), pointer :: nexcraai !< number of exchanges in this monitoring transect
        integer(kind = int_wp), dimension(:), pointer :: iexcraai !< exchange area numbers of the transect
        integer(kind = int_wp), dimension(:), pointer :: ioptraai !< option for the transects
        integer(kind = int_wp), intent(inout) :: nlines   !< cumulative record  space
        integer(kind = int_wp), intent(inout) :: npoins   !< cumulative pointer space
        integer(kind = int_wp), intent(out) :: nodump   !< number of monitoring points output
        integer(kind = int_wp), intent(in) :: num_integration_options    !< dimension of integration_id_list
        integer(kind = int_wp), intent(in) :: iwidth   !< width of the output file
        integer(kind = int_wp), intent(out) :: ndmpar   !< number of dump areas
        integer(kind = int_wp), intent(out) :: ntdmps   !< total number segments in dump area
        integer(kind = int_wp), intent(out) :: noraai   !< number of raaien
        integer(kind = int_wp), intent(out) :: ntraaq   !< total number of exch. in raaien
        integer(kind = int_wp), intent(in) :: nosys    !< number of transported substances
        integer(kind = int_wp), intent(inout) :: notot    !< total number of substances
        integer(kind = int_wp), intent(out) :: nototp   !< notot inclusive of partcle substances
        integer(kind = int_wp), intent(in) :: output_verbose_level   !< flag for more or less output

        logical, intent(out) :: is_date_format !< 'date'-format 1st timescale
        logical, intent(out) :: is_ddhhmmss_format !< 'date'-format 2nd timescale
        logical, intent(out) :: is_yyddhh_format !< 'date'-format (F;ddmmhhss,T;yydddhh)

        type(error_status) :: status !< current error status

        ! local

        character(20), dimension(:), pointer :: duname
        character(20), dimension(:), pointer :: raname
        character(255) :: cdummy !  help variables to read tokens
        character(8) :: date1  !  help variables to read date strings
        character(8) :: date2  !  help variables to read date strings

        integer(kind = int_wp), dimension(:), pointer :: dmpbal
        integer(kind = int_wp), dimension(:), allocatable :: int_array        !  integer workspace
        integer(kind = int_wp) :: ierr2      !  local error and warning variables
        integer(kind = int_wp) :: iwar2      !  local error and warning variables
        integer(kind = int_wp) :: i          !  loop counters
        integer(kind = int_wp) :: k          !  loop counters
        integer(kind = int_wp) :: i2         !  loop counters
        integer(kind = int_wp) :: ibrk       !  loop counters
        integer(kind = int_wp) :: num_records      !  number of breakpoints
        integer(kind = int_wp) :: ifound     !  help variable for string search
        integer(kind = int_wp) :: idummy     !  help variables to read tokens
        integer(kind = int_wp) :: itype      !  help variables to read tokens
        integer(kind = int_wp) :: ierr_alloc !  help variable to identify allocation errors
        integer(kind = int_wp) :: ioerr      !  help variable for errors on open files
        integer(kind = int_wp) :: ibflag     !  balances ?
        integer(kind = int_wp) :: ithndl = 0

        real(kind = real_wp) :: aint !  to read the integration option

        if (timon) call timstrt("read_block_2_from_input", ithndl)

        !       Initialisation of timers

        is_date_format = .false.
        is_ddhhmmss_format = .false.
        is_yyddhh_format = .false.
        isflag = 0
        iposr = 0
        ierr2 = 0
        iwar2 = 0
        alone = .true.      ! not coupled with Delpar
        file_name_list(45) = ' '      ! so no Delpar input file

        !     There should be at least one substance in the input.

        if (notot < 1) then
            write (file_unit, '(/, A)') ' ERROR: No substances have been specified.'
            call terminate_execution(1)
        endif

        !        Read timers

        if (gettoken(itfact, ierr2) > 0) goto 30
        if (gettoken(date1, ierr2) > 0) goto 30
        if (gettoken(date2, ierr2) > 0) goto 30
        write (file_unit, '(/,/,A, I10)') ' Factor between two clocks :  ', itfact

        !        Day string for date1                                              for date2:

        if (date1 == 'DDHHMMSS' .or. date1 == 'ddhhmmss') then
            is_date_format = .true.
            isflag = 1
            write (file_unit, *)' System clock in date-format  DDHHMMSS '
            if (date2 == 'DDHHMMSS' .or. date2 == 'ddhhmmss') then    ! allowed
                is_ddhhmmss_format = .true.
                write (file_unit, *) ' Auxiliary timer in date-format DDHHMMSS '
                if (itfact /= 1) write (file_unit, 2040)
            elseif (date2 == 'YYDDDHH'.or. date2 =='yydddhh') then    ! not allowed
                write (file_unit, '(/, A, /, A, /, A)') &
                        ' ERROR !!!! Auxiliary timer YYDDDHH-format ! ****', &
                        ' This option is invalid in combination with ', &
                        ' system clock in DDHHMMSS format !'
                call status%increase_error_count()
            elseif (date2 /= ' ') then                                  ! or blank
                write (file_unit, 2060) date2
                call status%increase_error_count()
            endif

            !        Year string for date1

        elseif (date1 == 'YYDDDHH' .or. date1 == 'yydddhh') then
            is_date_format = .true.
            is_yyddhh_format = .true.
            isflag = 1
            write (file_unit, *) ' System clock in date-format  YYDDDHH '
            if (date2 == 'YYDDDHH' .or. date2 == 'yydddhh') then      ! allowed
                is_ddhhmmss_format = .true.
                write (file_unit, *) ' Auxiliary timer in date-format YYDDDHH '
                if (itfact /= 1) write (file_unit, 2040)
            elseif (date2 =='DDHHMMSS' .or. date2 =='ddhhmmss') then  ! not allowed
                write (file_unit, '(/, A, /, A, /, A)') &
                        ' ERROR !!!! Auxiliary timer DDHHMMSS-format ! ****', &
                        ' This option is invalid in combination with ', &
                        ' system clock in YYDDDHH format !'
                call status%increase_error_count()
            elseif (date2 /= ' ') then                                  ! or blank
                write (file_unit, 2060) date2
                call status%increase_error_count()
            endif

            !        or blank for date1

        elseif (date1 /= ' ') then
            write (file_unit, '(/,A)') ' ERROR System timer format not recognised :' // date1
            call status%increase_error_count()
        endif

        !        Read integration type

        if (gettoken(aint, ierr2) > 0) goto 30
        intsrt = int(aint)
        intopt = int(aint * 100.0 - intsrt * 100 + 0.5)
        if (mod(intopt, 10) == 0) then
            intopt = intopt / 10
            ibflag = 0
        else
            if (mod(int(aint * 1000. + 0.5), 100) == 10) then
                intopt = intopt / 10 + 8
            elseif (mod(int(aint * 1000. + 0.5), 100) <= 20) then
                intopt = intopt / 10 + 8 + 16
            else
                intopt = intopt / 10 + 8 + 16 + 32
            endif
            ibflag = 1
        endif
        do i = 1, num_integration_options
            if (int(10. * aint) == integration_id_list(i)) goto 10
        enddo
        write (file_unit, '(/,A)') ' ERROR !!!! Invalid integration option ! ****'
        call status%increase_error_count()
        10 write (file_unit, '(/,A, F5.2)') ' Integration option :               ', aint

        !     No in-active substances for fast solver steady state

        if (notot /= nosys .and. (intsrt == 17 .or. intsrt == 18)) then
            call status%increase_error_count()
            write (file_unit, '(/,A)') ' ERROR No in-active substances allowed for' // &
                    ' fast-solver steady state scheme'
        endif

        !        Read optional keywords and start time of integration

        nototp = 0
        if (gettoken(cdummy, idummy, itype, ierr2) > 0) goto 30
        do while (itype == 1)                                      ! read a collection of tokens
            call check_integration_option (cdummy, intopt, file_unit, ierr2)
            if (btest(intopt, 17) .and. alone) then
                alone = .false.

                !                Delpar in Delwaq

                if (gettoken(cdummy, ierr2) > 0) goto 30          ! get the input file name for particles
                file_name_list(45) = cdummy
                call rdfnam (lunitp, cdummy, fnamep, nfilesp, 2, 1, .false.)
                call report_date_time  (lunitp(2))
                call rdlgri (nfilesp, lunitp, fnamep)
                call rdccol (nmaxp, mmaxp, lunitp(5), fnamep(5), &
                        lgrid2, xb, yb, lunitp(2))

                nolayp = layt
                call rdpart (lunitp(1), lunitp(2), fnamep(1))
                write (file_unit, '(A,i3,A)') &
                        ' The following ', nosubs, ' DELPAR substances are added as passive substances to DELWAQ.'
                do i = 1, nosubs
                    write (file_unit, '(i4,2x,a)') notot + i, substi(i)
                    write (file_unit_list(2)) substi(i)
                enddo
                nototp = nosubs
                notot = notot + nototp
                call exit_alloc (i2)
            endif

            if (ierr2 == 0) then
                if (gettoken(cdummy, idummy, itype, ierr2) > 0) goto 30
            else                                                        ! it was no keyword but a time string
                call convert_string_to_time_offset (cdummy, itstrt, .false., .false., ierr2)  ! returns start time in seconds since time offset,
                if (itstrt == -999) then                             ! max is about 68 year since time offset
                    write (file_unit, 2140) trim(cdummy)
                    goto 30
                endif
                if (ierr2 > 0) then
                    write (file_unit, '(/,A)') ' ERROR: String is not recognised as a keyword and' // &
                            ' it is not a valid absolute timer :' // &
                            trim(cdummy)
                    goto 30
                endif
                itype = 0                                                !  no further keywords expected
            endif
        enddo
        if (itype == 2) then
            itstrt = idummy                                             !  it was an integer for start time
            call convert_relative_time (itstrt, 1, is_date_format, is_yyddhh_format)    !  convert it to seconds
        endif
        if (.not. alone) then
            if (itstrt /= itstrtp) then
                write (file_unit, '(/, A, I10, A, I10)') &
                        ' ERROR: DELWAQ start time:', itstrt, ' not equal to DELPAR start time:', itstrtp
                call status%increase_error_count()
            endif
        endif

        !        Read stop time of integration

        if (gettoken(cdummy, idummy, itype, ierr2) > 0) goto 30
        if (itype == 1) then                                       !  a time string
            call convert_string_to_time_offset (cdummy, itstop, .false., .false., ierr2)
            if (itstop == -999) then
                write (file_unit, 2140) trim(cdummy)
                goto 30
            endif
            if (ierr2 > 0) then
                write (file_unit, *) ' ERROR: String is not recognised as a keyword and' // &
                        ' it is not a valid absolute timer :' // &
                        trim(cdummy)
                goto 30
            endif
        else                                                           !  an integer for stop time
            itstop = idummy
            call convert_relative_time (itstop, 1, is_date_format, is_yyddhh_format)
        endif
        if (.not. alone) then
            if (itstop /= itstopp) then
                write (file_unit, '(/,A, I10, A, I10)') &
                        ' ERROR: DELWAQ stop time :', itstop, ' not equal to DELPAR stop time :', itstopp
                call status%increase_error_count()
            endif
        endif
        if (itstrt < 0) then
            write (file_unit, '(A, I10, A, /, A)') &
                    ' ERROR: Start time (', itstrt, ') absolute timer is less than zero' // &
                    ' or auxiliary timer is set before T0.', &
                    '        This is not supported!'
            call status%increase_error_count()
        endif
        if (itstrt > itstop) then
            write (file_unit, '(A, I10, A, I10, A)') &
                    ' ERROR, Stop time (', itstop, &
                    ') smaller than start time(', itstrt, ').'
            call status%increase_error_count()
        endif
        if (is_date_format) then
            write (file_unit, '(A, A, /, A, A)') &
                    ' Start of simulation :', get_formatted_date_time(itstrt), &
                    ' End of simulation   :', get_formatted_date_time(itstop)
        else
            write (file_unit, '(A, I10, /, A, I10)') &
                    ' Start of simulation :        ', itstrt, &
                    ' End of simulation   :        ', itstop
        endif
        if ((intsrt >  5 .and. intsrt < 10).or. &           ! stationary solvers
                intsrt == 17 .or.  intsrt == 18) then
            idt = itstop - itstrt
            if (intsrt == 8 .or.  intsrt ==  9) then
                if (gettoken(itstop, ierr2) > 0) goto 30
                if (gettoken(imstop, ierr2) > 0) goto 30
                write (file_unit, '(A, I10, /, A, I2, A)') &
                        ' Maximum number of iterations:', itstop, &
                        ' Stop iteration after rel. difference smaller than' // &
                                ' 1.0E-', imstop, '.'
            endif
            goto 20
        endif

        !        Now the time step size

        if (gettoken(itype, ierr2) > 0) goto 30
        write (file_unit, '(A, I2)') ' Selected option for time step size : ', itype
        select case (itype)
            ! constant time step
        case (0)
            if (gettoken(idt, ierr2) > 0) goto 30
            if (is_date_format) then
                call convert_relative_time (idt, 1, is_date_format, is_yyddhh_format)
                write (file_unit, *) ' Integration time stepsize is :' // get_formatted_date_time(idt)
            else
                write (file_unit, '(A, I9)') ' Integration time stepsize is :', idt
            endif
            if (idt <= 0) then
                write (file_unit, '(/, A ,I8)') 'ERROR, constant time step must be greater than 0:', idt
                call status%increase_error_count()
                call terminate_execution(1)
            endif
            if (.not. alone) then
                if (idt /= idelt) then
                    write (file_unit, '(/,A, I10, A, I10)') ' ERROR: DELWAQ time step :', idt, ' not equal to DELPAR time step :', idelt
                    call status%increase_error_count()
                endif
            endif

            ! time varying time step
        case (1)
            if (.not. alone) then
                write (file_unit, '(/,A)') &
                        ' ERROR: DELWAQ time step is variable. DELPAR does not support variable step sizes.'
                call status%increase_error_count()
            endif
            if (gettoken(num_records, ierr2) > 0) goto 30
            write (file_unit, '(A,i8)') ' Variable time step with number of breakpoints is ', num_records
            allocate (int_array(num_records * 2), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write (file_unit, '(/, A, I4)') ' ERROR. allocating memory for variable timestep:', ierr_alloc
                call status%increase_error_count()
                goto 30
            endif
            do k = 1, num_records * 2
                if (gettoken(int_array(k), ierr2) > 0) goto 30
            enddo
            nrftot (1) = 1
            nlines = nlines + 2
            npoins = npoins + 1 + 3
            write (file_unit_list(4)) -1, (0, k = 1, 3)

            if (is_date_format) then
                call convert_time_format (int_array, num_records * 2, 1, is_date_format, is_yyddhh_format)
            end if

            if (output_verbose_level >= 4) then
                write (file_unit, '(A,/)') ' Breakpoint          Timestep '

                if (is_date_format) then
                    do k = 1, num_records * 2, 2
                        write (file_unit, '(A, 3X, A)') &
                                get_formatted_date_time(int_array(k)), &
                                get_formatted_date_time(int_array(k + 1)) // '.'
                    end do
                else
                    write (file_unit, '(I10,10X,I10)') (int_array(k), k = 1, num_records * 2)
                end if
            else
                write (file_unit, *) ' Variable timestep. Information will be printed for ' // &
                        'output option 4 or higher !'
            endif

            if (int_array(1) > itstrt) then
                write (file_unit, '(/, A, I10, A, I10)') &
                        ' ERROR', int_array(1), ' larger than start time:', itstrt
                call status%increase_error_count()
            endif
            call open_waq_files  (file_unit_list(5), file_name_list(5), 5, 1, ioerr)
            do ibrk = 1, num_records * 2, 2
                write (file_unit_list(5)) int_array(ibrk), float (int_array(ibrk + 1))
                if (int_array(ibrk + 1) <= 0) then
                    write (file_unit, '(/, A, I10)') ' ERROR variable time step must not be smaller 0:', int_array(ibrk + 1)
                    call status%increase_error_count()
                    call terminate_execution(1)
                endif
                if (ibrk == 1) cycle
                if (int_array(ibrk) <= int_array(ibrk - 2)) then
                    write (file_unit, '(/, A, I10, A, I10, A)') &
                            ' ERROR', int_array(ibrk), ' smaller than ', int_array(ibrk - 2), ' descending order !'
                    call status%increase_error_count()
                endif
            enddo
            close (file_unit_list(5))
            ! option not implemented
        case default
            write (file_unit, *) ' ERROR !!!! This option is not implemented !!!'
            call status%increase_error_count()
        end select

        !     Copy timers data to dlwqt0_data
        simulation_start_time_scu = itstrt
        simulation_stop_time_scu = itstop

        !     Read monitoring area's

        20 nodump = 0
        nullify(duname)
        !             new input processssing

        ierr2 = 0
        call read_monitoring_areas (file_unit_list, file_name_list, filtype, duname, nsegdmp, &
                isegdmp, dmpbal, ndmpar, ntdmps, output_verbose_level, &
                ierr2, status)
        if (ierr2 /= 0) goto 30

        if (ndmpar > 0) then
            write (file_unit_list(2)) (duname(k), k = 1, ndmpar)
            write (file_unit_list(2)) (dmpbal(k), k = 1, ndmpar)
        endif
        if (associated(duname)) deallocate(duname)
        if (associated(dmpbal)) deallocate(dmpbal)

        !     Read transects

        ierr2 = 0
        nullify(raname)
        call read_monitoring_transects (file_unit_list, file_name_list, filtype, raname, nexcraai, &
                iexcraai, ioptraai, noraai, ntraaq, output_verbose_level, &
                ierr2, status)
        if (ierr2 /= 0) goto 30
        if (noraai > 0 .and. ibflag == 0) then
            write (file_unit, '(/, A, /, A)') &
                    ' WARNING: Transects used without balance option specified,', &
                    ' Balances automaticaly switched on!'
            iwar2 = iwar2 + 1
            intopt = ibset(intopt, 3)
            intopt = ibset(intopt, 4)
        endif
        if (noraai > 0 .and. status%ierr == 0) then
            write (file_unit_list(2)) (raname(k), k = 1, noraai)
        endif
        if (associated(raname)) deallocate(raname)


        !       Read timings

        if ((intsrt <  6 .or. intsrt > 9) .and. &
                (intsrt < 17 .or. intsrt > 18)) then
            call timer  (is_date_format, imstrt, imstop, imstep, 1, is_yyddhh_format, ierr2)
            if (ierr2 > 0) goto 30

            call timer  (is_date_format, idstrt, idstop, idstep, 2, is_yyddhh_format, ierr2)
            if (ierr2 > 0) goto 30

            call timer  (is_date_format, ihstrt, ihstop, ihstep, 3, is_yyddhh_format, ierr2)
            if (ierr2 > 0) goto 30
        endif
        ierr2 = 0

        call validate_time_settings(file_unit, status, &
                itstrt, itstop, idt, &
                imstrt, imstop, imstep, &
                idstrt, idstop, idstep, &
                ihstrt, ihstop, ihstep)


        !        Check number of data in inputfile

        30 continue
        if (ierr2 /= 0) then
            call status%increase_error_count()
        end if
        if (ierr2 == 3) call terminate_execution(1)
        call check_error(cdummy, iwidth, 2, ierr2, status)
        call status%increase_warning_count_with(iwar2)
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2040 format (/, ' The auxiliary timer is not equal to the system timer.', &
                /' All the process fluxes will be scaled from the', &
                ' auxiliary timer to the system timer.')
        2060 format (/' ERROR Auxiliary timer format not recognised :', A)
        2140 format (/' ERROR: Absolute timer does not fit in timer format :', A, / &
                ' Is your T0 setting in block #1 correct?'/, &
                ' Allowed difference with T0 is usually ca. 68 years.')

    end subroutine read_block_2_from_input

    function get_formatted_date_time(date_time) result(date_time_str)

        integer(kind = int_wp), intent(in) :: date_time !< date time as int
        character(:), allocatable :: date_time_str !< Formatted date_time as string
        character(len = 100) :: buffer

        write(buffer, '(I2,A,I3,A,I2,A,I2,A,I2,A)') &
                date_time / 31536000, 'Y-', &
                mod(date_time, 31536000) / 86400, 'D-', &
                mod(date_time, 86400) / 3600, 'H-', &
                mod(date_time, 3600) / 60, 'M-', &
                mod(date_time, 60), 'S'

        date_time_str = trim(buffer)

    end function get_formatted_date_time

end module m_block_2_input_reader
