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

    subroutine read_block_2_from_input (lun, lchar, filtype, nrftot, nlines, &
            npoins, dtflg1, dtflg2, nodump, iopt, &
            noint, iwidth, dtflg3, ndmpar, ntdmps, &
            noraai, ntraaq, nosys, notot, nototp, &
            ioutpt, nsegdmp, isegdmp, nexcraai, &
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

        use m_conver
        use m_check
        use m_report_date_time
        use m_rdpart
        use m_rdlgri
        use m_rdfnam
        use m_rdccol
        use m_srstop
        use m_open_waq_files
        use rd_token     !   for the reading of tokens
        use partmem      !   for PARTicle tracking
        use fileinfo     !   a filename array in PART
        use alloc_mod
        use timers       !   performance timers
        use m_sysi          ! Timer characteristics
        use date_time_utils, only : convert_string_to_time_offset, simulation_start_time_scu, simulation_stop_time_scu, &
                convert_relative_time
        use m_waq_timer
        use m_rearaa
        use m_readmp
        use m_dlwq0i
        use m_time_validation

        !     parameters

        !     kind           function         name                Descriptipon

        character(*), intent(inout), dimension(*) :: lchar !< array with file names of the files

        integer(kind = int_wp), intent(inout), dimension(*) :: lun      !< array with unit numbers
        integer(kind = int_wp), intent(inout), dimension(*) :: filtype  !< type of binary file
        integer(kind = int_wp), intent(inout), dimension(*) :: nrftot   !< number of function items
        integer(kind = int_wp), intent(in), dimension(*) :: iopt     !< array with valid integration options
        integer(kind = int_wp), dimension(:), pointer :: nsegdmp  !< number of volumes in this monitoring area
        integer(kind = int_wp), dimension(:), pointer :: isegdmp  !< computational volume numbers
        integer(kind = int_wp), dimension(:), pointer :: nexcraai !< number of exchanges in this monitoring transect
        integer(kind = int_wp), dimension(:), pointer :: iexcraai !< exchange area numbers of the transect
        integer(kind = int_wp), dimension(:), pointer :: ioptraai !< option for the transects
        integer(kind = int_wp), intent(inout) :: nlines   !< cumulative record  space
        integer(kind = int_wp), intent(inout) :: npoins   !< cumulative pointer space
        integer(kind = int_wp), intent(out) :: nodump   !< number of monitoring points output
        integer(kind = int_wp), intent(in) :: noint    !< dimension of iopt
        integer(kind = int_wp), intent(in) :: iwidth   !< width of the output file
        integer(kind = int_wp), intent(out) :: ndmpar   !< number of dump areas
        integer(kind = int_wp), intent(out) :: ntdmps   !< total number segments in dump area
        integer(kind = int_wp), intent(out) :: noraai   !< number of raaien
        integer(kind = int_wp), intent(out) :: ntraaq   !< total number of exch. in raaien
        integer(kind = int_wp), intent(in) :: nosys    !< number of transported substances
        integer(kind = int_wp), intent(inout) :: notot    !< total number of substances
        integer(kind = int_wp), intent(out) :: nototp   !< notot inclusive of partcle substances
        integer(kind = int_wp), intent(in) :: ioutpt   !< flag for more or less output

        logical, intent(out) :: dtflg1 !< 'date'-format 1st timescale
        logical, intent(out) :: dtflg2 !< 'date'-format 2nd timescale
        logical, intent(out) :: dtflg3 !< 'date'-format (F;ddmmhhss,T;yydddhh)

        type(error_status) :: status !< current error status

        ! local

        character(20), dimension(:), pointer :: duname
        character(20), dimension(:), pointer :: raname
        character(255) :: cdummy !  help variables to read tokens
        character(8) :: date1  !  help variables to read date strings
        character(8) :: date2  !  help variables to read date strings

        integer(kind = int_wp), dimension(:), pointer :: dmpbal
        integer(kind = int_wp), dimension(:), allocatable :: iar        !  integer workspace
        integer(kind = int_wp) :: ierr2      !  local error and warning variables
        integer(kind = int_wp) :: iwar2      !  local error and warning variables
        integer(kind = int_wp) :: i          !  loop counters
        integer(kind = int_wp) :: k          !  loop counters
        integer(kind = int_wp) :: i2         !  loop counters
        integer(kind = int_wp) :: ibrk       !  loop counters
        integer(kind = int_wp) :: nobrk      !  number of breakpoints
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

        dtflg1 = .false.
        dtflg2 = .false.
        dtflg3 = .false.
        isflag = 0
        iposr = 0
        ierr2 = 0
        iwar2 = 0
        alone = .true.      ! not coupled with Delpar
        lchar(45) = ' '      ! so no Delpar input file

        !     There should be at least one substance in the input.

        if (notot < 1) then
            write (lunut, '(/, A)') ' ERROR: No substances have been specified.'
            call srstop(1)
        endif

        !        Read timers

        if (gettoken(itfact, ierr2) > 0) goto 30
        if (gettoken(date1, ierr2) > 0) goto 30
        if (gettoken(date2, ierr2) > 0) goto 30
        write (lunut, '(/,/,A, I10)') ' Factor between two clocks :  ', itfact

        !        Day string for date1                                              for date2:

        if (date1 == 'DDHHMMSS' .or. date1 == 'ddhhmmss') then
            dtflg1 = .true.
            isflag = 1
            write (lunut, *)' System clock in date-format  DDHHMMSS '
            if (date2 == 'DDHHMMSS' .or. date2 == 'ddhhmmss') then    ! allowed
                dtflg2 = .true.
                write (lunut, *) ' Auxiliary timer in date-format DDHHMMSS '
                if (itfact /= 1) write (lunut, 2040)
            elseif (date2 == 'YYDDDHH'.or. date2 =='yydddhh') then    ! not allowed
                write (lunut, '(/, A, /, A, /, A)') &
                        ' ERROR !!!! Auxiliary timer YYDDDHH-format ! ****', &
                        ' This option is invalid in combination with ', &
                        ' system clock in DDHHMMSS format !'
                call status%increase_error_count()
            elseif (date2 /= ' ') then                                  ! or blank
                write (lunut, 2060) date2
                call status%increase_error_count()
            endif

            !        Year string for date1

        elseif (date1 == 'YYDDDHH' .or. date1 == 'yydddhh') then
            dtflg1 = .true.
            dtflg3 = .true.
            isflag = 1
            write (lunut, *) ' System clock in date-format  YYDDDHH '
            if (date2 == 'YYDDDHH' .or. date2 == 'yydddhh') then      ! allowed
                dtflg2 = .true.
                write (lunut, *) ' Auxiliary timer in date-format YYDDDHH '
                if (itfact /= 1) write (lunut, 2040)
            elseif (date2 =='DDHHMMSS' .or. date2 =='ddhhmmss') then  ! not allowed
                write (lunut, '(/, A, /, A, /, A)') &
                        ' ERROR !!!! Auxiliary timer DDHHMMSS-format ! ****', &
                        ' This option is invalid in combination with ', &
                        ' system clock in YYDDDHH format !'
                call status%increase_error_count()
            elseif (date2 /= ' ') then                                  ! or blank
                write (lunut, 2060) date2
                call status%increase_error_count()
            endif

            !        or blank for date1

        elseif (date1 /= ' ') then
            write (lunut, '(/,A)') ' ERROR System timer format not recognised :' // date1
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
        do i = 1, noint
            if (int(10. * aint) == iopt(i)) goto 10
        enddo
        write (lunut, '(/,A)') ' ERROR !!!! Invalid integration option ! ****'
        call status%increase_error_count()
        10 write (lunut, '(/,A, F5.2)') ' Integration option :               ', aint

        !     No in-active substances for fast solver steady state

        if (notot /= nosys .and. (intsrt == 17 .or. intsrt == 18)) then
            call status%increase_error_count()
            write (lunut, '(/,A)') ' ERROR No in-active substances allowed for' // &
                    ' fast-solver steady state scheme'
        endif

        !        Read optional keywords and start time of integration

        nototp = 0
        if (gettoken(cdummy, idummy, itype, ierr2) > 0) goto 30
        do while (itype == 1)                                      ! read a collection of tokens
            call dlwq0i (cdummy, intopt, lunut, ierr2)
            if (btest(intopt, 17) .and. alone) then
                alone = .false.

                !                Delpar in Delwaq

                if (gettoken(cdummy, ierr2) > 0) goto 30          ! get the input file name for particles
                lchar(45) = cdummy
                call rdfnam (lunitp, cdummy, fnamep, nfilesp, 2, 1, .false.)
                call report_date_time  (lunitp(2))
                call rdlgri (nfilesp, lunitp, fnamep)
                call rdccol (nmaxp, mmaxp, lunitp(5), fnamep(5), &
                        lgrid2, xb, yb, lunitp(2))

                nolayp = layt
                call rdpart (lunitp(1), lunitp(2), fnamep(1))
                write (lunut, '(A,i3,A)') &
                        ' The following ', nosubs, ' DELPAR substances are added as passive substances to DELWAQ.'
                do i = 1, nosubs
                    write (lunut, '(i4,2x,a)') notot + i, substi(i)
                    write (lun(2)) substi(i)
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
                    write (lunut, 2140) trim(cdummy)
                    goto 30
                endif
                if (ierr2 > 0) then
                    write (lunut, '(/,A)') ' ERROR: String is not recognised as a keyword and' // &
                            ' it is not a valid absolute timer :' // &
                            trim(cdummy)
                    goto 30
                endif
                itype = 0                                                !  no further keywords expected
            endif
        enddo
        if (itype == 2) then
            itstrt = idummy                                             !  it was an integer for start time
            call convert_relative_time (itstrt, 1, dtflg1, dtflg3)    !  convert it to seconds
        endif
        if (.not. alone) then
            if (itstrt /= itstrtp) then
                write (lunut, '(/, A, I10, A, I10)') &
                        ' ERROR: DELWAQ start time:', itstrt, ' not equal to DELPAR start time:', itstrtp
                call status%increase_error_count()
            endif
        endif

        !        Read stop time of integration

        if (gettoken(cdummy, idummy, itype, ierr2) > 0) goto 30
        if (itype == 1) then                                       !  a time string
            call convert_string_to_time_offset (cdummy, itstop, .false., .false., ierr2)
            if (itstop == -999) then
                write (lunut, 2140) trim(cdummy)
                goto 30
            endif
            if (ierr2 > 0) then
                write (lunut, *) ' ERROR: String is not recognised as a keyword and' // &
                        ' it is not a valid absolute timer :' // &
                        trim(cdummy)
                goto 30
            endif
        else                                                           !  an integer for stop time
            itstop = idummy
            call convert_relative_time (itstop, 1, dtflg1, dtflg3)
        endif
        if (.not. alone) then
            if (itstop /= itstopp) then
                write (lunut, '(/,A, I10, A, I10)') &
                        ' ERROR: DELWAQ stop time :', itstop, ' not equal to DELPAR stop time :', itstopp
                call status%increase_error_count()
            endif
        endif
        if (itstrt < 0) then
            write (lunut, '(A, I10, A, /, A)') &
                    ' ERROR: Start time (', itstrt, ') absolute timer is less than zero' // &
                    ' or auxiliary timer is set before T0.', &
                    '        This is not supported!'
            call status%increase_error_count()
        endif
        if (itstrt > itstop) then
            write (lunut, '(A, I10, A, I10, A)') &
                    ' ERROR, Stop time (', itstop, &
                    ') smaller than start time(', itstrt, ').'
            call status%increase_error_count()
        endif
        if (dtflg1) then
            write (lunut, '(A, A, /, A, A)') &
                    ' Start of simulation :', get_formatted_date_time(itstrt), &
                    ' End of simulation   :', get_formatted_date_time(itstop)
        else
            write (lunut, '(A, I10, /, A, I10)') &
                    ' Start of simulation :        ', itstrt, &
                    ' End of simulation   :        ', itstop
        endif
        if ((intsrt >  5 .and. intsrt < 10).or. &           ! stationary solvers
                intsrt == 17 .or.  intsrt == 18) then
            idt = itstop - itstrt
            if (intsrt == 8 .or.  intsrt ==  9) then
                if (gettoken(itstop, ierr2) > 0) goto 30
                if (gettoken(imstop, ierr2) > 0) goto 30
                write (lunut, '(A, I10, /, A, I2, A)') &
                        ' Maximum number of iterations:', itstop, &
                        ' Stop iteration after rel. difference smaller than' // &
                                ' 1.0E-', imstop, '.'
            endif
            goto 20
        endif

        !        Now the time step size

        if (gettoken(itype, ierr2) > 0) goto 30
        write (lunut, '(A, I2)') ' Selected option for time step size : ', itype
        select case (itype)
            ! constant time step
        case (0)
            if (gettoken(idt, ierr2) > 0) goto 30
            if (dtflg1) then
                call convert_relative_time (idt, 1, dtflg1, dtflg3)
                write (lunut, *) ' Integration time stepsize is :' // get_formatted_date_time(idt)
            else
                write (lunut, '(A, I9)') ' Integration time stepsize is :', idt
            endif
            if (idt <= 0) then
                write (lunut, '(/, A ,I8)') 'ERROR, constant time step must be greater than 0:', idt
                call status%increase_error_count()
                call srstop(1)
            endif
            if (.not. alone) then
                if (idt /= idelt) then
                    write (lunut, '(/,A, I10, A, I10)') ' ERROR: DELWAQ time step :', idt, ' not equal to DELPAR time step :', idelt
                    call status%increase_error_count()
                endif
            endif

            ! time varying time step
        case (1)
            if (.not. alone) then
                write (lunut, '(/,A)') &
                        ' ERROR: DELWAQ time step is variable. DELPAR does not support variable step sizes.'
                call status%increase_error_count()
            endif
            if (gettoken(nobrk, ierr2) > 0) goto 30
            write (lunut, '(A,i8)') ' Variable time step with number of breakpoints is ', nobrk
            allocate (iar(nobrk * 2), stat = ierr_alloc)
            if (ierr_alloc /= 0) then
                write (lunut, '(/, A, I4)') ' ERROR. allocating memory for variable timestep:', ierr_alloc
                call status%increase_error_count()
                goto 30
            endif
            do k = 1, nobrk * 2
                if (gettoken(iar(k), ierr2) > 0) goto 30
            enddo
            nrftot (1) = 1
            nlines = nlines + 2
            npoins = npoins + 1 + 3
            write (lun(4)) -1, (0, k = 1, 3)

            if (dtflg1) then
                call conver (iar, nobrk * 2, 1, dtflg1, dtflg3)
            end if

            if (ioutpt >= 4) then
                write (lunut, '(A,/)') ' Breakpoint          Timestep '

                if (dtflg1) then
                    do k = 1, nobrk * 2, 2
                        write (lunut, '(A, 3X, A)') &
                                get_formatted_date_time(iar(k)), &
                                get_formatted_date_time(iar(k + 1)) // '.'
                    end do
                else
                    write (lunut, '(I10,10X,I10)') (iar(k), k = 1, nobrk * 2)
                end if
            else
                write (lunut, *) ' Variable timestep. Information will be printed for ' // &
                        'output option 4 or higher !'
            endif

            if (iar(1) > itstrt) then
                write (lunut, '(/, A, I10, A, I10)') &
                        ' ERROR', iar(1), ' larger than start time:', itstrt
                call status%increase_error_count()
            endif
            call open_waq_files  (lun(5), lchar(5), 5, 1, ioerr)
            do ibrk = 1, nobrk * 2, 2
                write (lun(5)) iar(ibrk), float (iar(ibrk + 1))
                if (iar(ibrk + 1) <= 0) then
                    write (lunut, '(/, A, I10)') ' ERROR variable time step must not be smaller 0:', iar(ibrk + 1)
                    call status%increase_error_count()
                    call srstop(1)
                endif
                if (ibrk == 1) cycle
                if (iar(ibrk) <= iar(ibrk - 2)) then
                    write (lunut, '(/, A, I10, A, I10, A)') &
                            ' ERROR', iar(ibrk), ' smaller than ', iar(ibrk - 2), ' descending order !'
                    call status%increase_error_count()
                endif
            enddo
            close (lun(5))
            ! option not implemented
        case default
            write (lunut, *) ' ERROR !!!! This option is not implemented !!!'
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
        call readmp (lun, lchar, filtype, duname, nsegdmp, &
                isegdmp, dmpbal, ndmpar, ntdmps, ioutpt, &
                ierr2, status)
        if (ierr2 /= 0) goto 30

        if (ndmpar > 0) then
            write (lun(2)) (duname(k), k = 1, ndmpar)
            write (lun(2)) (dmpbal(k), k = 1, ndmpar)
        endif
        if (associated(duname)) deallocate(duname)
        if (associated(dmpbal)) deallocate(dmpbal)

        !     Read transects

        ierr2 = 0
        nullify(raname)
        call rearaa (lun, lchar, filtype, raname, nexcraai, &
                iexcraai, ioptraai, noraai, ntraaq, ioutpt, &
                ierr2, status)
        if (ierr2 /= 0) goto 30
        if (noraai > 0 .and. ibflag == 0) then
            write (lunut, '(/, A, /, A)') &
                    ' WARNING: Transects used without balance option specified,', &
                    ' Balances automaticaly switched on!'
            iwar2 = iwar2 + 1
            intopt = ibset(intopt, 3)
            intopt = ibset(intopt, 4)
        endif
        if (noraai > 0 .and. status%ierr == 0) then
            write (lun(2)) (raname(k), k = 1, noraai)
        endif
        if (associated(raname)) deallocate(raname)


        !       Read timings

        if ((intsrt <  6 .or. intsrt > 9) .and. &
                (intsrt < 17 .or. intsrt > 18)) then
            call timer  (dtflg1, imstrt, imstop, imstep, 1, dtflg3, ierr2)
            if (ierr2 > 0) goto 30

            call timer  (dtflg1, idstrt, idstop, idstep, 2, dtflg3, ierr2)
            if (ierr2 > 0) goto 30

            call timer  (dtflg1, ihstrt, ihstop, ihstep, 3, dtflg3, ierr2)
            if (ierr2 > 0) goto 30
        endif
        ierr2 = 0

        call validate_time_settings(lunut, status, &
                itstrt, itstop, idt, &
                imstrt, imstop, imstep, &
                idstrt, idstop, idstep, &
                ihstrt, ihstop, ihstep)


        !        Check number of data in inputfile

        30 continue
        if (ierr2 /= 0) then
            call status%increase_error_count()
        end if
        if (ierr2 == 3) call srstop(1)
        call check  (cdummy, iwidth, 2, ierr2, status)
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
