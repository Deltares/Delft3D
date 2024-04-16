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
module m_block_1_input_reader
    use m_waq_precision
    use error_handling, only : check_error
    use m_srstop
    use m_read_version_number
    use time_module
    use m_error_status
    use rd_token     !   tokenized reading
    use timers       !   performance timers
    use m_string_utils, only : string_equals
    use date_time_utils, only : system_time_factor_seconds, base_julian_time, compute_reference_day

    implicit none

contains


    subroutine read_block_1_from_input (lun, syname, nosys, notot, nomult, &
            multp, iwidth, otime, isfact, refday, &
            output_verbose_level, status)

        !< Reads the model identification and substances IDs
        !> This routine reads:
        !>    - the first non tokenized line with line lengthes and comment character
        !>    - the version number of the input file
        !>    - the 3 40 character strings with model documentation
        !>    - a 4th 40 character strings with the optional absolute reference time
        !>    - number of transported and passive substances
        !>    - the substance names (need not necessarily be process library reserved names)
        !>      the names may end with *nn to indicate a multiple occurence of the substance

        !       Logical units     : lun(26) = unit user input file
        !                           lun(27) = unit stripped input file
        !                           lun(29) = unit formatted output file
        !                           lun( 2) = unit system-intermediate file

        ! Parameters

        !     Parameters

        character(20), dimension(:), pointer :: syname !< array with substance names

        integer(kind = int_wp), intent(in), dimension(*) :: lun    !< array with unit numbers
        integer(kind = int_wp), intent(out) :: output_verbose_level !< flag for more or less output
        integer(kind = int_wp), intent(out) :: isfact !< Units (in sec) of the system clock

        integer(kind = int_wp), intent(out) :: iwidth !< width of the output file
        integer(kind = int_wp), dimension(:, :), pointer :: multp !< multiple substance administration
        integer(kind = int_wp), intent(out) :: nomult !< number of multiple substances
        integer(kind = int_wp), intent(out) :: nosys  !< number of transported substances
        integer(kind = int_wp), intent(out) :: notot  !< total number of substances
        integer(kind = int_wp), intent(out) :: refday !< reference day, varying from 1 till 365

        real(kind = dp), intent(out) :: otime !< Offset of the system time (Julian)

        type(error_status), intent(inout) :: status

        !     Local

        character(255) :: cdummy !  character read help variable
        character(40) :: modid1 !  model identification strings
        character(40) :: modid2 !  model identification strings
        character(40) :: runid1 !  model identification strings
        character(40) :: runid2 !  model identification strings
        character(20), dimension(:), allocatable :: sname  !  help array for substance names

        integer(kind = int_wp), dimension(:), allocatable :: imult  !  help array for number of substances
        integer(kind = int_wp) :: idate  !  date of the time offset
        integer(kind = int_wp) :: iday   !  day of the time offset
        integer(kind = int_wp) :: idummy !  integer   read help variable
        integer(kind = int_wp) :: ierr2  !  local error   accumulator
        integer(kind = int_wp) :: ifound !  help variable for name search
        integer(kind = int_wp) :: ihour  !  hour of the time offset
        integer(kind = int_wp) :: ilen   !  length help variable
        integer(kind = int_wp) :: iminut !  minute of the time offset
        integer(kind = int_wp) :: imonth !  month of the time offset
        integer(kind = int_wp) :: isecnd !  second of the time offset
        integer(kind = int_wp) :: isys   !  loop counters for substances
        integer(kind = int_wp) :: isys2  !  loop counters for substances
        integer(kind = int_wp) :: itime  !  time of the time offset
        integer(kind = int_wp) :: itype  !  input type  0 = any, 1 = char, 2 = int, 3 = float
        integer(kind = int_wp) :: iwar2  !  local warning accumulator
        integer(kind = int_wp) :: iyear  !  year of the time offset
        integer(kind = int_wp) :: nosyss !  help variable for transported substance
        integer(kind = int_wp) :: notots !  help variable for total substance
        integer(kind = int_wp) :: ithndl = 0

        logical :: intread !  flag for read of substance numbers

        real(kind = real_wp) :: adummy               !  real      read help variable
        real(kind = real_wp) :: input_version_number !  version number of this input

        if (timon) call timstrt("read_block_1_from_input", ithndl)

        !        Initialize the read stack, output unit and error and warning help variables

        ierr2 = 0
        iwar2 = 0

        !     First line not tokenized : meta data

        read (lun(26), *, end = 110, err = 120) npos, iwidth, cchar

        if (iwidth /= 80 .and. iwidth /= 132) then
            write (lunut, 2000) iwidth
            iwidth = 80
        endif

        if (iwidth ==  80) iwidth = 5
        if (iwidth == 132) iwidth = 10

        !     Read version number and initialize position on start of new line

        call read_version_number (ilun(1), lch(1), lunut, npos, input_version_number, output_verbose_level)
        call compare_version_number_to_lower_limit(input_version_number, lunut)

        iposr = 0

        !     Read model documentation strings

        modid1 = ' '
        modid2 = ' '
        runid1 = ' '
        runid2 = ' '
        itype = 1
        if (gettoken (modid1, ierr2) > 0) goto 100
        if (gettoken (modid2, ierr2) > 0) goto 100
        write (lun(2)) modid1, modid2
        write (lunut, 2010) modid1, modid2
        if (gettoken (runid1, ierr2) > 0) goto 100
        if (gettoken (runid2, ierr2) > 0) goto 100
        write (lun(2)) runid1, runid2
        write (lunut, 2020) runid1, runid2

        !     identify an timer offset value in the last string

        if (runid2(1:3) /= 't0: ' .and. &
                runid2(1:3) /= 'T0: ' .and. &
                runid2(1:3) /= 't0= ' .and. &
                runid2(1:3) /= 'T0= ') then
            write (lunut, 2030)
            call status%increase_warning_count()
            isfact = 1
            otime = 0.0d+00
        else
            write (lunut, 2040)
            read (runid2(5:8), '(i4)') iyear
            read (runid2(10:11), '(i2)') imonth
            read (runid2(13:14), '(i2)') iday
            read (runid2(16:17), '(i2)') ihour
            read (runid2(19:20), '(i2)') iminut
            read (runid2(22:23), '(i2)') isecnd
            read (runid2(31:38), '(i8)') isfact
            idate = iyear * 10000 + imonth * 100 + iday
            itime = ihour * 10000 + iminut * 100 + isecnd
            otime = julian_with_leapyears (idate, itime)
            if (isfact > 0) then
                write (lunut, 2050)  isfact
            else
                write (lunut, 2060) -isfact
            endif
        endif

        !     Compute refday
        call compute_reference_day(iyear, imonth, iday, refday)

        !     Copy timers data to dlwqt0_data
        base_julian_time = otime
        system_time_factor_seconds = isfact

        !     Read number of transported and number of passive systems

        if (gettoken (nosys, ierr2) > 0) goto 100
        if (gettoken (idummy, ierr2) > 0) goto 100
        notot = nosys + idummy

        !     allocate

        allocate (sname(notot), imult(notot))
        sname = ' '
        imult = 1

        !        Read system numbers, identification and optional multiplication factor

        intread = .false.
        nomult = 0
        do isys = 1, notot
            if (.not. intread) then                 ! read substance number
                if (gettoken (idummy, ierr2) > 0) goto 100
            endif                                     ! read substanceID
            if (gettoken (cdummy, ierr2) > 0) goto 100
            if (idummy <= 0 .or. idummy > notot) then
                write (lunut, 2070) idummy, cdummy
                call status%increase_error_count()
            else
                sname(idummy) = cdummy
            endif                                     ! read new substance nr or *n for multiples
            ifound = gettoken (cdummy, idummy, adummy, itype, ierr2)
            if (isys == notot .and. ifound == 2) exit
            if (ifound /= 0) goto 100
            if (itype == 1) then                  ! a string was found. must be *n
                if (cdummy(1:1) /= '*') then       ! only a multiplication is accepted
                    write(lunut, 2170) trim(cdummy)
                    goto 100
                endif
                read (cdummy(2:), *) imult(isys)
                nomult = nomult + 1
                intread = .false.
            else                                      ! an integer is found
                intread = .true.
            endif
        enddo

        !        Deal with multiple substances

        allocate (multp(2, nomult))
        nosyss = nosys
        notots = notot
        nosys = 0
        notot = 0
        nomult = 1
        do isys = 1, nosyss
            if (imult(isys) > 1) then
                multp(1, nomult) = nosys + 1
                multp(2, nomult) = nosys + 1 + imult(isys) - 1
                nomult = nomult + 1
            endif
            nosys = nosys + imult(isys)
            notot = notot + imult(isys)
        enddo
        do isys = nosyss + 1, notots
            if (imult(isys) > 1) then
                multp(1, nomult) = notot + 1
                multp(2, nomult) = notot + 1 + imult(isys) - 1
                nomult = nomult + 1
            endif
            notot = notot + imult(isys)
        enddo
        allocate (syname(notot + nomult))

        write (lunut, 2080) nosys, notot - nosys, notot
        if (nosys < 0 .or. idummy < 0 .or. notot <= 0) then
            write (lunut, 2090)
            call status%increase_error_count()
        endif

        !        Fill in their names

        if (output_verbose_level >= 1) then
            write (lunut, 2100)
        else
            write (lunut, 2110)
        endif
        nosyss = 1
        nomult = 0
        do isys = 1, notots

            !        Get an unique name

            if (sname(isys) == ' ') write (sname(isys), 2120) isys
            do isys2 = 1, isys - 1
                if (string_equals(sname(isys), sname(isys2))) then
                    write(lunut, 2130)
                    call status%increase_error_count()
                endif
            enddo

            !        Make the array with names

            if (imult(isys) == 1) then
                syname(nosyss) = sname(isys)
                if (output_verbose_level >= 1) write (lunut, 2140) nosyss, syname(nosyss)
                nosyss = nosyss + 1
            else
                nomult = nomult + 1
                syname(notot + nomult) = sname(isys)
                do isys2 = 1, imult(isys)
                    syname(nosyss) = sname(isys)
                    ilen = len_trim(syname(nosyss))
                    if (isys2 <   10) then
                        write (syname(nosyss)(ilen + 1:), '(''0'',i1)') isys2
                    elseif (isys2 < 100) then
                        write (syname(nosyss)(ilen + 1:), '(      i2)') isys2
                    else
                        write (syname(nosyss)(ilen + 1:), '(      i3)') isys2
                    endif
                    if (output_verbose_level >= 1) write (lunut, 2140) nosyss, syname(nosyss)
                    nosyss = nosyss + 1
                enddo
            endif
        enddo

        !        Watch out !! in dlwq02.f subsequently the names of the particle tracking
        !                     substances are written to lun(2)

        write (lun(2)) (syname(isys), isys = 1, notot)

        !        Check number of data in inputfile

        100 continue
        if (ierr2 /= 0 .and. ierr2 /= 2) call status%increase_error_count()
        if (ierr2 == 3) call srstop(1)
        call check_error  (cdummy, iwidth, 1, ierr2, status)
        call status%increase_warning_count_with(iwar2)
        if (timon) call timstop(ithndl)
        return

        110 write (lunut, 2150) ilun(1), lch(1)
        call srstop (1)
        return

        120 write (lunut, 2160) ilun(1), lch(1)
        call srstop (1)
        return

        !        Output formats

        2000 format (/' Invalid width of output file', I4, '! A width of 80 is assumed!')
        2010 format (//' Model :            ', A40, /20X, A40)
        2020 format (//' Run   :            ', A40, /20X, A40)
        2030 format (/' WARNING. No intermediate version absolute timer detected !', &
                /'          Use of date/time is not supported - date/time strings', &
                /'          may not be interpreted correctly')
        2040 format (/' Intermediate version absolute timer detected !')
        2050 format (' System clock is ', I5, ' seconds !')
        2060 format (' System clock is 1/', I3, 'th of a second !')
        2070 format (/' ERROR invalid system number:', I3, 5X, A20)
        2080 format (/' Number of active constituents   :', I3, / &
                ' Number of inactive constituents :', I3, / &
                ' Total number of constituents    :', I3)
        2090 format (/' ERROR invalid number of systems.')
        2100 format (' Number   name')
        2110 format (' Information on substances will be printed for', &
                ' output option 1 or higher !')
        2120 format (' Substance ', I3)
        2130 format (/' ERROR. system name not unique')
        2140 format (I5, 5X, A20)
        2150 format (/' ERROR. End of file on unit:', I3, /' Filename = ', A)
        2160 format (/' ERROR reading file on unit:', I3, ' - first line invalid. Please check', /' Filename = ', A)
        2170 format (/' ERROR encountered invalid repeat count - should start with an asterisk (*): ', A)

    end

end module m_block_1_input_reader
