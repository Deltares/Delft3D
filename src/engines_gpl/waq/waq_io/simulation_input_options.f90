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
module simulation_input_options
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

    private
    public :: process_simulation_input_options, validate_simulation_time_steps, read_constant_data, &
            read_constants_time_variables

contains

    subroutine process_simulation_input_options(iopt1, file_unit_list, is, file_name_list, filtype, &
            is_date_format, is_yyddhh_format, nitem, ierr, status, &
            dont_read)

        !!  Processing of first input file option
        !!      - Get the file name
        !!      - Open the file
        !!      - If ASCII, push file-info on include stack
        !! LOGICAL UNITS:
        !!      - file_unit_list(33) = working unit for opening binary files

        use m_open_waq_files
        use timers       !   performance timers
        use rd_token
        use m_file_path_utils, only : extract_file_extension
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        integer(kind = int_wp), intent(in) :: iopt1           !< Input option
        integer(kind = int_wp), intent(inout) :: file_unit_list  (*)        !< DELWAQ Unit number array
        integer(kind = int_wp), intent(in) :: is              !< entry in file_unit_list for item
        character*(*), intent(inout) :: file_name_list(*)       !< IN/OUT  Filenames
        logical, intent(in) :: is_date_format         !< 'date'-format 1st time scale
        logical, intent(in) :: is_yyddhh_format         !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: nitem           !< nr of input items expected
        integer(kind = int_wp), intent(inout) :: filtype(*)      !< type of binary file
        integer(kind = int_wp), intent(inout) :: ierr            !< Local error flag
        logical, intent(in) :: dont_read   !! do not actually read tokens, if true, the information is already provided
        type(error_status), intent(inout) :: status !< current error status

        integer(kind = int_wp) :: extpos, extlen
        character(255)  cdummy   ! Work string
        character(255)  sfile, filext
        character(25)  sstring
        integer(kind = int_wp) :: ifl       ! help variable for stack size
        integer(kind = int_wp) :: ierr2     ! help variable for error handling
        integer(kind = int_wp) :: input_file     ! help variable for opening external ASCII file
        integer(kind = int_wp) :: nfil      ! nr of files in hydrodynamics steering file
        integer(kind = int_wp) :: intopt    ! interpolation option in hydrodynamics steering file
        integer(kind = int_wp) :: ifil      ! loop counter for number of files
        real(kind = real_wp) :: fact      ! interpolation factor steering file
        integer(kind = int_wp) :: it1, it2, it3    ! timer variables
        integer(kind = int_wp) :: it1a, it2a, it3a   ! timer variables
        integer(kind = int_wp) :: itype     ! returned type of input from gettoken
        integer(kind = int_wp) :: k         ! implicit loop counter
        real(kind = real_wp) :: adummy    ! dummy to read data from file
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("process_simulation_input_options", ithndl)

        !           See what type of file it is
        !           -4 the file is steering file for a series of binary files
        !           -2 the file is a binary file interpolated when needed
        !           -1 the file is an external ASCII file (superfluous because INCLUDE)
        !            0 the file is a binary file one record per time step
        !            1 the file is this input file

        select case (iopt1)

        case (-1)                      !    External ASCII file
            do ifil = 1, lstack
                if (ilun(ifil) /= 0) ifl = ifil
            enddo
            if (ifl == lstack) then   !    No space on the stack
                write (file_unit, 2010) lstack
                ierr2 = 1
                goto 30
            endif
            if (gettoken(cdummy, ierr2) > 0) goto 30     !   Get file name
            write (file_unit, 2020)  cdummy
            ifl = ifl + 1
            input_file = 800 + ifl
            call open_waq_files  (input_file, cdummy, 33, 1, ierr2)    !   Open the file
            if (ierr2 > 0) then
                ifl = ifl - 1
                write (file_unit, 2030)
            else
                lch (ifl) = cdummy
                ilun(ifl) = input_file
            endif

        case (-2, 0)                   !    External binairy intermediate file
            if (dont_read) then
                cdummy = file_name_list(is)
            else
                10          if (gettoken(cdummy, ierr2) > 0) goto 30     !   Get file name
                if (cdummy == 'UNFORMATTED') then
                    filtype(is) = filtype(is) + 10
                    write (file_unit, *) 'UNFORMATTED file detected'
                    goto 10
                endif
                if (cdummy == 'BIG_ENDIAN') then
                    filtype(is) = filtype(is) + 20
                    write (file_unit, *) 'BIG_ENDIAN  file detected'
                    goto 10
                endif
            endif
            file_name_list(is) = cdummy
            write (file_unit, 2040) cdummy
            !                   Check if file exists
            call open_waq_files  (file_unit_list(33), cdummy, 33, 2, ierr2)
            if (ierr2 > 0) then
                ierr2 = -2
            else
                close (file_unit_list(33))
            endif

        case (-4)                      !    ASCII steering file taylored to read .hyd files
            if (nitem == 0) then
                write (file_unit, 2000)
                ierr2 = 1
                goto 30
            endif
            itype = 1
            do while (itype == 1)
                if (gettoken(cdummy, nfil, itype, ierr2) > 0) goto 30  !   Get number of files
                if (itype == 1) then
                    if (cdummy == 'UNFORMATTED') then
                        filtype(is) = filtype(is) + 10
                        write (file_unit, *) 'UNFORMATTED file detected'
                    else if (cdummy == 'BIG_ENDIAN') then
                        filtype(is) = filtype(is) + 20
                        write (file_unit, *) 'BIG_ENDIAN  file detected'
                    else
                        write (file_unit, 2150) cdummy
                        ierr2 = 1
                        goto 30
                    endif
                endif
            enddo
            if (gettoken(intopt, ierr2) > 0) goto 30     !   Get interpolation option
            if (gettoken(sstring, ierr2) > 0) goto 30     !   Get file string
            ! Open the binary intermediate file for output
            call extract_file_extension(file_name_list(27), filext, extpos, extlen)
            file_name_list(is) = file_name_list(27)(1:max(1, (extpos - 1))) // '-' // sstring
            call extract_file_extension(file_name_list(is), filext, extpos, extlen)
            file_name_list(is)(extpos:) = '.wrk'
            call open_waq_files  (file_unit_list(is), file_name_list(is), 1, 1, ierr2)
            if (ierr2 > 0) then
                ierr2 = -2
                goto 30
            endif
            write(file_unit, 2120)  file_name_list(is), intopt
            write(file_unit_list(is))  'Steering file '
            write(file_unit_list(is))   nfil, intopt
            write (file_unit, 2080)
            do  ifil = 1, nfil
                if (gettoken(fact, ierr2) > 0) goto 30  !   Get multiplication factor
                if (gettoken(cdummy, it1, itype, ierr2) > 0) goto 30    ! 'from' time
                if (itype == 1) then
                    call convert_string_to_time_offset(cdummy, it1, .false., .false., ierr2)
                    if (ierr2 > 0) then
                        write (file_unit, 2130) trim(cdummy)
                        goto 30
                    endif
                    if (it1   == -999) then
                        write (file_unit, 2140) trim(cdummy)
                        ierr2 = 1
                        goto 30
                    endif
                else
                    call convert_relative_time (it1, 1, is_date_format, is_yyddhh_format)
                endif
                if (gettoken(cdummy, it2, itype, ierr2) > 0) goto 30    ! 'to' time
                if (itype == 1) then
                    call convert_string_to_time_offset(cdummy, it2, .false., .false., ierr2)
                    if (ierr2 > 0) then
                        write (file_unit, 2130) trim(cdummy)
                        goto 30
                    endif
                    if (it2   == -999) then
                        write (file_unit, 2140) trim(cdummy)
                        ierr2 = 1
                        goto 30
                    endif
                else
                    call convert_relative_time (it2, 1, is_date_format, is_yyddhh_format)
                endif
                if (gettoken(cdummy, it3, itype, ierr2) > 0) goto 30    ! 'step'
                if (itype == 1) then
                    call convert_string_to_time_offset(cdummy, it3, .false., .false., ierr2)
                    if (ierr2 > 0) then
                        write (file_unit, 2130) trim(cdummy)
                        goto 30
                    endif
                    if (it3   == -999) then
                        write (file_unit, 2140) trim(cdummy)
                        ierr2 = 1
                        goto 30
                    endif
                else
                    call convert_relative_time (it3, 1, is_date_format, is_yyddhh_format)
                endif
                if (gettoken(sfile, ierr2) > 0) then       !     Get file string
                    ierr2 = -1
                    goto 30
                endif
                call extract_file_extension(sfile, filext, extpos, extlen)
                if (string_equals('hyd ', filext)) then                            !     hyd file processing
                    call validate_simulation_time_steps (file_unit, sstring, sfile, cdummy, it3, &
                            it1a, it2a, it3a, nitem, ierr)
                else                                               !     other file processing
                    cdummy = sfile
                    it2a = 0
                    call open_waq_files  (file_unit_list(33), cdummy, 33, 2, ierr2)
                    if (ierr2 > 0) then
                        ierr2 = -2
                        goto 30
                    endif
                    read (file_unit_list(33)) it1a, (adummy, k = 1, nitem)
                    read (file_unit_list(33)) it3a
                    it3a = it3a - it1a
                    close (file_unit_list(33))
                endif
                write (file_unit_list(is)) fact, it1, it2, it3, cdummy, it1a, it2a, it3a
                write (file_unit, 2090) ifil, fact, &
                        it1 / 31536000, mod(it1, 31536000) / 86400, &
                        mod(it1, 86400) / 3600, mod(it1, 3600) / 60, &
                        mod(it1, 60), &
                        it2 / 31536000, mod(it2, 31536000) / 86400, &
                        mod(it2, 86400) / 3600, mod(it2, 3600) / 60, &
                        mod(it2, 60), &
                        it3 / 31536000, mod(it3, 31536000) / 86400, &
                        mod(it3, 86400) / 3600, mod(it3, 3600) / 60, &
                        mod(it3, 60), cdummy
                if (ierr /= 0) then
                    ierr2 = -1
                    exit
                endif
            end do
            close (file_unit_list(is))

        case (1)            !   continue reading from current file
            write (file_unit, 2050)
            ierr2 = 0

        case default
            write (file_unit, 2000)
            ierr2 = 1

        30 end select

        select case (ierr2)

        case (-2)
            write (file_unit, 2060) cdummy
            call status%increase_warning_count()
            ierr = 0

        case (-1)
            write (file_unit, 2100) cdummy
            ierr = 1

        case (0)
            ierr = 0

        case (1:)
            write (file_unit, 2070)
            ierr = 1

        end select

        if (timon) call timstop(ithndl)
        return

        2000 format (/' ERROR: option not implemented !!!')
        2010 format (/' ERROR: nr of include stack levels (', I2, ') exceeded !')
        2020 format (/' Including file: ', A)
        2030 format (/' ERROR: Include file does not exist !')
        2040 format (/' Information from unformatted intermediate file.', &
                /' Filename is: ', A)
        2050 format (' Information from the standard input file.')
        2060 format (' WARNING file does not exist.'/' Filename: ', A)
        2070 format (' ERROR reading input!')
        2080 format (/' Nr: Interpolation        From                  ', &
                ' To                  Start with      Hydro-dynamic', &
                /'        factor                                  ', &
                '                                     description file:')
        2090 format (I3, F10.3, 6X, I2, 'Y-', I3, 'D-', I2, 'H-', I2, 'M-', I2, 'S. ', &
                I2, 'Y-', I3, 'D-', I2, 'H-', I2, 'M-', I2, 'S. ', &
                I2, 'Y-', I3, 'D-', I2, 'H-', I2, 'M-', I2, 'S. ', A)
        2100 format (/' ERROR: Reading ASCII description file ', A)
        2120 format (' Work file = ', A, ' Interpolation option:', I2)
        2130 format (/' ERROR: String is not a valid absolute timer:', A)
        2140 format (/' ERROR: Absolute timer does not fit in timer format :', A, / &
                ' Is your T0 setting in block #1 correct?'/, &
                ' Allowed difference with T0 is usually ca. 68 years.')
        2150 format (/' ERROR: Not a valid token at this position: ', A)

    end subroutine process_simulation_input_options

    subroutine validate_simulation_time_steps(file_unit, sget, afile, bfile, istep, &
            it2, it3, it4, numbr, ierr)

        use waq_file_utils_external, only : get_filepath_and_pathlen
        use m_open_waq_files
        use m_timer_variables          ! timer characteristics
        use m_waq_precision
        use time_module

        integer(kind = int_wp) :: ilun, ierr, i, k, a, it2, it3, it4
        integer(kind = int_wp) :: iyear1, imonth1, iday1, ihour1, imin1, isec1
        integer(kind = int_wp) :: iyear2, imonth2, iday2, ihour2, imin2, isec2
        integer(kind = int_wp) :: iyear3, imonth3, iday3, ihour3, imin3, isec3
        integer(kind = int_wp) :: iyear4, imonth4, iday4, ihour4, imin4, isec4
        integer(kind = int_wp) :: idate, itime, itim, itim2, istep, idtf, file_unit
        integer(kind = int_wp) :: numbr

        character(len=25)  sget, s1
        character(len=255) afile, bfile

        real(kind = dp) :: reftim, starttim, stoptim, afact
        character(len=255) filpath
        integer(kind = int_wp) :: pathlen

        ! open the ascii .hyd file
        ilun = 148
        call open_waq_files  (ilun, afile, 33, 1, ierr)
        if (ierr > 0) then
            write (file_unit, 1000) afile
            return
        endif
        call get_filepath_and_pathlen(afile, filpath, pathlen)

        ! search for the file name of this item
        do i = 1, 10000
            read (ilun, *, end = 20) s1, bfile
            if (s1 == 'conversion-ref-time  ') &
                    read (bfile, '(i4,i2,i2,i2,i2,i2)') &
                            iyear1, imonth1, iday1, ihour1, imin1, isec1
            if (s1 == 'conversion-start-time') &
                    read (bfile, '(i4,i2,i2,i2,i2,i2)') &
                            iyear2, imonth2, iday2, ihour2, imin2, isec2
            if (s1 == 'conversion-stop-time ') &
                    read (bfile, '(i4,i2,i2,i2,i2,i2)') &
                            iyear3, imonth3, iday3, ihour3, imin3, isec3
            if (s1 == 'conversion-timestep  ') &
                    read (bfile, '(i4,i2,i2,i2,i2,i2)') &
                            iyear4, imonth4, iday4, ihour4, imin4, isec4
            if (s1 == sget) goto 30
        end do
        20 write (file_unit, 1010) sget
        ierr = 1
        return
        30 continue
        if (pathlen > 0) then
            bfile = filpath(1:pathlen) // bfile
        endif
        idate = iyear1 * 10000 + imonth1 * 100 + iday1
        itime = ihour1 * 10000 + imin1 * 100 + isec1
        reftim = julian_with_leapyears (idate, itime)
        idate = iyear2 * 10000 + imonth2 * 100 + iday2
        itime = ihour2 * 10000 + imin2 * 100 + isec2
        starttim = julian_with_leapyears (idate, itime)
        idate = iyear3 * 10000 + imonth3 * 100 + iday3
        itime = ihour3 * 10000 + imin3 * 100 + isec3
        stoptim = julian_with_leapyears (idate, itime)
        it4 = iyear4 * 31536000 + imonth4 * 2592000 + iday4 * 86400 + &
                ihour4 * 3600 + imin4 * 60 + isec4
        afact = isfact / 864.0d+02
        if (isfact < 0) afact = -1.0d+00 / isfact / 864.0d+02
        it2 = (starttim - reftim) / afact + 0.5
        it3 = (stoptim - reftim) / afact + 0.5
        close (ilun)

        ! open the binary file for this item
        call open_waq_files  (ilun, bfile, 33, 2, ierr)
        if (ierr > 0) then
            write (file_unit, 1020) bfile
            return
        endif

        ! find the time step in the file where to start
        read (ilun, end = 50) itim, (a, k = 1, abs(numbr))
        if (itim /= it2) then
            write (file_unit, 1030) it2, itim, bfile
            ierr = 1
            return
        endif
        read (ilun, end = 50) itim2, (a, k = 1, abs(numbr))
        idtf = itim2 - itim
        if (idtf /= it4) then
            write (file_unit, 1040) it4, idtf, bfile
            ierr = 1
            return
        endif
        if (((istep - itim) / idtf) * idtf == istep - itim) then
            close (ilun)
            return
        endif

        50 write (file_unit, 1050) istep, bfile
        ierr = 1
        return

        1000 format (/' ERROR: Opening ASCII coupling file: ', A)
        1010 format (/' ERROR: Search string not found: ', A)
        1020 format (/' ERROR: Opening binary file: ', A)
        1030 format (/' ERROR: Start time is not: ', I10, ' but: ', I10, &
                ' in file: ', A)
        1040 format (/' ERROR: Time step is not: ', I10, ' but: ', I10, &
                ' in file: ', A)
        1050 format (/' ERROR: Time: ', I10, ' not found in file: ', A)

    end subroutine validate_simulation_time_steps

    subroutine read_constant_data(iopt2, array, nitem, nvals, nscale, &
            iwidth, lun1, output_verbose_level, ierr)

        !! Reads a block with constant data with and without defaults
        !!
        !! Function depends on value of iopt2
        !!      - if 1:
        !>          - nscale scale values
        !>          - nitem sets of nvals values
        !>      - if 2:
        !>          - nscale scale values
        !>          - nvals dfault values
        !>          - number of overridings
        !>          - that many integers + sets of nvals values
        !>      If lun1 is positive the array are written

        !!     Subroutines called : scale  : to scale the matrix with the sacle factors
        !!     Logical units      : input_file = unit input file
        !!                          file_unit = unit formatted output file
        !!                          LUN1  = unit intermediate file ( system )
        use timers       !   performance timers
        use rd_token
        use matrix_utils, only : scale_array

        integer(kind = int_wp), intent(in) :: iopt2       !< input option
        real(kind = real_wp), intent(out) :: array(nvals, nitem)  !< array for the values
        integer(kind = int_wp), intent(in) :: nitem       !< number of items to read
        integer(kind = int_wp), intent(in) :: nvals       !< number of values per item
        integer(kind = int_wp), intent(in) :: nscale      !< number of scale values
        integer(kind = int_wp), intent(in) :: iwidth      !< width of the output file
        integer(kind = int_wp), intent(in) :: lun1        !< output unit number
        integer(kind = int_wp), intent(in) :: output_verbose_level      !< how extensive the output ?
        integer(kind = int_wp), intent(inout) :: ierr        !< cumulative rror counter

        real(kind = real_wp), allocatable :: factor(:)        !  array for scale factors
        real(kind = real_wp) :: value              !  help variable values
        integer(kind = int_wp) :: nover              !  number of overridings
        integer(kind = int_wp) :: iscal              !  loop counter scale values
        integer(kind = int_wp) :: item               !  loop counter items
        integer(kind = int_wp) :: ival               !  loop counter values
        integer(kind = int_wp) :: iw                 !  loop counter print blocks
        integer(kind = int_wp) :: iover              !  loop counter overridings
        integer(kind = int_wp) :: ie1, ie2           !  limits of print blocks
        integer(kind = int_wp) :: ierr2              !  local error variable
        integer(kind = int_wp) :: ithndl = 0

        if (nitem == 0) return                   !  no items specified

        allocate (factor(nvals), stat = ierr2)
        if (ierr2 /= 0) then
            write (file_unit, 2000) nvals
            goto 100
        endif

        do iscal = 1, nscale
            if (gettoken(factor(iscal), ierr2) > 0) goto 100
        enddo

        select case (iopt2)

        case (1)                    !   read constant items without defaults
            write (file_unit, 2010)
            do item = 1, nitem
                do ival = 1, nvals
                    if (gettoken(array(ival, item), ierr2) > 0) goto 100
                enddo
            enddo
            if (output_verbose_level < 4) write (file_unit, 2020)
            do iw = 1, nvals, iwidth
                ie1 = min(iw + iwidth - 1, nscale)
                ie2 = min(iw + iwidth - 1, nvals)
                if (output_verbose_level >= 4) then
                    write (file_unit, 2030)          (ival, ival = iw, ie1)
                    write (file_unit, 2040)          (factor(ival), ival = iw, ie1)
                    write (file_unit, 2050)
                    do item = 1, nitem
                        write (file_unit, 2060) item, (array (ival, item), ival = iw, ie2)
                    enddo
                endif
            enddo

        case (2)                    !   Read constant items with defaults
            write (file_unit, 2070)
            do ival = 1, nvals
                if (gettoken(array(ival, 1), ierr2) > 0) goto 100
            enddo
            if (output_verbose_level < 3) write (file_unit, 2080)
            do iw = 1, nvals, iwidth
                ie1 = min(iw + iwidth - 1, nscale)
                ie2 = min(iw + iwidth - 1, nvals)
                if (output_verbose_level >= 3) then
                    write (file_unit, 2030) (ival, ival = iw, ie1)
                    write (file_unit, 2040) (factor(ival), ival = iw, ie1)
                    write (file_unit, 2090)
                    write (file_unit, 2040) (array (ival, 1), ival = iw, ie2)
                endif
            enddo
            do ival = 1, nvals
                value = array(ival, 1)
                do item = 2, nitem
                    array(ival, item) = value
                enddo
            enddo

            !           Read overridings of the constant values

            if (gettoken(nover, ierr2) > 0) goto 100
            write (file_unit, 2100) nover
            if (nover > 0 .and. output_verbose_level >= 3) write (file_unit, 2110)
            do iover = 1, nover
                if (gettoken(item, ierr2) > 0) goto 100
                if (item < 1 .or. item > nitem) then
                    if (output_verbose_level >= 3) write (file_unit, 2120) item, 1, nitem
                    ierr = ierr + 1
                    do ival = 1, nvals
                        if (gettoken(value, ierr2) > 0) goto 100
                    enddo
                else
                    do ival = 1, nvals
                        if (gettoken(array(ival, item), ierr2) > 0) goto 100
                    enddo
                    if (output_verbose_level >= 3) &
                            write (file_unit, 2130) item, (array(ival, item), ival = 1, nvals)
                endif
            enddo

        end select

        !     Scale the values

        if (nscale == 1) then
            do iscal = 2, nvals
                factor(iscal) = factor(1)
            enddo
        endif
        call scale_array (array, factor)

        ! Write if unit specified
        if (lun1 > 0) write (lun1) array
        if (timon) call timstop(ithndl)
        return

        !     Errors and ends

        100 ierr = ierr + 1
        write (file_unit, 2140)
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (/' ERROR allocating real array space of:', i10, ' values.')
        2010 format (' Constant values without defaults ', /)
        2020 format (' Values will be printed for output option 4 and higher !')
        2030 format (' Scale factors:', /, 6X, 10I12)
        2040 format (12X, 1P, 10E12.4)
        2050 format (' Unscaled values:')
        2060 format (I10, 2X, 1P, 10E12.4)
        2070 format (' Constant values with defaults ', /)
        2080 format (' Values will be printed for output option 3 and higher !')
        2090 format (' Unscaled default values:')
        2100 format (' Number of overridings :', I5)
        2110 format (' Number         values')
        2120 format (I6, ' ERROR, number too big or too small must be between ', I5, ' and ', I5, ' !!!')
        2130 format (I10, 2X, 1P, 10E12.4, /(12X, 10E12.4))
        2140 format (' ERROR reading input!')

    end subroutine read_constant_data

    subroutine read_time_dependent_variables(file_unit_list, file_name_list, is, nitem, nvals, &
            nscal, ifact, dtflg, is_yyddhh_format, nrfunc, &
            nrharm, iwidth, output_verbose_level, ierr)

        !!  Read time dependent variables
        !!
        !! Time depending data can come in 2 ways
        !>      - a table with values at breakpoints
        !>      - a table with harmonic or Fourier values
        !>          The values at breakpoints require following input:
        !>       - integration_id, should be 1 (no defaults) or 2 (defaults and overridings)
        !>       - number of items in this block    (nvarnw, read in read_item_num)
        !>       - that many ID values of the items (itemId, read in read_item_num)
        !>       - number of breakpoints (nobrk2, this in the number of time steps)
        !>       - scale values to be applied for this block ( 1 or nval1 )
        !>       - table of values in (nval1,nitem) order.
        !>       The function option requires the following input
        !>       - integration_id, should be 3 (harmonics) or 4 (Fouriers)
        !>       - number of items in this block    (nvarnw, read in read_item_num)
        !>       - that many ID values of the items (itemId, read in read_item_num)
        !>       - number of harmonics or Fourier components (nhar  , read in read_fourier_harmoic_func_values)
        !>       - nval1 values for the zero-th harmonic  (the mean , read in read_fourier_harmoic_func_values)
        !>       - nhar times:
        !>          - a period of the harmonic    ( NOT for the Fouriers )
        !>          - the phase of the harmonic, or Fourier
        !>          - nval1 amplitudes of this component
        !>       A number of these blocks are read, untill all items got a value for all nval1
        !>       For the new processing of Bounds, Wastes and Funcs, the file is
        !>       - initialised with a specific header
        !>       - written per block with:
        !>          - heading block information
        !>          - breakpoint + matrix at the breakpoint for each breakpoint.
        !>          For the old processing the blocks are merged to one big matrix. The
        !>          information on the items is written in the system file. The big matrix
        !>          of size (nval1,nitem,nobrkt) is written to the binary file. Because
        !>          the size of this total matrix is not clear in advance, the matrix is
        !>          reallocated for every new block. Previous versions that used a swap file
        !>          for the matrix have been phased out, because memory is likely not a
        !>          problem at the moment any more.

        !! Logical units:
        !!      file_unit   = unit formatted output file
        !!      file_unit_list( 3) = unit binary intermediate file for harmonics
        !!      file_unit_list( 4) = unit binary intermediate file for pointers
        !!      file_unit_list(is) = unit binary intermediate file for function

        use matrix_utils, only : dmatrix
        use m_open_waq_files
        use timers       !   performance timers
        use rd_token
        use m_waq_memory_dimensions          ! System characteristics

        integer(kind = int_wp), intent(inout) :: file_unit_list   (*)          !< array with unit numbers
        character(*), intent(in) :: file_name_list (*)         !< array with file names of the files
        integer(kind = int_wp), intent(in) :: is                 !< entry in file_unit_list for this call
        integer(kind = int_wp), intent(in) :: nitem              !< number of required items
        integer(kind = int_wp), intent(in) :: nvals              !< number of values per item
        integer(kind = int_wp), intent(in) :: nscal              !< number of scale values
        integer(kind = int_wp), intent(in) :: ifact              !< factor between clocks
        logical, intent(in) :: dtflg             !< 'date'-format for output ?
        logical, intent(in) :: is_yyddhh_format            !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(out) :: nrfunc             !< number of functions
        integer(kind = int_wp), intent(out) :: nrharm             !< number of harmonic functions
        integer(kind = int_wp), intent(in) :: iwidth             !< width of the output file
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< flag for more or less output
        integer(kind = int_wp), intent(inout) :: ierr               !< error count / switch

        integer(kind = int_wp), pointer :: breaks(:)         !  breakpoints
        integer(kind = int_wp), allocatable :: break2(:)         !  breakpoints of a block
        integer(kind = int_wp), pointer :: break3(:)         !  help pointer for resizing
        real(kind = real_wp), pointer :: values(:, :)       !  values
        real(kind = real_wp), allocatable :: value2(:, :)       !  values of a block
        real(kind = real_wp), pointer :: value3(:, :)       !  help pointer for resizing
        integer(kind = int_wp) :: itemId(nitem)      !  array for itemIds
        real(kind = real_wp) :: factor(nvals)      !  array for scale factors
        integer(kind = int_wp) :: nval1              !  nval1 but at least 1
        logical(4) bound             !  boundary ?
        logical(4) waste             !  wastes ?
        logical(4) funcs             !  segment functions ?
        logical(4) found             !  help variable for finding strings
        integer(kind = int_wp) :: ierr2              !  local error variable
        integer(kind = int_wp) :: ifilsz             !  local counter of used integer array space
        integer(kind = int_wp) :: jfilsz             !  local counter of used real array space
        integer(kind = int_wp) :: ntot               !  nitem*nval1, real space of one breakpoint
        integer(kind = int_wp) :: ntotal             !  total number of items with input
        integer(kind = int_wp) :: nobrkt             !  total number of breakpoints
        integer(kind = int_wp) :: nobrk2             !  number of breakpoints in this block
        integer(kind = int_wp) :: newbrk             !  number of breakpoints for the new allocation
        integer(kind = int_wp) :: iopt3              !  option for this block
        integer(kind = int_wp) :: nvarnw             !  number of items in a block
        integer(kind = int_wp) :: lunuit             !  the unit of the binary file
        integer(kind = int_wp) :: i1, i2, k          !  loop counters
        integer(kind = int_wp) :: ibrk               !  loop counter breakpoints
        integer(kind = int_wp) :: iscal              !  loop counter scale values
        integer(kind = int_wp) :: nrec               !  total nr of rec's
        integer(kind = int_wp) :: nrec2              !  local nr of rec's
        integer(kind = int_wp) :: nvarar             !  number of items previous read
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_time_dependent_variables", ithndl)

        breaks => null()
        break3 => null()
        values => null()
        value3 => null()

        bound = .false.
        waste = .false.
        funcs = .false.
        if (ierr == -1) bound = .true.
        if (ierr == -2) waste = .true.
        if (ierr == -3) funcs = .true.
        nval1 = nvals
        if (funcs .and. nvals == 0) nval1 = 1
        ierr = 0
        ifilsz = 0
        jfilsz = 0
        ntot = nitem * nval1
        ntotal = 1
        nobrkt = 0
        nrec = 0

        ! write headers for new style time series files
        write (file_unit, 2000)
        !        open the output work file
        !        write nr of items and nr of substances
        !        write default values ( IORDER = 1 , NPNT = 0 )
        lunuit = file_unit_list(3)
        if (.not. funcs) call open_waq_files (file_unit_list(is), file_name_list(is), is, 1, ierr2)
        if (bound) then
            write (file_unit_list(is)) ' 4.900BOUND '
            write (file_unit_list(is)) nitem, nval1
            write (file_unit_list(is)) 1, 0, nval1, (k, k = 1, nval1), 1, 0
        endif
        if (waste) then
            write (file_unit_list(is)) ' 4.900WASTE '
            write (file_unit_list(is)) nitem, nval1
            write (file_unit_list(is)) 1, 0, nval1, (k, k = 0, nval1 - 1), 1, 0
        endif
        if (bound .or. waste) then
            write (file_unit_list(is)) 1
            write (file_unit_list(is)) 0, (0.0, k = 1, nval1)
            ifilsz = ifilsz + 2 + 3 + nval1 + 3 + 1
            jfilsz = jfilsz + nval1
        endif
        if (bound .or. waste .or. funcs) lunuit = file_unit_list(is)

        do while (ntotal - 1 < nitem)     ! loop over blocks till completion

            ! read the type of block that comes
            if (gettoken(iopt3, ierr2) > 0) goto 100
            write (file_unit, 2010) iopt3
            if (iopt3 < 1 .or. iopt3 > 4) then
                write (file_unit, 2020)
                goto 100
            endif

            ! the items in this block by itemnumber
            call read_item_num(nitem, iopt3, output_verbose_level, itemId(ntotal), nvarnw, ierr)

            ! new style for boundaries and wastes
            if (bound .or. funcs) &
                    write (file_unit_list(is)) 1, nvarnw, (iabs(itemId(ntotal + k)), k = 0, nvarnw - 1), &
                            nvals, (k, k = 1, nvals), iopt3, 1
            if (waste) &
                    write (file_unit_list(is)) 1, nvarnw, (iabs(itemId(ntotal + k)), k = 0, nvarnw - 1), &
                            nval1, (k, k = 0, nval1 - 1), iopt3, 1
            if (bound .or. waste .or. funcs) &
                    ifilsz = ifilsz + 5 + nvarnw + nvals

            select case (iopt3)

            case (1, 2)             !         Read time-dependent items on breakpoints
                if (gettoken(nobrk2, ierr2) > 0) goto 100
                write (file_unit, 2030) nobrk2
                allocate (break2(nobrk2), value2(nvarnw * nval1, nobrk2))
                do iscal = 1, nscal
                    if (gettoken(factor(iscal), ierr2) > 0) goto 100
                enddo
                call read_scale_block (nvarnw, itemId(ntotal), nval1, nscal, factor, &
                        nobrk2, break2, value2, dtflg, is_yyddhh_format, &
                        ifact, iwidth, output_verbose_level, ierr)

                if (bound .or. waste .or. funcs) then
                    write (file_unit_list(is)) nobrk2                      ! boundaries, wastes and
                    ifilsz = ifilsz + 1                           ! functions are written
                    do ibrk = 1, nobrk2                           ! directly per block
                        write (file_unit_list(is)) break2(ibrk), value2(:, ibrk)
                    enddo
                    ifilsz = ifilsz + nobrk2
                    jfilsz = jfilsz + nobrk2 * nvarnw * nval1
                else                                             ! other are merged into
                    newbrk = nobrkt + nobrk2                      ! one big matrix that is
                    allocate (break3(newbrk), value3(ntot, newbrk))  ! written at the end
                    if (nobrkt > 0) then
                        break3(1:nobrkt) = breaks(1:nobrkt)
                        value3(:, 1:nobrkt) = values(:, 1:nobrkt)    ! expand the matrix to allow
                        deallocate(breaks, values)               ! for the new values to enter
                    endif
                    breaks => break3
                    values => value3
                    nvarar = ntotal - 1
                    call dmatrix (ntot, nval1, nvarar, nvarnw, nobrkt, &
                            nobrk2, breaks, break2, values, value2, &
                            itemId(ntotal))
                endif
                deallocate (break2, value2)

            case (3, 4)            !         Read items as functions
                write (file_unit, 2050)
                nrec2 = 0                                        ! these function blocks are
                if (bound .or. funcs) then                     ! are written in the
                    ierr2 = -1                                    ! lunuit = file_unit_list(is) file
                endif                                            ! for bounds, wastes and funcs
                if (waste) then                                ! and to lunuit = file_unit_list(3), the
                    ierr2 = -2                                    ! system file, for others
                endif
                call read_fourier_harmoic_func_values(iopt3, nvarnw, nval1, itemId(ntotal), nrec2, &
                        harmonics_arr_len, ifact, dtflg, is_yyddhh_format, lunuit, &
                        iwidth, output_verbose_level, ierr2)
                ierr = ierr + ierr2
                if (bound .or. waste .or. funcs) then
                    ifilsz = ifilsz + nrec2
                    jfilsz = jfilsz + nrec2 * (nvarnw * nval1 + 1)
                else
                    nrec = nrec + nrec2
                endif

            end select

            ntotal = ntotal + nvarnw

        enddo

        if (ntotal - 1 > nitem) then
            write (file_unit, 2060) ntotal - 1, nitem
            ierr = ierr + 1
        endif

        ! Check complete pointer structure
        do i1 = 1, nitem
            found = .false.
            do i2 = 1, nitem
                if (abs(itemid(i2)) == i1) then
                    if (found) then
                        write (file_unit, 2070) i1
                        ierr = ierr + 1
                    else
                        found = .true.
                    endif
                endif
            enddo
            if (.not. found) then
                write (file_unit, 2080) i1
                ierr = ierr + 1
            endif
        enddo

        ! Finalize this input section
        if (bound .or. waste .or. funcs) then
            newrsp = newrsp + jfilsz
            newisp = newisp + ifilsz
        else                         !    write pointers and breakpoint matrix
            write (file_unit_list(4)) itemid, 0, 0, 0
            do ibrk = 1, nobrkt
                write (file_unit_list(is)) breaks(ibrk), values(:, ibrk)
            enddo
            close (file_unit_list(is))
            nlines = nlines + ntot * 2
            num_indices = num_indices + nitem + 3
            nrfunc = ntot
            nrharm = nrec
            num_harmonics = num_harmonics + nrec
        endif
        ierr = ierr + ierr2
        if (associated (breaks)) deallocate (breaks, values)
        if (timon) call timstop(ithndl)
        return

        100 ierr = ierr + 1
        return

        2000 format (' Time variable data.')
        2010 format (/, ' Option selected : ', I2)
        2020 format (/, ' ERROR, option not implemented')
        2030 format (' Number of breakpoints:', I7)
        2050 format (' Block with periodic functions.')
        2060 format (' ERROR, too many (', I5, ') items, ', I5, ' expected!')
        2070 format (' ERROR, duplicate item:', I5)
        2080 format (' ERROR, non-initialised item:', I5)

    end subroutine read_time_dependent_variables

    subroutine read_constants_time_variables(file_unit_list, is, noql1, noql2, noql3, &
            ndim2, ndim3, nrftot, nrharm, ifact, &
            is_date_format, disper, volume, iwidth, file_name_list, &
            filtype, is_yyddhh_format, output_verbose_level, ierr, &
            status, dont_read)

        !!  Reads a block with constant or time variable data
        !>      This is a main data aquisition sub system, it is
        !>      the only call to read:
        !>          - volumes ( in read_block_3_grid_layout )
        !>          - additional dispersions ( in read_block_4_flow_dims_pointers )
        !>          - additional velocities ( in read_block_4_flow_dims_pointers )
        !>          - areas ( in read_block_4_flow_dims_pointers )
        !>          - flows ( in read_block_4_flow_dims_pointers )
        !>          - mixing lengthes ( in read_block_4_flow_dims_pointers )
        !>          - old style open boundaries ( in read_block_5_boundary_conditions )
        !>          - old style waste loads ( in read_block_6_waste_loads_withdrawals )
        !>          read_block_7_process_parameters is a sort of dedicated verion of this routine
        !>          to read parameters and functions and segment functions\n
        !>          read_block_8_initial_conditions is a sort of dedicated verion of this routine
        !>          to read initial conditions

        !!  Logical units: file_unit_list(27) = unit stripped DELWAQ input file
        !!                  file_unit_list(28) = stripped workfile
        !!                  file_unit_list(29) = unit formatted output file
        !!                  file_unit_list( 2) = unit intermediate file (system)
        !!                  file_unit_list( 3) = unit intermediate file (harmos)
        !!                  file_unit_list( 4) = unit intermediate file (pointers)
        !!                  file_unit_list(is) = unit intermediate file (items)

        use m_open_waq_files
        use timers       !   performance timers
        use rd_token
        use m_waq_memory_dimensions          ! System characteristics

        integer(kind = int_wp), intent(inout) :: file_unit_list    (*)     !< array with unit numbers
        integer(kind = int_wp), intent(in) :: is             !< entry in file_unit_list for this call
        integer(kind = int_wp), intent(in) :: noql1          !< number of exchanges 1st direction
        integer(kind = int_wp), intent(in) :: noql2          !< number of exchanges 2nd direction
        integer(kind = int_wp), intent(in) :: noql3          !< number of exchanges 3rd direction
        integer(kind = int_wp), intent(in) :: ndim2          !< number of items per block
        integer(kind = int_wp), intent(in) :: ndim3          !< number of scale factors
        integer(kind = int_wp), intent(inout) :: nrftot         !< number of functions
        integer(kind = int_wp), intent(inout) :: nrharm         !< number of harmonics
        integer(kind = int_wp), intent(in) :: ifact          !< factor between time scales
        logical, intent(in) :: is_date_format        !< 'date'-format 1st time scale
        logical, intent(in) :: disper        !< .true. then dispersion
        integer(kind = int_wp), intent(inout) :: volume         !< if 1 then volume ( out: 0 = computed volumes )
        integer(kind = int_wp), intent(in) :: iwidth         !< width of the output file
        character(*), intent(inout) :: file_name_list  (*)    !< array with file names of the files
        integer(kind = int_wp), intent(inout) :: filtype(*)     !< type of binary file
        logical, intent(in) :: is_yyddhh_format        !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: output_verbose_level         !< how extensive is output ?
        integer(kind = int_wp), intent(inout) :: ierr           !< cumulative error count
        logical, intent(in) :: dont_read  !< do not actually read tokens, if true, the information is already provided

        type(error_status), intent(inout) :: status !< current error status

        logical        bound       !  if .true. then boundary call
        logical        waste       !  if .true. then waste call
        logical        skip        !  if .true. then waste call with skip
        integer(kind = int_wp) :: iopt1        !  first  option ( type of file e.g. 0 = binary file )
        integer(kind = int_wp) :: iopt2        !  second option ( 1,2 = constant, 3 = time varying )
        integer(kind = int_wp) :: ndim1        !  sum of input in 3 directions
        integer(kind = int_wp) :: ndtot        !  total size of matrix (ndim1*ndim2)
        integer(kind = int_wp) :: ierr2        !  local error flag
        integer(kind = int_wp) :: itype        !  to identify the data type read
        integer(kind = int_wp) :: idummy       !  work integer ( = 0 )
        real(kind = real_wp) :: adummy       !  work real    ( = 0.0 )
        character(128) cdummy      !  work character
        integer(kind = int_wp) :: k            !  loop counter
        real(kind = real_wp) :: disp(3, 1)    !  dispersions in 3 directions
        real(kind = real_wp), allocatable :: values(:, :)  ! read buffer for the values
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_constants_time_variables", ithndl)

        idummy = 0
        adummy = 0.0
        ndim1 = noql1 + noql2 + noql3
        ndtot = ndim1 * ndim2
        bound = .false.
        waste = .false.
        skip = .false.

        select case (ierr)
        case (-1)
            bound = .true.
            waste = .false.
            skip = .false.
        case (-2)
            bound = .false.
            waste = .true.
            skip = .false.
        case (:-3)
            bound = .false.
            waste = .true.
            skip = .true.
        end select
        ierr = 0
        if (skip) goto 10

        ! Read first option, write zero dispersion if process_simulation_input_options=0
        if (dont_read) then
            iopt1 = -2
            if (is == 13) then
                iopt1 = 0 ! Ugly hack, all other files are time-dependent
            endif
        else
            if (gettoken(cdummy, iopt1, itype, ierr2) > 0) goto 50
            if (itype == 1) then
                if (volume /= 1) then
                    write (file_unit, 2070) cdummy
                    ierr2 = 1
                    goto 50
                else
                    if (cdummy == 'FRAUD') then
                        volume = -1
                        write (file_unit, 2080)
                        if (gettoken(iopt1, ierr2) > 0) goto 50
                    else
                        write (file_unit, 2090) cdummy
                        ierr2 = 1
                        goto 50
                    endif
                endif
            endif
        endif

        write (file_unit, 2000) iopt1
        call process_simulation_input_options(iopt1, file_unit_list, is, file_name_list, filtype, &
                is_date_format, is_yyddhh_format, ndtot, ierr2, status, &
                dont_read)
        if (ierr2 > 0) goto 50

        ! Binary file, option = -2 (or sequence of binary files option = -4)
        ! everything is block function, except volume
        if (iopt1 == -2 .or. iopt1 == -4) then
            nlines = nlines + ndim1 * ndim2 * 2
            num_indices = num_indices + ndim1 + 3
            nrftot = ndim1 * ndim2
            nrharm = 0
            if (volume == 1) then
                write(file_unit_list(4)) (k, k = 1, ndim1), (idummy, k = 1, 3)
            else
                write(file_unit_list(4)) (-k, k = 1, ndim1), (idummy, k = 1, 3)
            endif
            iopt1 = 0
        endif

        ! Dispersion in three directions if DISPER, return if num_dispersion_arrays=0
        if (disper) then
            if (iopt1 == 0) then                            ! binary file, then
                write (file_unit_list(2)) idummy, (adummy, k = 1, 3)     ! no fixed dispersions
            else
                write (file_unit_list(2)) idummy
                call read_constant_data (1, disp, 1, 3, 3, &
                        iwidth, file_unit_list(2), output_verbose_level, ierr2)
                if (ierr2 > 0) goto 50
                if (ndim2 == 0) goto 9999
            endif
        endif

        ! binary file, we are ready
        if (iopt1 == 0) goto 9999

        ! Read second option, set volume flag if OPT2 > 3 AND VOLUME
        10 if (gettoken(iopt2, ierr2) > 0) goto 50
        write (file_unit, 2010) iopt2
        ! Computed volumes
        if (volume == 1 .and. iopt2 > 3) then
            volume = 0
            iopt2 = iopt2 - 3
        endif

        ! Get the data
        select case (iopt2)
        case (1, 2)              !   Constants with and without defaults in three directions
            allocate (values(ndim2, max(noql1, noql2, noql3)))
            call open_waq_files (file_unit_list(is), file_name_list(is), is, 1, ierr2)
            write (file_unit_list(is)) idummy
            if (noql1 > 0) write (file_unit, 2030)
            call read_constant_data (iopt2, values, noql1, ndim2, ndim3, &
                    iwidth, file_unit_list(is), output_verbose_level, ierr2)
            if (ierr2 > 0) goto 50

            if (noql2 > 0) write (file_unit, 2040)
            call read_constant_data (iopt2, values, noql2, ndim2, ndim3, &
                    iwidth, file_unit_list(is), output_verbose_level, ierr2)
            if (ierr2 > 0) goto 50

            if (noql3 > 0 .and. noql3 /= ndim1) write (file_unit, 2050)
            call read_constant_data (iopt2, values, noql3, ndim2, ndim3, &
                    iwidth, file_unit_list(is), output_verbose_level, ierr2)
            close (file_unit_list(is))
            if (ierr2 > 0) goto 50

        case (3)
            ! Time varying data
            ierr2 = 0
            if (bound) ierr2 = -1
            if (waste) ierr2 = -2
            call read_time_dependent_variables (file_unit_list, file_name_list, is, ndim1, ndim2, &
                    ndim3, ifact, is_date_format, is_yyddhh_format, nrftot, &
                    nrharm, iwidth, output_verbose_level, ierr2)
            if (ierr2 > 0) goto 50

        case default
            write (file_unit, 2020)
            goto 50

        end select
        9999 if (timon) call timstop(ithndl)
        return

        50 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        2000 format (/, ' First  selected option   : ', I7)
        2010 format (' Second selected option   : ', I7)
        2020 format (/, ' ERROR. Option not implemented !!!!!!')
        2030 format (/, ' First  direction:')
        2040 format (/, ' Second direction:')
        2050 format (/, ' Third  direction:')
        2070 format (/, ' ERROR. No character string allowed: ', A)
        2080 format (' Keyword FRAUD found for fraudulent computations.')
        2090 format (/, ' ERROR. This keyword is not allowed here: ', A)

    end subroutine read_constants_time_variables

    subroutine read_fourier_harmoic_func_values(integration_id, nitem, nvals, item, nrec, &
            nhtot, ifact, dtflg, is_yyddhh_format, lununf, &
            iwidth, output_verbose_level, ierr)

        !! Reads function values (Fourier and Harmonic components)
        !!
        !!           Harmonic components for integration_id = 3 and Fouriers for integration_id = 4/n
        !!           Reads:
        !!           - number of frequencies exclusive of average value
        !!           - matrix(nvals*nitem) of average values
        !!           - the base frequency (Fourrier series only)
        !!           Per frequency:
        !!           - period (only for Harmonics)
        !!           - a phase of the component
        !!           - matrix(nvals*nitem) of values for the component
        !!           Notes:
        !!           - because there is an average value, there are nharm+1 matrices
        !!           - because there is a phase, there are 1+nvals*nitem values per component
        !!           - the periods are filled in for the Fouriers, so they look further more the same
        !!           - the zero'th period contains the value nvals*nitem
        !!           - the zero'th phase contains the number of harmoncs
        !!           The routine is entered with ierr = -1  for boundaries and -2 for wastes
        !     Subroutines called : convert_time_format
        !!                          cnvtim
        !!
        !!     Functions called   : gettok tokenized input data file reading
        !!
        !!     Logical units      : file_unit  = unit formatted output file
        !!                          lununf = unit unformatted output file

        use date_time_utils, only : convert_time_format, convert_relative_time
        use rd_token       ! for the reading of tokens
        use timers       !   performance timers

        integer(kind = int_wp), intent(in) :: integration_id           !< 3 Harmonics, 4 Fourier
        integer(kind = int_wp), intent(in) :: nitem          !< number of input items
        integer(kind = int_wp), intent(in) :: nvals          !< number of values per item
        integer(kind = int_wp), intent(in) :: item (nitem)   !< item numbers
        integer(kind = int_wp), intent(inout) :: nrec           !< number of harmonic records
        integer(kind = int_wp), intent(inout) :: nhtot          !< total harmonic array space
        integer(kind = int_wp), intent(in) :: ifact          !< factor between clocks
        logical, intent(in) :: dtflg         !< "date"-format
        logical, intent(in) :: is_yyddhh_format        !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: lununf         !< unit nr unformatted file
        integer(kind = int_wp), intent(in) :: iwidth         !< width of theoutput file
        integer(kind = int_wp), intent(in) :: output_verbose_level         !< how extensive output ?
        integer(kind = int_wp), intent(inout) :: ierr           !< error count

        integer(kind = int_wp) :: ndim           ! total size of the matrix
        integer(kind = int_wp) :: nhar           ! number of harmonics
        integer(kind = int_wp) :: ibase          ! base period of Fouriers
        integer(kind = int_wp) :: ierr2          ! error hlp variable
        integer(kind = int_wp) :: i, k           ! loop variables
        integer(kind = int_wp) :: ib, ie         ! limits for printed output
        integer(kind = int_wp) :: i1, i2         ! print loop counters
        logical                      bound         ! true if boundary processing
        logical                      waste         ! true if waste processing
        integer(kind = int_wp), allocatable :: iperio(:)      ! workspace for frequencies
        real(kind = real_wp), allocatable :: value (:, :)    ! workspace for values
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_fourier_harmoic_func_values", ithndl)

        bound = .false.
        waste = .false.
        if (ierr == -1) bound = .true.
        if (ierr == -2) waste = .true.
        ierr = 0
        ndim = nitem * nvals

        if (gettoken(nhar, ierr2) > 0) goto 100
        allocate (iperio(nhar + 1), value(ndim + 1, nhar + 1))

        select case (integration_id)

        case (3)        !      read values if integration_id = 3 ( harmonic function )

            do k = 2, ndim + 1
                if (gettoken(value(k, 1), ierr2) > 0) goto 100
            enddo
            do i = 2, nhar + 1
                if (gettoken(iperio(i), ierr2) > 0) goto 100
                do k = 1, ndim + 1
                    if (gettoken(value(k, i), ierr2) > 0) goto 100
                enddo
            enddo
            call convert_time_format (iperio(2), nhar, ifact, dtflg, is_yyddhh_format)
            value(1, 1) = real(nhar)

        case (4)        !      read values if integration_id = 4 ( fourier function )

            if (gettoken(ibase, ierr2) > 0) goto 100
            do k = 2, ndim + 1
                if (gettoken(value(k, 1), ierr2) > 0) goto 100
            enddo
            do i = 2, nhar + 1
                do k = 1, ndim + 1
                    if (gettoken(value(k, i), ierr2) > 0) goto 100
                enddo
            enddo
            call convert_relative_time (ibase, ifact, dtflg, is_yyddhh_format)
            value(1, 1) = real(nhar)
            do i = 2, nhar + 1
                iperio(i) = ibase / (i - 1)
            enddo

        end select

        ! control writing
        if (output_verbose_level < 4) then
            write (file_unit, 2070)
        else
            if (integration_id == 3) then
                write (file_unit, 2000) nhar
            else
                write (file_unit, 2010) nhar, ibase
            endif
            write (file_unit, 2020)
            do i1 = 1, nvals, iwidth
                write (file_unit, 2030) (k, k = i1, min(i1 + iwidth - 1, nvals))
                do i2 = 1, nitem
                    ib = (i2 - 1) * nvals + 1 + i1
                    ie = (i2 - 1) * nvals + 1 + min(i1 + iwidth - 1, nvals)
                    write (file_unit, 2040) item(i2), (value(k, 1), k = ib, ie)
                enddo
            enddo
            do i = 2, nhar + 1
                write (file_unit, 2050) iperio(i), value(1, i)
                if (iperio(i) <= 0) then
                    write (file_unit, 2060)
                    ierr = ierr + 1
                endif
                do i1 = 1, nvals, iwidth
                    write (file_unit, 2030) (k, k = i1, min(i1 + iwidth - 1, nvals))
                    do i2 = 1, nitem
                        ib = (i2 - 1) * nvals + 1 + i1
                        ie = (i2 - 1) * nvals + 1 + min(i1 + iwidth - 1, nvals)
                        write (file_unit, 2040) item(i2), (value(k, i), k = ib, ie)
                    enddo
                enddo
            enddo
        endif

        ! calculate new settings
        iperio(1) = ndim
        if (bound .or. waste) write (lununf) nhar + 1
        do i = 1, nhar + 1
            write (lununf) iperio(i), (value(k, i), k = 1, ndim + 1)
        enddo
        nrec = nrec + nhar + 1
        nhtot = nhtot + (ndim + 1) * (nhar + 1)
        if (timon) call timstop(ithndl)
        return

        100 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        2000 format(/, ' Number of harmonics:', I4)
        2010 format(/, ' Number of Fouriers:', I4, ' base period:', I10)
        2020 format(' Mean values :')
        2030 format('      Item', I8, 9I12)
        2040 format(I10, 2X, 1P, 10E12.4)
        2050 format(' Period:', I10, '   Phase:', 1P, E12.4)
        2060 format(' ERROR, PERIOD is less or equal to zero!')
        2070 format(' Printed output for output option 4 and higher !')

    end subroutine read_fourier_harmoic_func_values

    subroutine read_scale_block(num_items, item, nvals, num_factors, factor, &
            num_records, ibrk, arrin, is_date_format, is_yyddhh_format, &
            ifact, iwidth, output_verbose_level, ierr)

        !! Reads blocks of matrices of input values and scales them
        !!
        !! This routine reads:
        !!      - num_records integer breakpoint values for time
        !!      - for each breakpoint num_items*nvals values
        !!      The values are scaled with nvals scale factors/n
        !!      If one scale factor exist, it is expanded to nvals factors
        !!
        !! Logical units : file_unit = unit formatted output file

        use timers       !   performance timers
        use rd_token       ! for the reading of tokens
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time
        use matrix_utils, only : scale_array

        integer(kind = int_wp), intent(in) :: num_items                      !< number of items
        integer(kind = int_wp), intent(in) :: item  (num_items)              !< item numbers
        integer(kind = int_wp), intent(in) :: nvals                      !< number of values per item
        integer(kind = int_wp), intent(in) :: num_factors                      !< number of scale factors
        real(kind = real_wp), intent(inout) :: factor(nvals)              !< scale factors
        integer(kind = int_wp), intent(in) :: num_records                      !< number of breakpoints
        integer(kind = int_wp), intent(out) :: ibrk  (num_records)              !< breakpoints read
        real(kind = real_wp), intent(out) :: arrin(nvals, num_items, num_records)  !< breakpoints read
        logical  (4), intent(in) :: is_date_format                     !< 'date'-format time scale
        logical  (4), intent(in) :: is_yyddhh_format                    !< (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: ifact                      !< factor between timings
        integer(kind = int_wp), intent(in) :: iwidth                     !< width of the output file
        integer(kind = int_wp), intent(in) :: output_verbose_level                     !< how extensive is output ?
        integer(kind = int_wp), intent(inout) :: ierr                       !< cumulative error count


        integer(kind = int_wp) :: ierr2         ! local error variable
        integer(kind = int_wp) :: i1, i2, i3    ! loop counters
        integer(kind = int_wp) :: k             ! loop counter
        integer(kind = int_wp) :: ie1, ie2      ! endpoint help variables
        character(255) ctoken       ! to read a time string
        integer(kind = int_wp) :: itype         ! to indicate what was read
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_scale_block", ithndl)

        if (output_verbose_level < 4) write (file_unit, 2000)

        do i1 = 1, num_records

            if (gettoken(ctoken, ibrk(i1), itype, ierr2) > 0) goto 10
            if (itype == 1) then                                    !  a time string
                if (output_verbose_level >= 4) write (file_unit, 2010) i1, ctoken
                call convert_string_to_time_offset (ctoken, ibrk(i1), .false., .false., ierr2)
                if (ibrk(i1) == -999) then
                    write (file_unit, 2020) trim(ctoken)
                    goto 10
                endif
                if (ierr2 > 0) then
                    write (file_unit, 2030) trim(ctoken)
                    goto 10
                endif
            else                                                        !  an integer for stop time
                if (output_verbose_level >= 4) write (file_unit, 2040) i1, ibrk(i1)
                call convert_relative_time(ibrk(i1), ifact, is_date_format, is_yyddhh_format)
            endif

            do i3 = 1, num_items
                do i2 = 1, nvals
                    if (gettoken(arrin(i2, i3, i1), ierr2) > 0) goto 10
                enddo
            enddo

            if (output_verbose_level >= 4) then

                do i2 = 1, nvals, iwidth
                    ie1 = min(i2 + iwidth - 1, num_factors)
                    ie2 = min(i2 + iwidth - 1, nvals)
                    write (file_unit, 2050)        (k, k = i2, ie2)
                    write (file_unit, 2060) (factor(k), k = i2, ie1)
                    write (file_unit, 2070)
                    do i3 = 1, num_items
                        write (file_unit, 2080)  abs(item(i3)), (arrin(k, i3, i1), k = i2, ie2)
                    enddo
                enddo

            endif

        enddo

        ! Scale values
        if (num_factors == 1) then
            do i1 = 2, nvals
                factor(i1) = factor (11)
            enddo
        endif
        do i1 = 1, num_records
            call scale_array(arrin(:, :, i1), factor)
        enddo
        if (timon) call timstop(ithndl)
        return

        ! An error occured during read
        10 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        2000 format (' Printed output only for option 4 or higher !')
        2010 format (' Breakpoint ', I7, ' :', A)
        2020 format (/' ERROR: Absolute timer does not fit in timer format :', A, / &
                ' Is your T0 setting in block #1 correct?'/, &
                ' Allowed difference with T0 is usually ca. 68 years.')
        2030 format (/' ERROR: String is not a valid absolute timer :', A)
        2040 format (' Breakpoint ', I7, ' :', I10)
        2050 format (' Scale factors:', /, 6X, 10I12)
        2060 format (12X, 1P, 10E12.4)
        2070 format (' Item nr.   Values')
        2080 format (I10, 2X, 1P, 10E12.4)

    end subroutine read_scale_block

    subroutine read_item_num(num_rows, integration_id, output_verbose_level, ipnt, npnt, ierr)

        !! Reads the item numbers of an input block
        !!
        !! This routine reads:
        !!      - amount of items contained in this block
        !!      - item numbers in this block
        !!          If integration_id = 1, then block function, item numbers negative

        use timers       !   performance timers
        use rd_token       ! for the reading of tokens

        integer(kind = int_wp), intent(in) :: num_rows               !< maximum amount of items
        integer(kind = int_wp), intent(in) :: integration_id               !< is 1 for block functions
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< how extensive is output ?
        integer(kind = int_wp), intent(out) :: ipnt  (num_rows)       !< the item numbers of this block
        integer(kind = int_wp), intent(out) :: npnt               !< amount of items of this block
        integer(kind = int_wp), intent(inout) :: ierr               !< cumulative error indicator

        integer(kind = int_wp) :: ierr2      ! local error variable
        integer(kind = int_wp) :: i          ! loop counter
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_item_num", ithndl)


        ! read number of items in this block
        if (gettoken(npnt, ierr2) > 0) goto 10

        ! read the item numbers
        do i = 1, npnt
            if (gettoken(ipnt(i), ierr2) > 0) goto 10
            ipnt(i) = abs(ipnt(i))
            if (ipnt(i) > num_rows) then
                write (file_unit, 2000) ipnt(i), num_rows
                ierr = ierr + 1
            endif
        enddo

        ! write them if needed
        write(file_unit, 2010) npnt
        if (output_verbose_level >= 3) then
            write(file_unit, 2020) (ipnt(i), i = 1, npnt)
        else
            write(file_unit, 2030)
        endif

        ! Set negative values if integration_id = 1 ( block function )
        if (integration_id == 1) then
            do i = 1, npnt
                ipnt(i) = -ipnt(i)
            enddo
        endif
        if (timon) call timstop(ithndl)
        return

        10 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        2000 format (' ERROR. Item number:', I4, ' larger than maximum (', I4, ')!')
        2010 format (/, ' Amount of numbers in this block:', I4)
        2020 format (' Numbers in their order of input:', /, (5X, 10I7))
        2030 format (' Printed output on input items only for option 3 and higher !')

    end subroutine read_item_num

end module simulation_input_options
