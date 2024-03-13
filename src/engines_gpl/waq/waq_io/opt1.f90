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
module m_opt1
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains

    subroutine opt1 (iopt1, lun, is, lchar, filtype, &
            dtflg1, dtflg3, nitem, ierr, status, &
            dont_read)

        !!  Processing of first input file option
        !!      - Get the file name
        !!      - Open the file
        !!      - If ASCII, push file-info on include stack
        !! LOGICAL UNITS:
        !!      - LUN(33) = working unit for opening binary files

        use m_open_waq_files
        use timers       !   performance timers
        use rd_token
        use m_file_path_utils, only : extract_file_extension
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        integer(kind = int_wp), intent(in) :: iopt1           !< Input option
        integer(kind = int_wp), intent(inout) :: lun  (*)        !< DELWAQ Unit number array
        integer(kind = int_wp), intent(in) :: is              !< entry in LUN for item
        character*(*), intent(inout) :: lchar(*)       !< IN/OUT  Filenames
        logical, intent(in) :: dtflg1         !< 'date'-format 1st time scale
        logical, intent(in) :: dtflg3         !< 'date'-format (F;ddmmhhss,T;yydddhh)
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
        integer(kind = int_wp) :: lunin     ! help variable for opening external ASCII file
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
        if (timon) call timstrt("opt1", ithndl)

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
                write (lunut, 2010) lstack
                ierr2 = 1
                goto 30
            endif
            if (gettoken(cdummy, ierr2) > 0) goto 30     !   Get file name
            write (lunut, 2020)  cdummy
            ifl = ifl + 1
            lunin = 800 + ifl
            call open_waq_files  (lunin, cdummy, 33, 1, ierr2)    !   Open the file
            if (ierr2 > 0) then
                ifl = ifl - 1
                write (lunut, 2030)
            else
                lch (ifl) = cdummy
                ilun(ifl) = lunin
            endif

        case (-2, 0)                   !    External binairy intermediate file
            if (dont_read) then
                cdummy = lchar(is)
            else
                10          if (gettoken(cdummy, ierr2) > 0) goto 30     !   Get file name
                if (cdummy == 'UNFORMATTED') then
                    filtype(is) = filtype(is) + 10
                    write (lunut, *) 'UNFORMATTED file detected'
                    goto 10
                endif
                if (cdummy == 'BIG_ENDIAN') then
                    filtype(is) = filtype(is) + 20
                    write (lunut, *) 'BIG_ENDIAN  file detected'
                    goto 10
                endif
            endif
            lchar(is) = cdummy
            write (lunut, 2040) cdummy
            !                   Check if file exists
            call open_waq_files  (lun(33), cdummy, 33, 2, ierr2)
            if (ierr2 > 0) then
                ierr2 = -2
            else
                close (lun(33))
            endif

        case (-4)                      !    ASCII steering file taylored to read .hyd files
            if (nitem == 0) then
                write (lunut, 2000)
                ierr2 = 1
                goto 30
            endif
            itype = 1
            do while (itype == 1)
                if (gettoken(cdummy, nfil, itype, ierr2) > 0) goto 30  !   Get number of files
                if (itype == 1) then
                    if (cdummy == 'UNFORMATTED') then
                        filtype(is) = filtype(is) + 10
                        write (lunut, *) 'UNFORMATTED file detected'
                    else if (cdummy == 'BIG_ENDIAN') then
                        filtype(is) = filtype(is) + 20
                        write (lunut, *) 'BIG_ENDIAN  file detected'
                    else
                        write (lunut, 2150) cdummy
                        ierr2 = 1
                        goto 30
                    endif
                endif
            enddo
            if (gettoken(intopt, ierr2) > 0) goto 30     !   Get interpolation option
            if (gettoken(sstring, ierr2) > 0) goto 30     !   Get file string
            ! Open the binary intermediate file for output
            call extract_file_extension(lchar(27), filext, extpos, extlen)
            lchar(is) = lchar(27)(1:max(1, (extpos - 1))) // '-' // sstring
            call extract_file_extension(lchar(is), filext, extpos, extlen)
            lchar(is)(extpos:) = '.wrk'
            call open_waq_files  (lun(is), lchar(is), 1, 1, ierr2)
            if (ierr2 > 0) then
                ierr2 = -2
                goto 30
            endif
            write(lunut, 2120)  lchar(is), intopt
            write(lun(is))  'Steering file '
            write(lun(is))   nfil, intopt
            write (lunut, 2080)
            do  ifil = 1, nfil
                if (gettoken(fact, ierr2) > 0) goto 30  !   Get multiplication factor
                if (gettoken(cdummy, it1, itype, ierr2) > 0) goto 30    ! 'from' time
                if (itype == 1) then
                    call convert_string_to_time_offset(cdummy, it1, .false., .false., ierr2)
                    if (ierr2 > 0) then
                        write (lunut, 2130) trim(cdummy)
                        goto 30
                    endif
                    if (it1   == -999) then
                        write (lunut, 2140) trim(cdummy)
                        ierr2 = 1
                        goto 30
                    endif
                else
                    call convert_relative_time (it1, 1, dtflg1, dtflg3)
                endif
                if (gettoken(cdummy, it2, itype, ierr2) > 0) goto 30    ! 'to' time
                if (itype == 1) then
                    call convert_string_to_time_offset(cdummy, it2, .false., .false., ierr2)
                    if (ierr2 > 0) then
                        write (lunut, 2130) trim(cdummy)
                        goto 30
                    endif
                    if (it2   == -999) then
                        write (lunut, 2140) trim(cdummy)
                        ierr2 = 1
                        goto 30
                    endif
                else
                    call convert_relative_time (it2, 1, dtflg1, dtflg3)
                endif
                if (gettoken(cdummy, it3, itype, ierr2) > 0) goto 30    ! 'step'
                if (itype == 1) then
                    call convert_string_to_time_offset(cdummy, it3, .false., .false., ierr2)
                    if (ierr2 > 0) then
                        write (lunut, 2130) trim(cdummy)
                        goto 30
                    endif
                    if (it3   == -999) then
                        write (lunut, 2140) trim(cdummy)
                        ierr2 = 1
                        goto 30
                    endif
                else
                    call convert_relative_time (it3, 1, dtflg1, dtflg3)
                endif
                if (gettoken(sfile, ierr2) > 0) then       !     Get file string
                    ierr2 = -1
                    goto 30
                endif
                call extract_file_extension(sfile, filext, extpos, extlen)
                if (string_equals('hyd ', filext)) then                            !     hyd file processing
                    call validate_simulation_time_steps (lunut, sstring, sfile, cdummy, it3, &
                            it1a, it2a, it3a, nitem, ierr)
                else                                               !     other file processing
                    cdummy = sfile
                    it2a = 0
                    call open_waq_files  (lun(33), cdummy, 33, 2, ierr2)
                    if (ierr2 > 0) then
                        ierr2 = -2
                        goto 30
                    endif
                    read (lun(33)) it1a, (adummy, k = 1, nitem)
                    read (lun(33)) it3a
                    it3a = it3a - it1a
                    close (lun(33))
                endif
                write (lun(is)) fact, it1, it2, it3, cdummy, it1a, it2a, it3a
                write (lunut, 2090) ifil, fact, &
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
            close (lun(is))

        case (1)            !   continue reading from current file
            write (lunut, 2050)
            ierr2 = 0

        case default
            write (lunut, 2000)
            ierr2 = 1

        30 end select

        select case (ierr2)

        case (-2)
            write (lunut, 2060) cdummy
            call status%increase_warning_count()
            ierr = 0

        case (-1)
            write (lunut, 2100) cdummy
            ierr = 1

        case (0)
            ierr = 0

        case (1:)
            write (lunut, 2070)
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

    end subroutine opt1

    subroutine validate_simulation_time_steps (lunut, sget, afile, bfile, istep, &
            it2, it3, it4, numbr, ierr)

        use m_get_filepath_and_pathlen
        use m_open_waq_files
        use m_sysi          ! timer characteristics
        use m_waq_precision
        use time_module

        integer(kind = int_wp) :: ilun, ierr, i, k, a, it2, it3, it4
        integer(kind = int_wp) :: iyear1, imonth1, iday1, ihour1, imin1, isec1
        integer(kind = int_wp) :: iyear2, imonth2, iday2, ihour2, imin2, isec2
        integer(kind = int_wp) :: iyear3, imonth3, iday3, ihour3, imin3, isec3
        integer(kind = int_wp) :: iyear4, imonth4, iday4, ihour4, imin4, isec4
        integer(kind = int_wp) :: idate, itime, itim, itim2, istep, idtf, lunut
        integer(kind = int_wp) :: numbr

        character*25  sget, s1
        character*255 afile, bfile

        real(kind = dp) :: reftim, starttim, stoptim, afact
        character*255 filpath
        integer(kind = int_wp) :: pathlen

        ! open the ascii .hyd file
        ilun = 148
        call open_waq_files  (ilun, afile, 33, 1, ierr)
        if (ierr > 0) then
            write (lunut, 1000) afile
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
        20 write (lunut, 1010) sget
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
            write (lunut, 1020) bfile
            return
        endif

        ! find the time step in the file where to start
        read (ilun, end = 50) itim, (a, k = 1, abs(numbr))
        if (itim /= it2) then
            write (lunut, 1030) it2, itim, bfile
            ierr = 1
            return
        endif
        read (ilun, end = 50) itim2, (a, k = 1, abs(numbr))
        idtf = itim2 - itim
        if (idtf /= it4) then
            write (lunut, 1040) it4, idtf, bfile
            ierr = 1
            return
        endif
        if (((istep - itim) / idtf) * idtf == istep - itim) then
            close (ilun)
            return
        endif

        50 write (lunut, 1050) istep, bfile
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

end module m_opt1
