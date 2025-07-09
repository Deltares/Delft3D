!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_attribute_array
    use m_waq_precision
    use timers

    implicit none

    private
    public :: update_attribute_array, change_attribute_array

contains

    !! Updates kenmerk array
    subroutine update_attribute_array(file_unit_list, itime, iktim, iknmrk, num_cells, &
            is, luntxt, isflag, ifflag, file_option_attributes)


        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     file_unit_list     INTEGER       *     INPUT   unit number intermediate file
        !     ITIME   INTEGER       1     INPUT   Model timer
        !     IKTIM   INTEGER       *     IN/OUT  Timers in file
        !     IKNMRK  INTEGER   num_cells,*   IN/OUT  Kenmerk array
        !     num_cells   INTEGER       1     INPUT   number of segments
        !     IS      INTEGER       1     INPUT   Index number intermediate file
        !     LUNTXT  CHAR*(*)      *     INPUT   text with the unit number
        !     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
        !     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
        !     file_option_attributes  INTEGER       1     IN/OUT  file option kenmerk array
        use m_logger_helper, only: stop_with_error
        use m_open_waq_files
        use m_extract_waq_attribute
        use m_array_manipulation, only: copy_integer_array_elements

        INTEGER(kind = int_wp) :: itime, num_cells, is, isflag, ifflag, file_option_attributes, ikmrk1
        INTEGER(kind = int_wp) :: file_unit_list(*), iknmrk(num_cells, *), iktim(*)

        character(len = *) LUNTXT(*)

        integer(kind = int_wp) :: ierr, cell_i, lunout, ikmrk4
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("update_attribute_array", ithandl)

        !
        !     allocate the work array
        !     if time variable then get variable kenmerk array
        if (file_option_attributes > 0) then
            lunout = file_unit_list(19)
            !
            !        if first time open intermediate file and
            !        move original kenmerk array (column 1) to constant kenmerk array
            !        (column 2)
            !
            if (ifflag == 1) then
                call open_waq_files (file_unit_list(is), luntxt(is), is, 2, ierr)
                call copy_integer_array_elements (iknmrk(1, 1), iknmrk(1, 2), num_cells)
            endif
            !
            !        evaluate file option; read time-dependent kenmerk array into column 3
            !
            if (file_option_attributes == 1) then
                !
                !           one record per time step
                !
                call dlwqkv(file_unit_list(is), lunout, itime, iknmrk(1, 3), num_cells, &
                        luntxt(is), isflag, ifflag)
                if (ifflag == -1) then
                    file_option_attributes = 0
                    ifflag = 1
                    close (file_unit_list(is))
                endif
                !
            elseif (file_option_attributes == 2) then
                !
                !           block function
                !
                call dlwqkb (file_unit_list(is), lunout, &
                        itime, iktim(1), &
                        iktim(2), iktim(3), &
                        iknmrk(1, 3), iknmrk(1, 4), &
                        num_cells, luntxt(is), &
                        isflag, ifflag)
                !
            else
                !
                !           wrong option
                !
                write(lunout, 2000)
                call stop_with_error()
                !
            endif
            !
            !        (column 2)
            !
            do cell_i = 1, num_cells
                call extract_waq_attribute(4, iknmrk(cell_i, 2), ikmrk4)
            end do
            !
            !        change the time-variable kenmerk-array (column 3) such that it
            !
            call change_attribute_array (file_unit_list(19), num_cells, iknmrk(1, 3))

            !
            !        or the constant and the time variable array's
            !
            do cell_i = 1, num_cells
                iknmrk(cell_i, 1) = iknmrk(cell_i, 2) + iknmrk(cell_i, 3)
            end do
            !
        endif

        if (timon) call timstop (ithandl)
        return

        2000 FORMAT ('ERROR: wrong file option for kenmerk array')
    end subroutine update_attribute_array


    !! Changes kenmerk array
    subroutine change_attribute_array(lunrep, num_cells, iknmrk)

        integer(kind = int_wp), intent(in) :: lunrep         ! unit number of output file
        integer(kind = int_wp), intent(in) :: num_cells          ! number of segments

        integer(kind = int_wp), intent(inout) :: iknmrk(num_cells)  ! property array

        integer(kind = int_wp) :: cell_i                ! segment index
        integer(kind = int_wp) :: var1, var2, var3, var4 ! 1st, 2nd, 3rd and 4th number of feature
        integer(kind = int_wp) :: icount              ! counts the number of changes
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("change_attribute_array", ithandl)
        !
        !     on input, the kenmerk-array contains two digits:
        !        1*var1 + 10*var2
        !     where var1 describes whether a segment is active or not, and var2
        !     describes its location with respect to the vertical (top segment,
        !     middle, bottom, whole column).
        !
        !     on output, the kenmerk-array contains four digits:
        !        1*var1 + 10*var2 + 100*var3 + 1000*var4
        !     Now var1 indicates whether a segment is active AND belongs to the
        !     current subdomain. Var2 is left untouched. Var3 contains the initial
        !     value of var1. And var4 shows whether the segment belongs to the current
        !     subdomain or not.
        !
        icount = 0
        do cell_i = 1, num_cells
            !  Segment belongs to current subdomain
            !      - maintain 1st digit
            !      - maintain 2nd digit
            !      - copy original 1st digit to 3rd digit
            !      - set 4th digit to 1
            var1 = mod(iknmrk(cell_i), 10)
            var2 = mod((iknmrk(cell_i) / 10), 10)
            var3 = mod(iknmrk(cell_i), 10)
            var4 = 1
            iknmrk(cell_i) = var1 + 10 * var2 + 100 * var3 + 1000 * var4
        end do
        if (icount > 0) then
            write(lunrep, *) 'number of changes in feature array', icount
        endif

        if (timon) call timstop (ithandl)

    end subroutine change_attribute_array


    !! Steps along in a time variable database for integer block functions
    subroutine dlwqkb (input_file, lunout, itime, idtime, itime1, &
            itime2, iarra1, iarra2, nftot, luntxt, &
            isflag, ifflag)

        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     input_file   INTEGER       1     INPUT   unit number intermediate file
        !     LUNOUT  INTEGER       1     INPUT   unit number monitor file
        !     ITIME   INTEGER       1     INPUT   Model timer
        !     IDTIME  INTEGER       1     IN/OUT  Delta for this function
        !     ITIME1  INTEGER       1     IN/OUT  Lower time in file
        !     ITIME2  INTEGER       1     IN/OUT  Higher time in file
        !     IARRA1  REAL       NFTOT    IN/OUT  record at lower time
        !     IARRA2  REAL       NFTOT    IN/OUT  record at higher time
        !     NFTOT   INTEGER       1     INPUT   record length
        !     LUNTXT  CHAR*(*)      1     INPUT   text with the unit number
        !     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
        !     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
        use m_logger_helper, only: stop_with_error
        use m_array_manipulation, only: copy_integer_array_elements

        integer(kind = int_wp) :: input_file, lunout, itime, idtime, itime1, &
                itime2, nftot, isflag, ifflag
        integer(kind = int_wp) :: iarra1(*), iarra2(*)
        character(len = *) luntxt

        ! Local
        logical        stream_access                     ! help variable to detect the type of file access
        character(20)  access                            ! help variable to detect the type of file access
        character(len = 16)  MSGTXT(3)
        DATA          MSGTXT / ' REWIND ON      ', ' WARNING READING', &
                ' REWIND ERROR   ' /
        integer(kind = int_wp) :: messge, k, ierr

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqkb", ithandl)
        !
        messge = 0
        if (nftot  == 0) goto 9999
        if (ifflag == 0) goto 10
        !
        !         this is the first time, so read.
        !
        read (input_file, end = 80, err = 80) itime1, (iarra1(k), k = 1, nftot)
        read (input_file, end = 80, err = 80) itime2, (iarra2(k), k = 1, nftot)
        idtime = 0
        !
        !         check for start time simulation before start time file
        !
        if (itime < itime1) messge = 2
        !
        !         a new record required?
        !
        10 if (itime - idtime < itime2) goto 100
        call copy_integer_array_elements (iarra2, iarra1, nftot)
        itime1 = itime2
        read (input_file, end = 60, err = 80) itime2, (iarra2(k), k = 1, nftot)
        goto 10

        ! normal rewind.
        60 messge = 1
        inquire(input_file, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(input_file, iostat = ierr, pos = 1)
        else
            rewind input_file                            ! Start at the beginning again
        endif
        idtime = idtime + itime1
        read (input_file, end = 80, err = 80) itime1, (iarra1(k), k = 1, nftot)
        read (input_file, end = 80, err = 80) itime2, (iarra2(k), k = 1, nftot)
        idtime = idtime - itime1
        goto 100

        ! error, reading the unit
        80 messge = 3
        goto 100

        ! write the messages
        100 if (messge == 0) goto 9999
        if (isflag /= 1) then
            write(lunout, 2000) msgtxt(messge), input_file, luntxt, &
                    itime, itime1
        else
            write(lunout, 2010) msgtxt(messge), input_file, luntxt, &
                    itime / 86400, mod(itime, 86400) / 3600, &
                    mod(itime, 3600) / 60, mod(itime, 60), &
                    itime1 / 86400, mod(itime1, 86400) / 3600, &
                    mod(itime1, 3600) / 60, mod(itime1, 60)
        endif
        if (messge == 1) then
            messge = 0
            goto 10
        endif
        if (messge == 2) goto 9999
        call stop_with_error()
        9999 if (timon) call timstop (ithandl)

        2000 FORMAT (A16, ' UNIT: ', I3, ', READING: ', A20, / &
                ' AT SIMULATION TIME:', I12, ' !', /, &
                ' TIME IN FILE:      ', I12, ' !')
        2010 FORMAT (A16, ' UNIT: ', I3, ', READING: ', A20, / &
                ' AT SIMULATION TIME:', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S !', / &
                ' TIME IN FILE:      ', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S !')
    end subroutine dlwqkb

    !! Makes values at ITIME for user supplied binary intermediate files
    subroutine dlwqkv (input_file, lunout, itime, iarray, ntotal, &
            luntxt, isflag, ifflag)

        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     input_file   INTEGER       1     INPUT   unit number intermediate file
        !     LUNOUT  INTEGER       1     INPUT   unit number monitor file
        !     ITIME   INTEGER       1     INPUT   Model timer
        !     IARRAY  INTEGER  NTOTAL     OUTPUT  result array at time ITIME
        !     NTOTAL  INTEGER       1     INPUT   number of items to be filled
        !     LUNTXT  CHAR*(*)      1     INPUT   text concerning unit numbers
        !     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
        !     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation

        use m_logger_helper, only: stop_with_error

        integer(kind = int_wp) :: input_file, lunout, itime, ntotal, isflag, &
                ifflag
        integer(kind = int_wp) :: iarray(ntotal)
        character(len = *) luntxt

        logical        stream_access                     ! help variable to detect the type of file access
        character(20)  access                            ! help variable to detect the type of file access
        character(len = 10)  msgtxt(3)
        data          MSGTXT /' REWIND   ', ' CONSTANT ', ' ERROR    '/

        integer(kind = int_wp) :: messge, itime1, ierr

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqkv", ithandl)

        ! is this the first time?
        messge = 0
        if (ifflag == 1) goto 20
        !
        !         normal time varying read
        !
        read  (input_file, end = 10, err = 40) itime1, iarray
        goto 9999
        !
        !         normal rewind.
        !
        10 messge = 1
        inquire(input_file, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(input_file, iostat = ierr, pos = 1)
        else
            rewind input_file                            ! start at the beginning again
        endif
        read  (input_file, end = 40, err = 40) itime1, iarray
        goto 50
        !
        !         this is the first time, check only for nr of records.
        !
        20 continue
        read  (input_file, end = 40, err = 40) itime1, iarray
        read  (input_file, end = 30, err = 40) itime1, iarray
        inquire(input_file, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(input_file, iostat = ierr, pos = 1)
        else
            rewind input_file                            ! start at the beginning again
        endif
        read  (input_file, end = 30, err = 40) itime1, iarray
        goto 9999
        !
        !         file has only one record, array is constant
        !
        30 messge = 2
        inquire(input_file, access = access)
        stream_access = access == 'STREAM'
        if (stream_access) then
            read(input_file, iostat = ierr, pos = 1)
        else
            rewind input_file                            ! start at the beginning again
        endif
        read  (input_file, end = 40, err = 40) itime1, iarray
        ifflag = -1
        goto 50
        !
        !         error, during read
        !
        40 messge = 3
        50 if (isflag /= 1) then
            write(lunout, 2000) msgtxt(messge), input_file, luntxt, &
                    itime, itime1
        else
            write(lunout, 2010) msgtxt(messge), input_file, luntxt, &
                    itime / 86400, mod(itime, 86400) / 3600, &
                    mod(itime, 3600) / 60, mod(itime, 60), &
                    itime1 / 86400, mod(itime1, 86400) / 3600, &
                    mod(itime1, 3600) / 60, mod(itime1, 60)
        endif
        if (messge < 3) goto 9999
        call stop_with_error()
        9999 if (timon) call timstop (ithandl)

        2000 FORMAT (A10, 'ON UNIT:', I10, ', READING: ', A20, / &
                ' SIMULATION TIME :', I10, ' !  TIME IN FILE: ', I10, ' !')
        2010 FORMAT (A10, 'ON UNIT:', I10, ', READING: ', A20, / &
                ' SIMULATION TIME :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ', &
                ' TIME IN FILE    :', I5, 'D ', I2, 'H ', I2, 'M ', I2, 'S ! ')

    end subroutine dlwqkv

end module m_attribute_array
