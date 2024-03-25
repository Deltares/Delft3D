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
module m_dlwq5d
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQ5D (file_unit, int_array, real_array, max_int_size, max_real_size, &
            IPOSR, num_significant_char, ILUN, LCH, LSTACK, &
            CCHAR, CHULP, NOTOT, NOTOTC, time_dependent, num_records, &
            time_function_type, is_date_format, is_yyddhh_format, ITFACT, ITYPE, &
            IHULP, RHULP, IERR, ierr3)
        !! Boundary and waste data new style

        !     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
        !                          LUN(29) = unit formatted output file
        !                          LUN( 2) = unit intermediate file (system)
        !                          LUN(14) = unit intermediate file (boundaries)
        !                          LUN(15) = unit intermediate file (wastes)
        !

        !     file_unit   INTEGER    1         INPUT   unit number for ASCII output
        !     int_array     INTEGER  max_int_size       IN/OUT  integer   workspace
        !     real_array     REAL     max_real_size       IN/OUT  real      workspace
        !     max_int_size   INTEGER    1         INPUT   max. int. workspace dimension
        !     max_real_size   INTEGER    1         INPUT   max. real workspace dimension
        !     IPOSR   INTEGER    1         IN/OUT  Start position on input line
        !     num_significant_char    INTEGER    1         INPUT   nr of significant characters
        !     ILUN    INTEGER   LSTACK     INPUT   unitnumb include stack
        !     LCH     CHAR*(*)  LSTACK     INPUT   file name stack, 4 deep
        !     LSTACK  INTEGER    1         INPUT   include file stack size
        !     CCHAR   CHAR*1     1         INPUT   comment character
        !     CHULP   CHAR*(*)   1         OUTPUT  space for limiting token
        !     NOTOT   INTEGER    1         INPUT   size of the matrix to be read
        !     ITTIM   INTEGER    1         INPUT   0 if steady, 1 if time function
        !     num_records   INTEGER    1         OUTPUT  number of records read
        !     time_function_type    INTEGER    1         INPUT   3 is harmonics, 4 is fourier
        !     is_date_format  LOGICAL    1         INPUT   True if time in 'date' format
        !     is_yyddhh_format  LOGICAL    1         INPUT   True if YYetc instead of DDetc
        !     ITFACT  INTEGER    1         INPUT   factor between clocks
        !     ITYPE   INTEGER    1         OUTPUT  type of info at end
        !     IERR    INTEGER    1         OUTPUT  return code
        !     IERR3   INTEGER    1         OUTPUT  actual error indicator

        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        !< true if the bc or waste load definition is time dependent (linear, harmonic or fourier), and false if it
        !! is constant.
        logical, intent(in) :: time_dependent

        integer(kind = int_wp) :: max_int_size, max_real_size, i
        character*(*) lch(lstack), chulp
        character*1   cchar
        dimension     int_array(*), ilun(lstack)
        logical       newrec, is_date_format, is_yyddhh_format, ignore
        integer(kind = int_wp) :: ihulp
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: num_records, itel, itel2, ierr3, itype
        integer(kind = int_wp) :: file_unit, ilun, iposr, num_significant_char, ierr, itfact
        integer(kind = int_wp) :: int_array, notot, nototc, lstack, time_function_type
        real :: real_array(:), rhulp

        if (timon) call timstrt("dlwq5d", ithndl)

        ! some initialisation
        ignore = .false.
        newrec = .false.
        if (time_dependent) newrec = .true.                          ! it is a time function
        num_records = 0
        itel = 1
        itel2 = 1
        ierr3 = 0
        if (itype /= 0) goto 20                                  ! it was called with an argument

        ! read loop
        10 if (newrec) then
            itype = 0                                                 ! everything is valid
        else
            itype = 3                                                 ! a real value schould follow
        endif
        call rdtok1 (file_unit, ilun, lch, lstack, cchar, &
                iposr, num_significant_char, chulp, ihulp, rhulp, &
                itype, ierr)
        ! a read error
        if (ierr  /= 0) goto 9999
        ! a token has arrived
        if (itype == 1) then                                     ! that must be an absolute timer string
            call convert_string_to_time_offset (chulp, ihulp, .false., .false., ierr)    !  2^31 =  2147483648
            if (ihulp == -999) then                              !       yyyydddhhmmss so 64 bits integer
                ierr = 1
                write (file_unit, 1020) trim(chulp)
                goto 9999
            endif
            if (ierr   /=    0) then                              ! the found entry is not a new time value
                if (num_records <= 1) then
                    write (file_unit, 1040) num_records
                    !ierr3 = ierr3 + 1
                endif
                ierr = 0
                goto 9999
            endif
            ihulp = itfact * ihulp
        elseif (itype == 2) then
            call convert_relative_time (ihulp, 1, is_date_format, is_yyddhh_format)
        else
            ihulp = 0
        endif
        ! getting the data of this block (no strings any more)
        20 if (time_dependent .and. newrec) then
            !          it was a non-real and characters has been caught
            if (ihulp == -999) then
                ignore = .true.
            else                                                      ! a new breakpoint found
                ignore = .false.
                num_records = num_records + 1
                if (num_records <= max_int_size) then
                    int_array(num_records) = ihulp
                    if (num_records > 1) then
                        if (ihulp <= int_array(num_records - 1)) then ! times not strinctly ascending
                            write (file_unit, 1030) ihulp, int_array(num_records - 1)
                            ierr3 = ierr3 + 1
                        endif
                    endif
                else
                    write (file_unit, 1000) max_int_size
                    ierr = 100
                    goto 9999
                endif
            endif
            newrec = .false.
            goto 10
        endif
        !
        if (.not. ignore) then
            do i = 1, notot / nototc
                real_array(itel + (i - 1) * nototc) = rhulp
            end do
        endif
        !        are we to expect a new record ?
        if (mod(itel2, nototc) == 0) then
            newrec = .true.
            itel = itel + notot - nototc
        end if
        !        it was a constant, so we can now return.
        if (newrec .and. (.not. time_dependent)) then
            num_records = 1
            int_array(1) = 0
            goto 9999
        endif
        !        increase the counter for the next real and go to input
        if (.not. ignore) itel = itel + 1
        itel2 = itel2 + 1
        goto 10
        9999 if (timon) call timstop(ithndl)
        return

        1000 FORMAT (' ERROR ! Number of breakpoints exceeds system', &
                ' maximum of: ', I10)
        1020 FORMAT (' ERROR ! Absolute timer does not fit in timer ', &
                'format: ', A, / &
                ' Is your T0 setting in block #1 correct?'/, &
                ' Allowed difference with T0 is usually ca. 68 years.')
        1030 FORMAT (/' ERROR ! Time value ', I10, ' not larger than previous time value ', I10)
        1040 FORMAT (/' WARNING ! There are only ', I2, ' breakpoints found for this time series')

    END SUBROUTINE DLWQ5D

end module m_dlwq5d
