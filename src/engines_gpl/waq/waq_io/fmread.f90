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
module m_fmread
    use m_waq_precision
    use m_scale

    implicit none

contains


    subroutine fmread (nitem, item, nvals, nfact, factor, &
            nobrk, ibrk, arrin, dtflg, dtflg3, &
            ifact, iwidth, ioutpt, ierr)

        !       Deltares Software Centre

        !>\file
        !>                   Reads blocks of matrices of input values and scales them
        !>
        !>                   This routine reads:
        !>                   - nobrk integer breakpoint values for time
        !>                   - for each breakpoint nitem*nvals values
        !>                   The values are scaled with nvals scale factors/n
        !>                   If one scale factor exist, it is expanded to nvals factors

        !     Created            : March '88  By M.E. Sileon / L. Postma

        !     Modified           : April 1997 by R. Bruinsma: Tokenized input data file reading added
        !                          May   2011    Leo Postma : Fortran 90 look and feel

        !     Subroutines called : none

        !     Functions   called : gettoken from rd_token to read the data
        !                          convert_string_to_time_offset   to convert a time string to seconds
        !                          cnvtim   to convert a time integer to seconds
        !                          scale    to scale the matrix

        !     Logical units      : lunut = unit formatted output file

        use timers       !   performance timers
        use rd_token       ! for the reading of tokens
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        implicit none

        !     Parameters

        !     kind           function         name                        Descriptipon

        integer(kind = int_wp), intent(in) :: nitem                      !< number of items
        integer(kind = int_wp), intent(in) :: item  (nitem)              !< item numbers
        integer(kind = int_wp), intent(in) :: nvals                      !< number of values per item
        integer(kind = int_wp), intent(in) :: nfact                      !< number of scale factors
        real(kind = real_wp), intent(inout) :: factor(nvals)              !< scale factors
        integer(kind = int_wp), intent(in) :: nobrk                      !< number of breakpoints
        integer(kind = int_wp), intent(out) :: ibrk  (nobrk)              !< breakpoints read
        real(kind = real_wp), intent(out) :: arrin (nvals, nitem, nobrk)  !< breakpoints read
        logical  (4), intent(in) :: dtflg                     !< 'date'-format time scale
        logical  (4), intent(in) :: dtflg3                    !< (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: ifact                      !< factor between timings
        integer(kind = int_wp), intent(in) :: iwidth                     !< width of the output file
        integer(kind = int_wp), intent(in) :: ioutpt                     !< how extensive is output ?
        integer(kind = int_wp), intent(inout) :: ierr                       !< cumulative error count

        !     local decalations

        integer(kind = int_wp) :: ierr2         ! local error variable
        integer(kind = int_wp) :: i1, i2, i3    ! loop counters
        integer(kind = int_wp) :: k             ! loop counter
        integer(kind = int_wp) :: ie1, ie2      ! endpoint help variables
        character(255) ctoken       ! to read a time string
        integer(kind = int_wp) :: itype         ! to indicate what was read
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("fmread", ithndl)

        if (ioutpt < 4) write (lunut, 2000)

        do i1 = 1, nobrk

            if (gettoken(ctoken, ibrk(i1), itype, ierr2) > 0) goto 10
            if (itype == 1) then                                    !  a time string
                if (ioutpt >= 4) write (lunut, 2010) i1, ctoken
                call convert_string_to_time_offset (ctoken, ibrk(i1), .false., .false., ierr2)
                if (ibrk(i1) == -999) then
                    write (lunut, 2020) trim(ctoken)
                    goto 10
                endif
                if (ierr2 > 0) then
                    write (lunut, 2030) trim(ctoken)
                    goto 10
                endif
            else                                                        !  an integer for stop time
                if (ioutpt >= 4) write (lunut, 2040) i1, ibrk(i1)
                call convert_relative_time (ibrk(i1), ifact, dtflg, dtflg3)
            endif

            do i3 = 1, nitem
                do i2 = 1, nvals
                    if (gettoken(arrin(i2, i3, i1), ierr2) > 0) goto 10
                enddo
            enddo

            if (ioutpt >= 4) then

                do i2 = 1, nvals, iwidth
                    ie1 = min(i2 + iwidth - 1, nfact)
                    ie2 = min(i2 + iwidth - 1, nvals)
                    write (lunut, 2050)        (k, k = i2, ie2)
                    write (lunut, 2060) (factor(k), k = i2, ie1)
                    write (lunut, 2070)
                    do i3 = 1, nitem
                        write (lunut, 2080)  abs(item(i3)), (arrin(k, i3, i1), k = i2, ie2)
                    enddo
                enddo

            endif

        enddo

        !      Scale values

        if (nfact == 1) then
            do i1 = 2, nvals
                factor(i1) = factor (11)
            enddo
        endif
        do i1 = 1, nobrk
            call scale  (arrin(1, 1, i1), factor, nitem, nvals)
        enddo
        if (timon) call timstop(ithndl)
        return

        !      An error occured during read

        10 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        !      Output formats

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

    end

end module m_fmread
