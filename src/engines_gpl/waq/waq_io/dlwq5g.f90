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
module m_dlwq5g
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains


    subroutine dlwq5g (lunut, i_array, count_items_assign, count_items_comp_rule, count_subs_assign, &
            count_subs_comp_rule, index_first, i_max, names_to_check, start_in_line, &
            npos, ilun, lch, lstack, cchar, &
            chulp, nocol, dtflg1, dtflg3, itfact, &
            itype, ihulp, rhulp, error_idx, status)
        !
        !
        !     Deltares        Sector Waterresources And Environment
        !
        !     Created            : March '00  by L. Postma
        !
        !     Modified           :
        !
        !     Function           : Checks if column header exists
        !
        !     Subroutines Called : none
        !
        !     Logical Units      : LUN(27) = unit stripped DELWAQ input file
        !                          LUN(29) = unit formatted output file
        !
        !     Parameters    :
        !
        !     Name                    Kind         Length     Funct.  Description
        !     ------------------------------------------------------------------------------------
        !     lunut                  integer        1            input   unit number for ascii output
        !     i_array                integer      i_max          in/out  integer workspace array
        !     i_max                  integer        1            input   max. integer workspace dimension
        !     count_items_assign     integer        1            in/out  number of items for assignment
        !     count_items_comp_rule  integer        1            in      number of items in computational rule
        !     count_items_entries    integer        1            in      number of items entries to be filled
        !     count_subs_assign      integer        1            in/out  number of subst for assignment
        !     count_subs_comp_rule   integer        1            in      number of subst in computational rule
        !     count_subs_entries     integer        1            in      number of subst entries to be filled
        !     index_first            integer        1            in      1 = items first, 2 = substances first
        !     names_to_check         char*(*)      count_names   input   names of items to check for presence
        !     count_names            integer        1            in/out  start position on input line
        !     start_in_line          integer        1            in/out  start position on input line
        !     npos                   integer        1            input   number of significant characters
        !     ilun                   integer       lstack        input   unitnumb include stack
        !     lch                    char*(*)      lstack        input   file name stack, 4 deep
        !     lstack                 integer        1            input   include file stack size
        !     cchar                  char*1         1            input   comment character
        !     chulp                  char*(*)       1            output  space for limiting token
        !     nocol                  integer        1            output  number of collums in matrix
        !     dtflg1                 logical        1            input   true if time in 'date' format
        !     dtflg3                 logical        1            input   true if yyetc instead of ddetc
        !     itfact                 integer        1            input   factor between clocks
        !     itype                  integer        1            output  type of info at end
        !     ihulp                  integer        1            output  parameter read to be transferred
        !     rhulp                  real           1            output  parameter read to be transferred
        !     error_idx              integer        1            output  error index within current subroutine
        !     offset_i_array         integer        1            output  offset  in i_array
        !     offset_names           integer        1            output  offset in names_to_check
        !     offset_common          integer        1            output  comon offset in i_array and names_to_check
        !
        !
        use m_dlwq5h
        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        integer(kind = int_wp) :: i_max
        character*(*) lch   (lstack), chulp, names_to_check(:)
        character     cchar*1, strng*8
        dimension     i_array(:), ilun(lstack)
        logical       dtflg1, dtflg3, first, must_read_more
        integer(kind = INT64) :: ihulp8
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: i, count_items_comp_rule, count_subs_assign, count_subs_comp_rule, index_first, offset_names
        integer(kind = int_wp) :: offset_common, notim
        integer(kind = int_wp) :: itype, lunut, ilun, start_in_line, nopos, ihulp, error_idx
        integer(kind = int_wp) :: i_array, nocol, ifound, itfact, icnt, iods, k
        integer(kind = int_wp) :: offset_i_array, count_items_assign, count_names, npos, lstack
        real(kind = real_wp) :: rhulp

        type(error_status), intent(inout) :: status !< current error status

        if (timon) call timstrt("dlwq5g", ithndl)
        !
        !     Array offsets
        !
        offset_i_array = count_items_assign + count_items_comp_rule + &
                count_subs_assign + count_subs_comp_rule
        if (index_first == 1) then ! items first
            offset_names = count_items_assign + count_items_comp_rule + count_subs_assign
            offset_common = count_items_assign + count_items_comp_rule
            count_names = count_subs_comp_rule
        else if (index_first == 2) then !substances first
            offset_names = count_items_assign + count_subs_comp_rule + count_subs_assign
            offset_common = count_subs_comp_rule + count_subs_assign
            count_names = count_items_comp_rule
        endif
        !
        !     read loop
        !
        first = .true.
        must_read_more = .true.
        do while (must_read_more)
            itype = 0
            call rdtok1 (lunut, ilun, lch, lstack, cchar, &
                    start_in_line, npos, chulp, ihulp, rhulp, &
                    itype, error_idx)

            if (error_idx  /= 0) then ! error occurred when reading
                if (timon) call timstop(ithndl)
                return !exit subroutine
            end if

            !         no error
            if (itype == 1) then ! a string has arrived
                call convert_string_to_time_offset (chulp, ihulp, .false., .false., error_idx)
                if (error_idx == 0) then
                    error_idx = -2
                    if (first) then
                        if (timon) call timstop(ithndl)
                        return  !exit subroutine
                    else
                        exit
                    endif
                endif
                if (first) then
                    first = .false.
                    do i = 1, count_names
                        i_array(offset_i_array + i) = 0
                    end do
                    nocol = 0
                    write (lunut, *)
                endif
                nocol = nocol + 1
                strng = 'NOT used'
                do i = 1, count_names
                    if (string_equals(chulp(:20), names_to_check(offset_names + i))) then
                        strng = 'used'
                        i_array(i + offset_i_array) = nocol
                    endif
                end do
                write (lunut, 1000) nocol, chulp, strng
            else
                if (itype == 2) then ! an integer has arrived
                    call convert_relative_time (ihulp, itfact, dtflg1, dtflg3)
                endif
                error_idx = -1
                must_read_more = .false.
                if (first) then
                    if (timon) call timstop(ithndl)
                    return !exit subroutine
                end if
            endif
        end do
        !
        !     is everything resolved ?
        icnt = 0
        iods = 0

        do i = 1, count_names
            k = i - icnt
            if ((names_to_check(offset_names + k) /= '&$&$SYSTEM_NAME&$&$!') &
                    .and.  (i_array(offset_i_array + k) <= 0)) then
                call compact_usefor_list (lunut, i_array, count_items_assign, &
                        count_items_comp_rule, count_subs_assign, &
                        count_subs_comp_rule, index_first, names_to_check, &
                        offset_i_array, offset_names, &
                        iods, offset_common, k, icnt, error_idx, status)
            end if
        end do
        !
        1000 FORMAT (' Column:', I3, ' contains: ', A40, ' Status: ', A8)
        !
    end subroutine dlwq5g

end module m_dlwq5g
