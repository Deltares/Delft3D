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
module m_print_matrix
    use m_waq_precision

    implicit none

contains


    subroutine print_matrix(lunut, iwidth, dlwqdata, strng1, strng2, &
            strng3, ioutpt)

        !     Deltares Software Centre

        !     function : prints blocks of data, also scale and convert

        !     Global declarations

        use timers        !   performance timers
        use dlwq_hyd_data ! for definition and storage of data
        implicit none

        !     declaration of arguments

        integer(kind = int_wp), intent(in) :: lunut         ! report file
        integer(kind = int_wp), intent(in) :: iwidth        ! width of output
        type(t_dlwqdata), intent(inout) :: dlwqdata     ! data block to be filled
        character(len = *), intent(in) :: strng1       ! write string 1 (items)
        character(len = *), intent(in) :: strng2       ! write string 2 (values/concs)
        character(len = *), intent(in) :: strng3       ! write string 3 (brkp/harm)
        integer(kind = int_wp), intent(in) :: ioutpt        ! output file option

        !     local declarations

        logical :: deflts       ! defaults for the parameters
        integer(kind = int_wp) :: nopar         ! dlwqdata%no_param
        integer(kind = int_wp) :: noloc         ! dlwqdata%no_loc
        integer(kind = int_wp) :: nobrk         ! dlwqdata%no_brk
        integer(kind = int_wp) :: ftype         ! dlwqdata%functype
        integer(kind = int_wp) :: iorder        ! dlwqdata%iorder
        integer(kind = int_wp) :: ipar          ! loop counter
        integer(kind = int_wp) :: iloc          ! loop counter
        integer(kind = int_wp) :: ibrk          ! loop counter
        integer(kind = int_wp) :: k, ie         ! loop counter
        integer(kind = int_wp) :: iploc         ! index number
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("print_matrix", ithndl)

        ! just print a message if data comes from an external source

        if (dlwqdata%extern) then
            if (dlwqdata%filetype == FILE_BINARY) then
                write (lunut, 1130) trim(dlwqdata%filename)
            else
                write (lunut, 1135) trim(dlwqdata%filename)
            endif
            goto 70
        endif

        ! initialisation

        nopar = dlwqdata%no_param
        noloc = dlwqdata%no_loc
        nobrk = dlwqdata%no_brk
        ftype = dlwqdata%functype
        iorder = dlwqdata%iorder
        deflts = dlwqdata%loc_defaults

        ! scale factors

        if (dlwqdata%param_scaled) then

            ! print scale factors

            if (ioutpt >= 4) then
                write (lunut, 1010)
                do ipar = 1, nopar, iwidth
                    ie = min(ipar + iwidth - 1, nopar)
                    if (dlwqdata%param_pointered) then
                        write (lunut, 1020) (dlwqdata%param_pointers(k), k = ipar, ie)
                    else
                        write (lunut, 1020) (k, k = ipar, ie)
                    endif
                    if (dlwqdata%param_named) then
                        write (lunut, 1025) (dlwqdata%param_name(k), k = ipar, ie)
                    else
                        if (dlwqdata%param_pointered) then
                            write (lunut, 1025) (car_used(dlwqdata%param_pointers(k)), k = ipar, ie)
                        else
                            write (lunut, 1025) (car_used(k), k = ipar, ie)
                        endif
                    endif
                    write (lunut, 1030) (dlwqdata%factor_param(k), k = ipar, ie)
                enddo
            endif

            ! perform the actual scaling

            do ibrk = 1, nobrk
                do iloc = 1, noloc
                    do ipar = 1, nopar
                        if (iorder == ORDER_PARAM_LOC) then
                            dlwqdata%values(ipar, iloc, ibrk) = dlwqdata%values(ipar, iloc, ibrk) * dlwqdata%factor_param(ipar)
                        else
                            dlwqdata%values(iloc, ipar, ibrk) = dlwqdata%values(iloc, ipar, ibrk) * dlwqdata%factor_param(ipar)
                        endif
                    enddo
                enddo
            enddo

            dlwqdata%param_scaled = .false.
            deallocate(dlwqdata%factor_param)

        endif

        ! convert breakpoints, no more, already been done directly after the read

        if (nobrk > 1) then
            if (ioutpt >= 4) write (lunut, 1040) strng3, nobrk
            if (deflts .and. ioutpt >= 4) write (lunut, 1050)
        else
            if (deflts) then
                if (ioutpt >= 4) write (lunut, 1050)
            else
                if (ioutpt >= 4) write (lunut, 1060)
            endif
        endif

        ! write formatted output

        if (ioutpt >= 4) then
            do ibrk = 1, nobrk
                if (nobrk > 1) then
                    if (ftype == 1) write (lunut, 1070) strng3, ibrk, dlwqdata%times(ibrk)
                    if (ftype == 2) write (lunut, 1070) strng3, ibrk, dlwqdata%times(ibrk)
                    if (ftype == 3) write (lunut, 1080) ibrk, dlwqdata%times(ibrk), dlwqdata%phase(ibrk)
                    if (ftype == 4) write (lunut, 1090) ibrk, dlwqdata%times(ibrk), dlwqdata%phase(ibrk)
                endif
                do ipar = 1, nopar, iwidth
                    ie = min(ipar + iwidth - 1, nopar)
                    if (dlwqdata%param_pointered) then
                        write (lunut, 1100) strng2, (dlwqdata%param_pointers(k), k = ipar, ie)
                    else
                        write (lunut, 1100) strng2, (k, k = ipar, ie)
                    endif
                    if (dlwqdata%param_named) then
                        write (lunut, 1150) strng1, (dlwqdata%param_name(k), k = ipar, ie)
                    else
                        if (dlwqdata%param_pointered) then
                            write (lunut, 1150) strng1, (car_used(dlwqdata%param_pointers(k)), k = ipar, ie)
                        else
                            write (lunut, 1150) strng1, (car_used(k), k = ipar, ie)
                        endif
                    endif
                    do iloc = 1, noloc
                        if (dlwqdata%loc_pointered) then
                            iploc = abs(dlwqdata%loc_pointers(iloc))
                        else
                            iploc = iloc
                        endif
                        if (iorder == ORDER_PARAM_LOC) then
                            write (lunut, 1120) iploc, (dlwqdata%values(k, iloc, ibrk), k = ipar, ie)
                        else
                            write (lunut, 1120) iploc, (dlwqdata%values(iloc, k, ibrk), k = ipar, ie)
                        endif
                    enddo
                enddo
            enddo
        endif
        !
        70 continue

        if (timon) call timstop(ithndl)
        return
        !
        1010 format (' scale factors for this block of data: ')
        1020 format (' scale    :', i6, 9i12)
        1025 format (' substance:', 10('  ', a10))
        1030 format (' values   :', 10e12.4)
        1040 format (/' number of ', a, 's with full data:', i5)
        1050 format (' default values in this block.')
        1060 format (' constant values in this block.')
        1070 format (' ', a, ' ', i3, ' :', i10)
        1080 format (' harmonic: ', i3, ' :', i10, ' phase: ', 10e12.4)
        1090 format (' fourier : ', i3, ' :', i10, ' phase: ', 10e12.4)
        1100 format (' ', a, i6, 9i12)
        1150 format (' ', a, ' ', 10('  ', a10))
        1120 format (i10, 2x, 1p, 10e12.4)
        1130 format (' info comes at runtime from binary file: ', a)
        1135 format (' info comes at runtime from external source: ', a)
        !
    end subroutine print_matrix


    character*20 function car_used(i)
        integer(kind = int_wp) :: i
        if (i > 0) then
            car_used = 'used'
        elseif (i == 0) then
            car_used = 'FLOW'
        else
            car_used = 'ignored'
        endif
        return
    end function car_used

end module m_print_matrix
