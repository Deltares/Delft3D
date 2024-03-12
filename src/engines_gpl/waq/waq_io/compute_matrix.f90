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
module m_compute_matrix
    use m_waq_precision

    implicit none

contains


    subroutine compute_matrix (lunut, data_param, data_loc, waq_param, waq_loc, &
            amiss, fdata, wdata)

        !     Deltares Software Centre

        !     function : assign matrix according to computational
        !                rules


        !     Global declarations

        use dlwq_hyd_data  ! for definition and storage of data
        use timers         ! performance timers

        implicit none

        !     declaration of arguments

        integer(kind = int_wp), intent(in) :: lunut         ! report file
        type(t_dlwq_item), intent(in) :: data_param   ! list of param items in the data
        type(t_dlwq_item), intent(in) :: data_loc     ! list of loc items in the data
        type(t_dlwq_item), intent(in) :: waq_param    ! list of waq param items to be set
        type(t_dlwq_item), intent(in) :: waq_loc      ! list of waq loc items to be set
        real(kind = real_wp), intent(in) :: amiss         ! missing value
        type(t_dlwqdata), intent(in) :: fdata        ! data block input
        type(t_dlwqdata), intent(out) :: wdata        ! data block output

        !     local declarations

        integer(kind = int_wp) :: iorder        ! order of the parameters and locations in the data array
        integer(kind = int_wp) :: functype      ! function type
        integer(kind = int_wp) :: nobrk         ! number of breakpoints
        integer(kind = int_wp) :: ndim1         ! first dimension of values
        integer(kind = int_wp) :: ndim2         ! second dimension of values
        integer(kind = int_wp) :: ibrk          ! loop counter breakpoints
        integer(kind = int_wp) :: iloc          ! loop counter locations
        integer(kind = int_wp) :: ipar          ! loop counter parameters
        integer(kind = int_wp) :: ipar_out      ! index output parameter
        integer(kind = int_wp) :: ip            ! assignment rule and indx pointer for parameter
        integer(kind = int_wp) :: ip2           ! index pointer for parameter in data array
        integer(kind = int_wp) :: iparo         ! parameter which is subject to current min or max
        logical :: miniem       ! is minimum to be applied
        logical :: maxiem       ! is maximum to be applied
        logical :: close_accum  ! is calculation ready for this output parameter
        real(kind = real_wp) :: accum         ! accumulation of calculation
        real(kind = real_wp) :: aminv         ! minimum value to be applied
        real(kind = real_wp) :: amaxv         ! maximum value to be applied
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("compute_matrix", ithndl)

        ! some initialisation

        iorder = wdata%iorder
        functype = wdata%functype
        miniem = .false.
        maxiem = .false.
        ndim1 = wdata%no_param
        ndim2 = wdata%no_loc
        nobrk = fdata%no_brk
        allocate(wdata%times(nobrk), wdata%values(ndim1, ndim2, nobrk))
        wdata%no_brk = nobrk
        wdata%times = fdata%times

        ! assignment loop

        breakpoint_loop : do ibrk = 1, nobrk

            ! if harmonics then deal with the phase

            if (functype == FUNCTYPE_HARMONIC .or. &
                    functype == FUNCTYPE_FOURIER) then
                !           phase = phase(ibrk)
            endif

            location_loop : do iloc = 1, data_loc%no_item

                ipar_out = 1
                accum = 0.0
                parameter_loop : do ipar = 1, data_param%no_item

                    ip = data_param%ipnt(ipar)
                    ip2 = data_param%sequence(ipar)

                    ! normal processing

                    if (ip > -900000) then
                        if (ip > 0) then
                            accum = fdata%values(ip2, iloc, ibrk)
                        else
                            accum = data_param%constant(ipar)
                        endif
                        wdata%values(ipar_out, iloc, ibrk) = 0.0
                        wdata%values(ipar_out, iloc, ibrk) = 0.0
                        iparo = ip
                    endif

                    ! ignore value

                    if (ip <= -1300000000) then
                        ip = 0
                    endif

                    ! a maximum value need to be applied

                    if (ip <= -1190000000) then
                        ip = ip + 1200000000
                        if (wdata%values(ipar_out, iloc, ibrk) /= amiss .and. accum /= amiss) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = amiss
                        endif
                        accum = 0.0
                        maxiem = .true.
                        if (ip == 0) then
                            amaxv = fdata%values(ip2, iloc, ibrk)
                        endif
                        if (ip < 0) amaxv = data_param%constant(ipar)
                        if (ip > 0) amaxv = wdata%values(ip, iloc, ibrk)
                    endif

                    ! a minimum value need to be applied

                    if (ip <= -1090000000) then
                        ip = ip + 1100000000
                        if (wdata%values(ipar_out, iloc, ibrk) /= amiss .and. accum /= amiss) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = amiss
                        endif
                        accum = 0.0
                        miniem = .true.
                        if (ip == 0) then
                            aminv = fdata%values(ip2, iloc, ibrk)
                        endif
                        if (ip < 0) aminv = data_param%constant(ipar)
                        if (ip > 0) aminv = wdata%values(ip, iloc, ibrk)
                    endif

                    ! a minus sign need to be applied

                    if (ip <= -900000000) then
                        ip = ip + 1000000000
                        if (wdata%values(ipar_out, iloc, ibrk) /= amiss .and. accum /= amiss) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = amiss
                        endif
                        if (ip == 0) then
                            accum = -fdata%values(ip2, iloc, ibrk)
                        endif
                        if (ip < 0) accum = -data_param%constant(ipar)
                        if (ip > 0) accum = -wdata%values(ip, iloc, ibrk)
                        if (accum == -amiss) accum = amiss
                    endif

                    ! a plus sign need to be applied

                    if (ip <= -90000000) then
                        ip = ip + 100000000
                        if (wdata%values(ipar_out, iloc, ibrk) /= amiss .and. accum /= amiss) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = amiss
                        endif
                        if (ip == 0) then
                            accum = fdata%values(ip2, iloc, ibrk)
                        endif
                        if (ip < 0) accum = data_param%constant(ipar)
                        if (ip > 0) accum = wdata%values(ip, iloc, ibrk)
                    endif

                    ! a division need to be applied

                    if (ip <= -9000000) then
                        ip = ip + 10000000
                        if (ip == 0) then
                            if (fdata%values(ipar, iloc, ibrk) /= amiss .and. accum /= amiss) then
                                accum = accum / fdata%values(ip2, iloc, ibrk)
                            else
                                accum = amiss
                            endif
                        endif
                        if (ip < 0) then
                            if (data_param%constant(ipar) /= amiss .and. accum /= amiss) then
                                accum = accum / data_param%constant(ipar)
                            else
                                accum = amiss
                            endif
                        endif
                        if (ip > 0) then
                            if (wdata%values(ip, iloc, ibrk) /= amiss .and. accum /= amiss) then
                                accum = accum / wdata%values(ip, iloc, ibrk)
                            else
                                accum = amiss
                            endif
                        endif
                    endif

                    ! a multiplication need to be applied

                    if (ip <= -900000) then
                        ip = ip + 1000000
                        if (ip == 0) then
                            if (fdata%values(ipar, iloc, ibrk) /= amiss .and. accum /= amiss) then
                                accum = accum * fdata%values(ip2, iloc, ibrk)
                            else
                                accum = amiss
                            endif
                        endif
                        if (ip < 0) then
                            if (data_param%constant(ipar) /= amiss .and. accum /= amiss) then
                                accum = accum * data_param%constant(ipar)
                            else
                                accum = amiss
                            endif
                        endif
                        if (ip > 0) then
                            if (wdata%values(ip, iloc, ibrk) /= amiss .and. accum /= amiss) then
                                accum = accum * wdata%values(ip, iloc, ibrk)
                            else
                                accum = amiss
                            endif
                        endif
                    endif

                    ! close accumulation if last parameter or next one is normal

                    if (ipar == data_param%no_item) then
                        close_accum = .true.
                    else
                        if (data_param%ipnt(ipar + 1) > -900000) then
                            close_accum = .true.
                        else
                            close_accum = .false.
                        endif
                    endif

                    if (close_accum) then
                        if (wdata%values(ipar_out, iloc, ibrk) /= amiss .and. accum /= amiss) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = amiss
                        endif
                        if (maxiem) then
                            if (wdata%values(ipar_out, iloc, ibrk) > amaxv .and. &
                                    wdata%values(ipar_out, iloc, ibrk) /= amiss) then
                                write (lunut, 1000) ibrk, iparo, iloc
                                write (lunut, 1010) wdata%values(ipar_out, iloc, ibrk), amaxv
                                wdata%values(ipar_out, iloc, ibrk) = amaxv
                            endif
                        endif
                        if (miniem) then
                            if (wdata%values(ipar_out, iloc, ibrk) < aminv .and. &
                                    wdata%values(ipar_out, iloc, ibrk) /= amiss) then
                                write (lunut, 1000) ibrk, iparo, iloc
                                write (lunut, 1020) wdata%values(ipar_out, iloc, ibrk), aminv
                                wdata%values(ipar_out, iloc, ibrk) = aminv
                            endif
                        endif
                        ipar_out = ipar_out + 1
                        maxiem = .false.
                        miniem = .false.
                        accum = 0.0
                    endif

                enddo parameter_loop

            enddo location_loop

        enddo breakpoint_loop

        if (timon) call timstop(ithndl)
        return
        !
        1000 format (' warning: processing breakpoint', i6, ' for substance', i3, &
                ' at station', i5)
        1010 format (' the value of ', e15.6, ' is overwritten by the maximum ', &
                ' of ', e15.6, ' !')
        1020 format (' the value of ', e15.6, ' is overwritten by the minimum ', &
                ' of ', e15.6, ' !')
        !
    end

end module m_compute_matrix
