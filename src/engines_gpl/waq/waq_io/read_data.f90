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
module m_read_data
    use m_waq_precision

    implicit none

contains


    subroutine read_data(data_block, itfact, dtflg1, dtflg3, ierr)

        !     Deltares Software Centre

        !     function : read a (time dependent) data matrix from input

        !     global declarations

        use dlwq_hyd_data
        use rd_token
        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        implicit none

        !     declaration of the arguments

        type(t_dlwqdata), intent(inout) :: data_block   ! data block
        integer(kind = int_wp), intent(in) :: itfact        ! factor between clocks
        logical, intent(in) :: dtflg1       ! true if time in 'date' format
        logical, intent(in) :: dtflg3       ! true if yyetc instead of ddetc
        integer(kind = int_wp), intent(inout) :: ierr          ! cummulative error count

        !     local declarations

        integer(kind = int_wp) :: ftype          ! function type (constant,block,linear,harmonic,fourier)
        integer(kind = int_wp) :: mxbrk          ! allocate dimension of third dimension
        integer(kind = int_wp) :: ndim1          ! first dimension
        integer(kind = int_wp) :: ndim2          ! second dimension
        integer(kind = int_wp) :: nobrk          ! third dimension
        integer(kind = int_wp), pointer :: times2(:)      ! used to resize
        real(kind = real_wp), pointer :: phase2(:)      ! used to resize
        real(kind = real_wp), pointer :: values2(:, :, :) ! used to resize
        integer(kind = int_wp) :: t_asked        ! type of token asked
        integer(kind = int_wp) :: t_token        ! type of token
        character(len = 256) :: ctoken        ! character token
        integer(kind = int_wp) :: itoken         ! integer token
        real(kind = real_wp) :: rtoken         ! real token
        character :: cdummy        ! dummy
        integer(kind = int_wp) :: idummy         ! dummy
        real(kind = real_wp) :: rdummy         ! dummy
        integer(kind = int_wp) :: i1, i2, i3       ! indexes
        integer(kind = int_wp) :: ibrk           ! indexe
        integer(kind = int_wp) :: ierr_alloc     ! error status
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_data", ithndl)

        ! dimension according to order

        if (data_block%iorder == ORDER_PARAM_LOC) then
            ndim1 = data_block%no_param
            ndim2 = data_block%no_loc
        else
            ndim1 = data_block%no_loc
            ndim2 = data_block%no_param
        endif

        ! read dependent on type of function

        ftype = data_block%functype
        if (ftype == FUNCTYPE_CONSTANT) then

            ! read only one "time"

            nobrk = 1
            allocate(data_block%values(ndim1, ndim2, nobrk), stat = ierr_alloc)
            data_block%values = 0.0
            allocate(data_block%times(nobrk), stat = ierr_alloc)
            data_block%times(1) = 0
            do i2 = 1, ndim2
                do i1 = 1, ndim1
                    if (gettoken(rtoken, ierr) /= 0) goto 9999
                    data_block%values(i1, i2, nobrk) = rtoken
                enddo
            enddo

        else

            ! read breakpoints in loop till next token is no longer a valid time

            mxbrk = 10
            allocate(data_block%times(mxbrk), data_block%values(ndim1, ndim2, mxbrk))
            if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                allocate(data_block%phase(mxbrk))
            endif

            nobrk = 0
            breakpoints : do

                ! get next time

                if (gettoken(ctoken, itoken, rtoken, t_token, ierr) /= 0) then
                    ierr = 0
                    push = .true.
                    exit breakpoints
                endif

                ! check if character is a time string and convert

                if (t_token == TYPE_CHAR) then
                    call convert_string_to_time_offset (ctoken, itoken, .false., .false., ierr)
                    if (ierr /= 0) then
                        ierr = 0
                        push = .true.
                        exit breakpoints
                    endif
                else
                    call convert_relative_time (itoken, itfact, dtflg1, dtflg3)
                endif

                nobrk = nobrk + 1
                if (nobrk > mxbrk) then ! resize
                    mxbrk = mxbrk * 2
                    allocate(times2(mxbrk), values2(ndim1, ndim2, mxbrk))
                    do ibrk = 1, nobrk - 1
                        times2(ibrk) = data_block%times(ibrk)
                    end do
                    do ibrk = 1, nobrk - 1
                        do i2 = 1, ndim2
                            do i1 = 1, ndim1
                                values2(i1, i2, ibrk) = data_block%values(i1, i2, ibrk)
                            enddo
                        enddo
                    enddo
                    deallocate(data_block%times, data_block%values)
                    data_block%times => times2
                    data_block%values => values2
                    nullify(times2)
                    nullify(values2)
                    if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                        allocate(phase2(mxbrk))
                        do ibrk = 1, nobrk - 1
                            phase2(ibrk) = data_block%phase(ibrk)
                        end do
                        deallocate(data_block%phase)
                        data_block%phase => phase2
                        nullify(phase2)
                    endif
                endif
                data_block%times(nobrk) = itoken

                ! for harmonics and fourier get phase

                if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                    if (gettoken(rtoken, ierr) /= 0) exit
                    data_block%phase(nobrk) = rtoken
                endif

                ! get the data_block%values for this time

                do i2 = 1, ndim2
                    do i1 = 1, ndim1
                        if (gettoken(rtoken, ierr) /= 0) goto 9999
                        data_block%values(i1, i2, nobrk) = rtoken
                    enddo
                enddo

            enddo breakpoints

            !        input ready, resize back the arrays

            if (nobrk /= mxbrk) then
                allocate(times2(nobrk), values2(ndim1, ndim2, nobrk))
                do ibrk = 1, nobrk
                    times2(ibrk) = data_block%times(ibrk)
                end do
                do ibrk = 1, nobrk
                    do i2 = 1, ndim2
                        do i1 = 1, ndim1
                            values2(i1, i2, ibrk) = data_block%values(i1, i2, ibrk)
                        enddo
                    enddo
                enddo
                deallocate(data_block%times, data_block%values)
                data_block%times => times2
                data_block%values => values2
                nullify(times2)
                nullify(values2)
                if (ftype == FUNCTYPE_HARMONIC .or. ftype == FUNCTYPE_FOURIER) then
                    allocate(phase2(mxbrk))
                    do ibrk = 1, nobrk
                        phase2(ibrk) = data_block%phase(ibrk)
                    end do
                    deallocate(data_block%phase)
                    data_block%phase => phase2
                    nullify(phase2)
                endif
            endif

        endif

        data_block%no_brk = nobrk

        9999 if (timon) call timstop(ithndl)

    end subroutine read_data
end module m_read_data
