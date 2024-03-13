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
module m_matrix
    use m_waq_precision

    implicit none

    private
    public :: dmatrix, compute_matrix_size

contains


    subroutine dmatrix (ntot, nvals, nvarar, nvarnw, nobrk1, &
            nobrk2, ibrk, ibrknw, tab, tabnw, &
            item)

        !!  Merges a new table with a table with existing values
        !!
        !!  This is old style DELWAQ processing
        !!      - The base table is ( ntot, nobrk1 ) with the second dimension
        !!          an expandable number of breakpoints up to nobrk1+nobrk2
        !!      - The first nvarar variables of ntot are filled, with nobrk1
        !!          breakpoints at the timings of ibrk(nobrk1) that is also expandable
        !!      - Added are nvarnw variables, with nobrk2 different breakpoints
        !!          at the locations ibrknw(nobrk2)
        !!      - Where needed, rows are added to the original table at locations
        !!          of ibrknw(nobrk2) before breakpoint j ( statement 123-130 )./n
        !!       The values at the new rows for the first nvarar variables are
        !!       interpolated or expanded as block function depending on item()
        !!       ( statement 131-138 )./n
        !!       The new nvarnw variables are copied (142-144)
        !!       - Also the values of the nvarnw varibales at the old breakpoints
        !!          are interpolated  ( statement 101-108 )
        !!       - if breakpoints equal, the new variables are added (142-144)
        !!       - if the new breakpoints expand over the old set, the breakpoints
        !!          are added and the old values are copied ( statement 113-118 )
        !!        - The result is a wider table (nvarar is increased with nvarnw)
        !!          the sum will always be smaller or equal to ntot, because that
        !!          is the total amount of data that is expected
        !!        - The result is also a deeper table with potentially more break points
        !!        - Note that each variable has ndim2 numbers. A variable could be
        !!          a wasteload and ndim2 is then the amount of substances
        !!        This leads to very hughe tables ( river flows per hour, merged
        !!        35 substance concentrations gives all 36 values per hour). That
        !!        is why the new input processing stores the individual tables.

        use timers       !   performance timers
        use m_char1

        integer(kind = int_wp), intent(in) :: ntot                         !< first dimension of tab and tabnw
        integer(kind = int_wp), intent(in) :: nvals                        !< number of values per variable
        integer(kind = int_wp), intent(inout) :: nvarar                       !< number of existing variables in tab
        integer(kind = int_wp), intent(in) :: nvarnw                       !< number of variables to add to tab
        integer(kind = int_wp), intent(inout) :: nobrk1                       !< number of existing breakpoints
        integer(kind = int_wp), intent(in) :: nobrk2                       !< number of breakpoints to add
        integer(kind = int_wp), intent(inout) :: ibrk  (nobrk1 + nobrk2)        !< values of existing breakpoints
        integer(kind = int_wp), intent(in) :: ibrknw(nobrk2)               !< values of breakpoints to add
        real(kind = real_wp), intent(inout) :: tab   (ntot, nobrk1 + nobrk2)   !< existing table
        real(kind = real_wp), intent(in) :: tabnw (nvarnw * nvals, nobrk2)          !< table to merge
        integer(kind = int_wp), intent(in) :: item  (ntot)                 !< for type of interpolation per variable

        integer(kind = int_wp) :: i         ! loop counter for position in array IBRKNW
        integer(kind = int_wp) :: j         ! loop counter for position in array IBRK
        integer(kind = int_wp) :: k         ! help counter breakpoints
        integer(kind = int_wp) :: iv        ! help counter variables*values
        integer(kind = int_wp) :: iset      ! help variable for breakpoint
        integer(kind = int_wp) :: nposar    ! linear position first index for existing variables
        integer(kind = int_wp) :: nposnw    ! amount of values first index of variables to add
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("matrix", ithndl)

        ! If no breakpoints in IBRK/TAB then IBRK/ITAB==IBRKNW/TABNW
        if (nobrk1 == 0) then
            nobrk1 = nobrk2
            !jvb     nvarar = nvarnw
            ibrk = ibrknw
            tab(1:nvarnw * nvals, :) = tabnw
            goto 9999
        endif

        ! Some help variables
        nposar = nvarar * nvals
        nposnw = nvarnw * nvals

        ! begin loop over all breakpoints to add
        j = 0
        do i = 1, nobrk2
            iset = ibrknw(i)                     ! breakpoint to insert

            ! find first J with IBRK(J) >= IBRKNW(I)
            do k = j + 1, nobrk1
                j = k
                if (iset < ibrk(j)) goto 20
                if (iset == ibrk(j)) goto 30
                if (j == 1) then
                    do iv = 1, nposnw                             ! initialize expanded collumns for row j
                        tab(nposar + iv, j) = tabnw(iv, i)
                    enddo
                else
                    call interpolate_2d_array (tab(nposar + 1, j), tabnw(1, i), tab(nposar + 1, j - 1), ibrk(j), iset, &
                            ibrk(j - 1), nvarnw, nvals, item(nvarar + 1))
                endif
            end do

            ! add values if IBRKNW(I) > IBRK(NOBRK1)
            ibrk(j + 1) = iset                                     ! make a new row, initialise it with
            do iv = 1, nposar                                     ! existing collumns and go to the
                tab(iv, j + 1) = tab(iv, j)                     ! copy of the added collumns
            enddo
            j = j + 1
            nobrk1 = nobrk1 + 1
            goto 30

            ! insert values if IBRKNW(I) < IBRK(J)
            20    do k = nobrk1, j, -1                                 ! shift existing values up one row
                ibrk(k + 1) = ibrk (k)
                do iv = 1, nposar
                    tab(iv, k + 1) = tab(iv, k)
                enddo
            enddo
            nobrk1 = nobrk1 + 1                                 ! nr of breakpoints increases
            ibrk(j) = iset                                       ! added breakpoint at the start
            if (j == 1) then                                 ! copy existing values upfront
                do iv = 1, nposar
                    tab(iv, 1) = tab(iv, 2)
                enddo
            else                                                 ! interpolate existing values
                call interpolate_2d_array(tab (1, j), tab(1, j + 1), tab(1, j - 1), ibrk(j), ibrk(j + 1), &
                        ibrk(j - 1), nvarar, nvals, item)
            endif

            ! add values if IBRKNW(I) = IBRK(J)
            30    do iv = 1, nposnw                                    ! expand the row with the new
                tab(nposar + iv, j) = tabnw(iv, i)              ! columns
            enddo

        end do

        ! end the procedure with IBRKNW(NOBRK2) < IBRK(NOBRK1)
        do i = j + 1, nobrk1
            do iv = 1, nposnw
                tab(nposar + iv, i) = tabnw(iv, nobrk2)
            enddo
        enddo

        9999 if (timon) call timstop(ithndl)

    end subroutine dmatrix

    subroutine interpolate_2d_array(result, higher, lower, tset, thigh, &
            tlow, nvar, ndim2, iftyp)


        !! interpolates a (ndim2,nvar) array
        !!
        !! Depending on sign of iftyp routine does:
        !!      - if negative, choses lower ( block wave, propagate first )
        !!      - if positive, linear interpolation
        !! Note that iftyp may differ per variable in the matrix

        use timers       !   performance timers

        integer(kind = int_wp), intent(in) :: nvar                  !! number of variables
        integer(kind = int_wp), intent(in) :: ndim2                 !! data per variable
        integer(kind = int_wp), intent(in) :: tset                  !! interpolation time
        integer(kind = int_wp), intent(in) :: thigh                 !! time at end of interval
        integer(kind = int_wp), intent(in) :: tlow                  !! time at start of interval
        real(kind = real_wp), intent(out) :: result(ndim2, nvar)    !! resulting array
        real(kind = real_wp), intent(in) :: lower (ndim2, nvar)    !! lower end array
        real(kind = real_wp), intent(in) :: higher(ndim2, nvar)    !! higher end array
        integer(kind = int_wp), intent(in) :: iftyp (nvar)         !! interpolation type per variable

        real(kind = real_wp) :: factor1       ! weight of the higher end
        real(kind = real_wp) :: factor2       ! weight of the lower end
        integer(kind = int_wp) :: ivar          ! loop counter
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("interpolate_2d_array", ithndl)

        ! interpolate
        factor1 = float(tset - tlow) / float(thigh - tlow)
        factor2 = 1.0 - factor1

        do ivar = 1, nvar
            if (iftyp(ivar) < 0) then           !    block function
                result(:, ivar) = lower(:, ivar)
            else                                     !    linear interpolation
                result(:, ivar) = lower(:, ivar) * factor2 + higher(:, ivar) * factor1
            endif
        enddo

        if (timon) call timstop(ithndl)

    end subroutine interpolate_2d_array

    subroutine compute_matrix_size (noq1, noq2, noq3, noseg, ipoint, nomat)
        !! Compute size of the fast solver matrix
        !!
        !!    KHT      note that open boundaries (negative pointers) do not yield/n
        !!    KHT      off-diagonal elements. This is correct for the moment but needs/n
        !!    KHT      to be changed (in future) for (advanced) domain decomposition purposes/n

        use timers       !   performance timers

        integer(kind = int_wp), intent(in) :: noq1               !< nr of exchanges first direction
        integer(kind = int_wp), intent(in) :: noq2               !< nr of exchanges second direction
        integer(kind = int_wp), intent(in) :: noq3               !< nr of exchanges third direction
        integer(kind = int_wp), intent(in) :: noseg              !< nr of computational volumes
        integer(kind = int_wp), intent(in) :: ipoint(4, noq1 + noq2 + noq3)   !< exchange pointer
        integer(kind = int_wp), intent(out) :: nomat              !< size of the fast solver matrix

        integer(kind = int_wp) :: iq            ! loop counter exchanges
        integer(kind = int_wp) :: ifrom, ito    ! help variables exchanges
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("compute_matrix_size", ithndl)

        ! compute number of offdiagonals to be stored in the matrix
        nomat = 0
        do iq = 1, noq1 + noq2
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom > 0) nomat = nomat + 1
            if (ito   > 0) nomat = nomat + 1
        enddo

        ! see if there is a third direction
        if (noq3 /= 0) nomat = nomat + 2 * noseg

        if (timon) call timstop(ithndl)
        return
    end subroutine compute_matrix_size

end module m_matrix
