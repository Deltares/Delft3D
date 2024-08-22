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
module matrix_utils
    use m_waq_precision
    use timers, only : timstrt, timstop, timon

    implicit none

    private
    public :: dmatrix, compute_matrix_size, compute_matrix, print_matrix, scale_array, assign_matrix

contains

    subroutine scale_array(array_2d, factor)
        !! Scales an array by multiplying each row with a factor


        real(kind = real_wp), intent(inout) :: array_2d(:, :)        !< number of items
        real(kind = real_wp), intent(in) :: factor(size(array_2d, 1))              !< scale factors

        integer(kind = int_wp) :: row, col        ! loop counters
        real(kind = real_wp) :: fact          ! factor
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("scale_array", ithndl)

        do row = 1, size(array_2d, 1)
            fact = factor(row)
            do col = 1, size(array_2d, 2)
                array_2d (row, col) = array_2d (row, col) * fact
            enddo
        enddo

        if (timon) call timstop(ithndl)

    end subroutine scale_array

    subroutine assign_matrix(file_unit, int_array, noitm, itmnr, nodim, &
            idmnr, iorder, real_array, integration_id, rmat, &
            nocol, num_records, missing_value, iarp, rmatu)

        !! Assign matrix according to computational rules
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     int_array     INTEGER    *         INPUT   array with arithmetic rules
        !     NOITM   INTEGER    1         IN      number of items for computation
        !     ITMNR   INTEGER    1         IN      number of items for output
        !     NODIM   INTEGER    1         IN/OUT  number of conc. for comput.
        !     IDMNR   INTEGER    1         IN/OUT  number of conc. for output
        !     IORDER  INTEGER    1         INPUT   =1 items first: =2 concen first
        !     real_array     REAL       *         INPUT   real constants in formulae
        !     integration_id    LOGICAL    *         INPUT   3 & 4 is Fourier or harmonics
        !     RMAT    REAL       *         INPUT   real matrix of read values
        !     num_records   INTEGER    1         OUTPUT  number of records read
        !     missing_value   REAL       1         INPUT   this is a missing value
        !     IARP    INTEGER    *         INPUT   array with item pointers in RMAT
        !     RMATU   REAL       *         OUTPUT  real matrix of evaluated values

        LOGICAL       MINIEM, MAXIEM
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: ioff1, noitm, itmnr, idmnr, nodim, iorder, ioff0
        integer(kind = int_wp) :: locbas, iloc, itel, itels, ifrst, ibrk, ioff, integration_id
        integer(kind = int_wp) :: ip, ip2, file_unit, iloco, nocol, num_records, ioff2
        integer :: int_array(:), i, iarp(:)
        real :: accum, rmatu(:), amaxv, missing_value, aminv
        real :: real_array(:), rmat(:)

        if (timon) call timstrt("assign_matrix", ithndl)

        ! some initialisation
        miniem = .false.
        maxiem = .false.
        ! loop end-value in integer array to determine ip
        ioff1 = noitm + itmnr + idmnr + nodim
        if (iorder == 1) then
            ioff0 = noitm + itmnr + idmnr
            locbas = nodim
        endif
        if (iorder == 2) then
            ioff0 = nodim + idmnr + itmnr
            locbas = noitm
        endif
        iloc = 0

        ! counter in output matrix
        itel = 0
        accum = 0.0
        itels = 0

        ! implied loop counter for matrix, outer loop
        ifrst = 0
        if (iorder == 1 .and. noitm == 0) ifrst = -1
        if (iorder == 2 .and. nodim == 0) ifrst = -1
        ibrk = 1
        ioff = 0

        ! assignment loop
        ! if harmonics then deal with the phase
        10 if (iloc == 0 .and. (integration_id==3.or.integration_id==4) &
                .and.  ifrst == 0) then
            ioff = ioff + 1
            accum = rmat(ioff)
            itel = itel + 1
            rmatu(itel) = 0
        endif
        iloc = iloc + 1
        ip = int_array(iloc + ioff0)
        ip2 = int_array(iloc + ioff1)
        ! normal processing
        if (ip > -900000) then
            ! close pending arrithmatic in the previous itel
            if (itel /= 0) then
                if (rmatu(itel) /= missing_value .and. accum /= missing_value) then
                    rmatu(itel) = rmatu(itel) + accum
                else
                    rmatu(itel) = missing_value
                endif
                if (maxiem) then
                    if (rmatu(itel) > amaxv .and. &
                            rmatu(itel) /= missing_value) then
                        write (file_unit, 1000) ibrk, iloco, ifrst + 1
                        write (file_unit, 1010) rmatu(itel), amaxv
                        rmatu(itel) = amaxv
                    endif
                endif
                if (miniem) then
                    if (rmatu(itel) < aminv .and. &
                            rmatu(itel) /= missing_value) then
                        write (file_unit, 1000) ibrk, iloco, ifrst + 1
                        write (file_unit, 1020) rmatu(itel), aminv
                        rmatu(itel) = aminv
                    endif
                endif
                maxiem = .false.
                miniem = .false.
            endif
            if (ip > 0) then
                accum = rmat(ip2 + ioff)
            else
                accum = real_array(-ip)
            endif
            itel = itel + 1
            rmatu(itel) = 0
            iloco = ip
        endif
        ! ignore value
        if (ip <= -1300000000) then
            ip = 0
        endif
        ! a maximum value need to be applied
        if (ip <= -1190000000) then
            ip = ip + 1200000000
            if (rmatu(itel) /= missing_value .and. accum /= missing_value) then
                rmatu(itel) = rmatu(itel) + accum
            else
                rmatu(itel) = missing_value
            endif
            accum = 0.0
            maxiem = .true.
            if (ip == 0) then
                amaxv = rmat(ip2 + ioff)
            endif
            if (ip < 0) amaxv = real_array(-ip)
            if (ip > 0) amaxv = rmatu(itels + ip)
        endif
        ! a minimum value need to be applied
        if (ip <= -1090000000) then
            ip = ip + 1100000000
            if (rmatu(itel) /= missing_value .and. accum /= missing_value) then
                rmatu(itel) = rmatu(itel) + accum
            else
                rmatu(itel) = missing_value
            endif
            accum = 0.0
            miniem = .true.
            if (ip == 0) then
                aminv = rmat(ip2 + ioff)
            endif
            if (ip < 0) aminv = real_array(-ip)
            if (ip > 0) aminv = rmatu(itels + ip)
        endif

        ! a minus sign need to be applied
        if (ip <= -900000000) then
            ip = ip + 1000000000
            if (rmatu(itel) /= missing_value .and. accum /= missing_value) then
                rmatu(itel) = rmatu(itel) + accum
            else
                rmatu(itel) = missing_value
            endif
            if (ip == 0) then
                accum = -rmat(ip2 + ioff)
            endif
            if (ip < 0) accum = -real_array(-ip)
            if (ip > 0) accum = -rmatu(itels + ip)
            if (accum == -missing_value) accum = missing_value
        endif

        ! a plus sign need to be applied
        if (ip <= -90000000) then
            ip = ip + 100000000
            if (rmatu(itel) /= missing_value .and. accum /= missing_value) then
                rmatu(itel) = rmatu(itel) + accum
            else
                rmatu(itel) = missing_value
            endif
            if (ip == 0) then
                accum = rmat(ip2 + ioff)
            endif
            if (ip < 0) accum = real_array(-ip)
            if (ip > 0) accum = rmatu(itels + ip)
        endif

        ! a division need to be applied
        if (ip <= -9000000) then
            ip = ip + 10000000
            if (ip == 0) then
                if (rmat(ip2 + ioff) /= missing_value .and. accum /= missing_value) then
                    accum = accum / rmat(ip2 + ioff)
                else
                    accum = missing_value
                endif
            endif
            if (ip < 0) then
                if (real_array(-ip) /= missing_value .and. accum /= missing_value) then
                    accum = accum / real_array(-ip)
                else
                    accum = missing_value
                endif
            endif
            if (ip > 0) then
                if (rmat(itels + ip) /= missing_value .and. accum /= missing_value) then
                    accum = accum / rmatu(itels + ip)
                else
                    accum = missing_value
                endif
            endif
        endif

        ! a multiplication need to be applied
        if (ip <= -900000) then
            ip = ip + 1000000
            if (ip == 0) then
                if (rmat(ip2 + ioff) /= missing_value .and. accum /= missing_value) then
                    accum = accum * rmat(ip2 + ioff)
                else
                    accum = missing_value
                endif
            endif
            if (ip < 0) then
                if (real_array(-ip) /= missing_value .and. accum /= missing_value) then
                    accum = accum * real_array(-ip)
                else
                    accum = missing_value
                endif
            endif
            if (ip > 0) then
                if (rmat(itels + ip) /= missing_value .and. accum /= missing_value) then
                    accum = accum * rmatu(itels + ip)
                else
                    accum = missing_value
                endif
            endif
        endif
        if (iloc == locbas) then
            if (rmatu(itel) /= missing_value .and. accum /= missing_value) then
                rmatu(itel) = rmatu(itel) + accum
            else
                rmatu(itel) = missing_value
            endif
            if (maxiem) then
                if (rmatu(itel) > amaxv .and. &
                        rmatu(itel) /= missing_value) then
                    write (file_unit, 1000) ibrk, iloco, ifrst + 1
                    write (file_unit, 1010) rmatu(itel), amaxv
                    rmatu(itel) = amaxv
                endif
            endif
            if (miniem) then
                if (rmatu(itel) < aminv .and. &
                        rmatu(itel) /= missing_value) then
                    write (file_unit, 1000) ibrk, iloco, ifrst + 1
                    write (file_unit, 1020) rmatu(itel), aminv
                    rmatu(itel) = aminv
                endif
            endif
            maxiem = .false.
            miniem = .false.
            accum = 0.0
            ifrst = ifrst + 1
            itels = itel
            iloc = 0
            ioff = ioff + nocol
        endif

        ! are we to expect a new record ?
        if ((iorder == 1 .and. ifrst==noitm) .or. &
                (iorder == 2 .and. ifrst==nodim))then
            ifrst = 0
            if (iorder == 1 .and. noitm == 0) ifrst = -1
            if (iorder == 2 .and. nodim == 0) ifrst = -1
            ibrk = ibrk + 1
        endif
        if (ibrk <= num_records) goto 10

        ! compact the pointers
        ioff1 = ioff1 + locbas
        if (iorder == 1) then
            ioff0 = itmnr
            ioff2 = noitm + itmnr
            do i = 1, idmnr
                int_array(ioff0 + i) = int_array(ioff2 + i) ! it's here where the value -1300000000 gets in
            end do
            do i = 1, num_records
                int_array(ioff0 + idmnr + i) = int_array(ioff1 + i)
            end do
        endif
        if (iorder == 2) then ! concentration first
            ioff0 = idmnr
            ioff2 = nodim + idmnr
            do i = 1, itmnr
                int_array(ioff0 + i) = int_array(ioff2 + i) ! it's here where the value -1300000000 gets in
            end do
            do i = 1, num_records
                int_array(ioff0 + itmnr + i) = int_array(ioff1 + i)
            end do
        endif
        if (timon) call timstop(ithndl)
        return

        1000 FORMAT (' INFO: Processing breakpoint', I6, ' for substance', I3, &
                ' at station', I5)
        1010 FORMAT (' the value of ', E15.6, ' is overwritten by the maximum ', &
                ' of ', E15.6, ' !')
        1020 FORMAT (' the value of ', E15.6, ' is overwritten by the minimum ', &
                ' of ', E15.6, ' !')

    end subroutine assign_matrix

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
        factor1 = real(tset - tlow) / real(thigh - tlow)
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

    subroutine compute_matrix_size (num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_cells, ipoint, fast_solver_arr_size)
        !! Compute size of the fast solver matrix
        !!
        !!    KHT      note that open boundaries (negative pointers) do not yield/n
        !!    KHT      off-diagonal elements. This is correct for the moment but needs/n
        !!    KHT      to be changed (in future) for (advanced) domain decomposition purposes/n

        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir               !< nr of exchanges first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir               !< nr of exchanges second direction
        integer(kind = int_wp), intent(in) :: num_exchanges_z_dir               !< nr of exchanges third direction
        integer(kind = int_wp), intent(in) :: num_cells              !< nr of computational volumes
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir)   !< exchange pointer
        integer(kind = int_wp), intent(out) :: fast_solver_arr_size

        integer(kind = int_wp) :: iq            ! loop counter exchanges
        integer(kind = int_wp) :: ifrom, ito    ! help variables exchanges
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("compute_matrix_size", ithndl)

        ! compute number of offdiagonals to be stored in the matrix
        fast_solver_arr_size = 0
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom == 0 .or. ito == 0) cycle
            if (ifrom > 0) fast_solver_arr_size = fast_solver_arr_size + 1
            if (ito   > 0) fast_solver_arr_size = fast_solver_arr_size + 1
        enddo

        ! see if there is a third direction
        if (num_exchanges_z_dir /= 0) fast_solver_arr_size = fast_solver_arr_size + 2 * num_cells

        if (timon) call timstop(ithndl)
        return
    end subroutine compute_matrix_size

    subroutine compute_matrix(file_unit, data_param, data_loc, waq_param, waq_loc, &
            missing_value, fdata, wdata)
        !! assign matrix according to computational rules

        use m_waq_data_structure  ! for definition and storage of data

        integer(kind = int_wp), intent(in) :: file_unit         ! report file
        type(t_waq_item), intent(in) :: data_param   ! list of param items in the data
        type(t_waq_item), intent(in) :: data_loc     ! list of loc items in the data
        type(t_waq_item), intent(in) :: waq_param    ! list of waq param items to be set
        type(t_waq_item), intent(in) :: waq_loc      ! list of waq loc items to be set
        real(kind = real_wp), intent(in) :: missing_value         ! missing value
        type(t_data_block), intent(in) :: fdata        ! data block input
        type(t_data_block), intent(out) :: wdata        ! data block output

        integer(kind = int_wp) :: iorder        ! order of the parameters and locations in the data array
        integer(kind = int_wp) :: functype      ! function type
        integer(kind = int_wp) :: num_records         ! number of breakpoints
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
        functype = wdata%function_type
        miniem = .false.
        maxiem = .false.
        ndim1 = wdata%num_spatial_parameters
        ndim2 = wdata%num_locations
        num_records = fdata%num_breakpoints
        allocate(wdata%times(num_records), wdata%values(ndim1, ndim2, num_records))
        wdata%num_breakpoints = num_records
        wdata%times = fdata%times

        ! assignment loop
        breakpoint_loop : do ibrk = 1, num_records

            ! if harmonics then deal with the phase
            if (functype == FUNCTYPE_HARMONIC .or. &
                    functype == FUNCTYPE_FOURIER) then
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
                        if (wdata%values(ipar_out, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = missing_value
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
                        if (wdata%values(ipar_out, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = missing_value
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
                        if (wdata%values(ipar_out, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = missing_value
                        endif
                        if (ip == 0) then
                            accum = -fdata%values(ip2, iloc, ibrk)
                        endif
                        if (ip < 0) accum = -data_param%constant(ipar)
                        if (ip > 0) accum = -wdata%values(ip, iloc, ibrk)
                        if (accum == -missing_value) accum = missing_value
                    endif

                    ! a plus sign need to be applied
                    if (ip <= -90000000) then
                        ip = ip + 100000000
                        if (wdata%values(ipar_out, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = missing_value
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
                            if (fdata%values(ipar, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                                accum = accum / fdata%values(ip2, iloc, ibrk)
                            else
                                accum = missing_value
                            endif
                        endif
                        if (ip < 0) then
                            if (data_param%constant(ipar) /= missing_value .and. accum /= missing_value) then
                                accum = accum / data_param%constant(ipar)
                            else
                                accum = missing_value
                            endif
                        endif
                        if (ip > 0) then
                            if (wdata%values(ip, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                                accum = accum / wdata%values(ip, iloc, ibrk)
                            else
                                accum = missing_value
                            endif
                        endif
                    endif

                    ! a multiplication need to be applied
                    if (ip <= -900000) then
                        ip = ip + 1000000
                        if (ip == 0) then
                            if (fdata%values(ipar, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                                accum = accum * fdata%values(ip2, iloc, ibrk)
                            else
                                accum = missing_value
                            endif
                        endif
                        if (ip < 0) then
                            if (data_param%constant(ipar) /= missing_value .and. accum /= missing_value) then
                                accum = accum * data_param%constant(ipar)
                            else
                                accum = missing_value
                            endif
                        endif
                        if (ip > 0) then
                            if (wdata%values(ip, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                                accum = accum * wdata%values(ip, iloc, ibrk)
                            else
                                accum = missing_value
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
                        if (wdata%values(ipar_out, iloc, ibrk) /= missing_value .and. accum /= missing_value) then
                            wdata%values(ipar_out, iloc, ibrk) = wdata%values(ipar_out, iloc, ibrk) + accum
                        else
                            wdata%values(ipar_out, iloc, ibrk) = missing_value
                        endif
                        if (maxiem) then
                            if (wdata%values(ipar_out, iloc, ibrk) > amaxv .and. &
                                    wdata%values(ipar_out, iloc, ibrk) /= missing_value) then
                                write (file_unit, 1000) ibrk, iparo, iloc
                                write (file_unit, 1010) wdata%values(ipar_out, iloc, ibrk), amaxv
                                wdata%values(ipar_out, iloc, ibrk) = amaxv
                            endif
                        endif
                        if (miniem) then
                            if (wdata%values(ipar_out, iloc, ibrk) < aminv .and. &
                                    wdata%values(ipar_out, iloc, ibrk) /= missing_value) then
                                write (file_unit, 1000) ibrk, iparo, iloc
                                write (file_unit, 1020) wdata%values(ipar_out, iloc, ibrk), aminv
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

        1000 format (' warning: processing breakpoint', i6, ' for substance', i3, &
                ' at station', i5)
        1010 format (' the value of ', e15.6, ' is overwritten by the maximum ', &
                ' of ', e15.6, ' !')
        1020 format (' the value of ', e15.6, ' is overwritten by the minimum ', &
                ' of ', e15.6, ' !')
    end subroutine compute_matrix

    subroutine print_matrix(file_unit, iwidth, dlwqdata, strng1, strng2, &
            strng3, output_verbose_level)

        !! prints blocks of data, also scale and convert
        use m_waq_data_structure ! for definition and storage of data

        integer(kind = int_wp), intent(in) :: file_unit         ! report file
        integer(kind = int_wp), intent(in) :: iwidth        ! width of output
        type(t_data_block), intent(inout) :: dlwqdata     ! data block to be filled
        character(len = *), intent(in) :: strng1       ! write string 1 (items)
        character(len = *), intent(in) :: strng2       ! write string 2 (values/concs)
        character(len = *), intent(in) :: strng3       ! write string 3 (brkp/harm)
        integer(kind = int_wp), intent(in) :: output_verbose_level        ! output file option

        logical :: deflts       ! defaults for the parameters
        integer(kind = int_wp) :: nopar         ! dlwqdata%num_spatial_parameters
        integer(kind = int_wp) :: num_local_vars         ! dlwqdata%num_locations
        integer(kind = int_wp) :: num_records         ! dlwqdata%num_breakpoints
        integer(kind = int_wp) :: ftype         ! dlwqdata%function_type
        integer(kind = int_wp) :: iorder        ! dlwqdata%iorder
        integer(kind = int_wp) :: ipar          ! loop counter
        integer(kind = int_wp) :: iloc          ! loop counter
        integer(kind = int_wp) :: ibrk          ! loop counter
        integer(kind = int_wp) :: k, ie         ! loop counter
        integer(kind = int_wp) :: iploc         ! index number
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("print_matrix", ithndl)

        ! just print a message if data comes from an external source

        if (dlwqdata%is_external) then
            if (dlwqdata%filetype == FILE_BINARY) then
                write (file_unit, 1130) trim(dlwqdata%filename)
            else
                write (file_unit, 1135) trim(dlwqdata%filename)
            endif
            goto 70
        endif

        ! initialisation
        nopar = dlwqdata%num_spatial_parameters
        num_local_vars = dlwqdata%num_locations
        num_records = dlwqdata%num_breakpoints
        ftype = dlwqdata%function_type
        iorder = dlwqdata%iorder
        deflts = dlwqdata%are_locations_default

        ! scale factors
        if (dlwqdata%need_parameters_scaling) then

            ! print scale factors
            if (output_verbose_level >= 4) then
                write (file_unit, 1010)
                do ipar = 1, nopar, iwidth
                    ie = min(ipar + iwidth - 1, nopar)
                    if (dlwqdata%is_parameter_pointered) then
                        write (file_unit, 1020) (dlwqdata%param_pointers(k), k = ipar, ie)
                    else
                        write (file_unit, 1020) (k, k = ipar, ie)
                    endif
                    if (dlwqdata%is_parameter_named) then
                        write (file_unit, 1025) (dlwqdata%param_name(k), k = ipar, ie)
                    else
                        if (dlwqdata%is_parameter_pointered) then
                            write (file_unit, 1025) (car_used(dlwqdata%param_pointers(k)), k = ipar, ie)
                        else
                            write (file_unit, 1025) (car_used(k), k = ipar, ie)
                        endif
                    endif
                    write (file_unit, 1030) (dlwqdata%parameter_scale_factor(k), k = ipar, ie)
                enddo
            endif

            ! perform the actual scaling
            do ibrk = 1, num_records
                do iloc = 1, num_local_vars
                    do ipar = 1, nopar
                        if (iorder == ORDER_PARAM_LOC) then
                            dlwqdata%values(ipar, iloc, ibrk) = dlwqdata%values(ipar, iloc, ibrk) * dlwqdata%parameter_scale_factor(ipar)
                        else
                            dlwqdata%values(iloc, ipar, ibrk) = dlwqdata%values(iloc, ipar, ibrk) * dlwqdata%parameter_scale_factor(ipar)
                        endif
                    enddo
                enddo
            enddo

            dlwqdata%need_parameters_scaling = .false.
            deallocate(dlwqdata%parameter_scale_factor)

        endif

        ! convert breakpoints, no more, already been done directly after the read
        if (num_records > 1) then
            if (output_verbose_level >= 4) write (file_unit, 1040) strng3, num_records
            if (deflts .and. output_verbose_level >= 4) write (file_unit, 1050)
        else
            if (deflts) then
                if (output_verbose_level >= 4) write (file_unit, 1050)
            else
                if (output_verbose_level >= 4) write (file_unit, 1060)
            endif
        endif

        ! write formatted output
        if (output_verbose_level >= 4) then
            do ibrk = 1, num_records
                if (num_records > 1) then
                    if (ftype == 1) write (file_unit, 1070) strng3, ibrk, dlwqdata%times(ibrk)
                    if (ftype == 2) write (file_unit, 1070) strng3, ibrk, dlwqdata%times(ibrk)
                    if (ftype == 3) write (file_unit, 1080) ibrk, dlwqdata%times(ibrk), dlwqdata%phase(ibrk)
                    if (ftype == 4) write (file_unit, 1090) ibrk, dlwqdata%times(ibrk), dlwqdata%phase(ibrk)
                endif

                do ipar = 1, nopar, iwidth
                    ie = min(ipar + iwidth - 1, nopar)
                    if (dlwqdata%is_parameter_pointered) then
                        write (file_unit, 1100) strng2, (dlwqdata%param_pointers(k), k = ipar, ie)
                    else
                        write (file_unit, 1100) strng2, (k, k = ipar, ie)
                    endif
                    if (dlwqdata%is_parameter_named) then
                        write (file_unit, 1150) strng1, (dlwqdata%param_name(k), k = ipar, ie)
                    else
                        if (dlwqdata%is_parameter_pointered) then
                            write (file_unit, 1150) strng1, (car_used(dlwqdata%param_pointers(k)), k = ipar, ie)
                        else
                            write (file_unit, 1150) strng1, (car_used(k), k = ipar, ie)
                        endif
                    endif
                    do iloc = 1, num_local_vars
                        if (dlwqdata%are_locations_pointered) then
                            iploc = abs(dlwqdata%location_pointers(iloc))
                        else
                            iploc = iloc
                        endif
                        if (iorder == ORDER_PARAM_LOC) then
                            write (file_unit, 1120) iploc, (dlwqdata%values(k, iloc, ibrk), k = ipar, ie)
                        else
                            write (file_unit, 1120) iploc, (dlwqdata%values(iloc, k, ibrk), k = ipar, ie)
                        endif
                    enddo
                enddo
            enddo
        endif

        70 continue

        if (timon) call timstop(ithndl)
        return

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

    end subroutine print_matrix

    character(len=20) function car_used(i)
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

end module matrix_utils
