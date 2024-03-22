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

module pointr_mod
    use m_waq_precision
    use m_bound
    use m_array_manipulation, only : create_pointer_table
    use m_open_waq_files

contains
    subroutine pointr (lun, lchar, noseg, nmax, mmax, &
            kmax, noq, noq1, noq2, noq3, &
            noqt, nobnd, ipnt, intsrt, ipopt1, &
            jtrack, ioutpt, iwidth, GridPs, cellpnt, &
            flowpnt, status)

        !       Deltares Software Centre

        !>\file
        !>            Reads exchange pointers on regular grid
        !>
        !>            Routine
        !>            - reads and checks the dimensions of the regular matrix
        !>            - reads the regular matrix
        !>            - makes a backpointer from boundary entries to matrix locations
        !>            - calls create_pointer_table.f to make a 'from-to' pointer table
        !>            - calls bound.f to:
        !>              - compute number of open boundaries
        !>              - adds the bed pointers to the pointer set to make noqt
        !>              - compute number of codiagonals for direct implicit matrices
        !>            This leans on full matrices and does not support 'active only'
        !>            coupling.

        !     CREATED            : April 1989  by L. Postma

        !     Modified           : May   2011 by Leo Postma, Fortran90 look and feel

        !     SUBROUTINES CALLED : create_pointer_table
        !                          bound
        !                          open_waq_files

        !     LOGICAL UNITS      : lunut   = unit formatted output file
        !                          lun( 8) = unit intermediate file ('to-from')

        use dlwqgrid_mod        !   for the storage of contraction grids
        use rd_token     !   for the reading of tokens
        use timers       !   performance timers
        use m_error_status

        implicit none

        !     Parameters         :

        !     kind           function         name            Descriptipon

        integer(kind = int_wp), intent(inout) :: lun   (*)      !< array with unit numbers
        character(*), intent(inout) :: lchar (*)     !< array with file names of the files
        integer(kind = int_wp), intent(in) :: noseg          !< number of computational volumes
        integer(kind = int_wp), intent(in) :: nmax           !< dimension of first direction of grid
        integer(kind = int_wp), intent(in) :: mmax           !< dimension of second direction of grid
        integer(kind = int_wp), intent(in) :: kmax           !< dimension of third direction of grid
        integer(kind = int_wp), intent(out) :: noq            !< noq1 + noq2 + noq3
        integer(kind = int_wp), intent(out) :: noq1           !< number of exchanges 1st direction
        integer(kind = int_wp), intent(out) :: noq2           !< number of exchanges 2nd direction
        integer(kind = int_wp), intent(out) :: noq3           !< number of exchanges 3rd direction
        integer(kind = int_wp), intent(inout) :: noqt           !< total number of exchanges
        integer(kind = int_wp), intent(out) :: nobnd          !< number of open boundaries
        integer(kind = int_wp), pointer :: ipnt (:, :)     !< exchange pointer
        integer(kind = int_wp), intent(in) :: intsrt         !< integration number
        integer(kind = int_wp), intent(in) :: ipopt1         !< file option ( 0 = binary )
        integer(kind = int_wp), intent(out) :: jtrack         !< number of codiagonals of matrix
        integer(kind = int_wp), intent(in) :: ioutpt         !< flag for more or less output
        integer(kind = int_wp), intent(in) :: iwidth         !< width of the output file
        type(GridPointerColl)           GridPs        !< Collection of grid pointers
        integer(kind = int_wp), pointer :: cellpnt(:)     !< backpointer noseg to mnmaxk
        integer(kind = int_wp), pointer :: flowpnt(:)     !< backpointer noq to 3*mnmaxk-mnmax

        type(error_status) :: status !< error status

        !     local declarations

        integer(kind = int_wp), allocatable :: imat  (:)    ! regular grid matrix
        integer(kind = int_wp) :: ntot         ! nmax * mmax
        integer(kind = int_wp) :: ierr2        ! local error count
        integer(kind = int_wp) :: i1, i2, i3   ! loop counters
        integer(kind = int_wp) :: ist, k       ! help variable for loops
        integer(kind = int_wp) :: nobndl       ! number of boundaries per layer
        integer(kind = int_wp) :: nmax2        ! help variable to check nmax
        integer(kind = int_wp) :: mmax2        ! help variable to check mmax
        integer(kind = int_wp) :: nm           ! noseg from file
        integer(kind = int_wp) :: nlay         ! number of layers from file
        real(kind = real_wp) :: dummy        !
        character(256)          filename    ! to open more files
        real(kind = real_wp) :: x0, y0       ! zero point cco file
        real(kind = real_wp) :: alpha        ! help variables cco file
        integer(kind = int_wp) :: npart        ! help variables cco file
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("pointr", ithndl)

        !        Read and check first line of matrix

        if (ipopt1 == 0)  then         ! binary file
            call open_waq_files  (lun(8), lchar(8), 8, 2, ierr2)
            if (ierr2 /= 0) goto 100
            read  (lun(8)) nmax2, mmax2, nm, nlay, noq1, noq2, noq3
        else
            if (gettoken(nmax2, ierr2) > 0) goto 100
            if (gettoken(mmax2, ierr2) > 0) goto 100
            if (gettoken(nm, ierr2) > 0) goto 100
            if (gettoken(nlay, ierr2) > 0) goto 100
            noq1 = 0
            noq2 = 0
            noq3 = 0
            if (nmax > 1) noq1 = noseg
            if (mmax > 1) noq2 = noseg
            if (kmax > 1) noq3 = (noseg / kmax) * (kmax - 1)
        endif
        if (nmax2 /= nmax .or. mmax2 /= mmax .or. nlay  /= kmax) then
            write (lunut, 2010) nmax2, nmax, mmax2, mmax, nlay, kmax
            ierr2 = 1
            goto 100
        endif
        noq = noq1 + noq2 + noq3
        write (lunut, 2050) noq1, noq2, noq3, noqt, noq + noqt

        !        Allocate pointer space

        noqt = noq + noqt
        allocate (ipnt(4, noqt), cellpnt(noseg), flowpnt(noq), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2160) ierr2, 4 * noqt
            goto 100
        endif

        !        Allocate matrix space

        ierr2 = 0
        ntot = nmax * mmax
        allocate (imat(ntot), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2000) ierr2, nmax * mmax
            goto 100
        endif

        !        Read the pointer itself, write it to the intermediate file

        if (ipopt1 == 0)  then
            read  (lun(8)) imat
        else
            do i1 = 1, ntot
                if (gettoken(imat(i1), ierr2) > 0) goto 100
            enddo
            call open_waq_files  (lun(8), lchar(8), 8, 1, ierr2)
            if (ierr2 /= 0) goto 100
            write (lun(8)) nmax, mmax, noseg, kmax, noq1, noq2, noq3
            write (lun(8)) imat
        endif
        close (lun(8))

        !     Print the matrix

        do i2 = 1, nmax, iwidth * 2
            i3 = min(nmax, i2 + iwidth * 2 - 1)
            write (lunut, 2020) (k, k = i2, i3)
            do i1 = 1, mmax
                ist = (i1 - 1) * nmax
                write (lunut, 2030) i1, (imat(k), k = ist + i2, ist + i3)
            enddo
        enddo

        !     make the trivial IKBND array

        nobndl = -minval(imat(1:ntot))
        nobnd = kmax * nobndl

        !     make pointer table

        call create_pointer_table(nmax, mmax, kmax, noseg, nobnd, &
                noq, noq1, noq2, imat, ipnt, &
                cellpnt, flowpnt)

        !     calculate number of boundaries and bandwith of matrix

        call bound  (lun, noseg, noq, noqt, intsrt, &
                ioutpt, GridPs, nobnd, jtrack, ipnt, &
                status)

        !     open cco-file

        filename = lchar(8)(1:index(lchar(8), '.', .true.)) // 'cco'
        call open_waq_files (lun(8), filename, 8, 2, ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2060) filename
            goto 100
        endif
        read (lun(8))
        read (lun(8)) mmax2, nmax2, x0, y0, alpha, npart, nlay
        if (mmax2 /= mmax .or. nmax2 /= nmax .or. &
                nlay  /= kmax) then
            write (lunut, 2010) nmax2, nmax, mmax2, mmax, nlay, kmax
            ierr2 = 1
            goto 100
        endif

        deallocate (imat)
        100 if (ierr2 /= 0) call status%increase_error_count()
        close (lun(8))
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (/, ' ERROR. allocating memory for grid:', i4, i10)
        2010 format (/, ' ERROR: Matrix dimensions do not correspond:', &
                /2I10/2I10/2I10)
        2020 format (/, 10X, 20I6, /)
        2030 format (1X, I6, ' * ', 20I6)
        2050 format (//, ' Dimensions of the system :', &
                /, ' Number of exchanges 1st direction : ', I7, &
                /, ' Number of exchanges 2nd direction : ', I7, &
                /, ' Number of exchanges 3rd direction : ', I7, &
                /, ' Number of exchanges 4th direction : ', I7, &
                /, ' Total number of exchanges         : ', I7)
        2060 format (/, ' ERROR. opening cco file: ', A)
        2160 format (/, ' ERROR. allocating memory for pointers:', I4, i10)

    end subroutine
end module
