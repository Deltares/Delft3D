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

module exchange_pointers
    use m_waq_precision
    use m_array_manipulation, only : create_pointer_table
    use m_open_waq_files
    use m_error_status

    private
    public :: read_exchange_pointers_regular_grid, read_exchange_pointers_irregular_grid, generate_bed_layer_pointers, &
            create_boundary_pointers


contains

    subroutine read_exchange_pointers_regular_grid(file_unit_list, file_name_list, noseg, nmax, mmax, &
            kmax, noq, noq1, noq2, noq3, &
            noqt, nobnd, ipnt, intsrt, ipopt1, &
            jtrack, output_verbose_level, iwidth, GridPs, cellpnt, &
            flowpnt, status)

        !! Reads exchange pointers on regular grid
        !!      - reads and checks the dimensions of the regular matrix
        !!      - reads the regular matrix
        !!      - makes a backpointer from boundary entries to matrix locations
        !!      - calls create_pointer_table.f to make a 'from-to' pointer table
        !!      - calls bound.f to:
        !!          - compute number of open boundaries
        !!          - adds the bed pointers to the pointer set to make noqt
        !!          - compute number of codiagonals for direct implicit matrices
        !!          This leans on full matrices and does not support 'active only' coupling.
        !! SUBROUTINES CALLED : create_pointer_table, bound, open_waq_files
        !! LOGICAL UNITS:
        !!          lunut   = unit formatted output file
        !!           file_unit_list( 8) = unit intermediate file ('to-from')

        use m_grid_utils_external        !   for the storage of contraction grids
        use rd_token     !   for the reading of tokens
        use timers       !   performance timers

        integer(kind = int_wp), intent(inout) :: file_unit_list   (*)      !< array with unit numbers
        character(*), intent(inout) :: file_name_list (*)     !< array with file names of the files
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
        integer(kind = int_wp), intent(in) :: output_verbose_level         !< flag for more or less output
        integer(kind = int_wp), intent(in) :: iwidth         !< width of the output file
        type(GridPointerColl)           GridPs        !< Collection of grid pointers
        integer(kind = int_wp), pointer :: cellpnt(:)     !< backpointer noseg to mnmaxk
        integer(kind = int_wp), pointer :: flowpnt(:)     !< backpointer noq to 3*mnmaxk-mnmax

        type(error_status) :: status !< error status

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
        if (timon) call timstrt("read_exchange_pointers_regular_grid", ithndl)

        ! Read and check first line of matrix
        if (ipopt1 == 0)  then         ! binary file
            call open_waq_files  (file_unit_list(8), file_name_list(8), 8, 2, ierr2)
            if (ierr2 /= 0) goto 100
            read  (file_unit_list(8)) nmax2, mmax2, nm, nlay, noq1, noq2, noq3
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

        ! Allocate pointer space
        noqt = noq + noqt
        allocate (ipnt(4, noqt), cellpnt(noseg), flowpnt(noq), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2160) ierr2, 4 * noqt
            goto 100
        endif

        ! Allocate matrix space
        ierr2 = 0
        ntot = nmax * mmax
        allocate (imat(ntot), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2000) ierr2, nmax * mmax
            goto 100
        endif

        ! Read the pointer itself, write it to the intermediate file
        if (ipopt1 == 0)  then
            read  (file_unit_list(8)) imat
        else
            do i1 = 1, ntot
                if (gettoken(imat(i1), ierr2) > 0) goto 100
            enddo
            call open_waq_files  (file_unit_list(8), file_name_list(8), 8, 1, ierr2)
            if (ierr2 /= 0) goto 100
            write (file_unit_list(8)) nmax, mmax, noseg, kmax, noq1, noq2, noq3
            write (file_unit_list(8)) imat
        endif
        close (file_unit_list(8))

        ! Print the matrix
        do i2 = 1, nmax, iwidth * 2
            i3 = min(nmax, i2 + iwidth * 2 - 1)
            write (lunut, 2020) (k, k = i2, i3)
            do i1 = 1, mmax
                ist = (i1 - 1) * nmax
                write (lunut, 2030) i1, (imat(k), k = ist + i2, ist + i3)
            enddo
        enddo

        ! make the trivial IKBND array
        nobndl = -minval(imat(1:ntot))
        nobnd = kmax * nobndl

        ! make pointer table
        call create_pointer_table(nmax, mmax, kmax, noseg, nobnd, &
                noq, noq1, noq2, imat, ipnt, &
                cellpnt, flowpnt)

        ! calculate number of boundaries and bandwith of matrix
        call create_boundary_pointers  (file_unit_list, noseg, noq, noqt, intsrt, &
                output_verbose_level, GridPs, nobnd, jtrack, ipnt, &
                status)

        ! open cco-file
        filename = file_name_list(8)(1:index(file_name_list(8), '.', .true.)) // 'cco'
        call open_waq_files (file_unit_list(8), filename, 8, 2, ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2060) filename
            goto 100
        endif
        read (file_unit_list(8))
        read (file_unit_list(8)) mmax2, nmax2, x0, y0, alpha, npart, nlay
        if (mmax2 /= mmax .or. nmax2 /= nmax .or. &
                nlay  /= kmax) then
            write (lunut, 2010) nmax2, nmax, mmax2, mmax, nlay, kmax
            ierr2 = 1
            goto 100
        endif

        deallocate (imat)
        100 if (ierr2 /= 0) call status%increase_error_count()
        close (file_unit_list(8))
        if (timon) call timstop(ithndl)
        return

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

    end subroutine read_exchange_pointers_regular_grid

    subroutine read_exchange_pointers_irregular_grid(file_unit_list, file_name_list, noseg, noq, noq1, &
            noq2, noq3, noqt, nobnd, ipnt, &
            intsrt, ipopt1, jtrack, ftype, output_verbose_level, &
            GridPs, status)

        !!  Reads exchange pointers on irregular grid
        !!      - reads the exchange pointers on irregular grid in the waterphase
        !!      - calls bound.f to:
        !!      - compute number of open boundaries
        !!      - adds the bed pointers to the pointer set to make noqt
        !!      - compute number of codiagonals for direct implicit matrices
        !! Logical units:
        !!          lunut   = unit formatted output file
        !           file_unit_list( 8) = unit intermediate file ('to-from')

        use m_open_waq_files
        use m_grid_utils_external          ! for the storage of contraction grids
        use rd_token       ! for the reading of tokens
        use timers       !   performance timers

        integer(kind = int_wp), intent(inout) :: file_unit_list   (*)      !< array with unit numbers
        character(*), intent(inout) :: file_name_list (*)     !< array with file names of the files
        integer(kind = int_wp), intent(in) :: noseg          !< number of computational volumes
        integer(kind = int_wp), intent(in) :: noq            !< noq1 + noq2 + noq3
        integer(kind = int_wp), intent(in) :: noq1           !< number of exchanges 1st direction
        integer(kind = int_wp), intent(in) :: noq2           !< number of exchanges 2nd direction
        integer(kind = int_wp), intent(in) :: noq3           !< number of exchanges 3rd direction
        integer(kind = int_wp), intent(in) :: noqt           !< total number of exchanges
        integer(kind = int_wp), intent(out) :: nobnd          !< number of open boundaries
        integer(kind = int_wp), intent(out) :: ipnt (4, noqt)  !< exchange pointer
        integer(kind = int_wp), intent(in) :: intsrt         !< integration number
        integer(kind = int_wp), intent(in) :: ipopt1         !< file option ( 0 = binary )
        integer(kind = int_wp), intent(out) :: jtrack         !< number of codiagonals of matrix
        integer(kind = int_wp), intent(in) :: ftype          !< type of the pointer file
        integer(kind = int_wp), intent(in) :: output_verbose_level         !< flag for more or less output
        type(GridPointerColl)           GridPs        !< Collection of grid pointers

        type(error_status) :: status !< current error status

        integer(kind = int_wp) :: noq12        ! noq1 + noq2 (horizontal exchanges
        integer(kind = int_wp) :: iq           ! loop counter exchanges
        integer(kind = int_wp) :: ip           ! loop counter pointers
        integer(kind = int_wp) :: ierr1        ! local I/O error
        integer(kind = int_wp) :: ierr2        ! local error count
        integer(kind = int_wp) :: idummy
        character(len = 1) :: cdummy
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_exchange_pointers_irregular_grid", ithndl)

        ierr2 = 0

        ! Read exchange pointers
        noq12 = noq1 + noq2
        if (ipopt1 == 0)  then
            call open_waq_files(file_unit_list(44), file_name_list(44), 44, 2 + ftype, ierr2)
            if (ierr2 /= 0) goto 100
            do iq = 1, noq
                read (file_unit_list(44), iostat = ierr1) ipnt(:, iq)
                if (ierr1 /= 0) then
                    write(lunut, 2100) iq - 1
                    close (file_unit_list(44))
                    ierr2 = 1
                    goto 100
                endif
            enddo

            ! Check that there are no more data in the file
            ! For DELWAQ-G applications, there may already be more data
            ! than the raw 4*noq numbers ...

            if (noqt > noq) then
                ! Any extra exchange pointers already present?
                read (file_unit_list(44), iostat = ierr1) idummy
                if (ierr1 == 0) then
                    ! Skip all extra exchange pointers that are expected
                    read (file_unit_list(44), iostat = ierr1) (idummy, iq = 2, 4 * (noqt - noq))
                    if (ierr1 /= 0) then
                        write(lunut, 2111)
                        close (file_unit_list(44))
                        ierr2 = 1
                        goto 100
                    endif
                endif
            endif

            ! Any data after the expected exchange pointers indicate a problem
            read (file_unit_list(44), iostat = ierr1) cdummy
            if (ierr1 == 0) then
                write(lunut, 2110)
                close (file_unit_list(44))
                ierr2 = 1
                goto 100
            endif

            ! No problems found, so continue
            close (file_unit_list(44))
            call open_waq_files  (file_unit_list(8), file_name_list(8), 8, 1, ierr2)
            if (ierr2 /= 0) goto 100
            if (noq1 > 0) write(file_unit_list(8))(ipnt(:, iq), iq = 1, noq1)
            if (noq2 > 0) write(file_unit_list(8))(ipnt(:, iq), iq = noq1 + 1, noq12)
            if (noq3 > 0) write(file_unit_list(8))(ipnt(:, iq), iq = noq12 + 1, noq)
        else
            do iq = 1, noq
                do ip = 1, 4
                    if (gettoken(ipnt(ip, iq), ierr2) > 0) goto 100
                enddo
            enddo
            call open_waq_files  (file_unit_list(8), file_name_list(8), 8, 1, ierr2)
            if (ierr2 /= 0) goto 100
            if (noq1 > 0) write(file_unit_list(8))(ipnt(:, iq), iq = 1, noq1)
            if (noq2 > 0) write(file_unit_list(8))(ipnt(:, iq), iq = noq1 + 1, noq12)
            if (noq3 > 0) write(file_unit_list(8))(ipnt(:, iq), iq = noq12 + 1, noq)

            if (output_verbose_level < 4) then
                write (lunut, 2000)
            else
                if (noq1 > 0) then
                    write (lunut, 2010)
                    write (lunut, 2020)
                    write (lunut, 2030) (iq, ipnt(:, iq), iq = 1, noq1)
                endif

                if (noq2 > 0) then
                    write (lunut, 2040)
                    write (lunut, 2020)
                    write (lunut, 2030) (iq, ipnt(:, iq), iq = noq1 + 1, noq12)
                endif

                if (noq3>0) then
                    write (lunut, 2050)
                    write (lunut, 2020)
                    write (lunut, 2030) (iq, ipnt(:, iq), iq = noq12 + 1, noq)
                endif
            endif
        endif

        ! calculate number of boundaries and bandwith of matrix

        call create_boundary_pointers  (file_unit_list, noseg, noq, noqt, intsrt, &
                output_verbose_level, GridPs, nobnd, jtrack, ipnt, &
                status)

        close (file_unit_list(8))
        100 if (ierr2 > 0) call status%increase_error_count()
        if (timon) call timstop(ithndl)
        return

        2000 format (/ ' Exchange pointers are printed for output option 4 and higher !')
        2010 format (/, '           First direction :')
        2020 format ('   Item nr.  From      To  From-1    To+1')
        2030 format (5I8)
        2040 format (/, '           Second direction :')
        2050 format (/, '           Third direction :')
        2100 format (/, ' ERROR: premature end of the file with the exchange pointers', &
                /, '        number of exchanged read: ', i0)
        2110 format (/, ' ERROR: more exchanges present in the exchanges file than expected')
        2111 format (/, ' ERROR: too few extra exchanges (DELWAQG) present in the exchanges file')

    end subroutine read_exchange_pointers_irregular_grid

    subroutine generate_bed_layer_pointers(file_unit_list, output_verbose_level, gridps, ibnd, ipoint, &
            noqt, status)

        !! Makes and write additional pointer for the water bed
        !!
        !! This routine makes pointers as follows:
        !!      - for the first bed layer the pointers with all water cells are made
        !!          per bed cell, using the backpointer from bed grid to base grid
        !!      - per bed layer the pointers within the bed are made
        !!      - at the end of the (may be varying) bed column length, an open boundary is created
        !!      - each column is doubled right afterwards ( for advection and diffusion )
        !!         The amount thus created exchanges is checked to the given amount noqt./n
        !!         The result is written to the system file and arrays are deallocated
        !! Logical units:
        !!          file_unit_list(29) = unit formatted output file
        !!          file_unit_list( 2) = unit intermediate file (system)

        use timers       !   performance timers
        use m_grid_utils_external ! for the storage of contraction grids
        use m_sysn          ! System characteristics

        integer(kind = int_wp), intent(in) :: file_unit_list   (*)         !< array with unit numbers
        integer(kind = int_wp), intent(in) :: output_verbose_level              !< how extensive is output ?
        type(GridPointerColl)           GridPs             !< Collection of grid pointers
        integer(kind = int_wp), intent(in) :: ibnd  (nobnd, 2)  !< normal boundary pointers
        integer(kind = int_wp), intent(in) :: noqt                !< total number of exchanges
        integer(kind = int_wp), intent(inout) :: ipoint(4, noqt)  !< exchange pointers

        type(error_status), intent(inout) :: status !< current error status
        !
        !     COMMON BLOCK  / SYSN / :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     NOSEG   INTEGER  1           INPUT   number of segments
        !     NSEG2   INTEGER  1           INPUT   number of bottom segments
        !     NOSYS   INTEGER  1           INPUT   number of active substances
        !     NODISP  INTEGER  1           OUTPUT  number of dispersion arrays
        !     NOVELO  INTEGER  1           OUTPUT  number of velocity arrays
        !     NOQ1    INTEGER  1           OUTPUT  number of exch. 1st direction
        !     NOQ2    INTEGER  1           OUTPUT  number of exch. 2nd direction
        !     NOQ3    INTEGER  1           OUTPUT  number of exch. 3rd direction
        !     NOQ4    INTEGER  1           OUTPUT  number of exch. bottom direction
        !     NOQ     INTEGER  1           OUTPUT  number of exchanges
        !     NOBND   INTEGER  1           OUTPUT  number of boundaries
        !     JTRACK  INTEGER  1           OUTPUT  number of codiagonals
        !     NDMPAR  INTEGER  1           INPUT   number of dump areas
        !     NDMPQ   INTEGER  1           OUTPUT  number exchanges dumped
        !     NDMPS   INTEGER  1           OUTPUT  number segments dumped
        !     NTDMPQ  INTEGER  1           OUTPUT  total number exchanges in dump area
        !     NTDMPS  INTEGER  1           INPUT   total number segments in dump area
        !     NORAAI  INTEGER  1           INPUT   number of raaien
        !     NTRAAQ  INTEGER  1           INPUT   total number of exch. in raaien
        !     NOMAT   INTEGER  1           OUTPUT  size of the fastsolvers matrix

        integer(kind = int_wp) :: lunut            ! output unit number (file_unit_list(29))
        integer(kind = int_wp), allocatable :: IAbnd(:, :)       ! array with boundary information in the bed
        integer(kind = int_wp) :: ilay             ! index layer number
        integer(kind = int_wp) :: isegb            ! counter for bed volumes
        integer(kind = int_wp) :: iq               ! loop counter for exchanges in one bed column
        integer(kind = int_wp) :: ib               ! loop counter for bed volumes
        integer(kind = int_wp) :: iqt              ! counter for exchanges in the bed
        integer(kind = int_wp) :: jbott            ! grid number of the bottom_grid
        integer(kind = int_wp) :: jbase            ! grid number of the base_grid
        integer(kind = int_wp) :: nsegl            ! nr of volumes per water layer
        integer(kind = int_wp) :: nlay             ! nr of layers in the water
        integer(kind = int_wp) :: nsegb            ! nr of volumes per bed layer
        integer(kind = int_wp) :: nlayb            ! nr of layers in the bed
        integer(kind = int_wp), allocatable :: botmatrix(:, :)   ! matrix with bottom segment number in case of space varying number of layers
        logical :: space_var_nolay ! space varying number of layers in the bed ?
        integer(kind = int_wp) :: nolaymax         ! maximum number of bed layers in a bed column
        integer(kind = int_wp) :: ioff1            ! offset volume nr's last water layer
        integer(kind = int_wp) :: ioff2            ! offset volume nr's one but last water layer
        integer(kind = int_wp) :: inaarplus        ! the 'to+1' exchange pointer
        integer(kind = int_wp) :: i, k             ! loop counters
        logical :: odd             ! mention only the first boundary
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("generate_bed_layer_pointers", ithndl)

        lunut = file_unit_list(29)

        ! is there a bottom direction ?
        if (noq4 == 0) then
            if (nobnd > 0) then
                write (file_unit_list(2)) (ibnd (k, 1), k = 1, nobnd)
                write (file_unit_list(2)) (ibnd (k, 2), k = 1, nobnd)
            endif
            goto 9999
        endif

        ! is there a bottom grid ?
        JBott = GridPs%bottom_grid
        if (JBott == 0) then
            write (lunut, 1050)
            call status%increase_error_count()
            goto 9999
        endif

        ! allocate memory
        JBase = GridPs%base_grid
        nsegl = GridPs%Pointers(JBase)%noseg_lay ! nr of segments per layer
        nlay = GridPs%Pointers(JBase)%nolay     !             in the water
        nlayb = GridPs%Pointers(JBott)%nolay     !
        nsegb = GridPs%Pointers(JBott)%noseg_lay ! and in the bottom
        allocate(IAbnd(nsegb, 2))

        space_var_nolay = GridPs%Pointers(JBott)%space_var_nolay
        if (space_var_nolay) then
            !        make complete bottom matrix with segment numbers in bottom
            !              (one could also construct the matrix for fixed layers in order to simplify the algorithm)
            nolaymax = maxval(GridPs%Pointers(JBott)%nolay_var)
        else
            nolaymax = nlayb
        endif
        allocate(botmatrix(nsegb, nolaymax))
        botmatrix = 0
        isegb = 0
        do ilay = 1, nolaymax
            do ib = 1, nsegb
                if (space_var_nolay) then
                    if (ilay <= GridPs%Pointers(JBott)%nolay_var(ib)) then
                        isegb = isegb + 1
                        botmatrix(ib, ilay) = isegb
                    endif
                else
                    isegb = isegb + 1
                    botmatrix(ib, ilay) = isegb
                endif
            enddo
        enddo

        ! sorted after bottom segment number !!

        if (output_verbose_level < 4) write (lunut, 1000)
        ioff1 = (nlay - 1) * nsegl
        ioff2 = max((nlay - 2) * nsegl, 0)
        iqt = noq
        write (lunut, *) ' nsegb: ', nsegb
        do isegb = 1, nsegb

            if (space_var_nolay) nlayb = GridPs%Pointers(JBott)%nolay_var(isegb)
            ib = botmatrix(isegb, 1)

            ! header for water-bottom
            if (output_verbose_level >= 4) then
                write (lunut, 1010) ib, noseg + ib
                write (lunut, 1030)
            endif

            if (nlayb > 1) then
                inaarplus = botmatrix(isegb, 2) + noseg
            else
                inaarplus = -nobnd - ib
            endif

            ! get every pointer for this bottom cell
            iq = 0
            do i = 1, nsegl          ! from water towards the bottom
                if (GridPs%Pointers(JBott)%iarray(i) == ib) then
                    iq = iq + 1
                    ipoint(1, iq + iqt) = ioff1 + i
                    ipoint(2, iq + iqt) = ib + noseg
                    ipoint(3, iq + iqt) = ioff2 + i
                    ipoint(4, iq + iqt) = inaarplus
                    if (output_verbose_level >= 4) write(lunut, 1040)iq + iqt, (ipoint(k, iq + iqt), k = 1, 4)
                endif
            end do
            ! header within the bottom
            if (output_verbose_level >= 4) then
                write (lunut, 1020)
                write (lunut, 1030)
            endif

            do ilay = 1, nlayb     ! from bottom to next bottom layer
                iq = iq + 1            ! the number of the pointer

                ! from pointer
                ipoint(1, iq + iqt) = botmatrix(isegb, ilay) + noseg

                ! to pointer
                if (ilay  < nlayb) then   ! 'to'  can be boundary
                    ipoint(2, iq + iqt) = botmatrix(isegb, ilay + 1) + noseg
                else
                    ipoint(2, iq + iqt) = -ib - nobnd
                    IAbnd(ib, 1) = iq + iqt
                    IAbnd(ib, 2) = ipoint(1, iq + iqt)
                endif

                ! from-1
                if (ilay == 1) then
                    ipoint(3, iq + iqt) = ipoint(1, iq + iqt)
                else
                    ipoint(3, iq + iqt) = botmatrix(isegb, ilay - 1) + noseg
                endif

                !to+1
                if (ilay < nlayb - 1) then ! 'to+1'  can be boundary
                    ipoint(4, iq + iqt) = botmatrix(isegb, ilay + 2) + noseg
                else
                    ipoint(4, iq + iqt) = -ib - nobnd
                endif
                if (output_verbose_level >= 4) write(lunut, 1040)iq + iqt, (ipoint(k, iq + iqt), k = 1, 4)

            end do
            ! copy the column
            do i = 1, iq
                ipoint(1, iq + iqt + i) = ipoint(1, iqt + i)
                ipoint(2, iq + iqt + i) = ipoint(2, iqt + i)
                ipoint(3, iq + iqt + i) = ipoint(3, iqt + i)
                ipoint(4, iq + iqt + i) = ipoint(4, iqt + i)
            end do
            iqt = iqt + 2 * iq
        end do
        if (noqt /= iqt) then
            write (lunut, 1110) noq4, iqt - noq
            call status%increase_error_count()
            goto 9999
        endif
        write (lunut, 1060) nsegb
        odd = .true.
        if (output_verbose_level >= 3) then
            write (lunut, 1070)
            do iq = noq + 1, noq + noq4
                if (ipoint(1, iq) < 0 .or. &
                        ipoint(2, iq) < 0) then
                    ib = min (ipoint(1, iq), ipoint(2, iq))
                    if (odd) then
                        write (lunut, 1080) ib, iq, (ipoint(k, iq), k = 1, 2)
                        odd = .false.
                    else
                        odd = .true.
                    endif
                endif
            enddo
        else
            write (lunut, 1090)
        endif
        write (file_unit_list(8)) ((ipoint(i, iq), i = 1, 4), iq = noq + 1, iqt)
        write (lunut, 1100)

        ! Write boundary pointers to work file
        if (nobnd > 0 .or. nsegb > 0) then
            write (file_unit_list(2)) (ibnd (k, 1), k = 1, nobnd), (iabnd(k, 1), k = 1, nsegb)
            write (file_unit_list(2)) (ibnd (k, 2), k = 1, nobnd), (iabnd(k, 2), k = 1, nsegb)
        endif
        deallocate (iabnd)
        nobnd = nobnd + nsegb

        9999 if (timon) call timstop(ithndl)
        return

        1000 FORMAT (/ ' Exchange pointers are printed for output option 4 and higher !')
        1010 FORMAT (/, '     Additional exchanges between water and ' &
                'bottom at bottom segment:', I10, ' WAQ:', I10)
        1020 FORMAT (/, '     Additional exchanges within the bottom:')
        1030 FORMAT ('   Item nr.  From      To  From-1    To+1')
        1040 FORMAT (5I8)
        1050 FORMAT (/, ' ERROR. No bottom grid information found')
        1060 FORMAT (/, ' Number of additional bottom boundaries  :', I4, ' times 2 !')
        1070 FORMAT (' boundary  exchange    from        to'/ &
                '  number    number    segment    segment')
        1080 FORMAT (I7, 3I10)
        1090 FORMAT (' exchanges with open boundaries are printed for', &
                ' output option 3 and higher !')
        1100 FORMAT (' all bottom exchanges are duplicated.')
        1110 FORMAT (/' ERROR, Theoretical number of bottom exchanges:', I10, &
                /'        does not match number in practice     :', I10)

    END SUBROUTINE generate_bed_layer_pointers

    subroutine create_boundary_pointers(file_unit_list, noseg, noq, noqt, intsrt, &
            output_verbose_level, GridPs, nobnd, jtrack, ipoint, &
            status)

        !! Determines boundary pointers and number of codiagonals
        !!     Subroutines called : generate_bed_layer_pointers to add pointers in the water bed
        !!     Logical units      : file_unit_list(29) = unit formatted output file
        !!                          file_unit_list( 2) = unit unformatted system file

        use m_grid_utils_external        !   for the storage of contraction grids
        use timers       !   performance timers

        integer(kind = int_wp), intent(in) :: file_unit_list   (*)          !< array with unit numbers
        integer(kind = int_wp), intent(in) :: noseg              !< number of volumes
        integer(kind = int_wp), intent(in) :: noq                !< number of exchanges from input
        integer(kind = int_wp), intent(in) :: noqt               !< total number of exchanges
        integer(kind = int_wp), intent(in) :: intsrt             !< integration option
        integer(kind = int_wp), intent(in) :: output_verbose_level             !< flag for more or less output
        type(GridPointerColl)        GridPs            !< Structure with grid info
        integer(kind = int_wp), intent(out) :: nobnd              !< number of open boundaries
        integer(kind = int_wp), intent(out) :: jtrack             !< number of codiagonals
        integer(kind = int_wp), intent(inout) :: ipoint(4, noqt)     !< exchange pointers

        type(error_status) :: status !< current error status

        integer(kind = int_wp), allocatable :: ibnd(:, :)      !  boundary pointer structure
        integer(kind = int_wp) :: ierr2      ! local error count
        integer(kind = int_wp) :: iwar2      ! local warning count
        integer(kind = int_wp) :: iwar2_old  ! local warning count help variable
        integer(kind = int_wp) :: iq         ! loop counter exchanges
        integer(kind = int_wp) :: ip1, ip2   ! from and to pointers
        integer(kind = int_wp) :: i          ! loop counter
        integer(kind = int_wp) :: lunut      ! output report file
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("create_boundary_pointers", ithndl)

        ierr2 = 0
        iwar2 = 0
        lunut = file_unit_list(29)

        ! calculate number of boundaries
        nobnd = 0
        do iq = 1, noq
            do i = 1, 4
                ip1 = ipoint(i, iq)
                if (ip1 > noseg) then
                    write (lunut, 2000) ip1, iq, noseg
                    call status%increase_error_count()
                endif
                nobnd = min(nobnd, ip1)
            enddo
        enddo
        nobnd = -nobnd
        write (lunut, 2010) nobnd

        ! Determine JTRACK
        jtrack = 0
        do iq = 1, noqt
            ip1 = ipoint(1, iq)
            ip2 = ipoint(2, iq)
            if (ip1 > 0 .and. ip2 > 0) jtrack = max(jtrack, abs(ip1 - ip2))
        enddo
        if (intsrt == 6 .or. intsrt == 7 .or. intsrt == 10) then
            write (lunut, 2020) jtrack
        endif

        ! Allocate and zero boundary pointers
        allocate (ibnd(nobnd, 2), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2030) ierr2
            call status%increase_error_count()
            goto 9999
        endif
        ibnd = 0

        ! Set boundary pointers
        if (nobnd > 0) then
            if (output_verbose_level < 3) then
                write (file_unit_list(29), 2040)
            else
                write (file_unit_list(29), 2050)
            endif
            do iq = 1, noq
                ip1 = ipoint(1, iq)
                ip2 = ipoint(2, iq)
                if (ip1 < 0) then
                    if (ip2 > 0) then
                        ibnd(-ip1, 1) = -iq
                        ibnd(-ip1, 2) = ip2
                        if (output_verbose_level >= 3) write (lunut, 2060) -ip1, iq, ip1, ip2
                    endif
                endif
                if (ip2 < 0) then
                    if (ip1 > 0) then
                        ibnd(-ip2, 1) = iq
                        ibnd(-ip2, 2) = ip1
                        if (output_verbose_level >= 3) write (lunut, 2060) -ip2, iq, ip1, ip2
                    endif
                endif
            enddo
        endif

        ! Check if boundary is active
        iwar2_old = 0
        do iq = 1, nobnd
            iwar2_old = iwar2
            if (ibnd(iq, 1) == 0) then
                write (lunut, 2070) iq
                iwar2 = iwar2 + 1
            endif
            if (ibnd(iq, 2) == 0) then
                write (lunut, 2080) iq
                iwar2 = iwar2 + 1
            endif
        enddo
        if (iwar2 > iwar2_old) then
            write (lunut, 2090)
            iwar2 = iwar2 + 1
        end if

        ! Additional pointers and boundaries bottom grid
        call generate_bed_layer_pointers (file_unit_list, output_verbose_level, gridps, ibnd, ipoint, &
                noqt, status)

        deallocate(ibnd)

        call status%increase_warning_count_with(iwar2)
        9999 if (timon) call timstop(ithndl)
        return

        2000 format (/, ' ERROR, segment number:', I8, ' in exchange:', I8, &
                ' larger than number of segments (', I8, ')')
        2010 format (/, ' Number of boundaries  :', I8, //)
        2020 format (/, ' Number of codiagonals of the system matrix is:', I8)
        2030 format (/, ' ERROR allocating memory for boundaries:', I8)
        2040 format (' exchanges with open boundaries are printed for', &
                ' output option 3 and higher !')
        2050 format (' boundary  exchange    from        to'/ &
                '  number    number    segment    segment')
        2060 format (I7, 3I10)
        2070 format (' WARNING, there is no flow associated', &
                ' with boundary nr:', I8)
        2080 format (' WARNING, there is no active segment associated', &
                ' with boundary nr:', I8)
        2090 format (' WARNING REMARK, warnings about no flow and/or no active segment associated', /, &
                ' are normally expected in models with z-layer hydrodynamics')

    end subroutine create_boundary_pointers

end module exchange_pointers
