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
module m_opt3
    use m_waq_precision
    use m_rwfunc

    implicit none

contains


    subroutine opt3 (lun, lchar, is, nitem, nvals, &
            nscal, ifact, dtflg, dtflg3, nrfunc, &
            nrharm, iwidth, ioutpt, ierr)

        !       Deltares Software Centre

        !>\file
        !>             Read time dependent variables
        !>
        !>             Time depending data can come in 2 ways
        !>             - a table with values at breakpoints
        !>             - a table with harmonic or Fourier values
        !>             The values at breakpoints require following input:
        !>             - iopt, should be 1 (no defaults) or 2 (defaults and overridings)
        !>             - number of items in this block    (nvarnw, read in rdpoin)
        !>             - that many ID values of the items (itemId, read in rdpoin)
        !>             - number of breakpoints (nobrk2, this in the number of time steps)
        !>             - scale values to be applied for this block ( 1 or nval1 )
        !>             - table of values in (nval1,nitem) order.
        !>             The function option requires the following input
        !>             - iopt, should be 3 (harmonics) or 4 (Fouriers)
        !>             - number of items in this block    (nvarnw, read in rdpoin)
        !>             - that many ID values of the items (itemId, read in rdpoin)
        !>             - number of harmonics or Fourier components (nhar  , read in rwfunc)
        !>             - nval1 values for the zero-th harmonic  (the mean , read in rwfunc)
        !>             - nhar times:
        !>               - a period of the harmonic    ( NOT for the Fouriers )
        !>               - the phase of the harmonic, or Fourier
        !>               - nval1 amplitudes of this component
        !>             A number of these blocks are read, untill all items got a value for
        !>             all nval1/n
        !>             For the new processing of Bounds, Wastes and Funcs, the file is
        !>             - initialised with a specific header
        !>             - written per block with:
        !>               - heading block information
        !>               - breakpoint + matrix at the breakpoint for each breakpoint.
        !>             For the old processing the blocks are merged to one big matrix. The
        !>             information on the items is written in the system file. The big matrix
        !>             of size (nval1,nitem,nobrkt) is written to the binary file. Because
        !>             the size of this total matrix is not clear in advance, the matrix is
        !>             reallocated for every new block. Previous versions that used a swap file
        !>             for the matrix have been phased out, because memory is likely not a
        !>             problem at the moment any more.

        !     Created            : April 1988 by M.E. Sileon / Leo Postma

        !     Modified           : April 1997 by R. Bruinsma : Tokenized input data file reading added
        !                          May   2011 by Leo Postma  : Fortran90 look and feel, own memory

        !     Subroutines called : rdpoin   - pointers of input variable to model items
        !                          fmread   - read of a breakpoint series of matrices
        !                          dmatrix  - merge of 2 matrices
        !                          rwfunc   - read of a block of harmonics or Fourier series
        !                          open_waq_files   - open a file

        !     Functions called   : gettok   - tokenized input data file reading

        !     Logical units      : lunut   = unit formatted output file
        !                          lun( 3) = unit binary intermediate file for harmonics
        !                          lun( 4) = unit binary intermediate file for pointers
        !                          lun(is) = unit binary intermediate file for function

        use m_rdpoin
        use m_matrix
        use m_fmread
        use m_open_waq_files
        use timers       !   performance timers
        use rd_token
        use m_sysn          ! System characteristics

        implicit none

        !     Parameters

        !     kind           function         name                Descriptipon

        integer(kind = int_wp), intent(inout) :: lun   (*)          !< array with unit numbers
        character(*), intent(in) :: lchar (*)         !< array with file names of the files
        integer(kind = int_wp), intent(in) :: is                 !< entry in lun for this call
        integer(kind = int_wp), intent(in) :: nitem              !< number of required items
        integer(kind = int_wp), intent(in) :: nvals              !< number of values per item
        integer(kind = int_wp), intent(in) :: nscal              !< number of scale values
        integer(kind = int_wp), intent(in) :: ifact              !< factor between clocks
        logical, intent(in) :: dtflg             !< 'date'-format for output ?
        logical, intent(in) :: dtflg3            !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(out) :: nrfunc             !< number of functions
        integer(kind = int_wp), intent(out) :: nrharm             !< number of harmonic functions
        integer(kind = int_wp), intent(in) :: iwidth             !< width of the output file
        integer(kind = int_wp), intent(in) :: ioutpt             !< flag for more or less output
        integer(kind = int_wp), intent(inout) :: ierr               !< error count / switch



        !     local decalations

        integer(kind = int_wp), pointer :: breaks(:)         !  breakpoints
        integer(kind = int_wp), allocatable :: break2(:)         !  breakpoints of a block
        integer(kind = int_wp), pointer :: break3(:)         !  help pointer for resizing
        real(kind = real_wp), pointer :: values(:, :)       !  values
        real(kind = real_wp), allocatable :: value2(:, :)       !  values of a block
        real(kind = real_wp), pointer :: value3(:, :)       !  help pointer for resizing
        integer(kind = int_wp) :: itemId(nitem)      !  array for itemIds
        real(kind = real_wp) :: factor(nvals)      !  array for scale factors
        integer(kind = int_wp) :: nval1              !  nval1 but at least 1
        logical(4) bound             !  boundary ?
        logical(4) waste             !  wastes ?
        logical(4) funcs             !  segment functions ?
        logical(4) found             !  help variable for finding strings
        integer(kind = int_wp) :: ierr2              !  local error variable
        integer(kind = int_wp) :: ifilsz             !  local counter of used integer array space
        integer(kind = int_wp) :: jfilsz             !  local counter of used real array space
        integer(kind = int_wp) :: ntot               !  nitem*nval1, real space of one breakpoint
        integer(kind = int_wp) :: ntotal             !  total number of items with input
        integer(kind = int_wp) :: nobrkt             !  total number of breakpoints
        integer(kind = int_wp) :: nobrk2             !  number of breakpoints in this block
        integer(kind = int_wp) :: newbrk             !  number of breakpoints for the new allocation
        integer(kind = int_wp) :: iopt3              !  option for this block
        integer(kind = int_wp) :: nvarnw             !  number of items in a block
        integer(kind = int_wp) :: lunuit             !  the unit of the binary file
        integer(kind = int_wp) :: i1, i2, k          !  loop counters
        integer(kind = int_wp) :: ibrk               !  loop counter breakpoints
        integer(kind = int_wp) :: iscal              !  loop counter scale values
        integer(kind = int_wp) :: nrec               !  total nr of rec's
        integer(kind = int_wp) :: nrec2              !  local nr of rec's
        integer(kind = int_wp) :: nvarar             !  number of items previous read
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("opt3", ithndl)

        !          Initialisations

        breaks => null()
        break3 => null()
        values => null()
        value3 => null()

        bound = .false.
        waste = .false.
        funcs = .false.
        if (ierr == -1) bound = .true.
        if (ierr == -2) waste = .true.
        if (ierr == -3) funcs = .true.
        nval1 = nvals
        if (funcs .and. nvals == 0) nval1 = 1
        ierr = 0
        ifilsz = 0
        jfilsz = 0
        ntot = nitem * nval1
        ntotal = 1
        nobrkt = 0
        nrec = 0

        !         write headers for new style time series files

        write (lunut, 2000)
        !        open the output work file
        !        write nr of items and nr of substances
        !        write default values ( IORDER = 1 , NPNT = 0 )
        lunuit = lun(3)
        if (.not. funcs) call open_waq_files (lun(is), lchar(is), is, 1, ierr2)
        if (bound) then
            write (lun(is)) ' 4.900BOUND '
            write (lun(is)) nitem, nval1
            write (lun(is)) 1, 0, nval1, (k, k = 1, nval1), 1, 0
        endif
        if (waste) then
            write (lun(is)) ' 4.900WASTE '
            write (lun(is)) nitem, nval1
            write (lun(is)) 1, 0, nval1, (k, k = 0, nval1 - 1), 1, 0
        endif
        if (bound .or. waste) then
            write (lun(is)) 1
            write (lun(is)) 0, (0.0, k = 1, nval1)
            ifilsz = ifilsz + 2 + 3 + nval1 + 3 + 1
            jfilsz = jfilsz + nval1
        endif
        if (bound .or. waste .or. funcs) lunuit = lun(is)

        do while (ntotal - 1 < nitem)     ! loop over blocks till completion

            !           read the type of block that comes

            if (gettoken(iopt3, ierr2) > 0) goto 100
            write (lunut, 2010) iopt3
            if (iopt3 < 1 .or. iopt3 > 4) then
                write (lunut, 2020)
                goto 100
            endif

            !           the items in this block by itemnumber

            call rdpoin (nitem, iopt3, ioutpt, itemId(ntotal), nvarnw, &
                    ierr)

            !           new style for boundaries and wastes

            if (bound .or. funcs) &
                    write (lun(is)) 1, nvarnw, (iabs(itemId(ntotal + k)), k = 0, nvarnw - 1), &
                            nvals, (k, k = 1, nvals), iopt3, 1
            if (waste) &
                    write (lun(is)) 1, nvarnw, (iabs(itemId(ntotal + k)), k = 0, nvarnw - 1), &
                            nval1, (k, k = 0, nval1 - 1), iopt3, 1
            if (bound .or. waste .or. funcs) &
                    ifilsz = ifilsz + 5 + nvarnw + nvals

            select case (iopt3)

            case (1, 2)             !         Read time-dependent items on breakpoints
                if (gettoken(nobrk2, ierr2) > 0) goto 100
                write (lunut, 2030) nobrk2
                allocate (break2(nobrk2), value2(nvarnw * nval1, nobrk2))
                do iscal = 1, nscal
                    if (gettoken(factor(iscal), ierr2) > 0) goto 100
                enddo
                call fmread (nvarnw, itemId(ntotal), nval1, nscal, factor, &
                        nobrk2, break2, value2, dtflg, dtflg3, &
                        ifact, iwidth, ioutpt, ierr)

                if (bound .or. waste .or. funcs) then
                    write (lun(is)) nobrk2                      ! boundaries, wastes and
                    ifilsz = ifilsz + 1                           ! functions are written
                    do ibrk = 1, nobrk2                           ! directly per block
                        write (lun(is)) break2(ibrk), value2(:, ibrk)
                    enddo
                    ifilsz = ifilsz + nobrk2
                    jfilsz = jfilsz + nobrk2 * nvarnw * nval1
                else                                             ! other are merged into
                    newbrk = nobrkt + nobrk2                      ! one big matrix that is
                    allocate (break3(newbrk), value3(ntot, newbrk))  ! written at the end
                    if (nobrkt > 0) then
                        break3(1:nobrkt) = breaks(1:nobrkt)
                        value3(:, 1:nobrkt) = values(:, 1:nobrkt)    ! expand the matrix to allow
                        deallocate(breaks, values)               ! for the new values to enter
                    endif
                    breaks => break3
                    values => value3
                    nvarar = ntotal - 1
                    call dmatrix (ntot, nval1, nvarar, nvarnw, nobrkt, &
                            nobrk2, breaks, break2, values, value2, &
                            itemId(ntotal))
                endif
                deallocate (break2, value2)

            case (3, 4)            !         Read items as functions
                write (lunut, 2050)
                nrec2 = 0                                        ! these function blocks are
                if (bound .or. funcs) then                     ! are written in the
                    ierr2 = -1                                    ! lunuit = lun(is) file
                endif                                            ! for bounds, wastes and funcs
                if (waste) then                                ! and to lunuit = lun(3), the
                    ierr2 = -2                                    ! system file, for others
                endif
                call rwfunc (iopt3, nvarnw, nval1, itemId(ntotal), nrec2, &
                        nharms, ifact, dtflg, dtflg3, lunuit, &
                        iwidth, ioutpt, ierr2)
                ierr = ierr + ierr2
                if (bound .or. waste .or. funcs) then
                    ifilsz = ifilsz + nrec2
                    jfilsz = jfilsz + nrec2 * (nvarnw * nval1 + 1)
                else
                    nrec = nrec + nrec2
                endif

            end select

            ntotal = ntotal + nvarnw

        enddo

        if (ntotal - 1 > nitem) then
            write (lunut, 2060) ntotal - 1, nitem
            ierr = ierr + 1
        endif

        !      Check complete pointer structure

        do i1 = 1, nitem
            found = .false.
            do i2 = 1, nitem
                if (iabs(itemid(i2)) == i1) then
                    if (found) then
                        write (lunut, 2070) i1
                        ierr = ierr + 1
                    else
                        found = .true.
                    endif
                endif
            enddo
            if (.not. found) then
                write (lunut, 2080) i1
                ierr = ierr + 1
            endif
        enddo

        !      Finalize this input section

        if (bound .or. waste .or. funcs) then
            newrsp = newrsp + jfilsz
            newisp = newisp + ifilsz
        else                         !    write pointers and breakpoint matrix
            write (lun(4)) itemid, 0, 0, 0
            do ibrk = 1, nobrkt
                write (lun(is)) breaks(ibrk), values(:, ibrk)
            enddo
            close (lun(is))
            nlines = nlines + ntot * 2
            npoins = npoins + nitem + 3
            nrfunc = ntot
            nrharm = nrec
            niharm = niharm + nrec
        endif
        ierr = ierr + ierr2
        if (associated (breaks)) deallocate (breaks, values)
        if (timon) call timstop(ithndl)
        return

        100 ierr = ierr + 1
        return

        !       Output formats

        2000 format (' Time variable data.')
        2010 format (/, ' Option selected : ', I2)
        2020 format (/, ' ERROR, option not implemented')
        2030 format (' Number of breakpoints:', I7)
        2050 format (' Block with periodic functions.')
        2060 format (' ERROR, too many (', I5, ') items, ', I5, ' expected!')
        2070 format (' ERROR, duplicate item:', I5)
        2080 format (' ERROR, non-initialised item:', I5)

    end

end module m_opt3
