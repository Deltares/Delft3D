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
module m_rwfunc
    use m_waq_precision

    implicit none

contains


    subroutine rwfunc (iopt, nitem, nvals, item, nrec, &
            nhtot, ifact, dtflg, dtflg3, lununf, &
            iwidth, ioutpt, ierr)

        !       Deltares Software Centre

        !>\file
        !>           Reads function values (Fourier and Harmonic components)
        !>
        !>           Harmonic components for iopt = 3 and Fouriers for iopt = 4/n
        !>           Reads:
        !>           - number of frequencies exclusive of average value
        !>           - matrix(nvals*nitem) of average values
        !>           - the base frequency (Fourrier series only)
        !>           Per frequency:
        !>           - period (only for Harmonics)
        !>           - a phase of the component
        !>           - matrix(nvals*nitem) of values for the component
        !>           Notes:
        !>           - because there is an average value, there are nharm+1 matrices
        !>           - because there is a phase, there are 1+nvals*nitem values per component
        !>           - the periods are filled in for the Fouriers, so they look further more the same
        !>           - the zero'th period contains the value nvals*nitem
        !>           - the zero'th phase contains the number of harmoncs
        !>           The routine is entered with ierr = -1  for boundaries and -2 for wastes

        !     Created            : March 1988  by M.E. Sileon

        !     Modified           : April 1997  by R. Bruinsma : Tokenized input data file reading added
        !                        : May   2011  by Leo Postma  : Fortran90 look and feel,
        !                                                       no external memory needed

        !     Subroutines called : conver
        !                          cnvtim

        !     Functions called   : gettok tokenized input data file reading

        !     Logical units      : lunut  = unit formatted output file
        !                          lununf = unit unformatted output file

        use m_conver
        use rd_token       ! for the reading of tokens
        use timers       !   performance timers
        use date_time_utils, only : convert_relative_time

        implicit none

        !     Parameters

        !     kind        function         name            Descriptipon

        integer(kind = int_wp), intent(in) :: iopt           !< 3 Harmonics, 4 Fourier
        integer(kind = int_wp), intent(in) :: nitem          !< number of input items
        integer(kind = int_wp), intent(in) :: nvals          !< number of values per item
        integer(kind = int_wp), intent(in) :: item (nitem)   !< item numbers
        integer(kind = int_wp), intent(inout) :: nrec           !< number of harmonic records
        integer(kind = int_wp), intent(inout) :: nhtot          !< total harmonic array space
        integer(kind = int_wp), intent(in) :: ifact          !< factor between clocks
        logical, intent(in) :: dtflg         !< "date"-format
        logical, intent(in) :: dtflg3        !< 'date'-format (F;ddmmhhss,T;yydddhh)
        integer(kind = int_wp), intent(in) :: lununf         !< unit nr unformatted file
        integer(kind = int_wp), intent(in) :: iwidth         !< width of theoutput file
        integer(kind = int_wp), intent(in) :: ioutpt         !< how extensive output ?
        integer(kind = int_wp), intent(inout) :: ierr           !< error count

        integer(kind = int_wp) :: ndim           ! total size of the matrix
        integer(kind = int_wp) :: nhar           ! number of harmonics
        integer(kind = int_wp) :: ibase          ! base period of Fouriers
        integer(kind = int_wp) :: ierr2          ! error hlp variable
        integer(kind = int_wp) :: i, k           ! loop variables
        integer(kind = int_wp) :: ib, ie         ! limits for printed output
        integer(kind = int_wp) :: i1, i2         ! print loop counters
        logical                      bound         ! true if boundary processing
        logical                      waste         ! true if waste processing
        integer(kind = int_wp), allocatable :: iperio(:)      ! workspace for frequencies
        real(kind = real_wp), allocatable :: value (:, :)    ! workspace for values
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("rwfunc", ithndl)

        bound = .false.
        waste = .false.
        if (ierr == -1) bound = .true.
        if (ierr == -2) waste = .true.
        ierr = 0
        ndim = nitem * nvals

        if (gettoken(nhar, ierr2) > 0) goto 100
        allocate (iperio(nhar + 1), value(ndim + 1, nhar + 1))

        select case (iopt)

        case (3)        !      read values if IOPT = 3 ( harmonic function )

            do k = 2, ndim + 1
                if (gettoken(value(k, 1), ierr2) > 0) goto 100
            enddo
            do i = 2, nhar + 1
                if (gettoken(iperio(i), ierr2) > 0) goto 100
                do k = 1, ndim + 1
                    if (gettoken(value(k, i), ierr2) > 0) goto 100
                enddo
            enddo
            call conver (iperio(2), nhar, ifact, dtflg, dtflg3)
            value(1, 1) = float(nhar)

        case (4)        !      read values if IOPT = 4 ( fourier function )

            if (gettoken(ibase, ierr2) > 0) goto 100
            do k = 2, ndim + 1
                if (gettoken(value(k, 1), ierr2) > 0) goto 100
            enddo
            do i = 2, nhar + 1
                do k = 1, ndim + 1
                    if (gettoken(value(k, i), ierr2) > 0) goto 100
                enddo
            enddo
            call convert_relative_time (ibase, ifact, dtflg, dtflg3)
            value(1, 1) = float(nhar)
            do i = 2, nhar + 1
                iperio(i) = ibase / (i - 1)
            enddo

        end select

        !        control writing

        if (ioutpt < 4) then
            write (lunut, 2070)
        else
            if (iopt == 3) then
                write (lunut, 2000) nhar
            else
                write (lunut, 2010) nhar, ibase
            endif
            write (lunut, 2020)
            do i1 = 1, nvals, iwidth
                write (lunut, 2030) (k, k = i1, min(i1 + iwidth - 1, nvals))
                do i2 = 1, nitem
                    ib = (i2 - 1) * nvals + 1 + i1
                    ie = (i2 - 1) * nvals + 1 + min(i1 + iwidth - 1, nvals)
                    write (lunut, 2040) item(i2), (value(k, 1), k = ib, ie)
                enddo
            enddo
            do i = 2, nhar + 1
                write (lunut, 2050) iperio(i), value(1, i)
                if (iperio(i) <= 0) then
                    write (lunut, 2060)
                    ierr = ierr + 1
                endif
                do i1 = 1, nvals, iwidth
                    write (lunut, 2030) (k, k = i1, min(i1 + iwidth - 1, nvals))
                    do i2 = 1, nitem
                        ib = (i2 - 1) * nvals + 1 + i1
                        ie = (i2 - 1) * nvals + 1 + min(i1 + iwidth - 1, nvals)
                        write (lunut, 2040) item(i2), (value(k, i), k = ib, ie)
                    enddo
                enddo
            enddo
        endif

        !        calculate new settings

        iperio(1) = ndim
        if (bound .or. waste) write (lununf) nhar + 1
        do i = 1, nhar + 1
            write (lununf) iperio(i), (value(k, i), k = 1, ndim + 1)
        enddo
        nrec = nrec + nhar + 1
        nhtot = nhtot + (ndim + 1) * (nhar + 1)
        if (timon) call timstop(ithndl)
        return

        100 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        !        output formats

        2000 format(/, ' Number of harmonics:', I4)
        2010 format(/, ' Number of Fouriers:', I4, ' base period:', I10)
        2020 format(' Mean values :')
        2030 format('      Item', I8, 9I12)
        2040 format(I10, 2X, 1P, 10E12.4)
        2050 format(' Period:', I10, '   Phase:', 1P, E12.4)
        2060 format(' ERROR, PERIOD is less or equal to zero!')
        2070 format(' Printed output for output option 4 and higher !')

    end

end module m_rwfunc
