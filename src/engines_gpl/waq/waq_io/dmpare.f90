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
module m_dmpare
    use m_waq_precision
    use m_error_status

    implicit none

contains


    subroutine dmpare (lun, ndmpar, ntdmps, noq, noseg, &
            nobnd, ipoint, ntdmpq, ndmpq, ndmps, &
            noraai, ntraaq, nsegdmp, isegdmp, nexcraai, &
            iexcraai, ioptraai, status)

        !       Deltares Software Centre

        !>\file
        !>            Make and write the monitoring areas and crosssection administrations
        !>
        !>            The routine receives arrays for:
        !>            - the number of volumes of each monitoring area
        !>            - their indices in consecutive order
        !>            - the amount of crosssections of each monitoring transect
        !>            - their indices in consecutive order
        !>            - an option per transect on how to sum flows
        !>            plus the dimensions of these arrays./n
        !>            The routine makes for monitoring areas arrays with:
        !>            - the number of flows involved for each monitoring area
        !>            - the flow numbers in consecutive order, negative if sign needs reversed
        !>            - an array with per flow at what location it should be stored in
        !>              a condensed array with only the required flow information
        !>            - an array with per computational volume at what location it should
        !>              be stored in a condensed array with only the required volume information
        !>            plus the dimensions of these arrays./n
        !>            All arrays are written to the binary intermediate file.

        !     Created            : March 1995  by Jan van Beek

        !     Modified           ! May   2011  by Leo Postma   ; Fortran90 look and feel

        !     Subroutines called : none

        !     Logical units      : lun( 2) = unit unformatted system file
        !                          lun(29) = unit number output report file

        use timers       !   performance timers

        implicit none

        !     Parameters         :

        !     kind           function         name                Descriptipon

        integer(kind = int_wp), intent(in) :: lun     (*)        !< array with unit numbers
        integer(kind = int_wp), intent(in) :: ndmpar             !< number of dump areas
        integer(kind = int_wp), intent(in) :: ntdmps             !< number of volumes in dump array
        integer(kind = int_wp), intent(in) :: noq                !< total number of exchange
        integer(kind = int_wp), intent(in) :: noseg              !< total number of computation volumes
        integer(kind = int_wp), intent(in) :: nobnd              !< number of open boundaries
        integer(kind = int_wp), intent(in) :: ipoint  (4, noq)    !< exchange pointers
        integer(kind = int_wp), intent(out) :: ntdmpq             !< total number exchanges in dump area
        integer(kind = int_wp), intent(out) :: ndmpq              !< number exchanges dumped
        integer(kind = int_wp), intent(out) :: ndmps              !< number segments dumped
        integer(kind = int_wp), intent(in) :: noraai             !< number of transects
        integer(kind = int_wp), intent(in) :: ntraaq             !< total number of exchanges in transects
        integer(kind = int_wp), intent(in) :: nsegdmp (ndmpar)   !< number of volumes per area
        integer(kind = int_wp), intent(inout) :: isegdmp (ntdmps)   !< volume numbers
        integer(kind = int_wp), intent(in) :: nexcraai(noraai)   !< number of exchanges per transect
        integer(kind = int_wp), intent(in) :: iexcraai(ntraaq)   !< exchange numbers
        integer(kind = int_wp), intent(in) :: ioptraai(ntraaq)   !< exchange accumulation option

        type(error_status), intent(inout) :: status !< current error status

        !     Local variables

        integer(kind = int_wp) :: lurep                        !  output report file
        integer(kind = int_wp) :: itel                         !  counter to run through isegdmp array
        integer(kind = int_wp) :: idump                        !  loop counter monitoring areas
        integer(kind = int_wp) :: iraai                        !  loop counter monitoring transects
        integer(kind = int_wp) :: nsc                          !  number of volumes in that monitoring area
        integer(kind = int_wp) :: nq                           !  number of exchanges in mon. area or transect
        integer(kind = int_wp) :: idmpq                        !  loop counter number of exchanges in mon. area or transect
        integer(kind = int_wp) :: iseg                         !  volume number
        integer(kind = int_wp) :: is                           !  volume number within an area
        integer(kind = int_wp) :: iq                           !  exchange number
        integer(kind = int_wp) :: iqr                          !  exchange number within a transect
        integer(kind = int_wp) :: i                            !  loop counter
        integer(kind = int_wp) :: ips2                         !  help variable to save offset
        integer(kind = int_wp) :: is2                          !  counter within the offset
        integer(kind = int_wp) :: ivan                         !  help variable 'from' volume number
        integer(kind = int_wp) :: inaar                        !  help variable 'to' volume number
        integer(kind = int_wp) :: max_ntdmpq                   !  maximum dimension of the ntdmpq array
        integer(kind = int_wp) :: mxnqseg                      !  maximum exchanges of any segment
        integer(kind = int_wp) :: iqs                          !  loop counter exchanges per segment
        integer(kind = int_wp) :: ierr2                        !  local error variable

        !     Local arrays

        integer(kind = int_wp), allocatable :: iqdmp(:)      !  pointer from exchange nr to condensed array
        integer(kind = int_wp), allocatable :: nqdmp(:)      !  per monitoring area the number of exchanges
        integer(kind = int_wp), allocatable :: isdmp(:)      !  pointer from volume nr to condensed array
        integer(kind = int_wp), pointer :: ipdmpq   (:)  !  array with consecutive interface numbers
        integer(kind = int_wp), pointer :: p2_ipdmpq(:)  !  help pointer to expand the consecutive array
        logical, allocatable :: indmp(:)     !  is the segment in the current area
        integer(kind = int_wp), allocatable :: nqseg(:)      !  number of exchanges per segment
        integer(kind = int_wp), allocatable :: iqseg(:, :)    !  exchanges per segment
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: ithndl1 = 0
        integer(kind = int_wp) :: ithndl2 = 0

        if (ndmpar == 0 .and. noraai == 0) return
        if (timon) call timstrt("dmpare", ithndl)

        allocate(iqdmp(noq))
        allocate(nqdmp(ndmpar))
        allocate(isdmp(noseg))
        allocate(indmp(noseg))

        !     init

        ntdmpq = 0
        ndmpq = 0
        ndmps = 0
        lurep = lun(29)

        !     check segment numbers in dump areas

        itel = 0
        do idump = 1, ndmpar
            nsc = nsegdmp(idump)
            do is = 1, nsc
                itel = itel + 1
                iseg = isegdmp(itel)
                if (iseg == 0) then
                    write (lurep, 2000) idump, is, iseg
                    call status%increase_error_count()
                elseif (iseg < -nobnd) then
                    write (lurep, 2000) idump, is, iseg
                    call status%increase_error_count()
                elseif (iseg < 0) then
                    if (nsc > 1) then
                        write (lurep, 2010) idump, is, iseg
                        call status%increase_error_count()
                    else
                        write (lurep, 2020) idump, is, iseg
                        call status%increase_warning_count()
                    endif
                elseif (iseg > noseg) then
                    write (lurep, 2000) idump, is, iseg
                    call status%increase_error_count()
                    isegdmp(itel) = 0
                endif
            enddo
        enddo

        if (status%ierr > 0) then
            call status%increase_error_count()
            return
        endif

        !     allocate

        !jvb max_ntdmpq = 6 * ndmpar
        max_ntdmpq = noq
        allocate (ipdmpq(max_ntdmpq), stat = ierr2)
        if (ierr2 /= 0) then
            write (lurep, 2030) ierr2, max_ntdmpq
            call status%increase_error_count()
            return
        endif

        ! analyse pointer table

        if (timon) call timstrt("dmpare1", ithndl1)
        allocate(nqseg(noseg))
        nqseg = 0
        do iq = 1, noq
            ivan = ipoint(1, iq)
            inaar = ipoint(2, iq)
            if (ivan  > 0) then
                nqseg(ivan) = nqseg(ivan) + 1
            endif
            if (inaar > 0) then
                nqseg(inaar) = nqseg(inaar) + 1
            endif
        enddo
        mxnqseg = maxval(nqseg)
        allocate(iqseg(mxnqseg, noseg))
        nqseg = 0
        do iq = 1, noq
            ivan = ipoint(1, iq)
            inaar = ipoint(2, iq)
            if (ivan  > 0) then
                nqseg(ivan) = nqseg(ivan) + 1
                iqseg(nqseg(ivan), ivan) = iq
            endif
            if (inaar > 0) then
                nqseg(inaar) = nqseg(inaar) + 1
                iqseg(nqseg(inaar), inaar) = iq
            endif
        enddo
        if (timon) call timstop(ithndl1)

        !     Zero workspace

        iqdmp = 0

        !     Loop over the dump area's

        if (timon) call timstrt("dmpare2", ithndl2)
        itel = 0
        do idump = 1, ndmpar                  ! look for all dumpareas
            nq = 0
            nsc = nsegdmp(idump)
            indmp = .false.
            ips2 = itel
            do is = 1, nsc                         ! set true if volume is in this area
                indmp(isegdmp(ips2 + is)) = .true.
            enddo
            ips2 = itel
            do is = 1, nsc                     ! for all comp. volumes therein
                itel = itel + 1
                iseg = isegdmp(itel)
                do iqs = 1, nqseg(iseg)  ! for all pointers to and from that volume
                    iq = iqseg(iqs, iseg)
                    ivan = ipoint(1, iq)
                    inaar = ipoint(2, iq)
                    if (iseg == ivan) then


                        ! skip if both are in the same dump area

                        if (inaar > 0) then
                            if (indmp(inaar)) goto 110
                        endif

                        !        raise the number of echanges for this dump area

                        nq = nq + 1

                        !        store the exchange number in the area section of this dump area

                        ntdmpq = ntdmpq + 1
                        if (ntdmpq > max_ntdmpq) then    !   extend workarray
                            max_ntdmpq = 2 * max_ntdmpq
                            allocate (p2_ipdmpq(max_ntdmpq), stat = ierr2)
                            if (ierr2 /= 0) then
                                write (lurep, 2040) ierr2, max_ntdmpq
                                call status%increase_error_count()
                                return
                            endif
                            do idmpq = 1, ntdmpq - 1
                                p2_ipdmpq(idmpq) = ipdmpq(idmpq)
                            end do
                            deallocate(ipdmpq)
                            ipdmpq => p2_ipdmpq
                        endif
                        ipdmpq(ntdmpq) = iq

                        !        mark this area to be involved and count areas that are involved

                        if (iqdmp(iq) == 0) then
                            ndmpq = ndmpq + 1
                            iqdmp(iq) = ndmpq
                        endif

                        110             continue

                    elseif (iseg == inaar) then


                        ! skip if both are in the same dump area

                        if (ivan > 0) then
                            if (indmp(ivan)) goto 210
                        endif

                        !        raise the number of echanges for this dump area

                        nq = nq + 1

                        !        store the exchange number in the area section of this dump area

                        ntdmpq = ntdmpq + 1
                        if (ntdmpq > max_ntdmpq) then    !   extend workarray
                            max_ntdmpq = 2 * max_ntdmpq
                            allocate (p2_ipdmpq(max_ntdmpq), stat = ierr2)
                            if (ierr2 /= 0) then
                                write (lurep, 2040) ierr2, max_ntdmpq
                                call status%increase_error_count()
                                return
                            endif
                            do idmpq = 1, ntdmpq - 1
                                p2_ipdmpq(idmpq) = ipdmpq(idmpq)
                            end do
                            deallocate(ipdmpq)
                            ipdmpq => p2_ipdmpq
                        endif
                        ipdmpq(ntdmpq) = -iq            ! negative sign, inward bound

                        !        mark this area to be involved and count areas that are involved

                        if (iqdmp(iq) == 0) then
                            ndmpq = ndmpq + 1
                            iqdmp(iq) = ndmpq
                        endif

                        210             continue

                    endif

                end do
            end do
            nqdmp(idump) = nq
        end do

        deallocate(indmp)
        deallocate(nqseg)
        deallocate(iqseg)

        if (timon) call timstop(ithndl2)

        !     Loop over raaien

        itel = 0
        do iraai = 1, noraai
            nq = nexcraai(iraai)
            do iq = 1, nq
                itel = itel + 1
                iqr = abs(iexcraai(itel))
                if (iqr == 0 .or. iqr > noq) then
                    write (lurep, 2050) iraai, iq, iexcraai(itel)
                    call status%increase_error_count()
                else
                    if (iqdmp(iqr) == 0) then
                        ndmpq = ndmpq + 1
                        iqdmp(iqr) = ndmpq
                    endif
                endif
            end do
        end do

        if (ndmpar > 0) then
            write(lun(2)) (nqdmp  (i), i = 1, ndmpar), (ipdmpq (i), i = 1, ntdmpq)
            write(lun(2)) (nsegdmp(i), i = 1, ndmpar), (isegdmp(i), i = 1, ntdmps)
        endif
        if (noraai > 0) then
            write(lun(2)) (ioptraai(i), i = 1, noraai)
            write(lun(2)) (nexcraai(i), i = 1, noraai)
            write(lun(2)) (iexcraai(i), i = 1, ntraaq)
        endif
        write(lun(2)) (iqdmp(i), i = 1, noq)

        deallocate(ipdmpq)

        !     Set the ISMDP array

        if (ndmpar > 0) then
            isdmp = 0
            do is = 1, ntdmps
                iseg = isegdmp(is)
                if (iseg > 0) then
                    if (isdmp(iseg) == 0) then
                        ndmps = ndmps + 1
                        isdmp(iseg) = ndmps
                    endif
                endif
            enddo
            write(lun(2)) (isdmp(i), i = 1, noseg)
        endif

        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (/, ' ERROR segment number in monitoring area/point out of range', &
                /' monitor number  :', I15, &
                /' follow number   :', I15, &
                /' segment number  :', I15)
        2010 format (/, ' ERROR segment number in monitoring area is negative', &
                /' monitor number  :', I15, &
                /' follow number   :', I15, &
                /' boundary number :', I15, &
                /' Negative numbers (boundaries) only allowed as single monitor point')
        2020 format (/, ' INFO segment number in monitoring point is negative, boundary assumed', &
                /' monitor number  :', I15, &
                /' follow number   :', I15, &
                /' boundary number :', I15, &
                /' Output only valid for concentration of active substances')
        2030 format (/, ' ERROR. allocating memory for structure for dump area''s (IPDMPQ):', i4, i10)
        2040 format (/, ' ERROR. allocating memory for structure for dump area''s (P2_IPDMPQ):', i4, i10)
        2050 format (/, ' ERROR exchange in transect out of range.', &
                /' raai number     :', I15, &
                /' follow number   :', I15, &
                /' exchamge number :', I15)

    end

end module m_dmpare
