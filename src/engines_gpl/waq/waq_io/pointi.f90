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
module m_pointi
    use m_waq_precision
    use m_error_status

    implicit none

contains


    subroutine pointi (lun, lchar, noseg, noq, noq1, &
            noq2, noq3, noqt, nobnd, ipnt, &
            intsrt, ipopt1, jtrack, ftype, ioutpt, &
            GridPs, status)

        !       Deltares Software Centre

        !>\file
        !>            Reads exchange pointers on irregular grid
        !>
        !>            Routine
        !>            - reads the exchange pointers on irregular grid in the waterphase
        !>            - calls bound.f to:
        !>              - compute number of open boundaries
        !>              - adds the bed pointers to the pointer set to make noqt
        !>              - compute number of codiagonals for direct implicit matrices

        !     Created            : April 1989 by M.E. Sileon and L. Postma

        !     Modified           : May   2011 by Leo Postma, Fortran90 look and feel

        !     Subroutines called : BOUND

        !     Logical units      : lunut   = unit formatted output file
        !                          lun( 8) = unit intermediate file ('to-from')

        use m_bound
        use m_open_waq_files
        use dlwqgrid_mod          ! for the storage of contraction grids
        use rd_token       ! for the reading of tokens
        use timers       !   performance timers

        implicit none

        !     Parameters         :

        !     kind           function         name            Descriptipon

        integer(kind = int_wp), intent(inout) :: lun   (*)      !< array with unit numbers
        character(*), intent(inout) :: lchar (*)     !< array with file names of the files
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
        integer(kind = int_wp), intent(in) :: ioutpt         !< flag for more or less output
        type(GridPointerColl)           GridPs        !< Collection of grid pointers

        type(error_status) :: status !< current error status

        !     Local variables    :

        integer(kind = int_wp) :: noq12        ! noq1 + noq2 (horizontal exchanges
        integer(kind = int_wp) :: iq           ! loop counter exchanges
        integer(kind = int_wp) :: ip           ! loop counter pointers
        integer(kind = int_wp) :: ierr1        ! local I/O error
        integer(kind = int_wp) :: ierr2        ! local error count
        integer(kind = int_wp) :: idummy
        character(len = 1) :: cdummy
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("pointi", ithndl)

        ierr2 = 0

        !        Read exchange pointers

        noq12 = noq1 + noq2
        if (ipopt1 == 0)  then
            call open_waq_files(lun(44), lchar(44), 44, 2 + ftype, ierr2)
            if (ierr2 /= 0) goto 100
            do iq = 1, noq
                read (lun(44), iostat = ierr1) ipnt(:, iq)
                if (ierr1 /= 0) then
                    write(lunut, 2100) iq - 1
                    close (lun(44))
                    ierr2 = 1
                    goto 100
                endif
            enddo

            !        Check that there are no more data in the file
            !        For DELWAQ-G applications, there may already be more data
            !        than the raw 4*noq numbers ...

            if (noqt > noq) then
                ! Any extra exchange pointers already present?
                read (lun(44), iostat = ierr1) idummy
                if (ierr1 == 0) then
                    ! Skip all extra exchange pointers that are expected
                    read (lun(44), iostat = ierr1) (idummy, iq = 2, 4 * (noqt - noq))
                    if (ierr1 /= 0) then
                        write(lunut, 2111)
                        close (lun(44))
                        ierr2 = 1
                        goto 100
                    endif
                endif
            endif

            !        Any data after the expected exchange pointers indicate a problem

            read (lun(44), iostat = ierr1) cdummy
            if (ierr1 == 0) then
                write(lunut, 2110)
                close (lun(44))
                ierr2 = 1
                goto 100
            endif

            !        No problems found, so continue

            close (lun(44))
            call open_waq_files  (lun(8), lchar(8), 8, 1, ierr2)
            if (ierr2 /= 0) goto 100
            if (noq1 > 0) write(lun(8))(ipnt(:, iq), iq = 1, noq1)
            if (noq2 > 0) write(lun(8))(ipnt(:, iq), iq = noq1 + 1, noq12)
            if (noq3 > 0) write(lun(8))(ipnt(:, iq), iq = noq12 + 1, noq)
        else
            do iq = 1, noq
                do ip = 1, 4
                    if (gettoken(ipnt(ip, iq), ierr2) > 0) goto 100
                enddo
            enddo
            call open_waq_files  (lun(8), lchar(8), 8, 1, ierr2)
            if (ierr2 /= 0) goto 100
            if (noq1 > 0) write(lun(8))(ipnt(:, iq), iq = 1, noq1)
            if (noq2 > 0) write(lun(8))(ipnt(:, iq), iq = noq1 + 1, noq12)
            if (noq3 > 0) write(lun(8))(ipnt(:, iq), iq = noq12 + 1, noq)

            if (ioutpt < 4) then
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

        !       calculate number of boundaries and bandwith of matrix

        call bound  (lun, noseg, noq, noqt, intsrt, &
                ioutpt, GridPs, nobnd, jtrack, ipnt, &
                status)

        close (lun(8))
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

    end

end module m_pointi
