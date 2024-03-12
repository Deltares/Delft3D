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
module m_bound
    use m_waq_precision
    use m_pointb

    implicit none

contains


    subroutine bound  (lun, noseg, noq, noqt, intsrt, &
            ioutpt, GridPs, nobnd, jtrack, ipoint, &
            status)

        !       Deltares Software Centre

        !>\file
        !>            Determines boundary pointers and number of codiagonals

        !     Created            : May  1988  by M.E. Sileon / L. Postma

        !     Modified           : May  2011  by Leo Postma : Fortran90 look and feel

        !     Subroutines called : pointb to add pointers in the water bed

        !     Logical units      : lun(29) = unit formatted output file
        !                          lun( 2) = unit unformatted system file

        use dlwqgrid_mod        !   for the storage of contraction grids
        use timers       !   performance timers
        use m_error_status

        implicit none

        !     Parameters

        !     kind           function         name             Descriptipon

        integer(kind = int_wp), intent(in) :: lun   (*)          !< array with unit numbers
        integer(kind = int_wp), intent(in) :: noseg              !< number of volumes
        integer(kind = int_wp), intent(in) :: noq                !< number of exchanges from input
        integer(kind = int_wp), intent(in) :: noqt               !< total number of exchanges
        integer(kind = int_wp), intent(in) :: intsrt             !< integration option
        integer(kind = int_wp), intent(in) :: ioutpt             !< flag for more or less output
        type(GridPointerColl)        GridPs            !< Structure with grid info
        integer(kind = int_wp), intent(out) :: nobnd              !< number of open boundaries
        integer(kind = int_wp), intent(out) :: jtrack             !< number of codiagonals
        integer(kind = int_wp), intent(inout) :: ipoint(4, noqt)     !< exchange pointers

        type(error_status) :: status !< current error status

        !     local declarations

        integer(kind = int_wp), allocatable :: ibnd(:, :)      !  boundary pointer structure
        integer(kind = int_wp) :: ierr2      ! local error count
        integer(kind = int_wp) :: iwar2      ! local warning count
        integer(kind = int_wp) :: iwar2_old  ! local warning count help variable
        integer(kind = int_wp) :: iq         ! loop counter exchanges
        integer(kind = int_wp) :: ip1, ip2   ! from and to pointers
        integer(kind = int_wp) :: i          ! loop counter
        integer(kind = int_wp) :: lunut      ! output report file
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("bound", ithndl)

        ierr2 = 0
        iwar2 = 0
        lunut = lun(29)

        !       calculate number of boundaries

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

        !     Determine JTRACK

        jtrack = 0
        do iq = 1, noqt
            ip1 = ipoint(1, iq)
            ip2 = ipoint(2, iq)
            if (ip1 > 0 .and. ip2 > 0) jtrack = max(jtrack, iabs(ip1 - ip2))
        enddo
        if (intsrt == 6 .or. intsrt == 7 .or. intsrt == 10) then
            write (lunut, 2020) jtrack
        endif

        !     Allocate and zero boundary pointers

        allocate (ibnd(nobnd, 2), stat = ierr2)
        if (ierr2 /= 0) then
            write (lunut, 2030) ierr2
            call status%increase_error_count()
            goto 9999
        endif
        ibnd = 0

        !     Set boundary pointers

        if (nobnd > 0) then
            if (ioutpt < 3) then
                write (lun(29), 2040)
            else
                write (lun(29), 2050)
            endif
            do iq = 1, noq
                ip1 = ipoint(1, iq)
                ip2 = ipoint(2, iq)
                if (ip1 < 0) then
                    if (ip2 > 0) then
                        ibnd(-ip1, 1) = -iq
                        ibnd(-ip1, 2) = ip2
                        if (ioutpt >= 3) write (lunut, 2060) -ip1, iq, ip1, ip2
                    endif
                endif
                if (ip2 < 0) then
                    if (ip1 > 0) then
                        ibnd(-ip2, 1) = iq
                        ibnd(-ip2, 2) = ip1
                        if (ioutpt >= 3) write (lunut, 2060) -ip2, iq, ip1, ip2
                    endif
                endif
            enddo
        endif

        !     Check if boundary is active

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

        !     Additional pointers and boundaries bottom grid

        call pointb (lun, ioutpt, gridps, ibnd, ipoint, &
                noqt, status)

        deallocate(ibnd)

        call status%increase_warning_count_with(iwar2)
        9999 if (timon) call timstop(ithndl)
        return

        !       Output formats

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

    end

end module m_bound
