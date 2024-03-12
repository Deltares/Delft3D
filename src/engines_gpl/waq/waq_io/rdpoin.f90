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
module m_rdpoin
    use m_waq_precision

    implicit none

contains


    subroutine rdpoin (nmax, iopt, ioutpt, ipnt, npnt, &
            ierr)

        !       Deltares Software Centre

        !>\file
        !>                   Reads the item numbers of an input block
        !>
        !>                   This routine reads:
        !>                   - amount of items contained in this block
        !>                   - item numbers in this block
        !>                   If iopt = 1, then block function, item numbers negative

        !     Created            : March '88  By M.E. Sileon / L. Postma

        !     Modified           : April 1997 by R. Bruinsma: Tokenized input data file reading added
        !                          May   2011    Leo Postma : Fortran 90 look and feel

        !     Subroutines called : none

        !     Functions   called : gettoken from rd_token to read the data

        !     Logical units      : lunut = unit formatted output file

        use timers       !   performance timers
        use rd_token       ! for the reading of tokens

        implicit none

        !     Parameters

        !     kind           function         name                Descriptipon

        integer(kind = int_wp), intent(in) :: nmax               !< maximum amount of items
        integer(kind = int_wp), intent(in) :: iopt               !< is 1 for block functions
        integer(kind = int_wp), intent(in) :: ioutpt             !< how extensive is output ?
        integer(kind = int_wp), intent(out) :: ipnt  (nmax)       !< the item numbers of this block
        integer(kind = int_wp), intent(out) :: npnt               !< amount of items of this block
        integer(kind = int_wp), intent(inout) :: ierr               !< cumulative error indicator

        !     local decalations

        integer(kind = int_wp) :: ierr2      ! local error variable
        integer(kind = int_wp) :: i          ! loop counter
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("rdpoin", ithndl)


        !          read number of items in this block

        if (gettoken(npnt, ierr2) > 0) goto 10

        !          read the item numbers

        do i = 1, npnt
            if (gettoken(ipnt(i), ierr2) > 0) goto 10
            ipnt(i) = iabs (ipnt(i))
            if (ipnt(i) > nmax) then
                write (lunut, 2000) ipnt(i), nmax
                ierr = ierr + 1
            endif
        enddo

        !          write them if needed

        write(lunut, 2010) npnt
        if (ioutpt >= 3) then
            write(lunut, 2020) (ipnt(i), i = 1, npnt)
        else
            write(lunut, 2030)
        endif

        !       Set negative values if IOPT = 1 ( block function )

        if (iopt == 1) then
            do i = 1, npnt
                ipnt(i) = -ipnt(i)
            enddo
        endif
        if (timon) call timstop(ithndl)
        return

        10 ierr = ierr + 1
        if (timon) call timstop(ithndl)
        return

        !       Output formats

        2000 format (' ERROR. Item number:', I4, ' larger than maximum (', I4, ')!')
        2010 format (/, ' Amount of numbers in this block:', I4)
        2020 format (' Numbers in their order of input:', /, (5X, 10I7))
        2030 format (' Printed output on input items only for option 3 and higher !')

    end

end module m_rdpoin
