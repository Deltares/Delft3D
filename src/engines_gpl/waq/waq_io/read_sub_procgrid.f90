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
module m_read_sub_procgrid
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains


    subroutine read_sub_procgrid(notot, syname, GridPs, isysg, status)

        !       Deltares Software Centre

        !>\file
        !>            read the SUBSTANCE_PROCESSGRID information and update the isysg array
        !>
        !>            several input possibilities exist:
        !>            - ALL indicates that all substances should work on the grid that will be mentioned
        !>            - a series of substances IDs indicating that those will work on the mentioned grid
        !>            then a grid name is required, to specify the grid where the substances work on.

        !     Created            : Somewhere 2003 by Jan van Beek as layerd bed special
        !     Modified           : May       2011 by Leo Postma   merge with standard version

        !     global declarations

        use dlwqgrid_mod
        use rd_token         !   for the reading of tokens
        use timers       !   performance timers

        implicit none

        !     declaration of arguments

        integer(kind = int_wp), intent(in) :: notot          !< nr of substances
        character(20), intent(in) :: syname(notot) !< substance names
        type(GridPointerColl), intent(in) :: GridPs        !< collection of all grid definitions
        integer(kind = int_wp), intent(inout) :: isysg (notot)  !< process gridnr of substances
        type(error_status), intent(inout) :: status !< current error status

        !     local declarations

        integer(kind = int_wp) :: itoken            ! integer token from input
        integer(kind = int_wp) :: idummy            ! dummy which content is not used
        real(kind = real_wp) :: adummy            ! dummy which content is not used
        character(len = 255) :: ctoken           ! character token from input
        character :: cdummy           ! dummy which content is not used
        integer(kind = int_wp) :: itype             ! type of input to be needded
        integer(kind = int_wp) :: ierr2             ! local error indication
        integer(kind = int_wp) :: sysused(notot)    ! work array substance selection
        integer(kind = int_wp) :: isys              ! index substance
        integer(kind = int_wp) :: i_grid            ! index grid in collection
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_sub_procgrid", ithndl)

        !     some init

        sysused = 0
        write (lunut, 2000)

        !     read input

        do

            if (gettoken(ctoken, ierr2) > 0) goto 1000

            select case (ctoken)

            case ('ALL')                                          ! use all substances
                sysused = 1
                write (lunut, 2030)

            case default
                isys = index_in_array(ctoken(:20), syname)       ! use this substance
                if (isys > 0) then
                    sysused(isys) = 1
                    write (lunut, 2040) syname(isys)
                else
                    i_grid = gridpointercollfind(GridPs, ctoken)
                    if (i_grid > 0) then                       ! use this grid, input is ready
                        write (lunut, 2050) trim(ctoken)
                        exit
                    else                                            ! unrecognised token
                        write (lunut, 2020) trim(ctoken)
                        goto 1000
                    endif
                endif

            end select

        enddo

        !     update the isysg array for all substances used in this block

        do isys = 1, notot
            if (sysused(isys) == 1) isysg(isys) = i_grid
        enddo

        if (timon) call timstop(ithndl)
        return

        1000 write (lunut, 2010)
        call status%increase_error_count()

        if (timon) call timstop(ithndl)
        return

        2000 format (/' Reading SUBSTANCE_PROCESSGRID information:')
        2010 format (' ERROR, reading SUBSTANCE_PROCESSGRID information.')
        2020 format (' ERROR, unrecognized token: ', A)
        2030 format (' Processgrid will be used for ALL substances')
        2040 format (' Processgrid will be used for substance: ', A)
        2050 format (' Processgrid for these substances is: ', A)

    end

end module m_read_sub_procgrid
