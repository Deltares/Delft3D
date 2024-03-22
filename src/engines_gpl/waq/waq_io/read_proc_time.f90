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
module m_read_proc_time
    use m_waq_precision
    use m_string_utils
    use m_error_status

    implicit none

contains


    subroutine read_proc_time(notot, syname, isyst, status)

        !       Deltares Software Centre

        !>\file
        !>         read the PROCESS_TIMESTEP_MULTIPLIER information update the isyst array
        !>
        !>            several input possibilities exist:
        !>            - ALL indicates that all substances should work with this time multiplier
        !>            - a series of substances IDs indicating that those will work with the multiplier
        !>            then the time multiplier to be used is required

        !     Created            : Somewhere 2003 by Jan van Beek as layerd bed special
        !     Modified           : May       2011 by Leo Postma   merge with standard version

        !     global declarations

        use rd_token         !   for the reading of tokens
        use timers       !   performance timers

        implicit none

        !     declaration of arguments

        integer(kind = int_wp), intent(in) :: notot           !< nr of substances
        character(20), intent(in) :: syname(notot)  !< substance names
        integer(kind = int_wp), intent(inout) :: isyst (notot)   !< process timestep multiplier
        type(error_status), intent(inout) :: status !< current error status

        !     local declarations

        character(len = 255) :: ctoken           ! character token from input
        integer(kind = int_wp) :: itype             ! type of input that was provided
        integer(kind = int_wp) :: ierr2             ! local error indication
        integer(kind = int_wp) :: sysused(notot)    ! work array substance selection
        integer(kind = int_wp) :: isys              ! index substance
        integer(kind = int_wp) :: idtmult           ! timestep multiplier
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("read_proc_time", ithndl)

        !     some init

        sysused = 0
        write (lunut, 2000)

        !     read input

        do

            if (gettoken(ctoken, idtmult, itype, ierr2) > 0) goto 1000

            if (itype == 1) then

                select case (ctoken)

                case ('ALL')                                    ! use all substances
                    sysused = 1
                    write (lunut, 2030)

                case default
                    isys = index_in_array(ctoken(1:20), syname(:notot))
                    if (isys > 0) then                      ! use this substance
                        sysused(isys) = 1
                        write (lunut, 2040) syname(isys)
                    else                                         ! unrecognised token
                        write (lunut, 2020) trim(ctoken)
                        goto 1000
                    endif

                end select

            else                                                  ! time multiplier read

                write (lunut, 2050) idtmult
                exit

            endif

        enddo

        !     update the isyst array for all substances used in this block

        do isys = 1, notot
            if (sysused(isys) == 1) isyst(isys) = idtmult
        enddo

        if (timon) call timstop(ithndl)
        return

        1000 continue
        write(lunut, 2010)
        call status%increase_error_count()

        if (timon) call timstop(ithndl)
        return

        2000 format (/' Reading PROCESS_TIMESTEP_MULTIPLIER information:')
        2010 format (' ERROR, reading PROCESS_TIMESTEP_MULTIPLIER information.')
        2020 format (' ERROR, unrecognized token: ', A)
        2030 format (' Timestep will be used for ALL substances')
        2040 format (' Timestep will be used for substance: ', A)
        2050 format (' Timestep multiplier for these substances is: ', I10)

    end

end module m_read_proc_time
