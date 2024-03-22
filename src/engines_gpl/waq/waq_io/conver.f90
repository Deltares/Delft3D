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
module m_conver
    use m_waq_precision

    implicit none

contains


    subroutine conver (ibrk, nobrk, ifact, dtflg, dtflg3)

        !     Deltares Software Centre

        !>\File
        !>        Conversion of a relative integer DDHHMMSS or YYDDDHH time to seconds
        !>
        !>        DDDDHHMMSS allows up to 2146 days or some 5.8 years\n
        !>        YYDDDHH allows more than needed (NB: a year is 365 days !)\n
        !>        If date is false, result is multiplied by ifact.\n
        !>        Absolute offset time should be chosen such that all relative
        !>        times fit into 68 years from the absolute offset !


        !     Logical units : none

        use timers       !   performance timers

        implicit none

        !     Parameters

        !     kind           function         name             Descriptipon

        integer(kind = int_wp), intent(in) :: nobrk           !< number of breakpoints
        integer(kind = int_wp), intent(inout) :: ibrk (nobrk)    !< breakpoint to convert
        integer(kind = int_wp), intent(in) :: ifact           !< factor between time scales
        logical, intent(in) :: dtflg          !< if true then 'date'-format
        logical, intent(in) :: dtflg3         !< if true then YYDDDHH

        !     Local

        integer(kind = int_wp) :: isec     ! seconds
        integer(kind = int_wp) :: imin     ! minutes
        integer(kind = int_wp) :: ihour    ! hours
        integer(kind = int_wp) :: iday     ! days
        integer(kind = int_wp) :: iyear    ! years
        integer(kind = int_wp) :: i        ! loop counter
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("conver", ithndl)

        if (dtflg) then
            if (dtflg3) then
                do i = 1, nobrk
                    ihour = mod (ibrk(i), 100)
                    iday = mod (int(ibrk(i) / 100), 1000)
                    iyear = int(ibrk(i) / 100000)
                    ibrk(i) = 3600 * ihour + 86400 * iday + 31536000 * iyear
                enddo
            else
                do i = 1, nobrk
                    isec = mod (ibrk(i), 100)
                    imin = mod (int(ibrk(i) / 100), 100)
                    ihour = mod (int(ibrk(i) / 10000), 100)
                    iday = int (int(ibrk(i) / 1000000))
                    ibrk(i) = isec + 60 * imin + 3600 * ihour + 86400 * iday
                enddo
            endif
        else
            if (ifact /= 1) then
                do i = 1, nobrk
                    ibrk(i) = ifact * ibrk(i)
                enddo
            endif
        endif

        if (timon) call timstop(ithndl)
        return
    end

end module m_conver
