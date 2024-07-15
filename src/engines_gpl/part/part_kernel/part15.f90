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
module m_part15

    implicit none

contains


    subroutine part15 (lunpr, itime, spawnd, num_cells, nowind, &
            iwndtm, wveloa, wdira, wvelo, wdir)

        !       Deltares Software Centre

        !>\file
        !>         interpolation for wind speed/direction in the wind table
        !>
        !>         Issue was correct dealing with the 360 degrees break of the direction:\n
        !>         Problem solved with discontinuity at the North direction
        !>         Note: use MODULO (not: MOD) function here, as this F90 function complies
        !>               with the mathematical definition.\n
        !>         See also: http://issues.deltares.nl/browse/DELFT3D-3966

        !     Created               : August 1991 by Marcel Zeeuw

        !     Modifies              : July   2011 by Leo Postma : cosmetics

        !     logical unit numbers  : lunpr: standard output report file

        !     subroutines called    : none.

        !     functions   called    :

        use m_stop_exit
        use m_waq_precision    ! single/double precision
        use timers
        use m_waq_timespace    ! meteo module ?

        implicit none

        !     Arguments

        !     kind            function         name                    description

        integer, intent(in) :: lunpr                 !< unit nr output file
        integer, intent(in) :: itime                 !< actual time
        logical, intent(in) :: spawnd                !< if true space varying wind
        integer, intent(in) :: num_cells                 !< size of the array with winds
        integer, intent(in) :: nowind                !< number of time breakpoints
        integer, intent(in) :: iwndtm(nowind)        !< time breakpoint values
        real     (4), intent(in) :: wveloa(nowind)        !< time series of wind velocity
        real     (4), intent(in) :: wdira (nowind)        !< time series of wind direction
        real     (8), intent(out) :: wvelo (num_cells)        !< wind velocity at this time
        real     (8), intent(out) :: wdir  (num_cells)        !< wind direction at this time

        !     locals

        integer(int_wp) :: id              ! loop counter time values
        real   (real_wp) :: fract           ! interpolation factor
        real   (sp) :: w1, w2, diff    ! help variables 360 degrees turn
        logical        yes1, yes2      ! to identify successful windmodule call
        real   (sp)    avelo, adir     ! help variables from x,y to magnitude, direction
        real   (8)     time            ! time in minutes
        integer        idum            !

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part15", ithndl)

        if (spawnd) then
            time = itime / 60.0D+00
            idum = 1
            yes1 = GetTimeSpaceValue (idum, "windx", time, wvelo)
            idum = 1
            yes2 = GetTimeSpaceValue (idum, "windy", time, wdir)
            if (.not. yes1 .or. .not. yes2) then
                write (lunpr, *) " Error in meteomodule at time:", itime
                write (*, *) " Error in meteomodule at time:", itime
                call stop_exit(1)
            endif
            do id = 1, num_cells
                avelo = wvelo(id) * wvelo(id) + wdir(id) * wdir(id)
                if (avelo < 1.0d-30) then
                    wvelo(id) = 0.0
                    wdir (id) = 0.0
                else
                    avelo = sqrt(avelo)
                    if (abs(wvelo(id)) < 1.0d-30) then
                        wvelo(id) = avelo
                        if (wdir(id) > 0) then
                            wdir (id) = 180.0
                        else
                            wdir (id) = 0.0
                        endif
                    else
                        adir = 270.0 - atan(wdir(id) / wvelo(id)) / 3.14159265 * 180.0
                        ! angle positive from North gives between 180.0 to N
                        ! and 360.0 to S through 270 for wind to E (from W)
                        if (wvelo(id) < 0.0) adir = adir - 180.0    ! x < 0 so angle between 0.0 and 180.0
                        wvelo(id) = avelo
                        wdir (id) = adir
                    endif
                endif
            enddo
        else
            if (nowind <= 0) then
                wvelo = 0.0
                wdir = 0.0
            else

                !           find the moment

                do id = 2, nowind
                    if (itime < iwndtm(id) .or. id == nowind) exit
                enddo

                !           determine interpolation factor

                fract = real(itime - iwndtm(id - 1)) / real(iwndtm(id) - iwndtm(id - 1))

                !           interpolate

                wvelo = wveloa(id - 1) + fract * (wveloa(id) - wveloa(id - 1))
                w1 = wdira (id - 1)
                w2 = wdira (id)
                diff = w2 - w1
                if (abs(diff) > 180.0) w2 = w2 - sign(1., diff) * 360.0 ! correct 360 degree jump
                wdir = modulo(w1 + fract * (w2 - w1), 360.0)

                !           write result to standard output

                write (lunpr, '(//a,f14.2,a/a,f14.2,a/)')  &
                        '      Wind direction (wrt North) : ', wdir (1), ' degrees', &
                        '      Wind speed                 : ', wvelo(1), ' m/s'
            endif
        endif

        !     end of subroutine

        if (timon) call timstop (ithndl)
        return
        !
    end subroutine

end module m_part15
