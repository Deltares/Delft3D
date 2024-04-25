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

module intpltd_diurnal_mod
!
!  data definition module(s)
!
use m_waq_precision          ! single/double precision
use timers
!
!  module procedure(s)
!
!
use intpltd_function_mod      ! explicit interface


implicit none

contains
    subroutine intpltd_diurnal ( lunrep, day, daytime)

        ! function  : Based on the time of the day the particles will move upwards during the night and
        !             down during the day. With an interpolation fucntion the difference in daytime
        !             over the year has been incorperated.

        ! arguments :
        integer(int_wp ), intent(in)     :: lunrep              ! report file
        integer(int_wp )                 :: ipart               ! particle index
        real   (sp), pointer        :: wsettl( : )         ! settling per particle
        real   (sp)                 :: day                 ! time in days (real)
        integer(int_wp )                 :: daynr               ! time in days (int)
        real   (sp)                 :: timeday             ! time on day as fraction(real)
        real   (sp)                 :: vz                  ! vz
        real   (sp)                 :: vzact               ! vzact
        real   (sp)                 :: buoy                ! buoy

        real   (sp), pointer        :: srxData(:), sryData(:), ssxData(:), ssyData(:)
        real   (sp)                 :: sunrise, sunset

        logical                     :: daytime             ! true if it is daytime, false in night

        !get daynr and timeday
        daynr = int(floor(day))
        timeday = day - real(daynr)
        if(daynr .gt. int(366)) then
            daynr = daynr - 366
        endif

        !Setup the data for sunrise and sunset
        allocate(srxData(5))
        srxData = (/ 0, 100, 150, 220, 366 /)
        allocate(sryData(5))
        sryData = (/ 8.5, 7.5, 7.5, 8.0, 8.5 /)

        allocate(ssxData(5))
        ssxData = (/ 0, 100, 150, 220, 366 /)
        allocate(ssyData(5))
        ssyData = (/ 21.0, 22.0, 22.0, 21.15, 21.0 /)

        !interpolate
        call intpltd_function(lunrep, srxData, sryData, real(daynr), sunrise)
        call intpltd_function(lunrep, ssxData, ssyData, real(daynr), sunset)

        !convert to fraction of day
        sunrise = sunrise / 24.0
        sunset = sunset / 24.0

        if((timeday .gt. (sunrise + 1.0/24.0)) .and. (timeday .lt. (sunset - 1/24.0))) then
            ! it is day time
            daytime = .true.
        else
            ! it is night time
            daytime = .false.
        endif

    return                                                                     	   !Return from the subroutine
    end subroutine
end module
