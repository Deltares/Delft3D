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

module intpltd_divelimit_mod
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
    subroutine intpltd_divelimit ( lunrep, idelt, ipart, zlevel , zdepth, wsettl )

        ! function  : Maximum dive depth is function of bathymetry. Offshore at great depths jellyfish will dive up tp a maximum of 500 m
        !             This depth will decrease as they go into shallower water.
        !

        ! arguments :
        integer(int_wp ), intent(in)    :: lunrep                  ! report file
        integer(int_wp )                :: idelt                   ! timesteps in seconds
        integer(int_wp )                :: ipart                   ! particle index

        real   (sp), pointer        :: wsettl( : )            ! settling per particle

        real   (sp)                :: zdepth                  ! z relative to water surface
        real   (sp)                :: zlevel                  ! z relative to bottom
        real   (sp)                :: totdep                  ! z water surface to bottom
        real   (sp), pointer       :: mdxData(:), mdyData(:)
        real   (sp)                :: maxdivedep              ! maximum diving depth for the totdep bathymertie

        !Setup the data for max diving depth
        allocate(mdxData(8))
        mdxData = (/ -5000.0, -1600.0, -1000.0, -900.0, -300.0, -100.0, -50.0, 0.0 /)
        allocate(mdyData(8))
        mdyData = (/ -500.0, -400.0, -350.0, -300.0, -100.0, -25.0, -12.0 , 0.0 /)

        totdep = zdepth + zlevel

        !interpolate
        call intpltd_function(lunrep, mdxData, mdyData, - real(totdep), maxdivedep)

        !apply max dive dep
        if(-maxdivedep .gt. (zdepth + (wsettl(ipart)*idelt))) then
            !wsettl remains the same
            continue
        else
            wsettl(ipart) = ((-maxdivedep) - zdepth )/idelt   ! swim up if to deep and down if not on maxdepth
        endif

    return                                                                     	   !Return from the subroutine
    end subroutine
end module
