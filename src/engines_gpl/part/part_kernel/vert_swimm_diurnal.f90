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

module vert_swimm_diurnal_mod
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
    subroutine vert_swimm_diurnal (   lunrep        , daytime  , k       , nolay   , stick_to_bottom ,    &
                                      dive_at_night , ipart    , wsettl  , kpart   , zpart           ,    &
                                      buoy          , vzact    , v_swim  , d_swim  )

        ! function  : Based on the day or night the particles will move downwards toward the bottom
        !             or move upwards toward the surface.
        !             Based on whether stick_to_bottom is set tot TRUE the particles will stick to the bottom
        !             when reached and remain unmovable untill the diurnal state has changed.
        !
        !

        ! arguments :
        integer(int_wp ), intent(in)    :: lunrep              ! report file
        integer(int_wp ), intent(in)    :: nolay               ! number of layers in calculation

        integer(int_wp ), pointer       :: kpart ( : )         ! third grid index of the particles
        real   (sp), pointer       :: zpart ( : )         ! z-value (0.0-1.0) third  direction within grid cell
        real   (sp), pointer       :: wsettl( : )         ! settling per particle
        real   (sp), pointer       :: angle ( : )         ! angle with horizontal

        real   (sp), pointer       :: v_swim( : )         ! horizontal swimming velocity m/s
        real   (sp), pointer       :: d_swim( : )         ! horizontal swimming direction (degree)

        real   (sp)                :: vzact               ! vzact
        real   (sp)                :: buoy                ! buoy

        ! local :
        integer(int_wp )                :: ipart               ! particle index
        integer                    :: k                   ! k

        logical                    :: daytime             ! true if it is daytime, false in night

        logical                    :: stick_to_bottom     ! stick to bottom when reached
        logical                    :: dive_at_night       ! dive during the night, if untrue dive during the day


        if(daytime) then

            ! it is day time
            if(dive_at_night) then

                if(nolay + 1 .eq. kpart(ipart)) then
                     !Get out of the layer
                      wsettl(ipart) = buoy + vzact                         ! Particle swims upwards
                      kpart(ipart)  = nolay                                ! Particle is placed in the storage layer
                      zpart(ipart)  = 0.5                                  ! Particle is positioned in the middle of the cell in the third dimension

                 else
                      ! swim upwards
                      wsettl(ipart) = buoy + vzact                         ! Particle swims upwards

                 endif

            else

                if ( k .ge. nolay  ) then                                !If the third dimension position of the particle is greater or equal to the number of layers
                   if ( stick_to_bottom ) then                           !If the particle should stick to the bottom

                        ! settle on bed if arrived in lowest layer
                        wsettl(ipart) = 0.0                               ! Particle stays at verticale position 0.0
                        kpart(ipart)  = nolay + 1                         ! Particle is placed in the storage layer
                        zpart(ipart)  = 0.5                               ! Particle is positioned in the middle of the cell in the third dimension
                        v_swim(ipart) = 0.0                              ! The swimming velocity is set to 0.0

                    else

                        ! keep swimming in the lowest layer
                        wsettl(ipart) = 0.0                               ! Particle stays at verticale position 0.0

                    endif
                else
                    ! swim downwards
                    wsettl(ipart) = buoy - vzact                                     ! Particle swims downwards

                endif

            endif
        else
            ! it is night time
            if(dive_at_night) then

                if ( k .ge. nolay  ) then                                !If the third dimension position of the particle is greater or equal to the number of layers
                   if ( stick_to_bottom ) then                           !If the particle should stick to the bottom

                        ! settle on bed if arrived in lowest layer
                        wsettl(ipart) = 0.0                              ! Particle stays at verticale position 0.0
                        kpart(ipart) = nolay + 1                         ! Particle is placed in the storage layer
                        zpart(ipart) = 0.5                               ! Particle is positioned in the middle of the cell in the third dimension
                        v_swim(ipart) = 0.0                              ! The swimming velocity is set to 0.0

                    else

                        ! keep swimming in the lowest layer
                        wsettl(ipart) = 0.0                              ! Particle stays at verticale position 0.0

                    endif
                else
                    ! swim downwards
                    wsettl(ipart) = buoy - vzact                         ! Particle swims downwards

                endif
            else
                 if(nolay + 1 .eq. kpart(ipart)) then
                     !Get out of the layer
                      wsettl(ipart) = buoy + vzact                        ! Particle swims upwards
                      kpart(ipart) = nolay                                ! Particle is placed in the storage layer
                      zpart(ipart) = 0.5                                  ! Particle is positioned in the middle of the cell in the third dimension


                 else
                      ! swim upwards
                       wsettl(ipart) = buoy + vzact                        ! Particle swims upwards

                 endif
            endif

        endif

    !Result:
    !Return the setting velocity (vertical swimming velocity) and the swimming velocity
    !and whether particles are sticking to the bottom



    return                                                                     	   !Return from the subroutine
    end subroutine
end module
