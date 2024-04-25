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

module intpltd_motility_mod
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
    subroutine intpltd_motility ( lunrep, n , m , k, nosegl, lgrid , vzact, temper1 )

        ! function  : Based on the time of the day the particles will move upwards during the day and
        !             down during the night. With an interpolation fucntion the difference in daytime
        !             over the year has been incorperated.

        ! arguments :
        integer(int_wp ), intent(in)    :: lunrep              ! report file
        integer(int_wp ), intent(in)    :: nosegl              ! number segments per layer
        integer(int_wp )                :: lgrid ( : , : )     ! grid with active grid numbers, negatives for open boundaries
        integer                    :: m                   ! m
        integer                    :: n                   ! n
        integer                    :: k                   ! k
        integer                    :: iseg                ! iseg
        integer                    :: isegl               ! isegl
        real   (sp), pointer       :: temper1( : )        ! temperature segment numbering
        real                       :: temp_n0
        real   (sp)                :: vzact               ! vzact


        real   (sp), pointer       :: mtxData(:), mtyData(:)
        real   (sp)                :: motility


        !Temperature of location gridcell

        iseg  = lgrid (n,m)                                                      ! Get the gridnumbering from the active grid in the middle of particle position
        if(iseg .le. 0) return                                                   ! Stop execution if particle has left the model

        isegl  = iseg + (k-1)*nosegl     !Segment number 3d of the particle(segment number + current layer * segments per layer)

        temp_n0     = temper1(isegl)     !Temperature of particle segment 3d numbering

        !Setup the data for sunrise and subset
        allocate(mtxData(6))
        mtxData = (/ 4.5, 13.5, 17.0, 20.0, 23.0, 23.1 /)
        allocate(mtyData(6))
        mtyData = (/ 0.000, 0.500, 1.000, 0.280, 0.085, 0.000 /)

        !interpolate
        call intpltd_function(lunrep, mtxData, mtyData, real(temp_n0), motility)

        !apply motility factor
        vzact = vzact * motility

    return                                                                     	   !Return from the subroutine
    end subroutine
end module
