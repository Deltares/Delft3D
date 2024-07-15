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

module intpltd_stagedev_mod
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
    subroutine intpltd_stagedev (lunrep, stemp, dev_factor)

        ! function  : Based on the encoutered temperature a stage development reduction factor
        !             will be given.
        !

        ! arguments :
        integer(int_wp), intent(in) :: lunrep              ! report file
        real   (sp) :: stemp

        real   (sp), pointer :: sdxData(:), sdyData(:)
        real   (sp) :: dev_factor

        !Setup the data for stagedev
        allocate(sdxData(6))
        sdxData = (/ 4.5, 13.5, 17.0, 19.0, 20.0, 25.0 /)
        allocate(sdyData(6))
        sdyData = (/ 0.00, 0.40, 1.00, 0.80, 0.75, 0.00 /)

        !interpolate
        call intpltd_function(lunrep, sdxData, sdyData, real(stemp), dev_factor)

        return                                                                           !Return from the subroutine
    end subroutine
end module
