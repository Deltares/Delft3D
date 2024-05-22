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
program delwaq2_two_instances
   !! Run two instances of the same computation in parallel.
   !! One has a timestep that is half that of the other.

use delwaq2_data
use m_actions
use m_delwaq2_main

implicit none

character(len=100), dimension(1) :: argv
integer                          :: argc
integer                          :: action
integer                          :: i
integer                          :: status

type(delwaq_data), dimension(2)  :: dlwqd

! Turn off the timers
dlwqd%set_timer = .true.
! dlwqd%islibrary = .true.

! Initialise both instances

action = ACTION_INITIALISATION
call dlwqmain(action, dlwqd(1))
call dlwqmain(action, dlwqd(2))

! Set the timestep directly for the second instance
!dlwqd(2)%in(3) = dlwqd(2)%in(3) / 2

! Set a few steps
action = ACTION_SINGLESTEP
do i = 1, 2000
   call dlwqmain(action, dlwqd(1))

   call dlwqmain(action, dlwqd(2))
end do

! Finish the computation
action = ACTION_FINALISATION
call dlwqmain(action, dlwqd(1))
call dlwqmain(action, dlwqd(2))

end program delwaq2_two_instances
