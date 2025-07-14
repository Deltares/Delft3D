!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

! actions.inc --
!     Parameters defining the actions for DELWAQ in DLL form
!
!     I have also thought about 'step' or 'phase' instead of 'action'
!     but as long as a fullcomputation is also part of this list,
!     these other names don't make sense. One option would be to
!     have each phase correspond to a specific bit and to have all
!     bits set in a fullcomputation, but a fullcomputation is more
!     than initialisation followed by a single step and finalisation.
!     'task' may be a better alternative for action
!
module m_actions
    integer, parameter :: ACTION_INITIALISATION = 1
    integer, parameter :: ACTION_FINALISATION = 2
    integer, parameter :: ACTION_SINGLESTEP = 3
    integer, parameter :: ACTION_FULLCOMPUTATION = 4
end module m_actions

