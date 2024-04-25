!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

module delwaq_loads
    implicit none

    private
    public :: wasteload, IDLEN, wasteloads

    integer, parameter :: IDLEN = 20               !! length ID strings
    integer, parameter :: NAMELEN = 40             !! length NAME strings

    type identifier
        character(len = IDLEN) :: id              !! identification string
        character(len = NAMELEN) :: name          !! descriptive name
        character(len = IDLEN) :: type            !! type identification
    end type identifier

    type location
        integer :: segnr           !! segment number
    end type location

    type wasteload
        type(identifier) :: id                ! id and name etc.
        type(location) :: loc                  ! location
        real :: flow              ! the actual value discharge flow
        real, allocatable :: set_factor(:)     ! the factors by which to multiply the flow rate and concentrations (D-RTC)
        real, pointer :: loads(:)          ! the actual value of the load for the substances
    end type wasteload

    type(wasteload), allocatable, save :: wasteloads(:)

end module delwaq_loads
