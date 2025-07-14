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

module partition_arrays
    use m_logger_helper, only : stop_with_error


    !     Deltares Software Centre

    !>\file utilities to partition a large array into smaller pieces
    !>
    !>    The module mimicks the old FMM library, but since the actual
    !>    allocation occurs within the SPACE routine, we only need
    !>    to compute the pointer into the overall array.
    !>
    !>    As this will be called in a multithreaded context, avoid
    !>    any local, saved, variables. This is done via the
    !>    type(memory_partition) argument.
    !>
    !>    Note that the use of default initialisation eliminates the
    !>    need to explicitly call an initialisation routine.
    !>

    !     Created : March    2011 by Arjen Markus

    implicit none

    type memory_partition            !< Private variables for memory partition
        integer :: jpoint = 1
        integer :: apoint = 1
        integer :: cpoint = 1
    end type memory_partition

    ! Taken from fsm-fix.i
    integer, parameter :: ityp = 1
    integer, parameter :: rtyp = 2
    integer, parameter :: dtyp = 3
    integer, parameter :: dctyp = 4
    integer, parameter :: ctyp = 5
    integer, parameter :: ltyp = 6
    integer, parameter :: chtyp = 7

contains

    !
    ! Reimplementation of the FSM routine
    !
    integer function makptr(part, arrnam, vtype, number)

        type(memory_partition) :: part
        character(len = *) :: arrnam
        integer :: vtype
        integer :: number

        select case (vtype)
        case (ityp)
            makptr = part%jpoint
            part%jpoint = part%jpoint + max(number, 1)
        case (rtyp)
            makptr = part%apoint
            part%apoint = part%apoint + max(number, 1)
        case (chtyp)
            makptr = part%cpoint
            part%cpoint = part%cpoint + max(number, 1)
        case default
            write(*, *) &
                    'Fatal error in MAKPTR: variable type not implemented: ', &
                    vtype
            call stop_with_error()
        end select

    end function makptr

end module partition_arrays
