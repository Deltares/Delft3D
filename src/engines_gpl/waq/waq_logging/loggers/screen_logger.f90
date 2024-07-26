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

!> module containing logic related to the screen_logger class
module m_screen_logger
    use m_waq_precision
    use m_log_level
    use m_base_logger
    use m_logger

    implicit none

    private
    public :: screen_logger

    !> Type to handle logging to the screen
    type, extends(base_logger) :: screen_logger
    contains
        procedure :: write_message
    end type screen_logger

contains

    !> writes the message to screen
    subroutine write_message(this, message)
        class(screen_logger), intent(in) :: this  !< instance of this logger
        character(len=*), intent(in) :: message   !< message to log

        write (*, FMT='(A)') message
    end subroutine write_message

end module m_screen_logger
