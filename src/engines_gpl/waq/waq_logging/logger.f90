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

!> module containing the definition of the logger interface
module m_logger
    use m_waq_precision
    use m_log_level

    implicit none

    private
    public :: logger

    !> Interface for logging messages
    type, abstract :: logger
        integer(kind=int_wp) :: log_level = DEBUG_LEVEL  !< level of logging (0 is no logging)

    contains
        procedure(log_message_interface), deferred :: log_error, log_warning, log_info, log_debug

    end type logger

    abstract interface

        !> signature/interface of the log_.. procedures
        subroutine log_message_interface(this, message, new_line)
            import logger
            class(logger), intent(in) :: this         !< instance of this logger
            character(len=*), intent(in) :: message   !< path of file to log towards
            logical, optional, intent(in) :: new_line !< add a new line before message
        end subroutine
    end interface

end module m_logger
