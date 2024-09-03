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

    !!  Module to help with the warning messages issued by PROTIST

module m_protistlog
    use m_logger_helper

    implicit none

    integer, private            :: message_counter = 0
    integer, private, parameter :: max_counter     = 100

contains

    !> Write and count the warning
    !! But only actually write it if there are less than max_counter messages already
    subroutine write_warning( string, cell )
        character(len=*), intent(in)   :: string     !< String to written
        integer, intent(in)            :: cell       !< Index of the cell (segment) that is involved

        character(len=len(string)+10)  :: string_out

        message_counter = message_counter + 1

        if ( message_counter <= max_counter ) then
            write( string_out, '(a,x,i0)' ) string, cell

            call write_log_message( string_out )

            if ( message_counter == max_counter ) then
                call write_log_message( "Maximum number of messages from PROTIST reached" )
            endif
        endif
    end subroutine write_warning

end module m_protistLog
