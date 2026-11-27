    !!  Copyright (C)  Stichting Deltares, 2012-2026.
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
    use ieee_arithmetic

    implicit none

    integer, private            :: message_counter = 0
    integer, private, parameter :: max_counter     = 100

contains

    !> Write and count the warning
    !! But only actually write it if there are less than max_counter messages already
    !
    ! Note:
    ! With very large models the counter may exceed the maximum positive value for integers,
    ! causing a wrap-around. So only increase it when the maximum has not been reached yet.
    !
    subroutine write_warning( string, cell )
        character(len=*), intent(in)   :: string     !< String to written
        integer, intent(in)            :: cell       !< Index of the cell (segment) that is involved

        character(len=len(string)+10)  :: string_out

        if ( message_counter <= max_counter ) then
            message_counter = message_counter + 1

            write( string_out, '(a,x,i0)' ) string, cell

            call write_log_message( string_out )

            if ( message_counter == max_counter ) then
                call write_log_message( "Maximum number of messages from PROTIST reached" )
            endif
        endif
    end subroutine write_warning

    subroutine check_value( module, segment, value, name )
        character(len=*), intent(in) :: module
        integer, intent(in)          :: segment
        real, intent(in)             :: value
        character(len=*), intent(in) :: name

        logical, save                :: first = .true.
        integer, save                :: luchk
        real                         :: rnd
        character(len=40)            :: filename

        if ( first ) then
            first = .false.
            call random_init( .true., .true. )
            call random_number( rnd )
            write( filename, '(a,i10,a)' ) 'protist_', int( 1.0e9 * rnd ), '.out'
            open( newunit = luchk, file = filename )
        endif

        if ( value < 0.0 .or. value > 1.0e5 ) then
            write( luchk, * ) trim(module), segment, ': ', trim(name), value
        elseif ( .not. ieee_is_finite(value) ) then
            write( luchk, * ) trim(module), segment, ': ', trim(name), value, ' -- not finite!'
        endif
    end subroutine check_value

end module m_protistLog
