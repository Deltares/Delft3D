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

!> Class for managing a connection_data collection
module m_connection_manager
    use m_waq_precision
    use m_connection_data

    implicit none

    private

    type, public :: connection_manager
        type(connection_data), dimension(:), allocatable, private :: connections !< stored connections (use get_connections to access)
    contains
        procedure, public :: get_connection_by_exchange_name, get_incoming_connections_by_category, add_connection
    end type

    type, public :: connection_wrapper
        type(connection_data), pointer :: connection_ptr
    end type connection_wrapper

contains

    !> Finds the connection that matches the with the provided key based on exchange name property
    !! Result: Index into the connection array or 0 if not found
    function get_connection_by_exchange_name(this, key_name) result(found_connection)
        class(connection_manager), intent(in), target :: this    !< instance of this connection_manager
        character(len=*), intent(in) :: key_name                 !< Connection key to find
        type(connection_data), pointer :: found_connection       !< Connection matching the key

        type(connection_data), dimension(:), pointer :: current_connections
        integer(kind=int_wp) :: index
        integer(kind=int_wp) :: i

        found_connection => null()

        if (.not. allocated(this%connections)) then
            return
        end if

        do i = 1, size(this%connections)
            if (this%connections(i)%exchange_name == key_name) then
                found_connection => this%connections(i)
                return
            end if
        end do
    end function get_connection_by_exchange_name

    !> Finds the incoming connections that have the provided category_type
    function get_incoming_connections_by_category(this, category_type) result(found_connections)
        class(connection_manager), intent(in), target :: this   !< instance of this connection_manager
        integer(kind=int_wp), intent(in) :: category_type       !< Name of the category to find
        type(connection_wrapper), dimension(:), allocatable :: found_connections !< incoming connections matching the key

        type(connection_data), pointer :: connection
        type(connection_wrapper) :: new_wrapper
        integer(kind=int_wp) :: i

        allocate (found_connections(0))

        if (.not. allocated(this%connections)) then
            return
        end if

        do i = 1, size(this%connections)
            connection => this%connections(i)
            if (connection%category == category_type .and. connection%incoming) then
                new_wrapper = connection_wrapper(connection)
                found_connections = [found_connections, new_wrapper]
            end if
        end do
    end function

    !< Adds a copy of the provided connecton to the list and returns the pointer to the new instance
    function add_connection(this, connection) result(new_connection)
        class(connection_manager), intent(inout), target :: this   !< instance of this connection_manager
        type(connection_data), intent(in) :: connection    !< connection to add
        type(connection_data), pointer :: new_connection   !< pointer to the new copy of the connection

        if (.not. allocated(this%connections)) then
            allocate (this%connections(0))
        end if

        this%connections = [this%connections, connection]
        new_connection => this%connections(size(this%connections))
    end function

end module m_connection_manager
