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

module m_waq_data_buffer

    implicit none

    type, public :: waq_data_buffer
        integer, dimension(:),allocatable              :: ibuf
        real, dimension(:),allocatable                 :: rbuf
        character(len=1), dimension(:),allocatable     :: chbuf

      contains
        procedure :: intialize => intialize_buffer
        final :: destruct_buffer

    end type waq_data_buffer

    contains
    subroutine intialize_buffer(this)
        class(waq_data_buffer), intent(out) :: this

        allocate( this%rbuf(0) )
        allocate( this%ibuf(0) )
        allocate( this%chbuf(0) )

    end subroutine intialize_buffer

    subroutine destruct_buffer(this)
        type(waq_data_buffer) :: this
        
        if (allocated(this%ibuf)) deallocate(this%ibuf)
        if (allocated(this%rbuf)) deallocate(this%rbuf)
        if (allocated(this%chbuf)) deallocate(this%chbuf)

    end subroutine destruct_buffer

end module m_waq_data_buffer