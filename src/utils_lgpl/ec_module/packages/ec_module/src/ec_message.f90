!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.


!> This module contains the messaging system for the EC-module.
module m_ec_message
   use precision

   implicit none(type, external)

   private

   public :: clear_ec_message
   public :: set_ec_message
   public :: dump_ec_message_stack

   integer, parameter, public :: MAXIMUM_EC_MESSAGE_LENGTH = 1000

   integer, parameter, public :: MSG_TYPE_ALL   = 0
   integer, parameter, public :: MSG_TYPE_DEBUG = 1
   integer, parameter, public :: MSG_TYPE_INFO  = 2
   integer, parameter, public :: MSG_TYPE_WARN  = 3
   integer, parameter, public :: MSG_TYPE_ERROR = 4
   integer, parameter, public :: MSG_TYPE_FATAL = 5
   integer, parameter, public :: MSG_TYPE_NONE  = 6

   !> type holding one message
   type t_EcMessage
      character(len=:), allocatable :: message                !< The actual message
      integer                       :: message_type           !< one of MSG_TYPE_ALL ... MSG_TYPE_NONE; but not yet used
      type (t_EcMessage), pointer   :: next_message => null() !< pointer to next message in the list of messages
   end type t_EcMessage

   type(t_EcMessage), pointer :: ec_message_list => null() !< list of messages

   interface set_ec_message
      module procedure set_ec_message_char
      module procedure set_ec_message_int
   end interface

   contains

      !> clear the message stack
      subroutine clear_ec_message()
         ! Local variables
         type (t_EcMessage), pointer :: current_message  !< local pointer to a message on the message stack
         type (t_EcMessage), pointer :: next_message !< idem

         current_message => ec_message_list
         do while (associated(current_message))
            next_message => current_message%next_message
            deallocate(current_message)
            current_message => next_message
         end do

         ec_message_list => null()

      end subroutine clear_ec_message

      !> add message to message stack
      subroutine set_ec_message_char(string, suffix)
         ! Parameters
         character(len=*), intent(in)           :: string  !< message to be added to message stack
         character(len=*), intent(in), optional :: suffix  !< suffix of message
         
         ! Local variables
         integer :: ierr
         type (t_EcMessage), pointer :: new_message

         allocate (new_message, stat=ierr)

         if (ierr /= 0) then
            write(*,*) 'Internal error in message stack.'
            write(*,*) 'message: ', string
         else
            new_message%next_message => ec_message_list
            ec_message_list => new_message

            if (present(suffix)) then
               new_message%message = trim(string)  // " " // suffix
            else
               new_message%message = trim(string)
            end if
            new_message%message_type = -1
         end if

      end subroutine set_ec_message_char

      !> add message to message stack
      subroutine set_ec_message_int(string, value)
         ! Parameters
         character(len=*), intent(in) :: string !< message to be added to message stack
         integer,          intent(in) :: value  !< number to be added to message

         ! Local variables
         character(len=8) :: value_string

         write(value_string, '(i8)') value

         call set_ec_message(trim(adjustl(string)) // ' ' // trim(value_string))

      end subroutine set_ec_message_int

      !> dump all messages of the stack using a user supplied messenger function
      function dump_ec_message_stack(message_level, messenger) result(return_value)
         ! Parameters
         integer, intent(in)                 :: message_level  !< message level; not used yet
         interface
            subroutine messenger(level, message)
            integer, intent(in)              :: level
            character(len=*), intent(in)     :: message
            end subroutine
         end interface

         ! Local variables
         character(len=32)          :: return_value !< function result
         type(t_EcMessage), pointer :: my_message !< local pointer to one of the messages in the stack

         my_message => ec_message_list

         if ( associated(my_message) ) then
            call messenger(message_level, "...")! separator
         end if
         
         ! loop over all messages in the stack
         do while (associated(my_message))
            call messenger(message_level, my_message%message)
            my_message => my_message%next_message
         end do

         call clear_ec_message()

         return_value = 'Fatal EC-error !!' ! TODO: make this a meaningful return string

      end function dump_ec_message_stack

end module m_ec_message
