!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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

!  
!  

!> This module contains the messaging system.
!! @author arjen.markus@deltares.nl
!! @author adri.mourits@deltares.nl
!! @author stef.hummel@deltares.nl
!! @author edwin.spee@deltares.nl
module m_ec_message
   use precision

   implicit none

   private

   public :: clearECMessage
   public :: setECMessage
   public :: dumpECMessageStack

   integer, parameter, public :: maxMessageLen = 1000

   integer, parameter, public :: MSG_TYPE_ALL   = 0
   integer, parameter, public :: MSG_TYPE_DEBUG = 1
   integer, parameter, public :: MSG_TYPE_INFO  = 2
   integer, parameter, public :: MSG_TYPE_WARN  = 3
   integer, parameter, public :: MSG_TYPE_ERROR = 4
   integer, parameter, public :: MSG_TYPE_FATAL = 5
   integer, parameter, public :: MSG_TYPE_NONE  = 6

   !> type holding one message
   type TEcMessage
      character(len=:), allocatable :: message                !< The actual message
      integer                       :: message_type           !< one of MSG_TYPE_ALL ... MSG_TYPE_NONE; but not yet used
      type (TEcMessage), pointer    :: next_message => null() !< pointer to next message in the list of messages
   end type TEcMessage

   !> list of messages
   type(TEcMessage), pointer :: EcMessages => null()

   interface setECMessage
      module procedure setECMessage_char
      module procedure setECMessage_int
   end interface

   contains

      ! =======================================================================

      !> clear the message stack
      subroutine clearECMessage()
         type (TEcMessage), pointer :: cur_msg  !< local pointer to a message on the message stack
         type (TEcMessage), pointer :: next_msg !< idem

         cur_msg => EcMessages
         do while (associated(cur_msg))
            next_msg => cur_msg%next_message
            deallocate(cur_msg)
            cur_msg => next_msg
         enddo
         EcMessages => null()
      end subroutine clearECMessage

      !> add message to message stack
      subroutine setECMessage_char(string, suffix)
         character(len=*), intent(in)           :: string  !< message to be added to message stack
         character(len=*), intent(in), optional :: suffix  !< suffix of message
         !
         type (TEcMessage), pointer :: NewMessage
         integer                    :: ierr

         allocate (NewMessage, stat=ierr)
         if (ierr /= 0) then
            write(*,*) 'Internal error in message stack.'
            write(*,*) 'message: ', string
         else
            NewMessage%next_message => EcMessages
            EcMessages => NewMessage

            if (present(suffix)) then
               NewMessage%message = trim(string)  // " " // suffix
            else
               NewMessage%message = trim(string)
            endif
            NewMessage%message_type = -1
         endif
      end subroutine setECMessage_char

      ! =======================================================================

      !> add message to message stack
      subroutine setECMessage_int(string, val)
         character(len=*), intent(in) :: string !< message to be added to message stack
         integer,          intent(in) :: val    !< number to be added to message
         !
         character(len=8) :: cvalue

         write(cvalue, '(i8)') val

         call setEcMessage(trim(adjustl(string)) // ' ' // trim(cvalue))

      end subroutine setECMessage_int

      ! =======================================================================

      !> dump all messages of the stack using a user supplied messenger function
      function dumpECMessageStack(msglevel, messenger) result(retval)
      implicit none
      integer, intent(in)                 :: msglevel  !< message level; not used yet
      interface
         subroutine messenger(lvl, msg)
         integer, intent(in)              :: lvl
         character(len=*), intent(in)     :: msg
         end subroutine
      end interface
      character(len=32)                   :: retval    !< function result

      type (TEcMessage), pointer    :: my_msg !< local pointer to one of the messages in the stack

      !> loop over all messages in the stack
      my_msg => EcMessages

      if ( associated(my_msg) ) then
         call messenger (msglevel, "...")! separator
      end if
      
      do while (associated(my_msg))
         call messenger (msglevel, my_msg%message)
         my_msg => my_msg%next_message
      enddo

      call clearECMessage()

      retval = 'Fatal EC-error !!'           ! TODO: make this a meaningful return string

      end function dumpECMessageStack

end module m_ec_message
