module message_module
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
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: - Keep track of a stack of messages
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------

private

!
! constants
!
integer, parameter, public :: message_len = 1024

!
! data types
!
public message_stack

!
! functions and subroutines
!
public initstack
public clearstack
public isempty
public addmessage
public adderror
public addwarning
public getmessage
public writemessages
!
public write_error
public write_warning
!                                                                1         2         3
character( 16), parameter, public :: FILE_NOT_FOUND  = 'File not found: '
character( 23), parameter, public :: PREMATURE_EOF   = 'Premature EOF in file: '
character( 22), parameter, public :: FILE_READ_ERROR = 'Read error from file: '
character( 20), parameter, public :: ERROR_FILE_OPEN = 'Error opening file: '
!
type message_type
    private
    character(message_len)       :: message
    type(message_type) , pointer :: other_messages
end type message_type

type message_stack
    private
    type(message_type), pointer  :: message_list
end type message_stack

contains
!
!
!
!==============================================================================
subroutine initstack(stack)
    !    Function: - Create a message stack
    !
    implicit none
    !
    ! Arguments
    !
    type(message_stack)          :: stack
    !
    ! Local variables
    !
    !
    ! body
    !
    nullify(stack%message_list)
end subroutine initstack
!
!
!
!==============================================================================
subroutine clearstack(stack)
    !    Function: - Empty a message stack
    !                This function is identical to writemessages, except the
    !                writing part
    !
    implicit none
    !
    ! Arguments
    !
    type(message_stack)          :: stack
    !
    ! Local variables
    !
    character(message_len)       :: message
    !
    ! body
    !
    do while (.not.isempty(stack))
       call getmessage(stack,message)
    end do
end subroutine clearstack
!
!
!
!==============================================================================
subroutine addmessage(stack,usermessage)
    !    Function: - Add a new message to the bottom of the message stack
    !
    implicit none
    !
    ! Arguments
    !
    character(*)        , intent(in) :: usermessage
    type(message_stack)              :: stack
    !
    ! Local variables
    !
    integer                          :: istat
    type(message_type)  , pointer    :: localstack
    !
    ! body
    !
    if (associated(stack%message_list)) then
       localstack => stack%message_list
       do while (associated(localstack%other_messages))
          localstack => localstack%other_messages
       end do
       allocate(localstack%other_messages, stat = istat)
       if (istat /= 0) then
          localstack%message = "ERROR message module: memory alloc error"
       else
          localstack => localstack%other_messages
          localstack%message = usermessage
          nullify(localstack%other_messages)
       endif
    else
       allocate(stack%message_list, stat = istat)
       if (istat /= 0) then
          stack%message_list%message = "ERROR message module: memory alloc error"
       else
          localstack => stack%message_list
          localstack%message = usermessage
          nullify(localstack%other_messages)
       endif
    endif
end subroutine addmessage
!
!
!
!==============================================================================
subroutine adderror(stack,usermessage)
    !    Function: - Add a new error message to the bottom of the message stack
    !
    implicit none
    !
    ! Arguments
    !
    type(message_stack)                :: stack
    character(*)          , intent(in) :: usermessage
    !
    ! Local variables
    !
    character(message_len)             :: newmessage
    !
    ! body
    !
    call write_error(usermessage,string=newmessage)
    call addmessage(stack,newmessage)
end subroutine adderror
!
!
!
!==============================================================================
subroutine addwarning(stack,usermessage)
    !    Function: - Add a new warning message to the bottom of the message stack
    !
    implicit none
    !
    ! Arguments
    !
    type(message_stack)                :: stack
    character(*)          , intent(in) :: usermessage
    !
    ! Local variables
    !
    character(message_len)             :: newmessage
    !
    ! body
    !
    call write_warning(usermessage,string=newmessage)
    call addmessage(stack,newmessage)
end subroutine addwarning
!
!
!
!==============================================================================
function isempty(stack)
    !    Function: - Checks whether there is any message on the stack
    !
    implicit none
    !
    ! Arguments
    !
    type(message_stack) :: stack
    logical             :: isempty
    !
    ! Local variables
    !
    !
    ! body
    !
    isempty = .not.associated(stack%message_list)
end function isempty
!
!
!
!==============================================================================
subroutine getmessage(stack,message)
    !    Function: - Get a message from the top of the message stack
    !
    implicit none
    !
    ! Arguments
    !
    type(message_stack)    :: stack
    character(message_len) :: message
    !
    ! Local variables
    !
    type(message_type), pointer :: localstack
    !
    ! body
    !
    if (associated(stack%message_list)) then
       localstack => stack%message_list
       message = localstack%message
       stack%message_list => localstack%other_messages
       deallocate(localstack)
    else
       message = 'Trying to get message from empty stack.'
    endif
end subroutine getmessage
!
!
!
!==============================================================================
subroutine writemessages(stack,unit)
    !    Function: - Write all messages to file
    !
    implicit none
    !
    ! Arguments
    !
    type(message_stack)          :: stack
    integer                      :: unit
    !
    ! Local variables
    !
    character(message_len)       :: message
    !
    ! body
    !
    do while (.not.isempty(stack))
       call getmessage(stack,message)
       write(unit,'(A)') trim(message)
    end do
end subroutine writemessages
!
!
!
!==============================================================================
subroutine write_error(message, string, unit)
    !    Function: - Write error message to string or file
    !
    implicit none
    !
    ! Arguments
    !
    character(*)          , intent(in) :: message
    character(*), optional, intent(out):: string
    integer     , optional, intent(in) :: unit
    !
    ! Local variables
    !
    character(10) :: label = '*** ERROR '
    !
    !! executable statements ---------------------------------------------------
    !
    if (present(string)) string = label // message
    if (present(unit)) write(unit,'(A)') label // trim(message)
end subroutine write_error
!
!
!
!==============================================================================
subroutine write_warning(message, string, unit)
    !    Function: - Write warning message to string or file
    !
    implicit none
    !
    ! Arguments
    !
    character(*)          , intent(in) :: message
    character(*), optional, intent(out):: string
    integer     , optional, intent(in) :: unit
    !
    ! Local variables
    !
    character(12) :: label = '*** WARNING '
    !
    !! executable statements ---------------------------------------------------
    !
    if (present(string)) string = label // message
    if (present(unit)) write(unit,'(A)') label // trim(message)
end subroutine write_warning

end module message_module
