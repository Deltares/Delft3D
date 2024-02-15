!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 
!> Info and error messages within Unstruc.
!! Messages will be printed on stdout and the diagnostics file.
!! A buffer 'msgbuf' is available to write to.
module unstruc_messages
use MessageHandling

implicit none

logical, parameter, private :: printToStdout = .true.
integer :: threshold_abort = LEVEL_ERROR

! Verbosity levels for logging on screen and in diagnostics file.
! Configurable at runtime with '--verbose:...'-flag or set_var('verbose') in bmi
integer :: loglevel_StdOut = LEVEL_INFO
integer :: loglevel_file   = LEVEL_INFO

integer, parameter :: maxerrprint = 50 !< Maximum number of errors of same type printed on screen (only used by some code pieces).

!> The message buffer allows you to write any number of variables in any
!! order to a character string. Call msg_flush or err_flush to output
!! the actual message or error.

contains

!> Initializes the MessageHandling module with the mdia file pointer.
subroutine initMessaging(mdia)
    integer, intent(in) :: mdia
    external :: unstruc_errorhandler, unstruc_guimessage

    call SetMessagehandling(printToStdout, .false., mdia, unstruc_errorhandler, &
         thresholdLevel_stdout = loglevel_StdOut, thresholdLevel_file = loglevel_file)

    ! Set the qnerror wrapper, for use in gridoperations, etc.
    call set_msgbox_callback(unstruc_guimessage)

end subroutine initMessaging

subroutine callback_msg(lvl,msg)
   integer, intent(in)              :: lvl
   character(len=*), intent(in)    :: msg 
   call mess(lvl,trim(msg))
end subroutine


end module unstruc_messages