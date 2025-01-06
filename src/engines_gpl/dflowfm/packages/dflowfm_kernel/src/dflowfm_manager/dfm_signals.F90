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

!> DFM_SIGNALS - Respond to process signals (POSIX-style).
!! Lets the program watch signals such as Control-C and Control-Z, and
!! perform some final tasks before quitting the program.
!! For example: write some last map file.
!!
!! DFM's signal handlers do not interrupt the program immediately, but merely
!! 'record' the given signal (via libsigwatch), and only at certain moments
!! the actual actions for a given signal are performed (typically after time
!! step has completed).
module dfm_signals
use m_flow_externaloutput_direct, only: flow_externaloutput_direct

   implicit none

#ifdef HAVE_SIGIWATCH

!!! Depending on which signal was caught, we may want to tell the calling
!!! subroutine to either stop or continue:
!integer, parameter :: ACT_NULL     = -1 !< Code for no specific action requested
!integer, parameter :: ACT_CONTINUE =  0 !< Code to continue simulation.
!integer, parameter :: ACT_STOP     =  1 !< Code to stop the simulation.

#ifdef _MSC_VER
   integer, parameter :: DFM_SIG_STOPANDGO = 21 ! SIGBREAK : Ctrl+Break keypress (Microsoft Visual C). MSVC does not allow handling the standard SIGTSP signal, so use SIGBREAK alternative.
#else
   integer, parameter :: DFM_SIG_STOPANDGO = 20 ! SIGTSTP  : Ctrl+Z keypress (ANSI C)
#endif

#endif
contains

!> Registers which specific signal numbers should be watched for.
!! libsigwatch will 'record' these signals for us.
   subroutine dfm_add_signalwatchers()
      use unstruc_messages
      implicit none

#ifdef HAVE_SIGIWATCH
      integer, external :: watchsignal !< from libsigwatch
      integer, external :: watchsignalname !< from libsigwatch
      integer :: status

      status = watchsignal(2) ! SIGINT (Ctrl-C)
      if (status == -1) then
         call mess(LEVEL_WARN, 'Failed to install interrupt signal handler. No data will be saved upon interrupt.')
      end if

      status = watchsignal(DFM_SIG_STOPANDGO) ! SIGBREAK / SIGTSTP (Ctrl-Break / Ctrl-Z)
      if (status == -1) then
         call mess(LEVEL_WARN, 'Failed to install suspend/break signal handler. No data will be saved upon suspend.')
      end if
#endif
   end subroutine dfm_add_signalwatchers

!> Performs the actual check whether an important signal was recently given.
   function dfm_check_signals() result(ierror)
      use unstruc_messages
      use dfm_error
      use m_datum
      implicit none
      integer :: ierror !< Tells the caller what to do (either DFM_NOERR, or DFM_SIGINT)
#ifdef HAVE_SIGIWATCH
      character(len=20) :: datetimestr

      integer, external :: getlastsignal, watchsignal !< from libsigwatch
      external :: datum

      integer :: lastsig, iwatch

      ierror = DFM_NOERR

      lastsig = getlastsignal()

      if (lastsig == 0) then
         return
      end if

      call DATUM(datetimestr)

      select case (lastsig)
      case (2) ! SIGINT (Ctrl-C)
         call mess(LEVEL_INFO, trim(datetimestr)//': Caught interrupt. Saving current data...')

         call flow_externaloutput_direct()
         ! iwatch = watchsignal(2)  ! No need to re-register this handler, because SIGINT should stop and quit this run anyway.
         ierror = DFM_SIGINT

      case (DFM_SIG_STOPANDGO) ! SIGBREAK or SIGTSTP (Windows: Ctrl-Break or Linux: Ctrl-Z, resp.)
         call mess(LEVEL_INFO, trim(datetimestr)//': Caught suspend. Saving current data...')

         call flow_externaloutput_direct()
         iwatch = watchsignal(DFM_SIG_STOPANDGO) ! We need to re-register the handler every time we catch a signal.

         call mess(LEVEL_INFO, 'Continuing execution...')
      end select
#else
      ierror = 0

#endif

   end function dfm_check_signals

end module dfm_signals
