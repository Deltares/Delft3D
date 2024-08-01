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
! Copied and modified from: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/engines_gpl/flow2d3d/packages/kernel/src/general/step_to_screen.f90
! Original file distributed  GNU General Public License <http://www.gnu.org/licenses/>.

!> Determines the remaining percentage of the simulation and
!! estimates the remaining time in d:hh:mm:ss and writes both to the
!! screen together with the number of remaining timesteps.
subroutine step_to_screen()
   use precision
   use Timers
   use m_flowtimes
   use unstruc_messages
   !
   implicit none

!    integer :: lunscr = 6 ! Hardcoded stdout
!
! Local variables
!
   integer :: itstrt, itfinish !! Start and finish iteration nrs (to resemble original Delft3D version)
   integer :: nst2go !!  estimated user time steps remaining
   integer(long) :: sec2go_long !!  seconds remaining (long integer)
   integer :: sec2go !!  seconds remaining (normal integer)
   integer :: min2go !!  minutes remaining
   integer :: hours2go !!  hours remaining
   integer :: days2go !!  days remaining
   real(fp) :: perc_compl !!  completed percentage of simulation
   character(32) :: timeremstr !!  string for remaining time
   character(20) :: string !!  string for remaining time
   integer, save :: ifirsttime = 1
   real(fp) :: dt_ave
   real(fp) :: tcpu
   real(fp), save :: timesav = 0d0
   real(fp), save :: dntsav = 0d0

   character(len=20), external :: seconds_to_dhms
!
!! executable statements -------------------------------------------------------
!

   if (ifirsttime == 1) then
      write (msgbuf, '(a)') '   Sim. time done   Sim. time left   Real time used   Real time left Steps left Complete% Interval-averaged time step'
      call msg_flush()
      ifirsttime = 0
      timesav = 0d0
      dntsav = 0d0
   end if

   !
   ! determine completed percentage of simulation
   ! Based on remaining simulation time, and *user* timestep (since we use wall clock time including drawing and output).
   !
   nst2go = int((tstop_user - time_user) / dt_user)
   itstrt = 0
   itfinish = dnt_user + nst2go ! completed user time steps + estimated remaining user time steps

   perc_compl = 100.0_fp * (real(dnt_user - itstrt, fp) / real(max(itfinish - itstrt, 1), fp))
   !
   ! initialise the remaining minutes, hours and days
   !
   days2go = 0
   hours2go = 0
   min2go = 0
   !
   ! determine total seconds remaining from timer_simulation
   !
   tcpu = tim_get_wallclock(handle_steps)
   sec2go_long = nint(tcpu * real(nst2go, hp) / real(max(int(dnt_user) - itstrt, 1), hp), long)
   if (tcpu <= 0.0) then
      sec2go_long = -1
   end if
   !
   ! extract days, hours, minutes and seconds remaining
   !
   if (sec2go_long > int(86400, long)) then
      days2go = int(sec2go_long / int(86400, long))
      sec2go = int(sec2go_long - int(days2go, long) * int(86400, long))
   else
      sec2go = int(sec2go_long)
   end if
   if (sec2go > 3600) then
      hours2go = int(sec2go / 3600)
      sec2go = sec2go - hours2go * 3600
   end if
   if (sec2go > 60) then
      min2go = int(sec2go / 60)
      sec2go = sec2go - min2go * 60
   end if
   !
   if (days2go >= 1) then
      if (min2go >= 30) then
         hours2go = hours2go + 1
      end if
      write (timeremstr, '(i0,a,i2,a)') days2go, 'd ', hours2go, 'h'
   elseif (hours2go >= 1) then
      if (sec2go >= 30) then
         min2go = min2go + 1
      end if
      write (timeremstr, '(i2,a,i2,a)') hours2go, 'h ', min2go, 'm'
   elseif (min2go >= 10) then
      if (sec2go >= 30) then
         min2go = min2go + 1
      end if
      write (timeremstr, '(i2,a)') min2go, 'm'
   elseif (min2go >= 1) then
      write (timeremstr, '(i2,a,i2,a)') min2go, 'm ', sec2go, 's'
   elseif (sec2go >= 0) then
      write (timeremstr, '(i2,a)') sec2go, 's'
   else
      write (timeremstr, '(a2,a)') '??', 's'
   end if

!   compute (sliding) average time step

   dt_ave = 0d0
   if (timesav > 0d0) then
      dt_ave = (time1 - timesav) / max(dnt - dntsav, 1d0)
   end if
   timesav = time1
   dntsav = dnt

   !
   ! write remaining steps, percentage and time to screen
   !
   !write(msgbuf, '(a,a,a,f6.1,a,a,i0)') '  Time to finish  ', &
   !      & trim(timeremstr), ', ', perc_compl, '% completed, ', &
   !      & 'user time steps left  ', nst2go
    !!
   !0    .    1    .    2    .    3    .    4    .    5    .    6    .    7
   string = seconds_to_dhms(nint(time_user - tstart_user, long))
   write (msgbuf, '(4(1x,a20),i11,f8.1,a1,f12.5)') &
      seconds_to_dhms(nint(time_user - tstart_user, long)), &
      seconds_to_dhms(nint(tstop_user - time_user, long)), &
      seconds_to_dhms(nint(tim_get_wallclock(handle_steps), long)), &
      seconds_to_dhms(sec2go_long), &
      nst2go, &
      perc_compl, &
      '%', &
      max(0d0, dt_ave)
   call msg_flush()
end subroutine step_to_screen

function seconds_to_dhms(secs_long) result(timestr)
   use precision
   implicit none
   integer(long), intent(in) :: secs_long !< total in seconds
   character(len=20) :: timestr

   integer(long) :: secs_l !!  Copy of total seconds for computations
   integer :: secs !!  seconds remaining (normal integer)
   integer :: mins !!  minutes remaining
   integer :: hours !!  hours remaining
   integer :: days !!  days remaining
   !
   ! extract days, hours, minutes and seconds remaining
   !
   secs_l = secs_long
   days = 0
   hours = 0
   mins = 0
   secs = 0
   if (secs_l >= int(86400, long)) then
      days = int(secs_long / int(86400, long))
      secs_l = secs_l - int(days, long) * int(86400, long)
   end if
   if (secs_l >= 3600_long) then
      hours = int(secs_l / 3600_long)
      secs_l = secs_l - hours * 3600_long
   end if
   if (secs_l >= 60_long) then
      mins = int(secs_l / 60_long)
      secs = int(secs_l - mins * 60_long)
   else
      secs = int(secs_l)
   end if
   if (secs >= 0) then
      write (timestr, '(i10,a,i2,a,i2.2,a,i2.2)') days, 'd ', hours, ':', mins, ':', secs
   else
      write (timestr, '(a6,a,a2,a,a2,a,a2)') '    ??', 'd ', '??', ':', '??', ':', '??'
   end if

end function seconds_to_dhms
