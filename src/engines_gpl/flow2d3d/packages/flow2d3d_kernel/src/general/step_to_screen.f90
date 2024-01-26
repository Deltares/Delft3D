subroutine step_to_screen(nst2go, itstrt, itfinish, nst, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!    Function: Determines the remaining percentage of the simulation and
!              estimates the remaining time in d:hh:mm:ss and writes both to the 
!              screen together with the number of remaining timesteps.
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow2d3d_timers
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: lunscr
!
! Global variables
!
    integer       :: nst2go       !!  number of timesteps to go
    integer       :: itstrt       !!  first timestep
    integer       :: itfinish     !!  last timestep
    integer       :: nst          !!  current timestep
!
! Local variables
!
    integer(long) :: sec2go_long  !!  seconds remaining (long integer)
    integer       :: sec2go       !!  seconds remaining (normal integer)
    integer       :: min2go       !!  minutes remaining
    integer       :: hours2go     !!  hours remaining
    integer       :: days2go      !!  days remaining
    real(fp)      :: perc_compl   !!  completed percentage of simulation
    character(32) :: timeremstr   !!  string for remaining time
!
!! executable statements -------------------------------------------------------
!
    lunscr  => gdp%gdinout%lunscr
    !
    ! determine completed percentage of simulation
    !
    perc_compl  = 100.0_fp*(real(nst-itstrt,fp)/real(itfinish-itstrt,fp))
    !
    ! initialise the remaining minutes, hours and days 
    !
    days2go  = 0
    hours2go = 0
    min2go   = 0
    !
    ! determine total seconds remaining from timer_simulation
    !
    sec2go_long   = nint(timer_sum(timer_simulation,gdp) * real(nst2go,hp) / real(max(nst-itstrt,1),hp),long)
    !
    ! extract days, hours, minutes and seconds remaining
    !
    if (sec2go_long > int(86400,long)) then
       days2go  = int(sec2go_long/int(86400,long))
       sec2go   = int(sec2go_long - int(days2go,long) * int(86400,long))
    else
       sec2go = int(sec2go_long)
    endif
    if (sec2go > 3600) then
       hours2go = int(sec2go/3600)
       sec2go   = sec2go - hours2go*3600
    endif
    if (sec2go > 60) then
       min2go   = int(sec2go/60)
       sec2go   = sec2go - min2go*60
    endif
    !
    if (days2go >= 1) then
       if (min2go >= 30) then
          hours2go = hours2go + 1
       endif
       write(timeremstr, '(i0,a,i2,a)') days2go,'d ',hours2go,'h'
    elseif (hours2go >= 1) then
       if (sec2go >= 30) then 
          min2go = min2go + 1
       endif
       write(timeremstr, '(i2,a,i2,a)') hours2go,'h ',min2go,'m'
    elseif (min2go >= 10) then
       if (sec2go >= 30) then
          min2go = min2go + 1
       endif
       write(timeremstr, '(i2,a)') min2go,'m'
    elseif (min2go >= 1) then
       write(timeremstr, '(i2,a,i2,a)') min2go,'m ',sec2go,'s'
    else
       write(timeremstr, '(i2,a)') sec2go,'s'
    endif
    !
    ! write remaining steps, percentage and time to screen
    !
    write(lunscr, '(a,a,a,f6.1,a,a,i0)') '  Time to finish  ', &
          & trim(timeremstr), ', ', perc_compl, '% completed, ', &
          & 'time steps left  ', nst2go
    !
end subroutine step_to_screen
