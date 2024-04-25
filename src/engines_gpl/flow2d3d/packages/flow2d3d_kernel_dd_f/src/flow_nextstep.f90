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
!   Delft3D-FLOW
!   Next step interface routines between Fortran and C.
!   Call DD next step routines if there is more than one domain, and just
!   return immediately in the single domain case.
!
!!--pseudo code and references--------------------------------------------------
!
!   Irv.Elshoff@deltares.nl
!   Adri.Mourits@deltares.nl
!   07 jul 06
!
!!------------------------------------------------------------------------------


function nxtstp (currentstep, gdp)
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
!
! Return value
!
    integer :: nxtstp
!
! Local parameters
!
    integer, parameter :: noneighbors = 777     ! must match flow_steps_f.inc
!
! Global variables
!
    integer, intent(in) :: currentstep     ! completed Delft3D-FLOW step
!
! Local variables
!
    integer, external  :: communicatenextstepwithmapper
    integer            :: retval
!
!! executable statements -------------------------------------------------------
!
    integer      , pointer :: nummappers
    nummappers   => gdp%gdprognm%nummappers
    !
    retval = noneighbors
    if (nummappers >= 1) then
        call timer_start(timer_waitdd, gdp)
        retval = communicatenextstepwithmapper (currentstep, -1)
        call timer_stop(timer_waitdd, gdp)
    endif
    !
    nxtstp = retval
end function nxtstp


function nxtdry (currentstep, driedornot, gdp)
    use flow2d3d_timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Return value
!
    integer :: nxtdry
!
! Local parameters
!
    integer, parameter :: noneighbors = 777     ! must match flow_steps_f.inc
!
! Global variables
!
    integer, intent(in) :: currentstep     ! completed Delft3D-FLOW step
    integer, intent(in) :: driedornot      ! dry points in my domain or not (1 = new dry points)
!
! Local variables
!
    integer, external :: communicatenextstepwithmapper
    integer           :: retval
!
!! executable statements -------------------------------------------------------
!
    integer      , pointer :: nummappers
    nummappers   => gdp%gdprognm%nummappers
    !
    retval = noneighbors
    if (nummappers >= 1) then
        call timer_start(timer_waitdd, gdp)
        retval = communicatenextstepwithmapper (currentstep, driedornot)
       call timer_stop(timer_waitdd, gdp)
    endif
    !
    nxtdry = retval
end function nxtdry

