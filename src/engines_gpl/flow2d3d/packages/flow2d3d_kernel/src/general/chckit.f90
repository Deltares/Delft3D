subroutine chckit(lundia    ,error     ,filnam    ,timrd     ,dt        , &
                & ittdep    ,itold     ,itstrt    ,ntimrd    ,gdp       )
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
!    Function: Check time read for time dependent data
!              repetitive checks used by various routines
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer      , intent(in)      :: itold       ! Help var. to store last read time to test ascending order
    integer      , intent(in)      :: itstrt      ! Description and declaration in inttim.igs
    integer                        :: ittdep      ! Help var. for the time read (now defined as multiples of DT, but in future it may take any value)
    integer                        :: lundia      ! Description and declaration in inout.igs
    integer      , intent(in)      :: ntimrd      ! Sequence nr of the time read
    logical      , intent(out)     :: error       ! Flag=TRUE if an error is encountered
    real(fp)                       :: dt          ! Description and declaration in esm_alloc_real.f90
    real(fp)     , intent(in)      :: timrd       ! Time in minutes read
    character(*) , intent(in)      :: filnam      ! Name of the specified data file
!
!
! Local variables
!
    integer                        :: it
    integer                        :: namlen      ! trimmed length of file name  
    logical                        :: dtn
    real(fp)                       :: t
    character(256)                 :: errmsg
    character(256)                 :: localfilnam !!  copy of filnam; needed because chckit can be called with a constant string. 
                                                  !!  That causes problems for remove_leading_spaces. 
!
!
!! executable statements -------------------------------------------------------
!
    !
    ! Strip blanks from file name
    !
    localfilnam = filnam
    call remove_leading_spaces(localfilnam, namlen)
    !
    ! NOTE: in future interpolation over DT should be possible
    ! CHECK TIMES
    !
    ittdep = nint(timrd/dt)
    if (dtn(ittdep, timrd, dt)) then
       errmsg = 'Times in ' // localfilnam(1:namlen)
       call prterr(lundia    ,'U044'    ,errmsg    )
       !
       error = .true.
    endif
    !
    ! ITTDEP > simulation start time ITSTRT, not allowed
    !
    if (ntimrd==1) then
       if (ittdep>itstrt) then
          errmsg = 'First time in ' // localfilnam(1:namlen) // ' >'
          call prterr(lundia    ,'U041'    ,errmsg    )
          !
          error = .true.
       endif
    endif
    !
    ! ITTDEP <= last read time ITOLD, not allowed
    ! times not increasing
    !
    if (ittdep<=itold) then
       call prterr(lundia    ,'U060'    ,localfilnam(1:namlen))
       !
       error = .true.
    endif
end subroutine chckit
