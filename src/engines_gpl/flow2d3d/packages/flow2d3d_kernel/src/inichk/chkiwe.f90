subroutine chkiwe(lundia    ,error     ,nmax      ,mmax      ,nmaxus    , &
                & vicoww    ,dicoww    ,kcs       ,gdp       )
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
!    Function: - Checks the location of the observation point
!                defined for Internal Wave Energy if flag
!                IWEDIA = .true.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: mwriwe
    integer , pointer :: nwriwe
    logical , pointer :: iwedia
!
! Global variables
!
    integer                                                                   :: lundia !  Description and declaration in inout.igs
    integer                                                     , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    logical                                                                   :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                    , intent(out) :: dicoww !  Description and declaration in tricom.igs
    real(fp)                                                    , intent(out) :: vicoww !  Description and declaration in tricom.igs
!
!
! Local variables
!
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    mwriwe  => gdp%gdiwearr%mwriwe
    nwriwe  => gdp%gdiwearr%nwriwe
    iwedia  => gdp%gdiwearr%iwedia
    !
    if (iwedia) then
       !
       !--------test values with model dimensions
       !
       if (mwriwe<1 .or. nwriwe<1) then
          call prterr(lundia    ,'U007'    ,'Observation point IWE'         )
          !
          error = .true.
          goto 9999
       endif
       if (mwriwe>mmax .or. nwriwe>nmaxus) then
          call prterr(lundia    ,'U140'    ,'Observation point IWE'         )
          !
          error = .true.
          goto 9999
       endif
       !
       !--------test discharge is inner point (kcs = 1)
       !
       if (kcs(nwriwe, mwriwe)/=1 .and. kcs(nwriwe, mwriwe)/=-1) then
          call prterr(lundia    ,'V051'    ,'Observation point IWE'         )
          !
          error = .true.
       endif
    endif
    if (error) goto 9999
    !
    !-----Re-define DICOWW and VICOWW at 0. for IWE
    !
    dicoww = 0.
    vicoww = 0.
    call prterr(lundia    ,'U183'    ,' '       )
    !
    !
    !
 9999 continue
end subroutine chkiwe
