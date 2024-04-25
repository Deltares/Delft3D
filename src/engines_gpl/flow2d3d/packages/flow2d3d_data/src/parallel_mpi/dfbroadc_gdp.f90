subroutine dfbroadc_gdp ( iptr, ilen, itype, gdp )
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
!   Broadcasts data from the master to all other processes
!
!!--pseudo code and references--------------------------------------------------
!
!   wrapper for DFBROADC without GDP (which is an MPI_BCAST wrapper)
!
!
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat), target    :: gdp
!
! Global variables
!
    integer, pointer       :: lundia
    integer, intent(inout) :: iptr  ! pointer to first element of array to be sent
    integer                :: ilen  ! length of array to be sent
    integer, intent(inout) :: itype ! type of data
!
! Local variables
!
    logical       :: error  ! error flag
    character(80) :: msgstr ! error message
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    !
    call dfbroadc ( iptr, ilen, itype, error, msgstr )
    if ( error ) then
       call prterr(lundia, 'U021', trim(msgstr))
       call d3stop(1, gdp)
    endif

end subroutine dfbroadc_gdp
