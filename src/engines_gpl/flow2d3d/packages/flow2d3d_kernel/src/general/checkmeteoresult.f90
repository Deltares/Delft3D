subroutine checkmeteoresult(success, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: checkmeteoresult.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/general/checkmeteoresult.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Handle the occurence of an error generated in the meteo module
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use meteo
    !
    ! global data declaration; compare with include 'globdat.igd'
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    integer, pointer :: lundia
!
! Global variables
!
    logical        :: success
!
! Local variables
!
    character(500) :: message
!
!! executable statements -------------------------------------------------------
!
    lundia => gdp%gdinout%lundia
    message = getmeteomessage()
    if (.not. success) then
       call prterr(lundia, 'P004', trim(message))
       call d3stop(3, gdp)
    elseif (message /= ' ') then
       call prterr(lundia, 'G051', trim(message))
    endif
end subroutine checkmeteoresult
