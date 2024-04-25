subroutine clrmassbal(istat, gdp)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use flow_tables
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer,intent(out) :: istat
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdp%gdmassbal%volnames))  deallocate(gdp%gdmassbal%volnames , STAT = istat)
    if (associated(gdp%gdmassbal%volnr))     deallocate(gdp%gdmassbal%volnr    , STAT = istat)
    if (associated(gdp%gdmassbal%exchnr))    deallocate(gdp%gdmassbal%exchnr   , STAT = istat)
    if (associated(gdp%gdmassbal%neighb))    deallocate(gdp%gdmassbal%neighb   , STAT = istat)
    !
    if (associated(gdp%gdmassbal%accdps))    deallocate(gdp%gdmassbal%accdps   , STAT = istat)
    if (associated(gdp%gdmassbal%horareas))  deallocate(gdp%gdmassbal%horareas , STAT = istat)
    if (associated(gdp%gdmassbal%volumes))   deallocate(gdp%gdmassbal%volumes  , STAT = istat)
    if (associated(gdp%gdmassbal%mass_r1))   deallocate(gdp%gdmassbal%mass_r1  , STAT = istat)
    if (associated(gdp%gdmassbal%fluxes))    deallocate(gdp%gdmassbal%fluxes   , STAT = istat)
    if (associated(gdp%gdmassbal%fluxes_r1)) deallocate(gdp%gdmassbal%fluxes_r1, STAT = istat)
    if (associated(gdp%gdmassbal%fluxes_sd)) deallocate(gdp%gdmassbal%fluxes_sd, STAT = istat)
    !
    gdp%gdmassbal%nbalpol = 0
    gdp%gdmassbal%nneighb = 0
    gdp%gdmassbal%massbal = .false.
    !
end subroutine clrmassbal
