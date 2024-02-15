subroutine initmassbal(gdp)
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
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    gdp%gdmassbal%massbal     = .false.
    gdp%gdmassbal%resetfluxes = .false.
    gdp%gdmassbal%nbalpol     = 0
    gdp%gdmassbal%nneighb     = 0
    !
    nullify(gdp%gdmassbal%volnames)
    nullify(gdp%gdmassbal%volnr)
    nullify(gdp%gdmassbal%exchnr)
    nullify(gdp%gdmassbal%neighb)
    !
    nullify(gdp%gdmassbal%accdps)
    nullify(gdp%gdmassbal%horareas)
    nullify(gdp%gdmassbal%volumes)
    nullify(gdp%gdmassbal%mass_r1)
    nullify(gdp%gdmassbal%fluxes)
    nullify(gdp%gdmassbal%fluxes_r1)
    nullify(gdp%gdmassbal%fluxes_sd)
end subroutine initmassbal
