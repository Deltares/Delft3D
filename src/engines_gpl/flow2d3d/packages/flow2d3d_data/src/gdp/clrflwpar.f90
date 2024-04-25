subroutine clrflwpar(istat, gdp)
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
    if (associated(gdp%gdflwpar%fluxu))     deallocate(gdp%gdflwpar%fluxu    , STAT = istat)
    if (associated(gdp%gdflwpar%fluxuc))    deallocate(gdp%gdflwpar%fluxuc   , STAT = istat)
    if (associated(gdp%gdflwpar%fluxv))     deallocate(gdp%gdflwpar%fluxv    , STAT = istat)
    if (associated(gdp%gdflwpar%fluxvc))    deallocate(gdp%gdflwpar%fluxvc   , STAT = istat)
    if (associated(gdp%gdflwpar%fluxw))     deallocate(gdp%gdflwpar%fluxw    , STAT = istat)
    if (associated(gdp%gdflwpar%fluxwc))    deallocate(gdp%gdflwpar%fluxwc   , STAT = istat)
    !
    if (associated(gdp%gdflwpar%flwoutput)) deallocate(gdp%gdflwpar%flwoutput, STAT = istat)
    call cleartable(gdp%gdflwpar%fbcrfile)
    if (associated(gdp%gdflwpar%fbcrfile))  deallocate(gdp%gdflwpar%fbcrfile , STAT = istat)
end subroutine clrflwpar
