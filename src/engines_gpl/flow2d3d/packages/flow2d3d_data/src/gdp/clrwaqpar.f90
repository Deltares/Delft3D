subroutine clrwaqpar(istat, gdp)
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
! Global variables
!
    integer,intent(out) :: istat
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdp%gdwaqpar%quwaq))      deallocate(gdp%gdwaqpar%quwaq     , stat = istat)
    if (associated(gdp%gdwaqpar%qvwaq))      deallocate(gdp%gdwaqpar%qvwaq     , stat = istat)
    if (associated(gdp%gdwaqpar%qwwaq))      deallocate(gdp%gdwaqpar%qwwaq     , stat = istat)
    if (associated(gdp%gdwaqpar%discumwaq))  deallocate(gdp%gdwaqpar%discumwaq , stat = istat)
    if (associated(gdp%gdwaqpar%ifrmto))     deallocate(gdp%gdwaqpar%ifrmto    , stat = istat)
    if (associated(gdp%gdwaqpar%isaggr))     deallocate(gdp%gdwaqpar%isaggr    , stat = istat)
    if (associated(gdp%gdwaqpar%iqaggr))     deallocate(gdp%gdwaqpar%iqaggr    , stat = istat)
    if (associated(gdp%gdwaqpar%ilaggr))     deallocate(gdp%gdwaqpar%ilaggr    , stat = istat)
    if (associated(gdp%gdwaqpar%ifsmax))     deallocate(gdp%gdwaqpar%ifsmax    , stat = istat)
    if (associated(gdp%gdwaqpar%vol))        deallocate(gdp%gdwaqpar%vol       , stat = istat)
    if (associated(gdp%gdwaqpar%sag))        deallocate(gdp%gdwaqpar%sag       , stat = istat)
    if (associated(gdp%gdwaqpar%vol2))       deallocate(gdp%gdwaqpar%vol2      , stat = istat)
    if (associated(gdp%gdwaqpar%sag2))       deallocate(gdp%gdwaqpar%sag2      , stat = istat)
    if (associated(gdp%gdwaqpar%qag))        deallocate(gdp%gdwaqpar%qag       , stat = istat)
    if (associated(gdp%gdwaqpar%horsurf))    deallocate(gdp%gdwaqpar%horsurf   , stat = istat)
    if (associated(gdp%gdwaqpar%kmk))        deallocate(gdp%gdwaqpar%kmk       , stat = istat)
    if (associated(gdp%gdwaqpar%loads))      deallocate(gdp%gdwaqpar%loads     , stat = istat)
    if (associated(gdp%gdwaqpar%iwlk))       deallocate(gdp%gdwaqpar%iwlk      , stat = istat) 
    if (associated(gdp%gdwaqpar%lunsed))     deallocate(gdp%gdwaqpar%lunsed    , stat = istat) 
    if (associated(gdp%gdwaqpar%lunsedflx))  deallocate(gdp%gdwaqpar%lunsedflx , stat = istat) 
    if (associated(gdp%gdwaqpar%cumsedflx))  deallocate(gdp%gdwaqpar%cumsedflx , stat = istat) 
    if (associated(gdp%gdwaqpar%cumresflx))  deallocate(gdp%gdwaqpar%cumresflx , stat = istat) 
    !
end subroutine clrwaqpar
