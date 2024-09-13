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
!  $Id: gdp_for_dll.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/gdp/gdp_for_dll.f90 $
!
module gdp_entry
    use globaldata

    implicit none
    !
    ! Module parameters
    !
    type(globdat) ,save, pointer :: gdp

contains
!
!==============================================================================
subroutine gdpAlloc(componentID, schemID)
    !
    ! arguments
    character(*), intent(in) :: componentID  ! RR, RTC, etc.
    character(*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! body
    if ( componentID == '' .or. schemID == '' ) then
       ! todo: ERROR message
    endif
    allocate(gdp)
    nullify(gdp%runid)
    !
    ! runid must be allocated here; it is used before gdp_alloc is called
    ! see function GETERROR in this file
    !
    allocate (gdp%runid)

end subroutine gdpAlloc
!
!==============================================================================
subroutine gdpDealloc(componentID, schemID)
    !
    
    implicit none
    
    ! arguments
    character(len=*), intent(in) :: componentID  ! RR, RTC, etc.
    character(len=*), intent(in) :: schemID      ! schem. file (*.fnm)
    !
    ! locals
    integer :: ierr
    !
    ! body
    if ( componentID == '' .or. schemID == '' ) then
       ! todo: ERROR message
    endif
    
    ! deallocate gdp contents

    call gdp_dealloc(gdp)
    deallocate(gdp, stat=ierr)

end subroutine gdpDealloc
!

end module gdp_entry
