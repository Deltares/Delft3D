subroutine clrbedformpar(istat, gdp)
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
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    if (associated(gdp%gdbedformpar%duneheight))     deallocate(gdp%gdbedformpar%duneheight, STAT = istat)
    if (associated(gdp%gdbedformpar%duneheightequi)) deallocate(gdp%gdbedformpar%duneheightequi, STAT = istat)
    if (associated(gdp%gdbedformpar%dunelength))     deallocate(gdp%gdbedformpar%dunelength, STAT = istat)
    if (associated(gdp%gdbedformpar%qbedformx))      deallocate(gdp%gdbedformpar%qbedformx, STAT = istat)
    if (associated(gdp%gdbedformpar%qbedformy))      deallocate(gdp%gdbedformpar%qbedformy, STAT = istat)
    if (associated(gdp%gdbedformpar%ubedform))       deallocate(gdp%gdbedformpar%ubedform, STAT = istat)

    if (associated(gdp%gdbedformpar%rksr))           deallocate(gdp%gdbedformpar%rksr, STAT = istat)
    if (associated(gdp%gdbedformpar%rksmr))          deallocate(gdp%gdbedformpar%rksmr, STAT = istat)
    if (associated(gdp%gdbedformpar%rksd))           deallocate(gdp%gdbedformpar%rksd, STAT = istat)

    if (associated(gdp%gdbedformpar%hdpar))          deallocate(gdp%gdbedformpar%hdpar, STAT = istat)
    if (associated(gdp%gdbedformpar%ldpar))          deallocate(gdp%gdbedformpar%ldpar, STAT = istat)
    if (associated(gdp%gdbedformpar%kdpar))          deallocate(gdp%gdbedformpar%kdpar, STAT = istat)
    if (associated(gdp%gdbedformpar%cdpar))          deallocate(gdp%gdbedformpar%cdpar, STAT = istat)
    !
    if (associated(gdp%gdbedformpar%bedformD50))     deallocate(gdp%gdbedformpar%bedformD50, STAT = istat)
    if (associated(gdp%gdbedformpar%bedformD90))     deallocate(gdp%gdbedformpar%bedformD90, STAT = istat)
end subroutine clrbedformpar
