subroutine iniFLUXuv(kmax,lstsci,lundia,gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!  $Id: iniFLUXuv.f90 6021 2016-04-15 14:20:51Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/iniFLUXuv.f90 $
!!--description-----------------------------------------------------------------
!
!     INTIALIZE fluxu fluxv (to be used in difu at the first half time step when not yet initialized
!
! Author: Alberto Canestrelli
!
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
    integer, intent(IN) :: kmax
    integer, intent(IN) :: lstsci
    integer, intent(IN) :: lundia
    integer istat

    istat = 0
    if (.not. associated(gdp%gdflwpar%fluxu)) then
       if (istat==0) allocate (gdp%gdflwpar%fluxu(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       if (istat==0) allocate (gdp%gdflwpar%fluxv(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
       !
       if (istat /= 0) then
          call prterr(lundia, 'U021', 'DIFUFLUX: memory alloc error')
          call d3stop(1, gdp)
       endif
    endif
!
    gdp%gdflwpar%fluxu = 0._fp
    gdp%gdflwpar%fluxu = 0._fp
!
return
end subroutine iniFLUXuv
