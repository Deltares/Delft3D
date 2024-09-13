subroutine griddims_admin( kcs, gdp )
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
!  $Id: griddims_admin.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/data/src/general/griddims_admin.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: ...
!
!!--declarations----------------------------------------------------------------
    use globaldata
    use m_alloc
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    !   NONE

!
! Global variables
!
   integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcs
!
! Local variables
!
    integer                :: i
    integer                :: icx
    integer                :: icy
    integer                :: istat
    integer                :: nm
    integer                :: nm2
!
!! executable statements -------------------------------------------------------
!
    icx = 1
    icy = gdp%d%nmax + 2*gdp%d%ddbound
    !
    do nm = gdp%d%nmlb, gdp%d%nmub
       gdp%griddim%celltype(nm) = kcs(nm)
    enddo
    !
    i = 0
    do nm = 1, gdp%d%nmmax
       if (kcs(nm)==2) i = i+1
    enddo
    !
    call reallocP(gdp%griddim%nmbnd, (/i,2/), stat=istat)
    i = 0
    do nm = 1, gdp%d%nmmax
       if (kcs(nm)==2) then
          i = i+1
          !
          if (kcs(nm-icx) == 1) then
             ! ndm
             nm2 = nm-icx
          elseif (kcs(nm+icx) == 1) then
             ! num
             nm2 = nm+icx
          elseif (kcs(nm-icy) == 1) then
             ! nmd
             nm2 = nm-icy
          else
             ! nmu
             nm2 = nm+icy
          endif
          !
          gdp%griddim%nmbnd(i,1) = nm   ! open boundary cell
          gdp%griddim%nmbnd(i,2) = nm2  ! corresponding internal cell
       endif
    enddo
end subroutine griddims_admin