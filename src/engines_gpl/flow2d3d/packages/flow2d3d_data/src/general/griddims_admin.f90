subroutine griddims_admin( kcs, xz, yz, xcor, ycor, gdp )
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
    integer          , pointer :: nmlb
    integer          , pointer :: nmub
    type(griddimtype), pointer :: griddim
!
! Global variables
!
   integer , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcs
   real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: xz
   real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: yz
   real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: xcor
   real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: ycor
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
    nmlb         => gdp%d%nmlb
    nmub         => gdp%d%nmub
    griddim      => gdp%griddim
    !
    icx = 1
    icy = griddim%nmax + 2*gdp%d%ddbound
    !
    do nm = nmlb, nmub
       griddim%celltype(nm) = kcs(nm)
    enddo
    !
    i = 0
    do nm = 1, griddim%nmmax
       if (kcs(nm)==2) i = i+1
    enddo
    !
    call reallocP(gdp%griddim%nmbnd, (/i,2/), stat=istat)
    i = 0
    do nm = 1, griddim%nmmax
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
          griddim%nmbnd(i,1) = nm   ! open boundary cell
          griddim%nmbnd(i,2) = nm2  ! corresponding internal cell
       endif
    enddo
    !
    allocate(griddim%xz(nmlb:nmub), stat=istat)
    allocate(griddim%yz(nmlb:nmub), stat=istat)
    allocate(griddim%xnode(nmlb:nmub), stat=istat)
    allocate(griddim%ynode(nmlb:nmub), stat=istat)
    griddim%xz(:) = xz(:)
    griddim%yz(:) = yz(:)
    griddim%xnode(:) = xcor(:)
    griddim%ynode(:) = ycor(:)
end subroutine griddims_admin