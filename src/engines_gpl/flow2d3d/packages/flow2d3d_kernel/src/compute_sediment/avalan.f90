subroutine avalan(dps       ,depchg    ,gvu       ,guv       , &
                & icx       ,icy       ,gsqs      ,kcs       ,gdp       )
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
! Smooth bottom, using avalance effect
! At initialization :
! Checks flag by calling Input() and returns if OFF.
! Get nof points and slope from Input()
! Allocates local memory
! Continues processing :
! When bottom slope in the model becomes steeper then the
! criterium given by the user, avalancing is applied.
! Avalancing means distributing the volume difference
! between a cell and it's neighbour, so they get the same
! depth.
! The whole grid is tested. Once in a direction along
! M-lines using the cell to the right as it's neighbour
! and once in a direction along N-lines, using the cell
! above as it's neighbour.
! Updates depths for avalance effects.
! Depth changes caused by avalance effects are
! accumulatively held in a local array, and afterwards
! added to depchg().
!
! If compiled with option /d_lines the routine will produce
! a file = avalan.rpt, with debug information.
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
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: nmmax
    real(fp), dimension(:) , pointer :: depchange
    logical                , pointer :: scour
    real(fp)               , pointer :: slope
!
! Global variables
!
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: depchg !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: icx
    integer :: icy
    integer :: nm
    integer :: nmu
    integer :: num
    real(fp):: anm
    real(fp):: anmu
    real(fp):: anum
    real(fp):: ddep
    real(fp):: ddnm
    real(fp):: ddnmu
    real(fp):: ddnum
    real(fp):: dvol
    real(fp):: size
    !
    real(hp):: depnm
    real(hp):: depnmu
    real(hp):: depnum
!
!! executable statements -------------------------------------------------------
!
    nmmax      => gdp%d%nmmax
    depchange  => gdp%gdscour%depchange
    scour      => gdp%gdscour%scour
    slope      => gdp%gdscour%slope
    !
    ! reset local array
    !
    depchange = 0.0
    !
    do nm = 1, nmmax-icx
       nmu = nm + icx
       !
       ! Slump in first grid direction.
       !
       ! WARNING: mass conservation violation
       ! This code does not take into account the amount of sediment available
       ! at the grid cell, nor its composition.
       !
       ! WARNING: code asymmetry.
       ! Note that slump cascades may propagate over multiple grid cells if the
       ! slump direction matches the direction in which the grid cells are
       ! traversed, whereas slumps in opposite direction will propagate not
       ! further than one grid cell.
       !
       if (kcs(nm)>0 .and. kcs(nmu)>0) then
          depnm  = dps(nm)
          depnmu = dps(nmu)
          ddep   = real(depnmu - depnm,fp)
          size   = gvu(nm)
          if (abs(ddep/size)>slope) then
             anm            = gsqs(nm)
             anmu           = gsqs(nmu)
             dvol           = 0.25*ddep*(anm + anmu)
             !
             ddnm           = dvol/anm
             dps(nm)        = depnm + real(ddnm,prec)
             depchange(nm)  = depchange(nm) - ddnm
             ddnmu          = -dvol/anmu
             dps(nmu)       = depnmu + real(ddnmu,prec)
             depchange(nmu) = depchange(nmu) - ddnmu
          endif
       endif
    enddo
    !
    do nm = 1, nmmax-icy
       num = nm + icy
       !
       ! Do the same thing as before but now in the other grid direction
       !
       if (kcs(nm)>0 .and. kcs(num)>0) then
          depnm  = dps(nm)
          depnum = dps(num)
          ddep   = real(depnum - depnm,fp)
          size   = guv(nm)
          if (abs(ddep/size)>slope) then
             anm            = gsqs(nm)
             anum           = gsqs(num)
             dvol           = 0.25*ddep*(anm + anum)
             !
             ddnm           = dvol/anm
             dps(nm)        = depnm + real(ddnm,prec)
             depchange(nm)  = depchange(nm) - ddnm
             ddnum          = -dvol/anum
             dps(num)       = depnum + real(ddnum,prec)
             depchange(num) = depchange(num) - ddnum
          endif
       endif
    enddo
    do nm = 1, nmmax
       depchg(nm) = depchg(nm) + depchange(nm)
    enddo
end subroutine avalan
