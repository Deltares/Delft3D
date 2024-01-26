subroutine dfupdgeo ( alfas     ,fcorio    ,xcor      ,ycor      ,xz        , &
                    & yz        ,guu       ,gvv       ,guv       ,gvu       , &
                    & gsqs      ,gsqd      ,guz       ,gvz       ,gud       , &
                    & gvd       ,gsqiu     ,gsqiv     ,gdp       )
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
!   exchange geometrical information, e.g. guu, gvu, etc. with neighbours
!
!!--pseudo code and references--------------------------------------------------
!
!
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: fcorio !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gsqd   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gsqiu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gsqiv  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: xcor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: ycor   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(inout) :: yz     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer               :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    ! if not parallel, return
    !
    if (.not.parll) return
    !
    ! exchange geometrical information (obtained with inigeo) with neighbours
    !
    nm_pos = 1
    call dfexchg ( alfas , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( fcorio, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( gsqd  , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( gsqiu , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( gsqiv , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( gsqs  , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( gud   , 1, 1, dfloat, nm_pos, gdp )
!
!   Note: for obvious reasons (?), guu and gvv do not have to be exchanged.
!    
!    call dfexchg ( guu   , 1, 1, dfloat, gdp )
    call dfexchg ( guv   , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( guz   , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( gvd   , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( gvu   , 1, 1, dfloat, nm_pos, gdp )
!    call dfexchg ( gvv   , 1, 1, dfloat, gdp )
    call dfexchg ( gvz   , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( xcor  , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( xz    , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( ycor  , 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( yz    , 1, 1, dfloat, nm_pos, gdp )
    !
end subroutine dfupdgeo
