subroutine dersig(j         ,nmmaxj    ,nmmax     ,icx       ,icy       , &
                & kfu       ,kfv       ,dpd       ,s1        ,dddksi    , &
                & dddeta    ,dzdksi    ,dzdeta    ,gdp       )
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
!    Function: Computes transformation coefficients sigma-
!              transformation; DZDKSI, DZDETA, DDDKSI and DDDETA.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                 :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: dddeta !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: dddksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: dpd !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: dzdeta !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: dzdksi !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: s1 !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                        :: ndm
    integer                        :: nm
    integer                        :: nmd
    integer                        :: nmu
    integer                        :: num
    integer                        :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    nmd = -icx
    ndm = -icy
    nmu = icx
    num = icy
    do nm = 1, nmmax
       nmd = nmd + 1
       ndm = ndm + 1
       nmu = nmu + 1
       num = num + 1
       !
       ! DZETA/DKSI IN U-POINTS
       !
       if (kfu(nm)==1) then
          dzdksi(nm) = s1(nmu) - s1(nm)
       else
          dzdksi(nm) = 0.0
       endif
       !
       ! DDEPTH/DETA IN U-POINTS
       !
       dddeta(nm) = dpd(nm) - dpd(ndm)
       !
       ! DZETA/DETA IN V-POINTS
       !
       if (kfv(nm)==1) then
          dzdeta(nm) = s1(num) - s1(nm)
       else
          dzdeta(nm) = 0.0
       endif
       !
       ! DDEPTH/DKSI IN V-POINTS
       !
       dddksi(nm) = dpd(nm) - dpd(nmd)
    enddo
    !
    ! exchange transformation coefficients with neighbours for parallel runs
    !
    nm_pos = 1
    call dfexchg ( dzdksi, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( dddeta, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( dzdeta, 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( dddksi, 1, 1, dfloat, nm_pos, gdp )
end subroutine dersig
