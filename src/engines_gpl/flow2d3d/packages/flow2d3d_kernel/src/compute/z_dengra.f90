subroutine z_dengra(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                  & icy       ,kfsz0     ,kfumin    ,kfumx0    ,kfvmin    , &
                  & kfvmx0    ,kfu       ,kfv       , &
                  & rho       ,gvu       ,guv       ,drhodx    , &
                  & drhody    ,dzu0      ,dzv0      ,gdp       )
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
! Computes densities from temperature, salinity and
! equation of state.
! Equation of state following Eckart, (C. Eckart,
! The equation of state of water and sea water at
! low temperatures and pressures, American Journal
! of Science, april 1958)
! Fixed Layer Approach
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
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
    integer                                         , intent(in) :: icx
    integer                                         , intent(in) :: icy
    integer                                                      :: j
    integer                                         , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer                                                      :: nmmaxj !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfvmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: drhodx !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             :: drhody !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: rho    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: iken
    integer :: ikenup
    integer :: k
    integer :: kup
    integer :: nm
    integer :: nmu
    integer :: num
!
!! executable statements -------------------------------------------------------
!
    !
    do nm = 1, nmmax
       nmu = nm + icx
       num = nm + icy
       if (kfu(nm) == 1) then
          if (kfumin(nm) <= kfumx0(nm)) then
             drhodx(nm, kfumx0(nm)) = 0.0_fp
             do k = kfumx0(nm), kfumin(nm), -1
                kup = k + 1
                if (k == kfumx0(nm)) then
                   iken          = kfsz0(nm, k)*kfsz0(nmu, k)
                   drhodx(nm, k) = iken*dzu0(nm, k)*0.5_fp*(rho(nmu, k) - rho(nm, k))  &
                                 & /gvu(nm)
                else
                   ikenup        = kfsz0(nm, kup)*kfsz0(nmu, kup)
                   iken          = kfsz0(nm, k)*kfsz0(nmu, k)
                   drhodx(nm, k) = drhodx(nm, kup) + ikenup*0.5_fp*dzu0(nm, kup)       &
                                 & *(rho(nmu, kup) - rho(nm, kup))/gvu(nm)         &
                                 & + iken*0.5_fp*dzu0(nm, k)*(rho(nmu, k) - rho(nm, k))&
                                 & /gvu(nm)
                endif
             enddo
          endif
       endif
       !
       if (kfv(nm) == 1) then
          if (kfvmin(nm) <= kfvmx0(nm)) then
             drhody(nm, kfvmx0(nm)) = 0.0_fp
             do k = kfvmx0(nm), kfvmin(nm), -1
                kup = k + 1
                if (k == kfvmx0(nm)) then
                   iken          = kfsz0(num, k)*kfsz0(nm, k)
                   drhody(nm, k) = iken*dzv0(nm, k)*0.5_fp*(rho(num, k) - rho(nm, k))  &
                                 & /guv(nm)
                else
                   ikenup        = kfsz0(nm, kup)*kfsz0(num, kup)
                   iken          = kfsz0(nm, k)*kfsz0(num, k)
                   drhody(nm, k) = drhody(nm, kup) + ikenup*0.5_fp*dzv0(nm, kup)       &
                                 & *(rho(num, kup) - rho(nm, kup))/guv(nm)         &
                                 & + iken*0.5_fp*dzv0(nm, k)*(rho(num, k) - rho(nm, k))&
                                 & /guv(nm)
                endif
             enddo
          endif
       endif
    enddo
end subroutine z_dengra
