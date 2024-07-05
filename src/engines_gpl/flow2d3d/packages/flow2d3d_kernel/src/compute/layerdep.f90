subroutine layerdep(dep   , thick, kmax, nm    , dpd   , wlev, &
                  & zmodel, zk   , dzu1, kfumin, kfumax, gdp)
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
! Calculate depth of all layers
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
!
! Global variables
!
    integer                                         , intent(in)  :: kmax
    integer                                         , intent(in)  :: nm
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(0:kmax+1)                   , intent(out) :: dep    !  Depth of layers
    real(fp), dimension(kmax)                       , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dpd
    real(fp)                                        , intent(in)  :: wlev
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                     , intent(in)  :: zk
    logical                                         , intent(in)  :: zmodel
!
! Local variables
!
    integer  :: k
    real(fp) :: tmpval
!
!! executable statements -------------------------------------------------------
!
    ! dep(0) always denotes the extrapolation region at lower side
    ! dep(0) = 0 for both sigma and zmodel
    !
    dep(0) = 0.0_fp
    !
    ! dep(kmax+1) always denotes the extrapolation region at upper side
    ! dep(kmax+1) = total water depth for both sigma and zmodel
    !
    dep(kmax+1) = dpd(nm) + wlev
    !
    if (zmodel) then
       do k = 1, kmax
          if (k < kfumin(nm)) then
             dep(k) = -zk(kfumin(nm)) + dzu1(nm, kfumin(nm))
          elseif (k == kfumax(nm)) then
             dep(k) = 0.5_fp * dzu1(nm, k)
          elseif (k > kfumax(nm)) then
             dep(k) = 0.0_fp
          else
             dep(k) = -zk(k) + 0.5_fp * dzu1(nm, k)
          endif
       enddo
    else
       tmpval = 0.5_fp   * (dpd(nm) + wlev)
       dep(1) = thick(1) * tmpval
       do k = 2, kmax
          dep (k) = dep(k-1) + (thick(k)+thick(k-1))*tmpval
       enddo
    endif
end subroutine layerdep
