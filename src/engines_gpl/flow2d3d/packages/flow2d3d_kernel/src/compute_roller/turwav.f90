subroutine turwav(nmmax     ,kmax      ,kfs       , &
                & vicuv     ,dis       ,dps       ,s0        ,gdp       )
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
! Computes eddy viscosity as function of roller energy,
! phase velocity and wave height
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
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: thr
    logical                , pointer :: wave
    logical                , pointer :: htur2d
!
! Global variables
!
    integer                                       , intent(in) :: kmax  !  Description and declaration in esm_alloc_int.f90
    integer                                       , intent(in) :: nmmax !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in) :: kfs   !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,4), intent(in) :: dis   !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in) :: dps   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in) :: s0    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)     :: vicuv !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: khtur   ! denotes the k-index of vicuv/dicuv containing the Horizontal eddy values
    integer  :: nm
    real(fp) :: h
!
!! executable statements -------------------------------------------------------
!
    thr        => gdp%gdbetaro%thr
    rhow       => gdp%gdphysco%rhow
    wave       => gdp%gdprocs%wave
    htur2d     => gdp%gdprocs%htur2d
    !
    ! Initialization
    !
    khtur = kmax + 2
    !
    ! HORIZONTAL EDDY VISCOSITIES
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          h = max(0.01_fp, s0(nm) + real(dps(nm),fp))
          !
          ! check on HLES
          ! in that case wave generated visc. should be added
          !
          if (htur2d) then
            vicuv(nm, khtur) = vicuv(nm, khtur)+(dis(nm,2)/rhow)**(1.0/3.0)*h
          else
            vicuv(nm, khtur) = (dis(nm,2)/rhow)**(1.0/3.0)*h
          endif
          vicuv(nm, khtur) = max(vicuv(nm, khtur), thr)
       else
          vicuv(nm, khtur) = thr
       endif
    enddo
end subroutine turwav
