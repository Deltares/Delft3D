subroutine rollu(nmmax     ,kfs       ,sourr     ,df        , &
               & sinkr     ,sinkw     ,ewave0    ,c         ,eroll0    , &
               & dis       ,tp        ,dps       ,s0        ,gdp       )
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
! Computes roller energy source and sink terms, as well as roller energy dissipation
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: betarol
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
!
! Global variables
!
    integer                                       , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,4), intent(out) :: dis    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: eroll0 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: ewave0 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(out) :: sinkr  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: sinkw  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: df     !  Description and declaration in esm_alloc_real.f90    
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(out) :: sourr  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)  , intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: nm
    real(fp) :: src
    real(fp) :: hrms
    real(fp) :: hdis
    real(fp) :: kwav
    real(fp) :: btr
!
!! executable statements -------------------------------------------------------
!
    betarol   => gdp%gdbetaro%betarol
    rhow      => gdp%gdphysco%rhow
    ag        => gdp%gdphysco%ag
    !
    do nm = 1, nmmax
       dis(nm,2) = 0.0_fp
       if (kfs(nm) /= 0) then
          if (c(nm) > 0.01_fp) then
             if (betarol < 0.0_fp) then
                hdis = max(0.1_fp , s0(nm)+real(dps(nm),fp))
                kwav = 2.0_fp * pi / (c(nm) * tp(nm))
                hrms = (8.0_fp * ewave0(nm) /(rhow * ag))**0.5_fp
                btr  = 0.025_fp * (1.0_fp/(kwav*hdis)) * ((hdis-hrms)/max(0.01_hp,real(hrms, hp)))**2
             else
                btr  = betarol
             endif
             sinkr(nm) = 2.0_fp * ag * btr / c(nm)
             if (eroll0(nm) > 0.01_fp) then
               dis(nm,2) = sinkr(nm) * eroll0(nm)
             endif
             if (ewave0(nm) < 0.0_fp) then
                src = 0.0_fp
             else
                src = max(0.0_fp , sinkw(nm)*ewave0(nm)-df(nm))
             endif
             sourr(nm) = src
          else
             sinkr(nm) = 0.0_fp
             sourr(nm) = 0.0_fp
          endif
       else
          sinkr(nm) = 0.0_fp
          sourr(nm) = 0.0_fp
       endif
       dis(nm,1) = dis(nm,2) + dis(nm,3) + dis(nm,4)
    enddo
end subroutine rollu
