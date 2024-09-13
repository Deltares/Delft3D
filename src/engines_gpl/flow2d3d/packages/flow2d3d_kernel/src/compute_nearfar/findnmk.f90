subroutine findnmk(xz     ,yz     ,dps    ,s1     ,kcs    ,nmmax  ,thick   , &
                 & kmax   , x_jet ,y_jet  ,z_jet  ,nm_jet ,k_jet  ,gdp     )
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
!  $Id: findnmk.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/findnmk.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Finds n,m and k coordinates of "jet" points
!
! Method used:
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
! Global variables
!
    integer                                       , intent(in)  :: kmax   !  Description and declaration in tricom.igs
    integer                                       , intent(in)  :: nmmax  !  Description and declaration in tricom.igs
    integer                                       , intent(out) :: nm_jet
    integer                                       , intent(out) :: k_jet
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: kcs    !  Description and declaration in
    real(fp)                                      , intent(in)  :: x_jet
    real(fp)                                      , intent(in)  :: y_jet
    real(fp)                                      , intent(in)  :: z_jet
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90 gs
    real(fp)   , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                  , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90 gs
    real(prec) , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: k
    integer       :: nm
    real(fp)      :: dist
    real(fp)      :: distmin
    real(fp)      :: r_onder
    real(fp)      :: r_boven
!
!! executable statements -------------------------------------------------------
!
    nm_jet  = 0
    distmin = 1.0e+30_fp
    do nm = 1, nmmax
       if (kcs(nm) == 1) then
          dist = sqrt((xz(nm) - x_jet)**2 + (yz(nm) - y_jet)**2)
          if (dist < distmin) then
             nm_jet  = nm
             distmin = dist
          endif
       endif
    enddo
    !
    k_jet   = 0
    r_boven = -1.0_fp * s1(nm_jet)
    do k = 1, kmax
       r_onder = r_boven + thick(k)*(real(dps(nm_jet),fp) + s1(nm_jet))
       if (z_jet < r_onder .and. z_jet >= r_boven) then
          k_jet = k
          exit
       endif
       r_boven = r_onder
    enddo
    !
end subroutine findnmk
