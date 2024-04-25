subroutine orbvel(nmmax     ,kfs       ,dps       ,ubot      , &
                & s1        ,rlabda    ,tp        ,ewave1    ,uorb      , &
                & hrms      ,gdp       )
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
! NONE
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
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    logical                , pointer :: ubot_from_com
    logical                , pointer :: wlen_from_com
!
! Global variables
!
    integer                                     , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: ewave1 !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: rlabda !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              :: ubot   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: hrms   !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: nm
    real(fp) :: e
    real(fp) :: h
    real(fp) :: hk
    real(fp) :: omega
    real(fp) :: k0
    real(fp) :: k0h
    real(fp) :: k
!
!! executable statements -------------------------------------------------------
!
    rhow            => gdp%gdphysco%rhow
    ag              => gdp%gdphysco%ag
    ubot_from_com   => gdp%gdprocs%ubot_from_com
    wlen_from_com   => gdp%gdprocs%wlen_from_com
    !
    do nm = 1, nmmax
       if (kfs(nm)/=0) then
          !
          h = max(0.01_fp, s1(nm) + real(dps(nm),fp))
          e = ewave1(nm)
          !
          if (e>1.0e-2 .and. tp(nm)>1.0e-2 .and. rlabda(nm)>1.0e-2) then
             !
             ! Wave height          
             !          
             hrms(nm) = sqrt(8.0_fp*e/rhow/ag)
             !
             ! Wave length          
             !          
             if (.not.wlen_from_com) then
                omega      = 2.0_fp*pi/tp(nm)
                k0         = omega*omega/ag
                k0h        = k0*h
                if (k0h>pi) then
                   k = k0
                elseif (k0h<0.005) then
                   k = omega/sqrt(ag*h)
                else
                   call wavenr(h        ,tp(nm)       ,k         ,ag        )
                endif
                rlabda(nm) = 2.0_fp*pi/k
             endif
             !
             ! Orbital velocity          
             !          
             if (ubot_from_com) then
                uorb(nm) = ubot(nm)
             else
                hk = 2.0_fp*pi/rlabda(nm)*h
                hk = min(100.0_fp, hk)
                uorb(nm) = pi*hrms(nm)/tp(nm)/sinh(hk)
             endif
          else
             uorb(nm) = 0.0_fp
             hrms(nm) = 0.0_fp
          endif
       else
          uorb(nm) = 0.0_fp
          hrms(nm) = 0.0_fp
       endif
    enddo
end subroutine orbvel
