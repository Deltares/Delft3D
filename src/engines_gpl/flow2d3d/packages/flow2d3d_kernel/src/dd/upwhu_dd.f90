subroutine upwhu_dd(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                  & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
                  & s0        ,dpu       ,umean     ,hu        ,gdp       )
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
! Switch that is identical to UPWHU but applicable
! only to subdomain interfaces. UPWHU is sometimes
! called by passing KFU instead of KCU. At domain
! interface the use of KCU is imperative. Therefore
! it was necessary to write a separate routine.
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
    real(fp)     , pointer :: dco
    character(8) , pointer :: dpuopt
!
! Global variables
!
    integer                                          , intent(in)  :: icx
    integer                                                        :: j
    integer                                                        :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                          , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                        :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: kspu   !  Description and declaration in esm_alloc_int.f90
    logical                                          , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(out) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: nm  ! Counter for loop 1,NMMAX
    integer :: nmd
    integer :: nmu
!
!! executable statements -------------------------------------------------------
!
    dco     => gdp%gdnumeco%dco
    dpuopt  => gdp%gdnumeco%dpuopt
    !
    ! In case DPUOPT = 'UPW' apply upwind approach for for the total
    ! water depth. This implies that upwind also applies for DPU (both
    ! in SIGMA and Z-model)
    ! Reference: "A staggered conservative scheme for every Froude
    !             number in rapidly varied shallow water flows"; by
    !             GS Stelling and SPA Duijnmeijer
    !
    ! Apply this only at the subdomain interface:
    !
    ! KCU at Domain  I: ------------ [1/3] 3
    ! KCU at Domain II:            3 [1/3] ---------------
    !
    !                                  ^
    !                                  | subdomain interface
    !
    if (dpuopt=='UPW') then
       do nm = 1, nmmax
          nmd = nm - icx
          nmu = nm + icx
          if (kcu(nm)==3 .and. kcs(nm)*kcs(nmu)==3) then
             if (umean(nm)>=0.001) then
                dpu(nm) = real(dps(nm),fp)
             elseif (umean(nm)<= - 0.001) then
                dpu(nm) = real(dps(nmu),fp)
             else
                dpu(nm) = min(real(dps(nmu),fp), real(dps(nm),fp))
             endif
          endif
       enddo
    endif
    !
    ! Apply Upwind of water level if:
    !  -  HU(initial) < DCO (in Z-model not yet tested) OR
    !  -  KSPU        > 0   (in Z-model not yet tested) OR
    !  -  DPUOPT      = 'UPW'
    !
    ! Start by computing HU(initial) and see whether it is < DCO value
    ! and upwind should be applied (one of the criteria above).
    !
    ! In SIGMA case HU(initial) is computed as in WAQUA code
    ! In Z-model HU(initial) is computed conform the reference.
    !
    do nm = 1, nmmax
       nmd = nm - icx
       nmu = nm + icx
       if (kcu(nm)==3) then
          if (kcs(nm)*kcs(nmu)==3) then
             !
             ! at subdomain interface
             !
             hu(nm) = 0.5*(s0(nm) + s0(nmu)) + dpu(nm)
             if (hu(nm)<dco .or. kspu(nm, 0)>0 .or. dpuopt=='UPW') then
                if (umean(nm)>=0.001) then
                   hu(nm) = s0(nm) + dpu(nm)
                elseif (umean(nm)<= - 0.001) then
                   hu(nm) = s0(nmu) + dpu(nm)
                else
                   hu(nm) = max(s0(nmu), s0(nm)) + dpu(nm)
                endif
             endif
          elseif (kcs(nm)==0 .and. kcs(nmu)==3) then
             !
             ! cell is left of the interface
             !
             hu(nm) = s0(nmu) + dpu(nm)
          elseif (kcs(nmd)==1 .and. kcs(nm)==3) then
             !
             ! cell is right of the interface
             !
             hu(nm) = s0(nm) + dpu(nm)
          else
             hu(nm) = s0(nm) + dpu(nm)
          endif
          !
          ! correct for positive values
          !
          if (hu(nm) < 1.0e-3 ) then
             hu(nm) = 1.0e-3
          endif
       endif
    enddo
end subroutine upwhu_dd
