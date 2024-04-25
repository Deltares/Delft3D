subroutine cdwkad(nmmax     ,kmax      ,zmodel    ,kspu      ,kfsmax    , &
                & kfsmin    ,kfumax    ,kfumin    ,sig       ,thick     , &
                & zk        ,zktop     ,zkbot     ,dzk       , &
                & dpu       ,hu        ,dzu1      ,porosu    ,ubrlsu    , &
                & cdwztu    ,cdwzbu    ,cdwlsu    ,gdp    )
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
! Computes POROSU/V and UBRLSU/V values for gates with fixed position
!
! Assumption: maximum of one structure per cell
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
    real(fp) , pointer :: dzmin
    real(fp) , pointer :: zbot
!
! Global variables
!
    integer                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(out) :: kspu   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(0:kmax)                       , intent(in)  :: zk     !  Array sig with z-model information
    real(fp), dimension(kmax)                         , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                         , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                       :: dzk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: porosu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ubrlsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: cdwlsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: cdwztu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: cdwzbu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                       :: zktop  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                       :: zkbot  !  Description and declaration in esm_alloc_real.f90
    logical                                           , intent(in)  :: zmodel !  Description and declaration in procs.igs
!
! Local variables
!
    integer :: k     ! Loop counter for loop 1,KMAX
    integer :: kk    ! Loop counter for loop 1,KMAX
    integer :: nm    ! Loop counter for loop 1,NMMAX
    integer :: kup  
    integer :: kdown
    integer :: kfrst
    integer :: klast
    integer :: kstep
!
!! executable statements -------------------------------------------------------
!
    dzmin => gdp%gdzmodel%dzmin
    zbot  => gdp%gdzmodel%zbot
!
! POROSU(NM,K) is initialized to 1.0 for whole model domain 
! in case of a CDW (KSPU/KSPV (NM,0) = 10) UBRLSU(NM,K) is initialized to 0.0
! 
do nm = 1, nmmax
   do k = 1,kmax
      zktop(k) = 0.0
      zkbot(k) = 0.0
      dzk(k)   = 0.0
      porosu(nm, k) = 1.0
      if (abs(kspu(nm,0))==10) then
         kspu  (nm, k) = 0 
      endif    
   enddo
   kfrst = 1
   klast = kmax
   kstep = 1
   if (zmodel) then
      kfrst = kfumax(nm)
      klast = kfumin(nm)
      kstep = -1
   endif
   if (abs(kspu(nm,0))==10) then
      do k = kfrst, klast, kstep
         !
         ! Determine the vertical position of the top of layer k
         ! taking into account the free surface
         !
         if (zmodel) then
            if (k == kfrst) then
               zktop (k) = hu(nm)
            else
               zktop (k) = dpu(nm)+zk(k)
            endif
            dzk(k) = dzu1(nm,k)
         else
            zktop (k) = (1.0 + sig(k) + 0.5*thick(k))*hu(nm)
            dzk (k)   = thick(k)*hu(nm)
         endif
         !
         ! Determine the vertical position of the bottom of layer k
         ! taking into account the bed level
         !
         if (zmodel) then
            if (k == 1) then
               zkbot (k) = dpu(nm) + zbot
            else
               zkbot (k) = dpu(nm) + zk(k-1)
            endif
         else
            zkbot (k) = (1.0 + sig(k) - 0.5*thick(k))*hu(nm)
         endif
         if (cdwztu(nm)>=zktop(k)-dzmin .and. cdwzbu(nm)<= zkbot(k)+dzmin) then
            kspu(nm, k)   = 1
            porosu(nm, k) = 0.0
         endif
      enddo
      !
      ! The formulation to determine the porosity factor below requires 
      ! at least 1 vertical cell to be closed (KSPU == 1). Implication: 
      ! Gate with a height < the thicknes of 1 layer can not be modelled
      !
      do k = kfrst,klast,kstep
         if (zmodel) then
            kup   = min(k+1,kfumax(nm))
            kdown = max(k-1,kfumin(nm))
         else
            kup   = max(k-1,1)
            kdown = min(k+1,kmax)
         endif
         if (kspu(nm,k)==0 .and. kspu(nm,kdown)==1) then
            porosu(nm,k) = zktop(k)- cdwztu(nm)
            if (porosu(nm,k) > 0) then
               porosu(nm,k) = porosu(nm,k) / dzk(k)
               porosu(nm,k) = min(1.0_fp,porosu(nm,k))
            else
               porosu(nm,k) = 0.0
            endif
         elseif (kspu(nm,k)==0 .and. kspu(nm,kup)==1) then
            porosu(nm,k) = cdwzbu(nm) - zkbot(k)
            do kk = k,klast,kstep
               ubrlsu(nm,kk) = cdwlsu(nm)
            enddo
            if (porosu(nm,k) > 0.0) then
               porosu(nm,k) = porosu(nm,k) / dzk(k)
               porosu(nm,k) = min(1.0_fp,porosu(nm,k))
            else
               porosu(nm,k) = 0.0
            endif
         else
            ! nothing
         endif
      enddo
   endif
enddo
end subroutine cdwkad
