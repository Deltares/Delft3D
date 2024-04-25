subroutine caltmx(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,zmodel    ,kfu       ,kfv       ,kfs       , &
                & kfuz1     ,kfvz1     ,kfsmin    ,taubxu    ,taubxv    , &
                & taubmx    ,hu        ,hv        ,dps       ,s1        , &
                & gdp       )
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
!    Function: Define Tau_max in zeta points using U- and V-
!              components calculated in TAUBOT
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
    integer                                           , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                           , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    logical                                           , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(out) :: taubmx !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: taubxu !!  Primary maximal bottom friction term in the x-dir. (velocity dependent)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: taubxv !!  Primary maximal bottom friction term in the y-dir. (velocity dependent)
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer    :: kfuv  ! Maximum for KFU for NM and NMD and for KFV NM and NDM 
    integer    :: kmin
    integer    :: ndm   ! NM - ICY 
    integer    :: nm    ! Index number 
    integer    :: nmd   ! NM - ICX 
    real(fp)   :: h1    ! water depth in cell centre 
    integer    :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    ! Calculate TAU_MAX in zeta points                           |nm
    !     Scalar adding of four velocity point components  nmd - + - nm
    !                                                            |ndm
    if (.not.zmodel) then
       ndm = -icy
       nmd = -icx
       do nm = 1, nmmax
          h1         = max(0.01_fp, s1(nm)+real(dps(nm),fp))
          ndm        = ndm + 1
          nmd        = nmd + 1
          kfuv       = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
          taubmx(nm) = (taubxu(nm)*kfu(nm)*hu(nm) + taubxu(nmd)*kfu(nmd)*hu(nmd) + &
                     &  taubxv(nm)*kfv(nm)*hv(nm) + taubxv(ndm)*kfv(ndm)*hv(ndm))/kfuv
          taubmx(nm) = taubmx(nm)/h1
       enddo
    else
       do nm = 1, nmmax
          ndm = nm - icy
          nmd = nm - icx
          if (kfs(nm)==1) then
             kmin = kfsmin(nm)
             kfuv = max(1, kfuz1(nm, kmin) + kfuz1(nmd, kmin) + kfvz1(nm, kmin) + kfvz1(ndm, kmin))
             taubmx(nm) = ( taubxu(nm)*kfuz1(nm, kmin) + taubxu(nmd)*kfuz1(nmd, kmin) &
                        & + taubxv(nm)*kfvz1(nm, kmin) + taubxv(ndm)*kfvz1(ndm, kmin) )/kfuv
          endif
       enddo
    endif
    !
    ! exchange taubmx with neighbours for parallel runs
    !
    nm_pos = 1
    call dfexchg ( taubmx, 1, 1, dfloat, nm_pos, gdp )
end subroutine caltmx
