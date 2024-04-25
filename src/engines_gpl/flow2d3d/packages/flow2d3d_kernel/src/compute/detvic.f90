subroutine detvic(lundia    ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                & icx       ,icy       ,kfs       ,kfu       , &
                & kfv       ,kcs       ,dps       ,s1        ,umean     , &
                & vmean     ,cvalu     ,cvalv     ,guv       ,gvu       , &
                & gsqs      ,ptke      ,vicuv     ,dicuv     , &
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
!    Function: Determines horizontal viscosity and
!              diffusivity according to "first2d.doc"
!              (R.E. Uittenboogaard 24-12-99)
!              NOTE: changes to incorporate HLES in DD
! Method used:
!     Comment: The HLES contribution to vicuv/dicuv is stored in array element
!              kmax+2, the background contribution in kmax+1
!              
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: nd
    real(fp)               , pointer :: sigmat
    real(fp)               , pointer :: flp
    real(fp)               , pointer :: gamma
    real(fp)               , pointer :: dicmol
    logical                , pointer :: elder
    real(fp)               , pointer :: drycrt
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: vicmol
!
! Global variables
!
    integer                                           , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                              !!  then computation proceeds in the X-
                                                                              !!  dir. If icx=1 then computation pro-
                                                                              !!  ceeds in the Y-dir.
    integer                                           , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j      !!  Begin pointer for arrays which have
                                                                              !!  been transformed into 1D arrays.
                                                                              !!  Due to the shift in the 2nd (M-)
                                                                              !!  index, J = -2*NMAX + 1
    integer                                           , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: lundia !  Description and declaration in inout.igs
    integer                                           , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: cvalu  !!  Chezy values in u-points
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: cvalv  !!  Chezy values in v-points
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: ptke   !!  TKE production due to velocity fluctuation
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)          :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)          :: vicuv  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer        :: ddb
    integer        :: icount
    integer        :: khtur
    integer        :: ken
    integer        :: kkfu
    integer        :: kkfv
    integer        :: m
    integer        :: n
    integer        :: ndm
    integer        :: nm
    integer        :: nmd
    logical        :: error
    real(fp)       :: b      ! Help var. See "first2d.doc" formula (1a) 
    real(fp)       :: chezy  ! Total chezy value in zeta point 
    real(fp)       :: depth  ! Total depth 
    real(fp)       :: drytrsh
    real(fp)       :: ks     ! Help var. See "first2d.doc" formula (3a) 
    real(fp)       :: sag
    real(fp)       :: ustar
    real(fp)       :: utot   ! Total mean velocity in zeta point 
    real(fp)       :: uuu    ! Mean velocity in zeta point 
    real(fp)       :: viceld ! Elder component of sub grid viscosity 
    real(fp)       :: vicmax
    real(fp)       :: vvv    ! Mean v-velocity in zeta point 
    character(256) :: errmsg
    integer        :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    nd        => gdp%gdhtur2d%nd
    sigmat    => gdp%gdhtur2d%sigmat
    flp       => gdp%gdhtur2d%flp
    gamma     => gdp%gdhtur2d%gamma
    dicmol    => gdp%gdhtur2d%dicmol
    elder     => gdp%gdhtur2d%elder
    drycrt    => gdp%gdnumeco%drycrt
    ag        => gdp%gdphysco%ag
    vonkar    => gdp%gdphysco%vonkar
    vicmol    => gdp%gdphysco%vicmol
    !
    ! Initialisation local variables
    !
    ddb     = gdp%d%ddbound
    icount  = 0
    error   = .false.
    sag     = sqrt(ag)
    drytrsh = drycrt
    nm_pos  = 1
    !
    ! The HLES contribution to vicuv/dicuv is stored in array element kmax+2
    ! The background contribution is stored in array element kmax+1
    !
    khtur  = kmax + 2
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          !
          ! determine B
          !
          nmd   = nm - icx
          ndm   = nm - icy
          kkfu  = max( 1 , kfu(nm)+kfu(nmd) )
          kkfv  = max( 1 , kfv(nm)+kfv(ndm) )
          uuu   = (umean(nmd)*kfu(nmd) + umean(nm)*kfu(nm)) / real(kkfu,fp)
          vvv   = (vmean(ndm)*kfv(ndm) + vmean(nm)*kfv(nm)) / real(kkfv,fp)
          utot  = sqrt(uuu*uuu + vvv*vvv)
          ken   = max( 1 , kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm) )
          !
          ! Limit Chezy to be larger than 1 to avoid division by zero in points that all inactive (although this should not be possible for kfs(nm)=1)
          !
          chezy = max( 1.0_fp , (kfu(nmd)*cvalu(nmd) + kfu(nm)*cvalu(nm) + kfv(ndm)*cvalv(ndm) + kfv(nm)*cvalv(nm)) / real(ken,fp) )
          depth = max( drytrsh , real(dps(nm),fp) + s1(nm) )
          b     = 0.75*ag*utot / (depth*chezy*chezy)
          !
          ! determine ks
          !
          ks = pi*flp/sqrt(gsqs(nm))
          !
          ! compute viscosity; bound VICUV maximum value allowed
          !
          if (kcs(nm) /= 3) then
             !
             ! not in coupling points because VICUV/DICUV is communicated
             ! by the mapper
             !
             vicuv(nm, khtur) = (sqrt(gamma*gamma*sigmat*sigmat*ptke(nm) + b*b) - b) / (ks*ks)
          endif
          !
          ! Add Elder to subgrid viscosity
          !
          if (kmax==1 .and. elder) then
             ustar  = utot*sag/chezy
             viceld = vonkar*ustar*depth/6.
             vicuv(nm, khtur) = vicuv(nm, khtur) + viceld
          endif
          !
          ! Compute diffusivity from viscosity
          ! Do not divide by Prandtl-Schmidt number here! 
          ! (Once is enough; see TURCLO.f90)
          !
          dicuv(nm, khtur) = vicuv(nm, khtur)
          !
          ! Add vicmol and dicmol to viscosity and diffusivity
          ! For 3D simulations, vicmol and dicmol are added in turclo.f90, via the vertical eddy viscosity/diffusivity
          ! coming from the vertical turbulence model
          !
          if (kmax == 1) then
             vicuv(nm, khtur) = vicuv(nm, khtur) + vicmol
             dicuv(nm, khtur) = dicuv(nm, khtur) + dicmol
          endif
       else
          vicuv(nm, khtur) = 0.0
          dicuv(nm, khtur) = 0.0
       endif
    enddo
    !
    ! exchange horizontal viscosity and diffusivity with neighbours for parallel runs
    !
    call dfexchg ( vicuv(:,khtur), 1, 1, dfloat, nm_pos, gdp )
    call dfexchg ( dicuv(:,khtur), 1, 1, dfloat, nm_pos, gdp )
end subroutine detvic
