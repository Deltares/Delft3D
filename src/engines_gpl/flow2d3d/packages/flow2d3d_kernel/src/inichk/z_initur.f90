subroutine z_initur(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                  & icy       ,ltur      ,lturi     ,kfu       ,kfv       , &
                  & kfs       ,kcs       ,kfumin    ,kfumax    ,kfvmin    , &
                  & kfvmax    ,kfsmin    ,kfsmax    ,rtur0     , &
                  & s0        ,dps       ,u0        ,v0        ,windu     , &
                  & windv     ,z0urou    ,z0vrou    ,dudz      ,dvdz      , &
                  & dzu1      ,dzv1      ,dzs1      ,bruvai    ,rich      , &
                  & rho       ,gdp       )
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
!    Function: Initialises turbulent energy and/or
!              turbulent dissipation FOR ZMODEL ONLY
! Method used: Reference: R.E. Uittenbogaard, J.A.Th.M. van
!              Kester, G.S. Stelling, Implementation of three
!              turbulence models in 3D-FLOW for rectangular
!              grids, Deltares report Z81, april 1992)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall
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
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: z0v
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: cde
    real(fp)               , pointer :: cmukep
    real(fp)               , pointer :: cewall
    real(fp)               , pointer :: zwi
    integer                , pointer :: inpzw
!
! Global variables
!
    integer                                                   , intent(in) :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                                     !!  then computation proceeds in the X-dir.
                                                                                     !!  If icx=1 then computation proceeds
                                                                                     !!  in the Y-dir.
    integer                                                   , intent(in) :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                                :: j      !!  Begin pointer for arrays which have
                                                                                     !!  been transformed into 1D arrays.
                                                                                     !!  Due to the shift in the 2nd (M-)
                                                                                     !!  index, J = -2*NMAX + 1
    integer                                                   , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in) :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in) :: lturi  !  Description and declaration in tricom.igs
    integer                                                   , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: windu  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: windv  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                   :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                   :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                   :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                   :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)             :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: dzu1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: dzv1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: v0     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                               :: k
    integer                               :: kmin
    integer                               :: kup
    integer                               :: maskval
    integer                               :: ndm
    integer                               :: nm
    integer                               :: nmd
    integer                               :: nmu
    integer                               :: num
    integer                               :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp)                              :: damping
    real(fp)                              :: drhodz
    real(fp)                              :: dz
    real(fp)                              :: ene
    real(fp)                              :: h0
    real(fp)                              :: reldik
    real(fp)                              :: rl
    real(fp)                              :: shear
    real(fp)                              :: tke
    real(fp)                              :: ust
    real(fp)                              :: utot
    real(fp)                              :: uuu
    real(fp)                              :: vvv
    real(fp), dimension(:)  , allocatable :: z0scratch
    real(fp)                              :: z00
    real(fp)                              :: zw
    real(fp)                              :: zwc
!
!! executable statements -------------------------------------------------------
!
    cde         => gdp%gdturcoe%cde
    cmukep      => gdp%gdturcoe%cmukep
    cewall      => gdp%gdturcoe%cewall
    zwi         => gdp%gdturcoe%zwi
    inpzw       => gdp%gdturcoe%inpzw
    rhow        => gdp%gdphysco%rhow
    ag          => gdp%gdphysco%ag
    z0          => gdp%gdphysco%z0
    z0v         => gdp%gdphysco%z0v
    vonkar      => gdp%gdphysco%vonkar
    !
    nm_pos = 1
    !
    allocate(z0scratch(gdp%d%nmlb:gdp%d%nmub)) 
    !
    nmd = -icx
    ndm = -icy
    do nm = 1, nmmax
       nmd = nmd + 1
       ndm = ndm + 1
       if (kfs(nm)==1) then
          z0scratch(nm) = (  kfu(nm)*z0urou(nm) + kfu(nmd)*z0urou(nmd)   &
                        &  + kfv(nm)*z0vrou(nm) + kfv(ndm)*z0vrou(ndm) ) &
                        & / max(1,kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
       endif
    enddo
    !
    !***PRODUCTION AND BUOYANCY TERM (ONLY VERTICAL GRADIENTS)
    !
    if (lturi>0) then
       do nm = 1, nmmax
          if (kfu(nm)==1) then
             do k = 1, kmax
                if (k>=kfumin(nm) .and. k<=kfumax(nm)-1) then
                   kup         = k + 1
                   dz          = 0.5_fp*(dzu1(nm, k) + dzu1(nm, kup))
                   dudz(nm, k) = (u0(nm, kup) - u0(nm, k))/dz
                else
                   dudz(nm, k) = 0.0_fp
                endif
             enddo
          endif
       enddo
       do nm = 1, nmmax
          if (kfv(nm)==1) then
             do k = 1, kmax
                if (k>=kfvmin(nm) .and. k<=kfvmax(nm)-1) then
                   kup         = k + 1
                   dz          = 0.5_fp*(dzv1(nm, k) + dzv1(nm, kup))
                   dvdz(nm, k) = (v0(nm, kup) - v0(nm, k))/dz
                else
                   dvdz(nm, k) = 0.0_fp
                endif
             enddo
          endif
       enddo
       !
       !***KOLMOGOROV-PRANDTL MIXING LENGTH MODEL
       !
       do nm = 1, nmmax
          if (kfs(nm)==1) then
             nmd = nm - icx
             ndm = nm - icy
             h0  = max(0.01_fp, real(dps(nm),fp)+s0(nm))
             if (kcs(nm)==3) then
                maskval = kcs(nm) - 2
             else
                maskval = kcs(nm)
             endif
             reldik = 0.
             do k = kfsmin(nm), kfsmax(nm)-1
                kup = k + 1
                dz     = 0.5_fp*(dzs1(nm, k) + dzs1(nm, kup))
                drhodz = (rho(nm, kup) - rho(nm, k))/dz
                bruvai(nm, k) = -ag*drhodz/rho(nm, k)
                !
                ! Mixing length
                !
                reldik = reldik + dzs1(nm, k)/h0
                rl     = vonkar * h0 * reldik * sqrt(1.0_fp-reldik)
                !
                !***INITIALISATION TKE and EPSILON from PRANDTL MIXING LENGTH MODEL
                !
                shear = maskval*0.5_fp *(  dudz(nm, k)**2 + dudz(nmd, k)**2  &
                      &                  + dvdz(nm, k)**2 + dvdz(ndm, k)**2 )
                shear = max(1e-8_fp, shear)
                rich  (nm, k) = bruvai(nm, k)/shear
                if (rich(nm,k) >= 0.0_fp) then
                    damping = exp(-2.3_fp*min(rich(nm,k), 30.0_fp))
                else
                    damping = (1.0_fp - 14.0_fp*rich(nm,k))**0.25_fp
                endif
                rl = rl*damping
                !
                ene   = maskval*0.25_fp*(  u0(nm, k)**2 + u0(nmd, k)**2  &
                      &                  + v0(nm, k)**2 + v0(ndm, k)**2 )
                tke   = min(ene, rl*rl*shear/sqrt(cmukep))
                rtur0(nm, k, 1) = tke
             enddo
          endif
       enddo
       !
       !***TKE AT FREE SURFACE AND BOTTOM
       !
       nmd = -icx
       ndm = -icy
       do nm = 1, nmmax
          nmd  = nmd + 1
          ndm  = ndm + 1
          kmin = kfsmin(nm)
          if (kfs(nm)==1) then
             h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
             if (kcs(nm)==3) then
                maskval = kcs(nm) - 2
             else
                maskval = kcs(nm)
             endif
             !
             !***AT FREE SURFACE
             !
             tke = sqrt((windu(nm)**2 + windv(nm)**2)/cmukep)/rhow
             rtur0(nm, kfsmax(nm), 1) = tke
             !
             !***AT BOTTOM
             !
             z00  = z0scratch(nm)
             uuu  = 0.5_fp*(u0(nmd, kmin) + u0(nm, kmin))*maskval
             vvv  = 0.5_fp*(v0(ndm, kmin) + v0(nm, kmin))*maskval
             utot = sqrt(uuu*uuu + vvv*vvv)
             ust  = utot*vonkar/log(1.0_fp + dzs1(nm, kmin)/z00)
             tke  = ust*ust/sqrt(cmukep)
             rtur0(nm, kmin - 1, 1) = tke
          endif
       enddo
    endif
    !
    !***KOLMOGOROV-PRANDTL MIXING LENGTH MODEL FOR EPS
    !
    if (ltur>1) then
       do nm = 1, nmmax
          if (kfs(nm)==1) then
             nmd    = nm - icx
             ndm    = nm - icy
             h0     = max(0.01_fp, real(dps(nm),fp) + s0(nm))
             reldik = 0.0_fp
             do k = kfsmin(nm), kfsmax(nm)-1
                !
                !***MIXING LENGTH
                !
                reldik = reldik + dzs1(nm, k)/h0
                rl     = vonkar * h0 * reldik * sqrt(1.0_fp-reldik)
                if (rich(nm,k) >= 0.0_fp) then
                    damping = exp(-2.3_fp*min(rich(nm,k), 30.0_fp))
                else
                    damping = (1.0_fp - 14.0_fp*rich(nm,k))**0.25_fp
                endif
                rl = rl*damping
                !
                !***INITIALISATION EPSILON from PRANDTL MIXING LENGTH MODEL
                !
                rtur0(nm, k, 2) = max(1.E-7_fp, cde*(rtur0(nm, k, 1))**1.5_fp/rl)
             enddo
          endif
       enddo
       !
       !***EPS AT FREE SURFACE AND BOTTOM
       !
       do nm = 1, nmmax
          kmin = kfsmin(nm)
          if (kfs(nm)==1) then
             h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
             !
             !***AT FREE SURFACE
             !
             zwc = 0.5_fp * dzs1(nm, kfsmax(nm))
             zw  = inpzw*zwi + (1-inpzw)*zwc
             rtur0(nm, kfsmax(nm), 2) = max(1.E-7_fp, cewall*(rtur0(nm, kfsmax(nm),1))**1.5_fp/zw)
             !
             !***AT BOTTOM
             !
             z00 = z0scratch(nm)
             rtur0(nm, kmin-1, 2) = max(1.E-7_fp, cewall*(rtur0(nm, kmin-1, 1))**1.5_fp/z00)
          endif
       enddo
    endif
    !
    if (lturi>0) then
       do k = 0, kmax
          do nm = 1, nmmax
             rtur0(nm, k, 1) = max(1.E-7_fp, rtur0(nm, k, 1))
          enddo
       enddo
    endif
    !
    ! For domaindecomposition (in couple points)
    !
    do k = 0, kmax
       do nm = 1, nmmax
          if (kcs(nm)==3) then
             rtur0(nm, k, 1) = 1.E-7_fp
             rtur0(nm, k, 2) = 1.E-7_fp
          endif
       enddo
    enddo
    !
    deallocate(z0scratch)
    !
    ! Delft3D-16494: NOT NECESSARY? No implicit loop was performed
    !
    ! exchange turbulence values with neighbours for parallel runs
    !
    call dfexchg ( rtur0(:,:,1), 0, kmax, dfloat, nm_pos, gdp )
    if (ltur == 2) then
       call dfexchg ( rtur0(:,:,2), 0, kmax, dfloat, nm_pos, gdp )
    endif
    !
end subroutine z_initur
