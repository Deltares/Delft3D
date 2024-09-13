subroutine initur(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,ltur      ,lturi     ,rtur0     , &
                & s0        ,dps       ,hu        ,hv        ,u0        , &
                & v0        ,thick     ,windsu    ,windsv    ,z0urou    , &
                & z0vrou    ,kfu       ,kfv       ,kfs       ,kcs       , &
                & dudz      ,dvdz      ,gdp       )
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
!  $Id: initur.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/inichk/initur.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Initialises turbulent energy and/or
!              turbulent dissipation
! Method used: Reference: R.E. Uittenbogaard, J.A.Th.M. van
!              Kester, G.S. Stelling, Implementation of three
!              turbulence models in 3D-FLOW for rectangular
!              grids, Deltares report Z81, april 1992)
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
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: z0v
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: cde
    real(fp)               , pointer :: cmukep
    real(fp)               , pointer :: cewall
    real(fp)               , pointer :: zwi
    real(fp)               , pointer :: ck
    integer                , pointer :: inpzw
!
! Global variables
!
    integer                                                   , intent(in) :: icx    !!  Increment in the X-dir., if ICX= NMAX then 
                                                                                     !!  computation proceeds in the X-dir. 
                                                                                     !!  If icx=1 then computation proceeds in the Y-dir.
    integer                                                   , intent(in) :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                                :: j      !!  Begin pointer for arrays which have been 
                                                                                     !!  transformed into 1D arrays. Due to the shift 
                                                                                     !!  in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                   , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in) :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in) :: lturi  !  Description and declaration in tricom.igs
    integer                                                   , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: windsv !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in) :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                   :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                   :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)             :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in) :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                               , intent(in) :: thick  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                               :: k
    integer                               :: kup
    integer                               :: maskval
    integer                               :: ndm
    integer                               :: nm
    integer                               :: nmd
    integer                               :: nm_pos    ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp)                              :: dz
    real(fp)                              :: ene
    real(fp)                              :: h0
    real(fp)                              :: rl
    real(fp)                              :: shear
    real(fp)                              :: sigw
    real(fp)                              :: tke
    real(fp)                              :: tsg
    real(fp)                              :: ust
    real(fp)                              :: utot
    real(fp)                              :: uuu
    real(fp)                              :: vvv
    real(fp), dimension(:,:), allocatable :: z0scratch
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
    ck          => gdp%gdturcoe%ck
    inpzw       => gdp%gdturcoe%inpzw
    rhow        => gdp%gdphysco%rhow
    z0          => gdp%gdphysco%z0
    z0v         => gdp%gdphysco%z0v
    vonkar      => gdp%gdphysco%vonkar
    !
    nm_pos      =  1
    !
    allocate(z0scratch(gdp%d%nmlb:gdp%d%nmub, kmax))
    !
    nmd = -icx
    ndm = -icy
    do nm = 1, nmmax
       nmd = nmd + 1
       ndm = ndm + 1
       if (kfs(nm)==1) then
          z0scratch(nm, kmax) = (  kfu(nm)*z0urou(nm) + kfu(nmd)*z0urou(nmd)   &
                              &  + kfv(nm)*z0vrou(nm) + kfv(ndm)*z0vrou(ndm) ) &
                              &  /(kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
       endif
    enddo
    !
    !***PRODUCTION AND BUOYANCY TERM (ONLY VERTICAL GRADIENTS)
    !
    if (lturi>0) then
       do k = 1, kmax-1
          kup = k + 1
          tsg = 0.5*(thick(k) + thick(kup))
          do nm = j, nmmax
             dudz(nm, k) = 0.0
             if (kfu(nm)==1) then
                dudz(nm, k) = (u0(nm, k) - u0(nm, kup))/(hu(nm)*tsg)
             endif
             dvdz(nm, k) = 0.0
             if (kfv(nm)==1) then
                dvdz(nm, k) = (v0(nm, k) - v0(nm, kup))/(hv(nm)*tsg)
             endif
          enddo
       enddo
       !
       !***KOLMOGOROV-PRANDTL MIXING LENGTH MODEL
       !
       sigw = 0.0
       do k = 1, kmax-1
          sigw = sigw - thick(k)
          nmd = -icx
          ndm = -icy
          do nm = 1, nmmax
             nmd = nmd + 1
             ndm = ndm + 1
             if (kfs(nm)==1) then
                h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
                if (kcs(nm)==3) then
                   maskval = kcs(nm) - 2
                else
                   maskval = kcs(nm)
                endif
                !
                !***MIXING LENGTH
                !
                rl = vonkar*h0*(1. + sigw)*sqrt( - sigw)
                !
                !***INITIALISATION TKE and EPSILON from PRANDTL MIXING LENGTH MODEL
                !
                shear = maskval*0.5 *(  dudz(nm, k)**2 + dudz(nmd, k)**2   &
                      &               + dvdz(nm, k)**2 + dvdz(ndm, k)**2 )
                ene   = maskval*0.25*(  u0(nm, k)**2 + u0(nmd, k)**2       &
                      &               + v0(nm, k)**2 + v0(ndm, k)**2 )
                tke   = min(ene, rl*rl*shear/sqrt(cmukep))
                rtur0(nm, k, 1) = tke
             endif
          enddo
       enddo
       !
       !***TKE AT FREE SURFACE AND BOTTOM
       !
       nmd = -icx
       ndm = -icy
       do nm = 1, nmmax
          nmd = nmd + 1
          ndm = ndm + 1
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
             tke = sqrt((windsu(nm)**2 + windsv(nm)**2)/cmukep)/rhow
             rtur0(nm, 0, 1) = tke
             !
             !***AT BOTTOM
             !
             z00  = z0scratch(nm, kmax)
             uuu  = 0.5*(u0(nmd, kmax) + u0(nm, kmax))*maskval
             vvv  = 0.5*(v0(ndm, kmax) + v0(nm, kmax))*maskval
             utot = sqrt(uuu*uuu + vvv*vvv)
             dz   = 0.5*thick(kmax)*h0
             ust  = utot*vonkar/log(1. + dz/z00)
             tke  = ust*ust/sqrt(cmukep)
             rtur0(nm, kmax, 1) = tke
          endif
       enddo
    endif
    !
    !***KOLMOGOROV-PRANDTL MIXING LENGTH MODEL FOR EPS
    !
    if (ltur>1) then
       sigw = 0.0
       do k = 1, kmax - 1
          sigw = sigw - thick(k)
          do nm = 1, nmmax
             if (kfs(nm)==1) then
                h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
                !
                !***MIXING LENGTH
                !
                rl = vonkar*h0*(1. + sigw)*sqrt( - sigw)
                !
                !***INITIALISATION EPSILON from PRANDTL MIXING LENGTH MODEL
                !
                rtur0(nm, k, 2) = max(1.E-7_fp, cde*(rtur0(nm, k, 1))**1.5/rl)
             endif
          enddo
       enddo
       !
       !***EPS AT FREE SURFACE AND BOTTOM
       !
       do nm = 1, nmmax
          if (kfs(nm)==1) then
             h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
             !
             !***AT FREE SURFACE
             !
             zwc = 0.5*thick(1)*h0
             zw  = inpzw*zwi + (1 - inpzw)*zwc
             rtur0(nm, 0, 2) = max(1.E-7_fp, cewall*(rtur0(nm, 0, 1))**1.5/zw)
             !
             !***AT BOTTOM
             !
             z00 = z0scratch(nm, kmax)
             rtur0(nm, kmax, 2) = max(1.E-7_fp, cewall*(rtur0(nm, kmax, 1))**1.5/z00)
          endif
       enddo
    endif
    !
    if (lturi>0) then
       do k = 1, kmax
          do nm = 1, nmmax
             rtur0(nm, k, 1) = max(1.E-7_fp, rtur0(nm, k, 1))
          enddo
       enddo
    endif
    !
    ! For domain decomposition (in couple points)
    do k = 1, kmax
       do nm = 1, nmmax
          if (kcs(nm)==3) then
             rtur0(nm, k, 1) = 1.E-7
             rtur0(nm, k, 2) = 1.E-7
          endif
       enddo
    enddo
    !
    deallocate(z0scratch)
    !
    ! Delft3D-16494: NOT NECESSARY? No implicit loop was performed
    ! Exchange turbulence values with neighbours for parallel runs
    !
    call dfexchg ( rtur0(:,:,1), 0, kmax, dfloat, nm_pos, gdp )
    call dfexchg ( rtur0(:,:,2), 0, kmax, dfloat, nm_pos, gdp )
end subroutine initur
