subroutine z_hydpres_nhfull(mmax      ,nmax      ,j         ,nmmaxj    ,nmmax     , &
                          & kmax      ,nst       ,icx       ,icy       ,nsrc      , &
                          & norow     ,nocol     ,irocol    ,s1        ,s0        , &
                          & u1        ,v1        ,w1        ,guu       ,gvv       , &
                          & u0        ,v0        ,w0        ,circ2d    ,            &
                          & guv       ,gvu       ,gsqs      ,guz       ,gvz       , &
                          & kfu       ,kfv       ,kfs       ,kcs       ,kfuz0     , &
                          & kfvz0     ,kfsz0     ,kfsmin    ,kfsmx0    ,kcshyd    , &
                          & mnksrc    ,umean     ,vmean     ,dps       , &
                          & dzu0      ,dzv0      ,dzs0      ,disch     , &
                          & p1        ,p0        ,pnhcor    ,aak       ,zk        , &
                          & bbk       ,cck       ,ddk       ,bbka      ,bbkc      , &
                          & aak2      ,cck2      ,dinv      ,pbbk      ,pbbkc     , &
                          & vj        ,rj        ,pj        ,rjshadow  ,sj        , &
                          & sjprec    ,tj        ,gdp       )
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
! The difference equations for the non-hydrostatic
! pressure are derived from the discretized
! incompressibility condition.
! The non-hydrostatic pressure is computed for
! a restricted horizontal area.
! At the open boundaries the discretization
! is such that it leads to a symmetric matrix.
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
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: tetaq
    real(fp)               , pointer :: tetaz
    integer                , pointer :: iro
    integer                , pointer :: m1_nhy
    integer                , pointer :: m2_nhy
    integer                , pointer :: n1_nhy
    integer                , pointer :: n2_nhy
    logical                , pointer :: flag_pp
    character(8)           , pointer :: krylov
    integer                , pointer :: lundia
!
! Global variables
!
    integer                                                        :: icx
    integer                                                        :: icy
    integer                                                        :: j
    integer                                                        :: kmax      !  Description and declaration in esm_alloc_int.f90
    integer                                                        :: mmax      !  Description and declaration in esm_alloc_int.f90
    integer                                                        :: nmax      !  Description and declaration in esm_alloc_int.f90
    integer                                                        :: nmmax     !  Description and declaration in dimens.igs
    integer                                                        :: nmmaxj    !  Description and declaration in dimens.igs
    integer                                           , intent(in) :: nocol     !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in) :: norow     !  Description and declaration in esm_alloc_int.f90
    integer                                           , intent(in) :: nsrc      !  Description and declaration in esm_alloc_int.f90
    integer                                                        :: nst
    integer, dimension(5, norow + nocol)              , intent(in) :: irocol    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                       , intent(in) :: mnksrc    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcshyd    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs       !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfs       !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmin    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmx0    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfu       !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfv       !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfsz0     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfuz0     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfvz0     !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in) :: dps       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gsqs      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guu       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guv       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvu       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvv       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guz       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvz       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: s0        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                     :: s1        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                     :: umean     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                     :: vmean     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: w1        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: aak2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: vj        ! Renamed apj into v for use in BiCGSTAB, Ullmann 21/02/2008
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: cck2
    real(fp), dimension(4, norow+nocol)               , intent(in) :: circ2d    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dinv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: dzs0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: dzu0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: dzv0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: p0        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: p1        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pbbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pbbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pj
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: rjshadow  ! Extra array for BiCGSTAB, Ullmann 21/02/2008
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: sj        ! Extra array for BiCGSTAB, Ullmann 21/02/2008
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: sjprec    ! Extra array for BiCGSTAB, Ullmann 21/02/2008
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: tj        ! Extra array for BiCGSTAB, Ullmann 21/02/2008
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pnhcor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: rj
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: u1        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: v1        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: u0        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: v0        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: w0        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in) :: disch     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                       , intent(in) :: zk        !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: ddb
    integer       :: i
    integer       :: ibf
    integer       :: ibl
    integer       :: ic
    integer       :: icxy
    integer       :: k
    integer       :: kdo
    integer       :: kenm
    integer       :: kk
    integer       :: kup
    integer       :: m
    integer       :: mf
    integer       :: mfu
    integer       :: ml
    integer       :: n
    integer       :: ndelta
    integer       :: ndm
    integer       :: nf
    integer       :: nfm
    integer       :: nfu
    integer       :: nfum
    integer       :: nfdm
    integer       :: nl
    integer       :: nlm
    integer       :: nlum
    integer       :: nm
    integer       :: nmd
    integer       :: nmf
    integer       :: nmfu
    integer       :: nml
    integer       :: nmlu
    integer       :: nmrow
    integer       :: nmst
    integer       :: nmstart
    integer       :: nmu
    integer       :: num
    real(fp)      :: corr
    real(fp)      :: dt
    real(fp)      :: dxd
    real(fp)      :: dxu
    real(fp)      :: dyd
    real(fp)      :: dyu
    real(fp)      :: dzdo
    real(fp)      :: dzup
    real(fp)      :: dz
    real(fp)      :: alfa
    real(fp)      :: alfai
    real(fp)      :: alfae
    real(fp)      :: coef
    real(fp)      :: alfak
    real(fp)      :: alfakm1
    real(fp)      :: z_k
    real(fp)      :: z_km1
    character(6)  :: method
    character(20) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    lundia    => gdp%gdinout%lundia
    m1_nhy   => gdp%gdnonhyd%m1_nhy
    m2_nhy   => gdp%gdnonhyd%m2_nhy
    n1_nhy   => gdp%gdnonhyd%n1_nhy
    n2_nhy   => gdp%gdnonhyd%n2_nhy
    flag_pp  => gdp%gdnonhyd%flag_pp
    tetaq    => gdp%gdnonhyd%tetaq
    tetaz    => gdp%gdnonhyd%tetaz
    krylov   => gdp%gdnonhyd%krylov
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    iro      => gdp%gdphysco%iro
    hdt      => gdp%gdnumeco%hdt
    !
    ddb     = gdp%d%ddbound
    icxy    = max(icx, icy)
    dt      = 2.0_fp * hdt
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy+ddb) + (m1_nhy-1+ddb)*icxy
    !
    !
    !
    ! initialise all coefficients
    !
    aak  = 0.0_fp
    aak2 = 0.0_fp
    bbk  = 1.0_fp
    cck  = 0.0_fp
    cck2 = 0.0_fp
    ddk  = 0.0_fp
    bbka = 0.0_fp
    bbkc = 0.0_fp
    p1   = 0.0_fp
    !
    ! inner area, closed boundaries: homogeneous Neumann horizontal gradients of velocity
    ! coefficients:
    !
    !   horizontal:             cck2(  m,n+1)                  vertical:  bbkc(k+1)
    !                            |                                         |
    !            aak(m-1,  n) - bbk (  m,  n) - cck (m+1,n)               bbk (  k)
    !                                   |                   |
    !                           aak2(  m,n-1)                             bbka(k-1)
    !
    
    do nm = 1,nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          nmd = nm - icx
          nmu = nm + icx
          ndm = nm - icy
          num = nm + icy
          do k = kfsmin(nm), kfsmx0(nm)
             !
             ! Construction of full system of equations for pressure
             ! loop over all cells where continuity equation is applied, i.e., over
             ! all (partially) filled cells, which includes bottom cells and surface cells
             !
             !    Construction includes boundary conditions at vertical faces (boundaries left/right,
             !    vertical bottom walls, etc.) by means of using switches kf...
             !
             kdo  = max( k-1, kfsmin(nm) )
             kup  = min( k+1, kfsmx0(nm) )
             dxu  = gvu(nm)
             dxd  = gvu(nmd)
             dyu  = guv(nm)
             dyd  = guv(ndm)
             dzup        = 0.5_fp * (zk(k)-zk(k-1)+zk(kup)-zk(kup-1))
             dzdo        = 0.5_fp * (zk(k)-zk(k-1)+zk(kdo)-zk(kdo-1))
             aak2(nm, k) = -  dt * dzv0(ndm, k)*gvv(ndm)/ (dyd*rhow)       &
                         &       * kfvz0(ndm, k) * kfsz0(ndm, k)
             cck2(nm, k) = -  dt * dzv0(nm , k)*gvv(nm) / (dyu*rhow)       &
                         &       * kfvz0(nm , k) * kfsz0(num, k)
             bbka(nm, k) = -  dt *gsqs(nm)/ (dzdo*rhow)
             bbkc(nm, k) = -  dt *gsqs(nm)/ (dzup*rhow)
             aak (nm, k) = -  dt * dzu0(nmd, k)*guu(nmd) / (dxd*rhow)      &
                         &       * kfuz0(nmd, k) * kfsz0(nmd, k)
             cck (nm, k) = -  dt * dzu0( nm, k)*guu(nm ) / (dxu*rhow)      &
                         &       * kfuz0(nm, k) * kfsz0(nmu, k)
             bbk (nm, k) = - aak (nm, k) - cck (nm, k) - aak2(nm, k)       &
                         & - cck2(nm, k) - bbka(nm, k) - bbkc(nm, k)
             ddk (nm, k) = - ( u1(nm , k)*dzu0(nm , k)*guu(nm )            &
                         &   - u1(nmd, k)*dzu0(nmd, k)*guu(nmd) )          &
                         & - ( v1(nm , k)*dzv0(nm , k)*gvv(nm )            &
                         &   - v1(ndm, k)*dzv0(ndm, k)*gvv(ndm) )          &
                         & - w1(nm, k)*gsqs(nm) + w1(nm, k - 1)*gsqs(nm)
             !
             ! If outflow at a vertical free-surface wall then hor.mom.eq. applied => horizontal
             ! pressure gradient required, but no pressure outside flow domain => increase only
             ! diagonal of system of equations
             !
             !coef = 0.0_fp
             !if ( kfvz0(ndm,k)==1 .and. kfsz0(ndm,k)==0 ) then
             !   coef = coef + dt * dzv0(ndm, k)*gvv(ndm) / (dyd*rhow)
             !endif
             !if ( kfvz0(nm ,k)==1 .and. kfsz0(num,k)==0 ) then
             !   coef = coef + dt * dzv0(nm , k)*gvv(nm ) / (dyu*rhow)
             !endif
             !if ( kfuz0(nmd,k)==1 .and. kfsz0(nmd,k)==0 ) then
             !   coef = coef + dt * dzu0(nmd, k)*guu(nmd) / (dxd*rhow)
             !endif
             !if ( kfuz0(nm, k)==1 .and. kfsz0(nmu,k)==0 ) then
             !   coef = coef + dt * dzu0(nm , k)*guu(nm ) / (dxu*rhow)
             !endif
             !bbk(nm,k) = bbk(nm,k) + coef
          enddo
       endif
    enddo
    !
    ! Boundary conditions at free surface and at bottom
    ! First the free surface, then the bottom, because the free-surface procedure uses pressure
    ! in 2 grid layers, which in case of flow in only 1 layer are put together in the bottom
    ! procedure.
    !
    do nm = 1,nmmax
       nmu = nm + icx
       num = nm + icy
       nmd = nm - icx
       ndm = nm - icy
       if (kcs(nm)*kfs(nm) == 1) then
          !
          k = kfsmx0(nm)
          !
          ! surface: insert dp/dz=0 at surface and add dzeta/dt effect
          !
          bbk (nm,k) = bbk(nm,k) + bbkc(nm,k)
          bbkc(nm,k) = 0.0_fp
          !
          if ( flag_pp ) then
             !
             ! Non-hydrostatic pressure at free surface equal to zero
             !
             z_k = 0.5_fp * ( zk(k) + zk(k-1) )
             if ( k > 2 ) then
                !
                ! interpolation possible
                !
                z_km1 = 0.5_fp * ( zk(k-1) + zk(k-2) )
             else
                !
                ! extrapolation
                !
                z_km1 = 1.5_fp * zk(k-1) - 0.5_fp * zk(k)
             endif
             coef = gsqs(nm) / dt / tetaq / tetaz                                       &
                  / ( (z_k - z_km1) *ag*rhow ) ! - p0(nm,k) + p0(nm,k-1) ) ! extra linearization
             !
             ! Correction for neglecting dq/dz at the free surface
             !
             corr    = 1.0_fp
             !corr   = 1.0_fp / (1.0_fp-((p0(nm,k)-p0(nm,k-1))/(ag*rhow*(z_k-z_km1))))
             !
             alfak   = coef * (s0(nm) - z_km1 ) * corr
             alfakm1 = coef * (z_k    - s0(nm)) * corr
          else
             !
             ! Top non-hydrostatic pressure equal to zero => hydrostatic pressure in top layer
             !
             alfak   = gsqs(nm) / dt / tetaq / tetaz / ( ag*rhow )
             alfakm1 = 0.0_fp
          endif
          !
          bbk (nm, k) = bbk (nm,k) + alfak
          bbka(nm, k) = bbka(nm,k) + alfakm1
          !
          ddk (nm, k) = ddk(nm,k)     + w1(nm,k)*gsqs(nm)     & ! Should not be necessary
                      - (1.0_fp-tetaz)/tetaz *                                        &
                                      (  (  u0(nm , k)*dzu0(nm , k)*guu(nm )          &
                                          - u0(nmd, k)*dzu0(nmd, k)*guu(nmd) )        &
                                       + (  v0(nm , k)*dzv0(nm , k)*gvv(nm )          &
                                          - v0(ndm, k)*dzv0(ndm, k)*gvv(ndm) )        &
                                       - w0(nm, k - 1)*gsqs(nm)                )      &
                      - alfak * p0(nm,k) !!! + ag*rhow*(s0(nm)-z_k  ) ) )
          if (k > kfsmin(nm)) then
             ddk (nm, k) = ddk(nm,k) - alfakm1 * p0(nm,k-1) !!! + ag*rhow*(s0(nm)-z_km1) )
          endif
          !
          k = kfsmin(nm)
          !
          ! bottom: insert dp/dz = 0 at bottom
          !
          ! dp/dn = 0 at vertical bottom walls as well. Already set above because those sides
          ! are closed => switches kfuz1 etc. set equal to 0 => coefficients cck, etc. set to 0
          !
          bbk (nm,k) = bbk(nm,k) + bbka(nm,k)
          bbka(nm,k) = 0.0_fp
       endif
    enddo
    !
    ! addition of discharges to continuity equation only when it lies inside NH domain
    !
    do i = 1, nsrc
       nm   = (mnksrc(2,i)+ddb)+((mnksrc(1,i)-1)+ddb) * icxy
       if (kcs(nm) == 1) then
          k    = mnksrc(3, i)
          kenm = min(1, kfu(nm)+kfu(nm-icx)+kfv(nm)+kfv(nm-icy))
          if (kenm/=0 .or. disch(i)>=0.0_fp) then
             if (k/=0 .or. kfsmx0(nm)<=kfsmin(nm)) then
                !
                ! The order is important at dry points
                !
                if (k > kfsmx0(nm)) then
                   k = kfsmx0(nm)
                endif
                if (k < kfsmin(nm)) then
                   k = kfsmin(nm)
                endif
                ddk(nm,k) = ddk(nm,k) + disch(i)/gsqs(nm)
             else
                do kk = kfsmin(nm), kfsmx0(nm)
                   ddk(nm,kk) = ddk(nm,kk) + disch(i)*dzs0(nm,kk)                &
                              & /((real(dps(nm),fp)+s0(nm))*gsqs(nm))
                enddo
             endif
          endif
       endif 
       !
       ! in case of an intake for an intake/outfall combination:
       !
       if (mnksrc(7,i) >= 2) then
          nm   = (mnksrc(2,i)+ddb)+((mnksrc(1,i)-1)+ddb) * icxy
          k    = mnksrc(3,i)
          kenm = min(1, kfu(nm)+kfu(nm-icx)+kfv(nm)+kfv(nm-icy))
          if (kenm/=0 .or. -disch(i)>=0.0_fp) then
             if (k/=0 .or. kfsmx0(nm)<=kfsmin(nm)) then
                !
                ! The order is important at dry points
                !
                if (k > kfsmx0(nm)) then
                   k = kfsmx0(nm)
                endif
                if (k < kfsmin(nm)) then
                   k = kfsmin(nm)
                endif
                ddk(nm,k) = ddk(nm,k) - disch(i)/gsqs(nm)
             else
                do kk = kfsmin(nm), kfsmx0(nm)
                      ddk(nm,kk) = ddk(nm,kk) - disch(i)*dzs0(nm,kk)            &
                                 & /((real(dps(nm),fp)+s0(nm))*gsqs(nm))
                enddo
             endif
          elseif (mnksrc(7,i) /= 3) then
             !
             ! in case of a culvert no warning generated
             !
             write (errtxt, '(i0,i3)') nst, i
             call prterr(lundia, 'S208', trim(errtxt))
          else
          endif
       endif
    enddo
    !
    !
    ! Open boundary conditions
    ! At water level boundaries a Dirichlet bc. and for other other boundary types
    ! a Neumann bc are prescribed.
    !
    do ic = 1, norow
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nmf  = (n+ddb)*icy + (mf+ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n+ddb)*icy + (ml+ddb)*icx - icxy
       nmlu =  nml + icx 
       if (ibf == 2) then
          do k = kfsmin(nmfu), kfsmx0(nmfu)
             ddk(nmfu,k) = ddk(nmfu,k) - aak(nmfu,k)*tetaq*ag*rhow*(circ2d(1,ic)-s0(nmf))
             aak(nmfu,k) = 0.0_fp
          enddo
       endif
       if (ibf > 2) then
          do k = kfsmin(nmfu), kfsmx0(nmfu)
             bbk(nmfu,k) = bbk(nmfu,k) + aak(nmfu,k)
             aak(nmfu,k) = 0.0_fp
          enddo
       endif
       if (ibl == 2) then
          do k = kfsmin(nml), kfsmx0(nml)
             ddk(nml,k) = ddk(nml,k) - cck(nml,k)*tetaq*ag*rhow*(circ2d(2,ic)-s0(nmlu))
             cck(nml,k) = 0.0_fp
          enddo
       endif
       if (ibl > 2) then
          do k = kfsmin(nml), kfsmx0(nml)
             bbk(nml,k) = bbk(nml,k) + cck(nml,k)
             cck(nml,k) = 0.0_fp
          enddo
       endif
    enddo
    do ic = norow + 1, norow + nocol
       m    = irocol(1, ic)
       nf   = irocol(2, ic) - 1
       nl   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nfm  = (nf+ddb)*icy + (m+ddb)*icx - icxy
       nfum = nfm + icy
       nlm  = (nl+ddb)*icy + (m+ddb)*icx - icxy
       nlum = nlm + icy
       if (ibf == 2) then
          do k = kfsmin(nfum), kfsmx0(nfum)
             ddk (nfum,k) = ddk(nfum, k)- aak2(nfum,k)*tetaq*ag*rhow*(circ2d(1,ic)-s0(nfm))
             aak2(nfum,k) = 0.0_fp
          enddo
       endif
       if (ibf > 2) then
          do k = kfsmin(nfum), kfsmx0(nfum)
             bbk (nfum,k) = bbk(nfum, k) + aak2(nfum, k)
             aak2(nfum,k) = 0.0_fp
          enddo
       endif
       if (ibl == 2) then
          do k = kfsmin(nlm), kfsmx0(nlm)
             ddk (nlm,k) = ddk(nlm, k)-cck2(nlm, k)*tetaq*ag*rhow*(circ2d(2,ic)-s0(nlum))
             cck2(nlm,k) = 0.0_fp
          enddo
       endif
       if (ibl > 2) then
          do k = kfsmin(nlm), kfsmx0(nlm)
             bbk (nlm,k)  = bbk(nlm,k) + cck2(nlm,k)
             cck2(nlm,k) = 0.0_fp
          enddo
       endif
    enddo
    !
    ! solve system of equations for pressure correction
    ! Now p0 as a better initial guess
    !
    call z_initcg_nhfull(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                       & bbka      ,bbkc      ,ddk       ,kmax      ,icx       , &
                       & icy       ,nmmax     ,nst       ,kfsz0     ,dinv      , &
                       & pbbk      ,pbbkc     ,pnhcor    ,rj        ,kfs       , &
                       & kfsmin    ,kfsmx0    ,gdp       )
    if ( krylov == 'bicgstab') then
        call z_solbicgstab(aak       ,bbk       ,cck      ,aak2      ,cck2      , &
                         & bbka      ,bbkc      ,ddk      ,kmax      ,icx       , &
                         & icy       ,nmmax     ,nst      ,kfsz0     ,pnhcor    , &
                         & pj        ,rj        ,vj       ,dinv      ,pbbk      , &
                         & pbbkc     ,p1        ,rjshadow ,sj        ,sjprec    , &
                         & tj        ,kfs       ,kfsmin   ,kfsmx0    ,gdp       )
 
    elseif ( krylov == 'cg') then
       !
       ! old implementation (see version nh-15) and rename "apj" by "vj"
       !
       call z_solcg_nhfull(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                         & bbka      ,bbkc      ,ddk       ,kmax      ,icx       , &
                         & icy       ,nmmax     ,nst       ,kfsz0     ,pnhcor    , &
                         & pj        ,rj        ,vj        ,dinv      ,pbbk      , &
                         & pbbkc     ,p1        ,kfs       ,kfsmin    ,kfsmx0    , &
                         & gdp       )
    else 
        write (lundia,*) '*** ERROR:z_hydpres_nhfull: THIS IS NOT ALLOWED'
    endif
    !
    !   pressure at open boundaries
    !
    do ic = 1, norow
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nmf  = (n+ddb)*icy + (mf+ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n+ddb)*icy + (ml+ddb)*icx - icxy
       nmlu = nml + icx
       if (ibf == 2) then
          s1(nmf) = circ2d(1,ic) 
          do k = kfsmin(nmf), kfsmx0(nmf)
             p1(nmf,k) = tetaq * ag * rhow * (s1(nmf)-s0(nmf))
             p0(nmf,k) = 0.0_fp
          enddo
       endif
       if (ibl == 2) then
          s1(nmlu) = circ2d(2,ic)
          do k = kfsmin(nmlu), kfsmx0(nmlu)
             p1(nmlu,k) = tetaq * ag * rhow * (s1(nmlu)-s0(nmlu))
             p0(nmlu,k) = 0.0_fp
          enddo
       endif
    enddo
    do ic = norow + 1, norow + nocol
       m    = irocol(1, ic)
       nf   = irocol(2, ic) - 1
       nl   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nfm  = (nf+ddb)*icy + (m+ddb)*icx - icxy
       nlm  = (nl+ddb)*icy + (m+ddb)*icx - icxy
       nfum = nfm + icy
       nlum = nlm + icy
       if (ibf == 2) then
          s1(nfm) = circ2d(1,ic)
          do k = kfsmin(nfm), kfsmx0(nfm)
             p1(nfm,k) = tetaq * ag * rhow * (s1(nfm)-s0(nfm))
             p0(nfm,k) = 0.0_fp
          enddo
       endif
       if (ibl == 2) then
          s1(nlum) = circ2d(2,ic)
          do k = kfsmin(nlum), kfsmx0(nlum)
             p1(nlum,k) = tetaq * ag * rhow * (s1(nlum)-s0(nlum))
             p0(nlum,k) = 0.0_fp
          enddo
       endif
    enddo
end subroutine z_hydpres_nhfull
