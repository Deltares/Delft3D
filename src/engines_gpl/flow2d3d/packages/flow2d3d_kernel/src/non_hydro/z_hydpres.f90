subroutine z_hydpres(mmax      ,nmax      ,j         ,nmmaxj    ,nmmax     , &
                   & kmax      ,nst       ,icx       ,icy       ,nsrc      , &
                   & norow     ,nocol     ,irocol    ,s1        ,s00       , &
                   & u1        ,v1        ,w1        ,guu       ,gvv       , &
                   & guv       ,gvu       ,gsqs      ,guz       ,gvz       , &
                   & kfu       ,kfv       ,kfs       ,kcs       ,kfuz0     , &
                   & kfvz0     ,kfsz0     ,kfsmin    ,kfsmx0    ,kcshyd    , &
                   & mnksrc    ,umean     ,vmean     ,dps       , &
                   & dzu0      ,dzv0      ,dzs1      ,disch     , &
                   & p1        ,p00       ,pnhcor    ,aak       , &
                   & bbk       ,cck       ,ddk       ,bbka      ,bbkc      , &
                   & aak2      ,cck2      ,dinv      ,pbbk      ,pbbkc     , &
                   & apj       ,rj        ,pj        ,gdp       )
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
    integer                , pointer :: iro
    integer                , pointer :: m1_nhy
    integer                , pointer :: m2_nhy
    integer                , pointer :: n1_nhy
    integer                , pointer :: n2_nhy
!
! Global variables
!
    integer                                                          :: icx
    integer                                                          :: icy
    integer                                                          :: j
    integer                                                          :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nmmax  !  Description and declaration in dimens.igs
    integer                                                          :: nmmaxj !  Description and declaration in dimens.igs
    integer                                             , intent(in) :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in) :: norow  !  Description and declaration in esm_alloc_int.f90
    integer                                             , intent(in) :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                                          :: nst
    integer   , dimension(5, norow + nocol)             , intent(in) :: irocol !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(7, nsrc)                      , intent(in) :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcshyd !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: s00    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in) :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                     :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in) :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: aak
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: aak2
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: apj
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbka
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: bbkc
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: cck
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: cck2
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: ddk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: dinv
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: p00    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pbbk
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pbbkc
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pj
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: pnhcor !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)               :: rj
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in) :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(nsrc)                         , intent(in) :: disch  !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer      :: ddb
    integer      :: i
    integer      :: ibf
    integer      :: ibl
    integer      :: ic
    integer      :: icxy
    integer      :: k
    integer      :: kd
    integer      :: kenm
    integer      :: kk
    integer      :: ku
    integer      :: m
    integer      :: mf
    integer      :: mfu
    integer      :: ml
    integer      :: n
    integer      :: ndelta
    integer      :: ndm
    integer      :: nf
    integer      :: nfm
    integer      :: nfu
    integer      :: nfum
    integer      :: nfdm
    integer      :: nl
    integer      :: nlm
    integer      :: nlum
    integer      :: nm
    integer      :: nmd
    integer      :: nmf
    integer      :: nmfu
    integer      :: nml
    integer      :: nmlu
    integer      :: nmrow
    integer      :: nmst
    integer      :: nmstart
    integer      :: nmu
    integer      :: num
    real(fp)     :: dt
    real(fp)     :: dx
    real(fp)     :: dxd
    real(fp)     :: dxu
    real(fp)     :: dy
    real(fp)     :: dyd
    real(fp)     :: dyu
    real(fp)     :: dzd
    real(fp)     :: dzup
    character(6) :: method
!
!! executable statements -------------------------------------------------------
!
    m1_nhy   => gdp%gdnonhyd%m1_nhy
    m2_nhy   => gdp%gdnonhyd%m2_nhy
    n1_nhy   => gdp%gdnonhyd%n1_nhy
    n2_nhy   => gdp%gdnonhyd%n2_nhy
    rhow     => gdp%gdphysco%rhow
    ag       => gdp%gdphysco%ag
    iro      => gdp%gdphysco%iro
    hdt      => gdp%gdnumeco%hdt
    !
    ddb     = gdp%d%ddbound
    icxy    = max(icx, icy)
    dt      = 2.0_fp * hdt
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    ! initialise all coefficients
    !
    do nm = 1, nmmax
       do k = 1, kmax
          aak (nm, k) = 0.0_fp
          aak2(nm, k) = 0.0_fp
          bbk (nm, k) = 1.0_fp
          cck (nm, k) = 0.0_fp
          cck2(nm, k) = 0.0_fp
          ddk (nm, k) = 0.0_fp
          bbka(nm, k) = 0.0_fp
          bbkc(nm, k) = 0.0_fp
       enddo
    enddo
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
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kcs(nm)==1) then
             nmd = nm - icx
             nmu = nm + icx
             ndm = nm - icy
             num = nm + icy
             do k = kfsmin(nm), kfsmx0(nm)
                kd   = max(k - 1, kfsmin(nm))
                ku   = min(k + 1, kfsmx0(nm))
                dxu  = gvu(nm)
                dxd  = gvu(nmd)
                dx   = gvz(nm)
                dyu  = guv(nm)
                dyd  = guv(ndm)
                dy   = guz(nm)
                dzup = 0.5_fp*(dzs1(nm, k) + dzs1(nm, ku))
                dzd  = 0.5_fp*(dzs1(nm, k) + dzs1(nm, kd))
                aak2(nm, k) = -dzv0(ndm, k)*dt/(dy*dyd*rhow)*kfvz0(ndm, k)*kfsz0(ndm, k)
                cck2(nm, k) = -dzv0(nm , k)*dt/(dy*dyu*rhow)*kfvz0(nm , k)*kfsz0(num, k)
                bbka(nm, k) = -dt/(dzd *rhow)
                bbkc(nm, k) = -dt/(dzup*rhow)
                aak (nm, k) = -dzu0(nmd, k)*dt/(dx*dxd*rhow)*kfuz0(nmd, k)*kfsz0(nmd, k)
                cck (nm, k) = -dzu0(nm , k)*dt/(dx*dxu*rhow)*kfuz0(nm , k)*kfsz0(nmu, k)
                bbk (nm, k) = - aak (nm, k) - cck (nm, k) - aak2(nm, k)                   &
                            & - cck2(nm, k) - bbka(nm, k) - bbkc(nm, k)
                ddk (nm, k) = -(u1(nm, k)*dzu0(nm, k) - u1(nmd, k)*dzu0(nmd, k))/dx       &
                            & -(v1(nm, k)*dzv0(nm, k) - v1(ndm, k)*dzv0(ndm, k))/dy       &
                            & - w1(nm, k) + w1(nm, k - 1)
             enddo
          endif
       enddo
    enddo
    !
    ! boundary conditions bottom and free surface
    ! s00 is the waterlevel at the end of the previous time-step (set in momcor.f90, not in f0isf1.f90!)
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          nmu = nm + icx
          num = nm + icy
          nmd = nm - icx
          ndm = nm - icy
          if (kfsmin(nm) <= kfsmx0(nm) .and. kcs(nm)==1) then
             !
             ! For bottom layer:
             !
             k           = kfsmin(nm)
             bbka(nm, k) = 0.0_fp
             bbk (nm, k) = - aak (nm, k) - cck (nm, k) - aak2(nm, k)- cck2(nm, k) - bbkc(nm, k)
             !
             ! For top layer:
             !
             k           = kfsmx0(nm)
             dx          = 0.5_fp*(gvv(nm) + gvv(ndm))
             dy          = 0.5_fp*(guu(nm) + guu(nmd))
             bbkc(nm, k) = 0.0_fp
             bbk (nm, k) = - aak (nm, k) - cck (nm, k) - aak2(nm, k)                  &
                         & - cck2(nm, k) - bbka(nm, k) + 1.0_fp/(dt*ag*rhow)
             ddk (nm, k) = - (u1(nm, k)*dzu0(nm, k) - u1(nmd, k)*dzu0(nmd, k))/dx     &
                         & - (v1(nm, k)*dzv0(nm, k) - v1(ndm, k)*dzv0(ndm, k))/dy     &
                         & + w1(nm, k - 1) - s1(nm)/dt + s00(nm)/dt
          endif
       enddo
    enddo
    !
    ! addition of discharges to continuity equation only when they are located inside the NH domain
    !
    do i = 1, nsrc
       nm   = (mnksrc(2, i) + ddb) + ((mnksrc(1, i) - 1) + ddb)*icxy
       if (kcshyd(nm) == 1) then
          k    = mnksrc(3, i)
          kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
          if (kenm/=0 .or. disch(i)>=0.0_fp) then
             if (k /= 0) then
                if (k>kfsmx0(nm)) then
                   k = kfsmx0(nm)
                endif
                if (k<kfsmin(nm)) then
                   k = kfsmin(nm)
                endif
                ddk(nm, k) = ddk(nm, k) + disch(i)/gsqs(nm)
             else
                do kk = kfsmin(nm), kfsmx0(nm)
                   ddk(nm, kk) = ddk(nm, kk) + disch(i)*dzs1(nm, kk)                &
                               & /((real(dps(nm),fp) + s1(nm))*gsqs(nm))
                enddo
             endif
          endif
       endif
    enddo
    !
    ! Open boundary conditions
    ! At water level boundaries a Dirichlet bc. and for other othen boundary types
    ! a Neumann bc.
    !
    do ic = 1, norow
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       if (ibf == 2) then
          do k = kfsmin(nmfu), kfsmx0(nmfu)
             aak(nmfu, k) = 0.0_fp
          enddo
       endif
       if (ibf > 2) then
          do k = kfsmin(nmfu), kfsmx0(nmfu)
             bbk(nmfu, k) = bbk(nmfu, k) + aak(nmfu, k)
             aak(nmfu, k) = 0.0_fp
          enddo
       endif
       if (ibl == 2) then
          do k = kfsmin(nml), kfsmx0(nml)
             cck(nml, k) = 0.0_fp
          enddo
       endif
       if (ibl > 2) then
          do k = kfsmin(nml), kfsmx0(nml)
             bbk(nml, k) = bbk(nml, k) + cck(nml, k)
             cck(nml, k) = 0.0_fp
          enddo
       endif
    enddo
    do ic = norow + 1, norow + nocol
       m    = irocol(1, ic)
       nf   = irocol(2, ic) - 1
       nl   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nfm  = (nf + ddb)*icy + (m + ddb)*icx - icxy
       nlm  = (nl + ddb)*icy + (m + ddb)*icx - icxy
       nfum = nfm + icy
       if (ibf == 2) then
          do k = kfsmin(nfum), kfsmx0(nfum)
             aak2(nfum, k) = 0.0_fp
          enddo
       endif
       if (ibf > 2) then
          do k = kfsmin(nfum), kfsmx0(nfum)
             bbk (nfum, k) = bbk(nfum, k) + aak2(nfum, k)
             aak2(nfum, k) = 0.0_fp
          enddo
       endif
       if (ibl == 2) then
          do k = kfsmin(nlm), kfsmx0(nlm)
             cck2(nlm, k) = 0.0_fp
          enddo
       endif
       if (ibl > 2) then
          do k = kfsmin(nlm), kfsmx0(nlm)
             bbk (nlm, k) = bbk(nlm, k) + cck2(nlm, k)
             cck2(nlm, k) = 0.0_fp
          enddo
       endif
    enddo
    !
    ! Internal boundary conditions in case non-hydrostatic pressure is computed
    ! in area smaller than total domain; Boundary treated as open boundary
    ! without non-hyd. pressure (choice of Neumann or Dirichlet)
    ! (Dirichlet is active; Neumann inactive in this version)
    !
    do n = n1_nhy, n2_nhy
       mf   = m1_nhy
       ml   = m2_nhy-1
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmlu = nml + icx
       if (kcs(nmf)==1 .and. kcshyd(nmfu)==1) then
          !
          ! West boundary non-hydrostatic area corner point excluded
          !
          do k = kfsmin(nmf), kfsmx0(nmf)
             aak(nmf, k) = 0.0_fp
             bbk(nmf, k) = 1.0_fp
             cck(nmf, k) = 0.0_fp
             ddk(nmf, k) = 0.0_fp
          enddo
          do k = kfsmin(nmfu), kfsmx0(nmfu)
             aak(nmfu, k) = 0.0_fp
          enddo
       endif
       if (kcshyd(nml)==1 .and. kcs(nmlu)==1) then
          !
          ! East boundary non-hydrostatic area corner point excluded
          !
          do k = kfsmin(nmlu), kfsmx0(nmlu)
             aak(nmlu,k) = 0.0_fp
             bbk(nmlu,k) = 1.0_fp
             cck(nmlu,k) = 0.0_fp
             ddk(nmlu,k) = 0.0_fp
          enddo
          do k = kfsmin(nml), kfsmx0(nml)
             cck (nml,k) = 0.0_fp
          enddo
       endif
    enddo
    !
    ! south boundary non-hydrostatic area 
    !
    do m = m1_nhy, m2_nhy
       nf   = n1_nhy
       nl   = n2_nhy-1
       nfm  = (nf + ddb)*icy + (m + ddb)*icx - icxy
       nfum = nfm + icy
       nlm  = (nl + ddb)*icy + (m + ddb)*icx - icxy
       nlum = nlm + icy
       if (kcs(nfm)==1 .and. kcshyd(nfum)==1) then
          !
          ! South boundary non-hydrostatic area
          !
          do k = kfsmin(nfm), kfsmx0(nfm)
             aak2(nfm,k) = 0.0_fp
             bbk (nfm,k) = 1.0_fp
             cck2(nfm,k) = 0.0_fp
             ddk (nfm,k) = 0.0_fp
          enddo
          do k = kfsmin(nfum), kfsmx0(nfum)
             aak2(nfum,k) = 0.0_fp
          enddo
       endif
       if (kcshyd(nlm)==1 .and. kcs(nlum)==1) then
          !
          ! North boundary non-hydrostatic area
          !
          do k = kfsmin(nlum), kfsmx0(nlum)
             aak2(nlum, k) = 0.0_fp
             bbk (nlum, k) = 1.0_fp
             cck2(nlum, k) = 0.0_fp
             ddk (nlum, k) = 0.0_fp
          enddo
          do k = kfsmin(nlm), kfsmx0(nlm)
             cck2(nlm, k) = 0.0_fp
          enddo
       endif
    enddo
    !
    ! solve system of equations for pressure correction
    !
    call z_initcg(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                & bbka      ,bbkc      ,ddk       ,kmax      ,icx       , &
                & icy       ,nmmax     ,nst       ,kfsz0     ,dinv      , &
                & pbbk      ,pbbkc     ,pnhcor    ,rj        ,pj        , &
                & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
    !
    call z_solcg(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
               & bbka      ,bbkc      ,ddk       ,kmax      ,icx       , &
               & icy       ,nmmax     ,nst       ,kfsz0     ,pnhcor    , &
               & pj        ,rj        ,apj       ,dinv      ,pbbk      , &
               & pbbkc     ,p1        ,kfs       ,kfsmin    ,kfsmx0    , &
               & gdp       )
end subroutine z_hydpres
