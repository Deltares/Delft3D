subroutine z_vermom_nhfull(nmmax     ,kmax      ,icx       ,icy       ,u0        , &
                         & v0        ,w0        ,vicww     ,rxz       ,ryz       , &
                         & guu       ,gvv       ,guv       ,gvu       ,kfs       , &
                         & kcs       ,aak       ,bbk       ,cck       ,ddk       , &
                         & bdx       ,bux       ,bdy       ,buy       ,uvdwk     ,vvdwk , &
                         & kfuz0     ,kfvz0     ,kfsz0     ,kfsmin    ,kfsmx0    , &
                         & kcshyd    ,w1        ,p0        ,zk        ,nst       , &
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
! Vertical momentum equation. Integration for
! full timestep. w0 is vertical velocity at
! end of previous non-hydrostatic timestep.
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
    real(fp)               , pointer :: eps
    integer                , pointer :: lundia
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: vicmol
    integer                , pointer :: m1_nhy
    integer                , pointer :: m2_nhy
    integer                , pointer :: n1_nhy
    integer                , pointer :: n2_nhy
    character(6)           , pointer :: momsol
!
! Global variables
!
    integer                                            , intent(in) :: icx
    integer                                            , intent(in) :: icy
    integer                                            , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcshyd !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: nst    !!  Time step number
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdx    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bux    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bdy    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: buy    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: uvdwk  !!  Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: vvdwk  !!  Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: w0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: p0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ryz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                        , intent(in) :: zk     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer            :: ddb
    integer            :: icxy
    integer            :: ifkx
    integer            :: ifky
    integer            :: ikenx
    integer            :: ikeny
    integer            :: iter
    integer            :: itr
    integer            :: k
    integer            :: kfad
    integer            :: ku
    integer            :: kuu
    integer            :: kd
    integer            :: m
    integer            :: maxk
    integer            :: mink
    integer            :: ndelta
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmst
    integer            :: nmstart
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: advecz
    real(fp)           :: adza
    real(fp)           :: adzc
    real(fp)           :: bi
    real(fp)           :: cuu
    real(fp)           :: cvv
    real(fp)           :: ddza
    real(fp)           :: ddzb
    real(fp)           :: ddzc
    real(fp)           :: dt
    real(fp)           :: dz
    real(fp)           :: dzdo
    real(fp)           :: dzu
    real(fp)           :: dzup
    real(fp)           :: dzv
    real(fp)           :: geta
    real(fp)           :: gksi
    real(fp)           :: uuu
    real(fp)           :: viscow
    real(fp)           :: visk
    real(fp)           :: viskup
    real(fp)           :: vix
    real(fp)           :: viy
    real(fp)           :: vvv
    real(fp)           :: wdo
    real(fp)           :: wup
    real(fp)           :: www
    real(fp)           :: ddkadx
    real(fp)           :: ddkady
    real(fp)           :: ddkadz
    integer            :: kadx
    integer            :: kady
    integer            :: kadz
    real(fp)           :: area       ! area of flux interface 
    real(fp)           :: uavg0      ! transport velocity at interface, east
    real(fp)           :: vavg0      ! transport velocity at interface, north
    real(fp)           :: wavg0      ! transport velocity at interface, top
    real(fp)           :: wzeta      ! conservation correction
    real(fp)           :: thvert     ! theta coefficient for vertical terms
    real(fp)           :: pcoef      ! temporary value for coefficient pressure derivative
    real(fp)           :: voltemp    ! work variable
    character(5)       :: errtxt
!
!! executable statements -------------------------------------------------------
!
    eps      => gdp%gdconst%eps
    lundia   => gdp%gdinout%lundia
    m1_nhy   => gdp%gdnonhyd%m1_nhy
    m2_nhy   => gdp%gdnonhyd%m2_nhy
    n1_nhy   => gdp%gdnonhyd%n1_nhy
    n2_nhy   => gdp%gdnonhyd%n2_nhy
    rhow     => gdp%gdphysco%rhow
    vicmol   => gdp%gdphysco%vicmol
    hdt      => gdp%gdnumeco%hdt
    momsol   => gdp%gdnumeco%momsol
    !
    ddb    = gdp%d%ddbound
    icxy   = max(icx,icy)
    dt     = 2.0_fp * hdt
    thvert = 0.0_fp
    !
    kadx   = 1
    kady   = 1
    kadz   = 1
    !
    ddkadx = 0.0_fp
    ddkady = 0.0_fp
    ddkadz = 0.0_fp
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    ! Array initialisation
    !
    aak = 0.0_fp
    bbk = 1.0_fp
    cck = 0.0_fp
    ddk = 0.0_fp
    bdx = 0.0_fp 
    bux = 0.0_fp 
    bdy = 0.0_fp 
    buy = 0.0_fp 
    w1  = 0.0_fp
    !
    ! Turbulent stresses rxz, ryz
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm)*kcs(nm) == 1) then
             nmu          = nm + icx
             num          = nm + icy
             rxz(nm,kmax) = 0.0_fp
             ryz(nm,kmax) = 0.0_fp
             do k = 1, kmax-1
                ku   = k + 1
                kd   = k - 1
                ifkx = kfuz0(nm,k) * kfuz0(nm,ku) * kfsz0(nm,k) * kfsz0(nmu,k)
                ifky = kfvz0(nm,k) * kfvz0(nm,ku) * kfsz0(nm,k) * kfsz0(num,k)
                vix  = 0.5_fp * (vicww(nm,k)+vicww(nmu,k))
                viy  = 0.5_fp * (vicww(nm,k)+vicww(num,k))
                dzu  = 0.5_fp * (zk(ku)-zk(kd))
                dzv  = 0.5_fp * (zk(ku)-zk(kd))
                if (ifkx == 1) then
                   rxz(nm,k) = vix * (  (w0(nmu,k )-w0(nm,k))/gvu(nm) &
                             &        + (u0(nm ,ku)-u0(nm,k))/dzu    )
                else
                   rxz(nm,k) = 0.0_fp
                endif
                if (ifky == 1) then
                   ryz(nm,k) = viy * (  (w0(num,k )-w0(nm,k))/guv(nm) &
                             &        + (v0(nm ,ku)-v0(nm,k))/dzv    )
                else
                   ryz(nm,k) = 0.0_fp
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! Horizontal advection: u dw/dx + v dw/dy
    !
    if (momsol == 'mdue' .or. momsol == 'flood') then
       !
       ! Multi-directional upwind explicit horizontal advection
       !
       call z_vermom_horadv_mdue(kmax  , icx   , icy    , icxy   , kcs   , &
                               & kfs   , guu   , guv    , gvu    , u0    , & 
                               & v0    , kfsmin, kfsmx0 , kfuz0  , kfsz0 , &
                               & kfvz0 , w0    , ddk    , gdp)
       !
    elseif (momsol == 'iupw' .or. momsol == 'mdui') then
       !
       ! First order implicit horizontal advection
       !
       call z_vermom_horadv_iupw(kmax  , icx   , icy    , icxy   , kcs   , &
                               & kfs   , guu   , guv    , gvu    , u0    , & 
                               & v0    , kfsmin, kfsmx0 , kfuz0  , kfsz0 , &
                               & kfvz0 , w0    , bbk    , ddk    , bdx   , &
                               & bux   , bdy   , buy    , gdp)
       !
    endif
    !
    ! Vertical advection and diffusion
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm)*kcs(nm) == 1) then
             nmd  = nm - icx
             ndm  = nm - icy
             ndmd = nm - icx - icy
             nmu  = nm + icx
             num  = nm + icy
             numu = nm + icx + icy
             ndmu = nm + icx - icy
             numd = nm - icx + icy
             gksi = gvu(nm)
             geta = guu(nm)
             !
             ! Loop over internal layers
             !
             do k = kfsmin(nm), kfsmx0(nm)-1
                ku     = k + 1
                kd     = k - 1
                !
                ! Vertical advection
                !
                dz   = 0.5_fp * (zk(ku)-zk(kd))
                kfad = 0
                if (k == kfsmin(nm)) then
                   kfad = 1
                endif
                dzup = zk(ku)-zk(k )
                dzdo = zk(k )-zk(kd)
                if (kfs(nm)*kcs(nm) == 1) then
                   www = w0(nm,k)
                   if (www < 0.0_fp) then
                      adza = -www / (dzup+dzdo) * real(1-abs(kfad),fp)
                      adzc =  www / (dzup+dzdo) * real(1-abs(kfad),fp)   &
                           & + kfad*(1+kfad)*www/(2.0_fp*dzup)
                   else
                      adza = -www / (dzup+dzdo) * real(1-abs(kfad),fp)  &
                           & + abs(kfad)*(-1+kfad)*www/(2.0_fp*dzdo)
                      adzc =  www / (dzup+dzdo) * real(1-abs(kfad),fp)
                   endif
                endif
                aak(nm,k) = adza
                bbk(nm,k) = bbk(nm,k) + 1.0_fp/dt - adza - adzc
                cck(nm,k) = adzc
                !
                ! Vertical viscosity (rzz)
                !
                viskup    = 0.5_fp * (2.0_fp*vicmol + vicww(nm, k) + vicww(nm, ku))
                visk      = 0.5_fp * (2.0_fp*vicmol + vicww(nm, k) + vicww(nm, kd))
                dzup      = zk(ku) - zk(k )
                dzdo      = zk(k ) - zk(kd)
                dz        = 0.5_fp * (dzup+dzdo)
                ddza      = visk   / (dzdo*dz)
                ddzc      = viskup / (dzup*dz)
                ddzb      = -ddza - ddzc
                aak(nm,k) = aak(nm,k) - ddza
                bbk(nm,k) = bbk(nm,k) - ddzb
                cck(nm,k) = cck(nm,k) - ddzc
                !
                viscow     =   (rxz(nm,k)-rxz(nmd,k)) / (0.5_fp*(gvv(nm)+gvv(ndm)))   &
                           & + (ryz(nm,k)-ryz(ndm,k)) / (0.5_fp*(guu(nm)+guu(nmd)))
                ddk(nm, k) = ddk(nm,k) + w0(nm,k)/dt + viscow      &
                           & - (p0(nm,ku)-p0(nm,k)) / (dz*rhow)
                if (k == kfsmin(nm)) then
                   aak(nm,kd) = 0.0_fp
                   bbk(nm,kd) = 1.0_fp
                   cck(nm,kd) = 0.0_fp
                   ddk(nm,kd) = 0.0_fp
                endif
                !
                ! Eq. for velocity above free surface
                !
                if (k == kfsmx0(nm)-1) then
                   aak(nm,ku) = -1.0_fp
                   bbk(nm,ku) =  1.0_fp
                   cck(nm,ku) =  0.0_fp
                   ddk(nm,ku) =  0.0_fp
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !    
    do nm = 1, nmmax
       if (kfs(nm) /= 0) then
          mink = kfsmin(nm)
          cck(nm,mink-1) = cck(nm,mink-1) / bbk(nm,mink-1)
          bbk(nm,mink-1) = 1.0_fp
          do k = kfsmin(nm), kfsmx0(nm)
             bi         = 1.0_fp/(bbk(nm, k) - aak(nm, k)*cck(nm, k - 1))
             bbk(nm, k) = bi
             cck(nm, k) = cck(nm, k) * bi
          enddo
       endif
    enddo
    !
    ! Iteration loop  
    !
    iter = 0
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          do k = kfsmin(nm)-1, kfsmx0(nm)
             w1(nm, k)    = w0(nm, k)
             uvdwk(nm, k) = w0(nm, k)
          enddo
       endif
    enddo
    !
  333  continue
    iter = iter + 1
    !
    ! ITERATIVE SOLUTION METHOD (JACOBI ITERATION)
    ! IN HORIZONTAL DIRECTION
    !
    itr=0
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfsmx0(nm)-kfsmin(nm)>0 .and. kcs(nm)*kfs(nm)==1) then
             do k = kfsmin(nm), kfsmx0(nm)
                uvdwk(nm, k) = bdx(nm,k) * w1(nm-icx, k) &
                &            + bdy(nm,k) * w1(nm-icy, k) &
                &            + buy(nm,k) * w1(nm+icy, k) &
                &            + bux(nm,k) * w1(nm+icx, k)
                uvdwk(nm, k) = ddk(nm,k) - uvdwk(nm, k) 
             enddo
          endif
       enddo
    enddo
    !
    ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfsmx0(nm)-kfsmin(nm)>0 .and. kcs(nm)==1) then
             mink           = kfsmin(nm) - 1
             vvdwk(nm,mink) = uvdwk(nm,mink) * bbk(nm, mink)
          endif
       enddo
    enddo
    !
    ! Forward sweep
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfs(nm)*kcs(nm) == 1) then
             do k = kfsmin(nm), kfsmx0(nm)
                vvdwk(nm,k) = (uvdwk(nm,k)-aak(nm,k)*vvdwk(nm,k-1)) * bbk(nm,k)
             enddo
          endif
       enddo
    enddo
    !
    ! Backward sweep
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfs(nm)*kcs(nm) == 1) then
             do k = kfsmx0(nm)-1, kfsmin(nm)-1,-1
                vvdwk(nm,k) = (vvdwk(nm,k)-cck(nm,k)*vvdwk(nm,k+1))
             enddo
          endif
       enddo
    enddo
    !
    ! CHECK FOR CONVERGENCE
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          do k = kfsmin(nm)-1, kfsmx0(nm)
             if (abs(vvdwk(nm, k) - w1(nm, k)) > eps) then
                itr = 1
             endif
             w1(nm, k) = vvdwk(nm, k)
          enddo
       endif
    enddo
    !
    if (itr>0 .and. iter<50) goto 333
    !
    if (iter >= 50) then
       write (errtxt, '(a,i0)') '*** WARNING No convergence in Z_VERMOM_NHFULL for tstep: #', nst
       call prterr(lundia    ,'U190'    ,errtxt    )
    endif 
    !
end subroutine z_vermom_nhfull
