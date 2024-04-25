subroutine z_tratur(dischy    ,nubnd     ,j         ,nmmaxj    ,nmmax     , &
                  & nmax      ,mmax      ,kmax      ,ltur      ,nto       , &
                  & icx       ,icy       ,kfs       ,kfu       ,kfv       , &
                  & kcs       ,mnbnd     ,s1        ,dps       ,u1        , &
                  & v1        ,w1        ,rtur0     ,rtur1     ,thick     , &
                  & sig       ,guu       ,gvv       ,guv       ,gvu       , &
                  & vicww     ,dicww     ,cfurou    ,cfvrou    ,z0urou    , &
                  & z0vrou    ,windsu    ,windsv    ,bruvai    ,dudz      , &
                  & dvdz      ,tkepro    ,tkedis    ,deltau    ,deltav    ,dfu       , &
                  & dfv       ,dis       ,hrms      ,uorb      ,tp        , &
                  & aak       ,bbk       ,cck       ,ddk       ,bdx       , &
                  & bux       ,bdy       ,buy       ,umea      ,vmea      , &
                  & ubnd      ,kdismx    , &
                  & hsurft    ,pkwav     ,kfsmin    ,kfsmn0    ,kfsmax    , &
                  & kfsmx0    ,kfuz1     ,kfvz1     ,dzs1      ,ueul      , &
                  & veul      ,gdp       )
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
! Computes transport in the u, v and w-direction of the turbulent kinetic energy
! and dissipation.
! The time integration is fully implicit. Timesplitting is used.
! In the first stage the horizontal advection and diffusion terms are
! implicitly. The systems of equations are decoupled in the vertical.
! A Gauss Seidel iterative solution method is used.
! In the second stage the vertical transport terms are integrated implicitly in
! combination with the source and sink terms.
!
! Sinks are treated implicitly and sources explicitly. The systems of equations
! are decoupled in the horizontal. In the vertical the tridiagonal systems
! of equations are solved by a double sweep.
!
! - Horizontal Advection in U-direction : implicit, first order upwind
! - Horizontal Advection in V-direction : implicit, first order upwind
! - Horizontal Diffusion                : not implemented,
!                                         dominated by numerical diffusion
! - Vertical Advection                  : implicit, first order upwind
! - Vertical Diffusion                  : implicit
! - Sources are integrated explicitly
! - Sinks are integrated implicitly
! - Production term contains only vertical gradients
! - Dissipation term is linearised to improve stability
! - Gauss Seidel iteration in the horizontal
!
! Comment:
! At the open boundaries a vertical equation is used to compute TKE and epsilon
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
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: z0v
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: vicmol
    integer                , pointer :: iro
    integer                , pointer :: irov
    integer                , pointer :: lundia
    real(fp)               , pointer :: cde
    real(fp)               , pointer :: cmukep
    real(fp)               , pointer :: cep1
    real(fp)               , pointer :: cep2
    real(fp)               , pointer :: cep3
    real(fp)               , pointer :: sigrho
    real(fp)               , pointer :: cewall
    real(fp)               , pointer :: ck
    logical                , pointer :: wave
!
! Global variables
!
    integer                                                      , intent(in)  :: icx    !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                      , intent(in)  :: icy    !  Increment in the Y-dir. (see ICX)
    integer                                                                    :: j      !  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                      , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                    :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                      , intent(in)  :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                                                                    :: nubnd  !  Description and declaration in trisol.igs
    integer      , dimension(7, nto)                                           :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kdismx
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmn0 !  Description and declaration in iidim.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    real(fp)     , dimension(2, ltur, 0:kmax, 2, nto)                          :: ubnd   !  Description and declaration in trisol.igs
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub,3)                          :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub,3)                          :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: deltau !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: deltav !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dfu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: dfv    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub,4)                          :: dis    !  Description and declaration in esm_alloc_real.f90
    real(prec)   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hsurft
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsv !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: aak    !  Internal work array
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bbk    !  Internal work array
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: cck    !  Internal work array
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: ddk    !  Internal work array, diagonal space at (N,M,K)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: pkwav
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in)  :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)              :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdx    !  Internal work array, implicit coupling of concentration in (N,M,K)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdy
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bux    !  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M+1,K)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: buy
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: tkedis !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: tkepro !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: ueul   !  Eulerian velocity in X-direction (including Stokes drift)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: umea   !  Mean horizontal velocity
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: veul   !  Eulerian velocity in Y-direction (including Stokes drift)
    real(fp)     , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: vmea   !  Mean horizontal velocity
    real(fp)     , dimension(kmax)                                             :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(kmax)                                             :: thick  !  Description and declaration in esm_alloc_real.f90
    character(8)                                                               :: dischy !  Description and declaration in tricom.igs
!
! Local variables
!
    integer      :: ddb
    integer      :: icxy
    integer      :: k
    integer      :: kd
    integer      :: ken
    integer      :: kmaxx
    integer      :: kmin
    integer      :: kmmx
    integer      :: kminmax
    integer      :: kminmin
    integer      :: kmaxmax
    integer      :: kmaxmin
    integer      :: knd
    integer      :: knu
    integer      :: ku
    integer      :: kz
    integer      :: l
    integer      :: m
    integer      :: maxk
    integer      :: n
    integer      :: ndm
    integer      :: ndmd
    integer      :: nm
    integer      :: nmd
    integer      :: nmu
    integer      :: num
    integer      :: nm_pos     ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    real(fp)     :: alfa
    real(fp)     :: bi
    real(fp)     :: buoflu
    real(fp)     :: cu
    real(fp)     :: cv
    real(fp)     :: deltas
    real(fp)     :: dfus
    real(fp)     :: dfvs
    real(fp)     :: dfdis
    real(fp)     :: difd
    real(fp)     :: difu
    real(fp)     :: du
    real(fp)     :: dv
    real(fp)     :: dx
    real(fp)     :: dy
    real(fp)     :: dz
    real(fp)     :: ee
    real(fp)     :: epsd
    real(fp)     :: epswav
    real(fp)     :: epswin
    real(fp)     :: fact
    real(fp)     :: h0
    real(fp)     :: hsurf
    real(fp)     :: laydep
    real(fp)     :: pransm
    real(fp)     :: rz
    real(fp)     :: s
    real(fp)     :: timest
    real(fp)     :: tkebot
    real(fp)     :: tkewin
    real(fp)     :: tkewav
    real(fp)     :: us
    real(fp)     :: utot
    real(fp)     :: uu
    real(fp)     :: vs
    real(fp)     :: vv
    real(fp)     :: uuu
    real(fp)     :: vcc
    real(fp)     :: viww
    real(fp)     :: vvv
    real(fp)     :: wdo
    real(fp)     :: wup
    real(fp)     :: www
    real(fp)     :: z0s
    real(fp)     :: zlev
    real(fp)     :: zw
!
    data epsd/1.E-20/
!
!! executable statements -------------------------------------------------------
!
    lundia      => gdp%gdinout%lundia
    cde         => gdp%gdturcoe%cde
    cmukep      => gdp%gdturcoe%cmukep
    cep1        => gdp%gdturcoe%cep1
    cep2        => gdp%gdturcoe%cep2
    cep3        => gdp%gdturcoe%cep3
    sigrho      => gdp%gdturcoe%sigrho
    cewall      => gdp%gdturcoe%cewall
    ck          => gdp%gdturcoe%ck
    rhow        => gdp%gdphysco%rhow
    z0          => gdp%gdphysco%z0
    z0v         => gdp%gdphysco%z0v
    vonkar      => gdp%gdphysco%vonkar
    vicmol      => gdp%gdphysco%vicmol
    iro         => gdp%gdphysco%iro
    irov        => gdp%gdphysco%irov
    hdt         => gdp%gdnumeco%hdt
    wave        => gdp%gdprocs%wave
    !
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    timest = 2.0 * hdt
    ee     = exp(1.0)
    nm_pos = 1
    !
    ! first step : advection
    !
    do n = 1, nmax, 1
       do m = 1, mmax, 1
          nm = (n + ddb)*icy + (m + ddb)*icx - icxy
          if (kfs(nm) == 1) then
             nmd = nm - icx
             ndm = nm - icy
             do k = kfsmin(nm), kfsmax(nm) - 1
                umea(nm, k) = 0.25 * (u1(nm, k) + u1(nmd, k) + u1(nm, k + 1) + u1(nmd, k + 1))
                vmea(nm, k) = 0.25 * (v1(nm, k) + v1(ndm, k) + v1(nm, k + 1) + v1(ndm, k + 1))
             enddo
          endif
       enddo
    enddo
    !
    ! initialisation turbulent variables for new layers or resetting for dry layers
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          kmaxmax = max(kfsmax(nm), kfsmx0(nm))
          kmaxmin = min(kfsmax(nm), kfsmx0(nm))
          kminmax = max(kfsmin(nm), kfsmn0(nm))
          kminmin = min(kfsmin(nm), kfsmn0(nm))
          do k = kfsmin(nm), kfsmax(nm)
             rtur1(nm, k, 1) = rtur0(nm, k, 1)
             if (ltur == 2) rtur1(nm, k, 2) = rtur0(nm, k, 2)
          enddo
          do k = kminmin, kfsmin(nm)-1
             rtur1(nm, k, 1) = 0.0
             if (ltur == 2) rtur1(nm, k, 2) = 0
          enddo
          do k = kfsmin(nm), kminmax-1
             rtur1(nm, k, 1) = rtur1(nm, kminmax, 1)
             if (ltur == 2) rtur1(nm, k, 2) = rtur1(nm, kminmax, 2)
          enddo
          do k = kfsmax(nm)+1, kmaxmax
             rtur1(nm, k, 1) = 0.0
             if (ltur == 2) rtur1(nm, k, 2) = 0
          enddo
          do k = kmaxmin+1, kfsmax(nm)
             rtur1(nm, k, 1) = rtur1(nm, kmaxmin, 1)
             if (ltur == 2) rtur1(nm, k, 2) = rtur1(nm, kmaxmin, 2)
          enddo
       endif
    enddo
    !
    ! user defined boundary conditions if nubnd <> 0
    ! in two directions
    !
    if (nubnd /= 0) then
       do l=1,ltur
          call usrbcc(j         ,nmmaxj    ,kmax      ,l         , &
                    & icx       ,icy       ,nto       ,ltur      , &
                    & mnbnd     ,ubnd      ,aak       ,bbk       ,cck       , &
                    & ddk       ,rtur0     ,rtur1    , gdp       )
       enddo
    endif
    !
    ! Delft3D-16494: NOT NECESSARY?
    !
    ! exchange rtur1 with neighbours for parallel runs
    !
    call dfexchg ( rtur1(:, :, 1), 1, kmax-1, dfloat, nm_pos, gdp )
    if (ltur == 2) then
       call dfexchg ( rtur1(:, :, 2), 1, kmax-1, dfloat, nm_pos, gdp )
    endif
    !
    ! advective transport
    !
    do n = 1, nmax, 1
       do m = 1, mmax, 1
          nm = (n + ddb)*icy + (m + ddb)*icx - icxy
          if (kcs(nm)*kfs(nm) == 1) then
             nmd = nm - icx
             ndm = nm - icy
             do k = kfsmin(nm), kfsmax(nm) - 1
                !
                ! mean horizontal velocities at layer interfaces
                !
                if (umea(nm, k) >= 0.0 .and. vmea(nm, k) >= 0.0) then
                   cu = real(kfuz1(nmd, k),fp) * umea(nm, k)                         &
                      & * timest / (gvu(nmd) + 1.0 - real(kfuz1(nmd, k),fp))
                   cv = real(kfvz1(ndm, k),fp) * vmea(nm, k)                         &
                      & * timest / (guv(ndm) + 1.0 - real(kfvz1(ndm, k),fp))
                   rtur1(nm, k, 1) = (rtur0(nm, k, 1) + cu*rtur1(nmd, k, 1)     &
                                   & + cv*rtur1(ndm, k, 1)) / (1.0 + cu + cv)
                   if (ltur == 2) then
                      rtur1(nm, k, 2) = (rtur0(nm, k, 2) + cu*rtur1(nmd, k, 2)  &
                                      & + cv*rtur1(ndm, k, 2)) / (1.0 + cu + cv)
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! exchange rtur1 with neighbours for parallel runs
    !
    call dfexchg ( rtur1(:, :, 1), 1, kmax-1, dfloat, nm_pos, gdp )
    if (ltur == 2) then
       call dfexchg ( rtur1(:, :, 2), 1, kmax-1, dfloat, nm_pos, gdp )
    endif
    !
    do n = nmax, 1, -1
       do m = mmax, 1, -1
          nm = (n + ddb)*icy + (m + ddb)*icx - icxy
          if (kcs(nm)*kfs(nm) == 1) then
             nmu = nm + icx
             num = nm + icy
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (umea(nm, k) <= 0.0 .and. vmea(nm, k) <= 0.0) then
                   cu = real(kfuz1(nm, k),fp) * umea(nm, k)                          &
                      & * timest / (gvu(nm) + 1.0 - real(kfuz1(nm, k),fp))
                   cv = real(kfvz1(nm, k),fp) * vmea(nm, k)                          &
                      & * timest / (guv(nm) + 1.0 - real(kfvz1(nm, k),fp))
                   rtur1(nm, k, 1) = (rtur0(nm, k, 1) - cu*rtur1(nmu, k, 1)     &
                                   & - cv*rtur1(num, k, 1)) / (1.0 - cu - cv)
                   if (ltur == 2) then
                      rtur1(nm, k, 2) = (rtur0(nm, k, 2) - cu*rtur1(nmu, k, 2)  &
                                      & - cv*rtur1(num, k, 2)) / (1.0 - cu - cv)
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! exchange rtur1 with neighbours for parallel runs
    !
    call dfexchg ( rtur1(:, :, 1), 1, kmax-1, dfloat, nm_pos, gdp )
    if (ltur == 2) then
       call dfexchg ( rtur1(:, :, 2), 1, kmax-1, dfloat, nm_pos, gdp )
    endif
    !
    do n = 1, nmax, 1
       do m = mmax, 1, -1
          nm = (n + ddb)*icy + (m + ddb)*icx - icxy
          if (kcs(nm)*kfs(nm) == 1) then
             nmu = nm + icx
             ndm = nm - icy
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (umea(nm, k) <= 0.0 .and. vmea(nm, k) >= 0.0) then
                   cu = real(kfuz1(nm, k),fp) * umea(nm, k)                          &
                      & * timest / (gvu(nm) + 1.0 - real(kfuz1(nm, k),fp))
                   cv = real(kfvz1(ndm, k),fp) * vmea(nm, k)                         &
                      & * timest / (guv(ndm) + 1.0 - real(kfvz1(ndm, k),fp))
                   rtur1(nm, k, 1) = (rtur0(nm, k, 1) - cu*rtur1(nmu, k, 1)     &
                                   & + cv*rtur1(ndm, k, 1)) / (1.0 - cu + cv)
                   if (ltur == 2) then
                      rtur1(nm, k, 2) = (rtur0(nm, k, 2) - cu*rtur1(nmu, k, 2)  &
                                      & + cv*rtur1(ndm, k, 2)) / (1.0 - cu + cv)
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! exchange rtur1 with neighbours for parallel runs
    !
    call dfexchg ( rtur1(:, :, 1), 1, kmax-1, dfloat, nm_pos, gdp )
    if (ltur == 2) then
       call dfexchg ( rtur1(:, :, 2), 1, kmax-1, dfloat, nm_pos, gdp )
    endif
    !
    do n = nmax, 1, -1
       do m = 1, mmax, 1
          nm = (n + ddb)*icy + (m + ddb)*icx - icxy
          if (kcs(nm)*kfs(nm) == 1) then
             nmd = nm - icx
             num = nm + icy
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (umea(nm, k) >= 0.0 .and. vmea(nm, k) <= 0.0) then
                   cu = real(kfuz1(nmd, k),fp) * umea(nm, k)                         &
                      & * timest / (gvu(nmd) + 1.0 - real(kfuz1(nmd, k),fp))
                   cv = real(kfvz1(nm, k),fp) * vmea(nm, k)                          &
                      & * timest / (guv(nm) + 1.0 - real(kfvz1(nm, k),fp))
                   rtur1(nm, k, 1) = (rtur0(nm, k, 1) + cu*rtur1(nmd, k, 1)     &
                                   & - cv*rtur1(num, k, 1)) / (1.0 + cu - cv)
                   if (ltur == 2) then
                      rtur1(nm, k, 2) = (rtur0(nm, k, 2) + cu*rtur1(nmd, k, 2)  &
                                      & - cv*rtur1(num, k, 2)) / (1.0 + cu - cv)
                   endif
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! exchange rtur1 with neighbours for parallel runs
    !
    call dfexchg ( rtur1(:, :, 1), 1, kmax-1, dfloat, nm_pos, gdp )
    if (ltur == 2) then
       call dfexchg ( rtur1(:, :, 2), 1, kmax-1, dfloat, nm_pos, gdp )
    endif
    !
    ! production term (only vertical gradients)
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          nmd = nm - icx
          ndm = nm - icy
          do k = kfsmin(nm), kfsmax(nm) - 1
             bdy(nm, k) =   0.5 * (dudz(nm, k)**2 + dudz(nmd, k)**2)              &
                        & / max(1, kfuz1(nmd, k) + kfuz1(nm, k))                  &
                        & + 0.5 * (dvdz(nm, k)**2 + dvdz(ndm, k)**2)              &
                        & / max(1, kfvz1(ndm, k) + kfvz1(nm, k))
          enddo
       endif
    enddo
    !
    ! production term (only horizontal gradients if IROV=1)
    !
    if (irov > 0) then
       do nm = 1, nmmax
          if (kfs(nm)*kcs(nm) == 1) then
             nmu = nm + icx
             num = nm + icy
             nmd = nm - icx
             ndm = nm - icy
             dx  = 0.5 * (gvv(nm) + gvv(ndm))
             dy  = 0.5 * (guu(nm) + guu(nmd))
             do k = kfsmin(nm), kfsmax(nm)
                bdx(nm, k) =   ((u1(nm, k) - u1(nmd, k))/dx)**2       &
                           & + ((v1(nm, k) - v1(ndm, k))/dy)**2
             enddo
          else
             do k = kfsmin(nm), kfsmax(nm)
                bdx(nm, k) = 0.0
             enddo
          endif
       enddo
       do nm = 1, nmmax
          nmu = nm + icx
          num = nm + icy
          nmd = nm - icx
          ndm = nm - icy
          kz  = kfv(nm) * kfv(nmu) * kfu(nm) * kfu(num)
          if (kz == 1) then
             dx = 0.5 * (gvv(nm) + gvv(nmu))
             dy = 0.5 * (guu(nm) + guu(num))
             do k = kfsmin(nm), kfsmax(nm)
                bux(nm, k) = ((u1(num, k) - u1(nm, k))/dy + (v1(nmu, k) - v1(nm,k))/dx)**2
             enddo
          else
             do k = kfsmin(nm), kfsmax(nm)
                bux(nm, k) = 0.0
             enddo
          endif
       enddo
       do nm = 1, nmmax
          if (kcs(nm)*kfs(nm) == 1) then
             nmd  = nm - icx
             ndm  = nm - icy
             ndmd = nm - icx - icy
             do k = kfsmin(nm), kfsmax(nm) - 1
                bdy(nm, k) = bdy(nm, k) + 0.5*(bdx(nm, k) + bdx(nm, k + 1))            &
                           & + ((bux(nm  , k    ) + bux(nmd , k    ) + bux(ndm, k    ) &
                           & +   bux(ndmd, k    ) + bux(nm  , k + 1) + bux(nmd, k + 1) &
                           & +   bux(ndm , k + 1) + bux(ndmd, k + 1))/16.0)
             enddo
          endif
       enddo
    endif
    do l = 1, ltur
       !
       ! Second step : vertical advection, diffusion, production and diffusion
       !
       if (l == 1) pransm = 1.0
       if (l == 2) pransm = 1.3
       !
       do nm = 1, nmmax
          if (kfs(nm) /= 0) then
             do k = kfsmin(nm) - 1, kfsmax(nm)
                aak(nm, k) = 0.0
                bbk(nm, k) = 1.0 / timest
                cck(nm, k) = 0.0
             enddo
          endif
       enddo
       do nm = 1, nmmax
          do k = kfsmin(nm), kfsmax(nm) - 1
             ddk(nm, k) = rtur1(nm, k, l) / timest
          enddo
       enddo
       do nm = 1, nmmax
          do k = kfsmin(nm), kfsmax(nm) - 1
             ku = k + 1
             !
             ! ADVECTION IN VERTICAL DIRECTION
             !
             if (kfs(nm)*kcs(nm) == 1) then
                h0  = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                www = w1(nm, k)
                wdo = 0.5 * (www + abs(www)) / dzs1(nm, k)
                wup = 0.5 * (www - abs(www)) / dzs1(nm, ku)
                if (k == kfsmin(nm)) wdo = 0.0
                aak(nm, k) = -wdo
                bbk(nm, k) = 1.0/timest + wdo - wup
                cck(nm, k) = wup
                ddk(nm, k) = rtur1(nm, k, l) / timest
             endif
          enddo
       enddo
       !
       do nm = 1, nmmax
          do k = kfsmin(nm), kfsmax(nm) - 1
             ku = k + 1
             kd = k - 1
             if (l == 2 .and. k == kfsmin(nm)) then
                knd = kd
                vcc = 0.0
             else
                knd = k
                vcc = vicmol
             endif
             if (l == 2 .and. k == kfsmax(nm) - 1) then
                knu = ku
             else
                knu = k
             endif
             !
             ! DIFFUSION IN VERTICAL DIRECTION
             !
             if (kfs(nm) == 1) then
                dz         = 0.5 * (dzs1(nm, k) + dzs1(nm, k + 1))
                difu       = (vicmol + 0.5*(vicww(nm, knu) + vicww(nm, ku ))/pransm) / (dzs1(nm, ku)*dz)
                difd       = (vcc    + 0.5*(vicww(nm, kd ) + vicww(nm, knd))/pransm) / (dzs1(nm, k )*dz)
                aak(nm, k) = aak(nm, k)        - difd
                bbk(nm, k) = bbk(nm, k) + difu + difd
                cck(nm, k) = cck(nm, k) - difu
             endif
          enddo
       enddo
       !
       ! source and sink terms
       !
       if (l == 1) then
          !
          ! buoyancy term
          !
          do nm = 1, nmmax
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (kfs(nm) == 1) then
                   buoflu     = vicww(nm, k) * bruvai(nm, k) / sigrho
                   ddk(nm, k) = ddk(nm, k) - min(0.0_fp, buoflu)
                   bbk(nm, k) = bbk(nm, k) + max(0.0_fp, buoflu)/max(rtur0(nm, k, 1), epsd)
                endif
             enddo
          enddo
          !
          ! production term horizontal and vertical gradients
          !
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                pkwav(nm, kfsmin(nm)) = 0.
                pkwav(nm, kfsmax(nm)) = 0.
                !
                do k = kfsmin(nm)+1, kfsmax(nm)-1
                   !
                   ! Reset pkwav tke sources due to waves
                   !
                   pkwav(nm, k   )       = 0.
                   !
                   ddk(nm, k) = ddk(nm, k) + 2*(vicmol + vicww(nm, k))*bdy(nm, k)
                enddo
             endif
          enddo
          !
          ! production term due to breaking of waves
          !
          ! first: determine length (vertical) of distribution of
          !        turbulent energy and fill production term in right hand side
          !
          if (wave) then
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   h0                    = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                   pkwav(nm, kfsmax(nm)) =  2.0_fp*(dis(nm,2)+dis(nm,3))/(rhow*max(hrms(nm),0.01_fp))
                   zw  = 0.0_fp
                   do k = kfsmax(nm)-1, kfsmin(nm), -1
                      zw  = zw  + dzs1(nm,k+1)
                      if (hrms(nm) > zw) then
                         pkwav(nm,k) = pkwav(nm,kfsmax(nm))*(1.0_fp - zw/(hrms(nm)))
                         ddk(nm,k)   = ddk(nm,k) + pkwav(nm, k)   
                      else
                         kdismx(nm)=k+1
                         exit
                      endif
                   enddo
                endif
             enddo
          endif
          !
          ! Addition of wall production due to slip on vertical walls
          !
          ! !!!! Must still be changed from SIGMA implementation to Z !!!!
          !
          !if (irov == 1) then
          !   do k = 1, kmax - 1
          !      ku = k + 1
          !      do nm = 1, nmmax
          !         if (kfs(nm)*kcs(nm) == 1) then
          !            nmd = nm - icx
          !            ndm = nm - icy
          !            nmu = nm + icx
          !            num = nm + icy
          !            if (kcs(num) == 0) then
          !               uu = 0.25 * (u1(nm, k) + u1(nmd, k) + u1(nm, ku) + u1(nmd, ku))
          !               us = vonkar * uu / log((0.25*(guu(nm) + guu(nmd)) + z0v)/z0v)
          !               du = abs(us) / (0.25*(guu(nm) + guu(nmd)) + z0v)
          !               ddk(nm, k) = ddk(nm, k) + 0.5*du*us*us
          !               bdy(nm, k) = bdy(nm, k) + 0.5*du*du
          !            endif
          !            if (kcs(ndm) == 0) then
          !               uu = 0.25 * (u1(nm, k) + u1(nmd, k) + u1(nm, ku) + u1(nmd, ku))
          !               us = vonkar * uu / log((0.25*(guu(nm) + guu(nmd)) + z0v)/z0v)
          !               du = abs(us) / (0.25*(guu(nm) + guu(nmd)) + z0v)
          !               ddk(nm, k) = ddk(nm, k) + 0.5*du*us*us
          !               bdy(nm, k) = bdy(nm, k) + 0.5*du*du
          !            endif
          !            if (kcs(nmu) == 0) then
          !               vv = 0.25 * (v1(nm, k) + v1(ndm, k) + v1(nm, ku) + v1(ndm, ku))
          !               vs = vonkar * vv / log((0.25*(gvv(nm) + gvv(nmd)) + z0v)/z0v)
          !               dv = abs(vs) / (0.25*(guu(nm) + guu(nmd)) + z0v)
          !               ddk(nm, k) = ddk(nm, k) + 0.5*dv*vs*vs
          !               bdy(nm, k) = bdy(nm, k) + 0.5*dv*dv
          !            endif
          !            if (kcs(nmd) == 0) then
          !               vv = 0.25 * (v1(nm, k) + v1(ndm, k) + v1(nm, ku) + v1(ndm, ku))
          !               vs = vonkar * vv / log((0.25*(gvv(nm) + gvv(nmd)) + z0v)/z0v)
          !               dv = abs(vs) / (0.25*(guu(nm) + guu(nmd)) + z0v)
          !               ddk(nm, k) = ddk(nm, k) + 0.5*dv*vs*vs
          !               bdy(nm, k) = bdy(nm, k) + 0.5*dv*dv
          !            endif
          !         endif
          !      enddo
          !   enddo
          !endif
       else
          !
          ! EPSILON equation
          ! buoyancy term
          !
          do nm = 1, nmmax
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (kfs(nm) == 1) then
                   if (bruvai(nm, k) < 0.0) then
                      !
                      ! in case of an unstable stratification CEP3 is set to zero
                      ! this means that the buoyancy flux is switched on
                      !
                      buoflu  = - vicww(nm, k) * bruvai(nm, k) / sigrho
                   else
                      !
                      ! in case of a stable stratification CEP3 is set to one
                      ! this means that the buoyancy flux is switched off
                      !
                      buoflu  = 0.0_fp
                   endif
                   ddk(nm, k) = ddk(nm, k) + cep1*buoflu*rtur0(nm, k, 2)/max(rtur0(nm, k, 1), epsd)
                endif
             enddo
          enddo
          !
          ! production term horizontal and vertical gradients
          !
          do nm = 1, nmmax
             nmd = nm - icx
             ndm = nm - icy
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (kfs(nm) == 1) then
                   ddk(nm, k) = ddk(nm, k) + 2*cmukep*cep1*rtur1(nm, k, 1)*bdy(nm, k)
                endif
             enddo
          enddo
          if (wave) then
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   !
                   ! production term breaking waves in epsilon
                   !
                   do k = kdismx(nm),kfsmax(nm)
                      ddk(nm, k) = ddk(nm, k) + cep1*pkwav(nm, k)*rtur0(nm, k, 2)/max(rtur0(nm, k, 1), epsd)
                   enddo
                   !
                   ! production term wave dissipation in bottom boundary layer
                   ! is switched off just as in the production of turb. kinetic energy
                   ! Already accounted for in increased z0 (in subroutine TAUBOT)
                   !
                endif
             enddo
          endif
       endif
       if (ltur == 1) then
          !
          ! dissipation in k-L model    (Newton linearisation)
          !
          do nm = 1, nmmax
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (kfs(nm) == 1) then
                   viww       = vicmol + vicww(nm, k)
                   ddk(nm, k) = ddk(nm, k) +   cmukep*(rtur1(nm, k, 1)**2)/viww
                   bbk(nm, k) = bbk(nm, k) + 2*cmukep* rtur1(nm, k, 1)    /viww
                endif
             enddo
          enddo
       !
       ! dissipation k-epsilon model (Newton linearisation)
       !
       elseif (l == 1) then
          do nm = 1, nmmax
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (kfs(nm) == 1) then
                   ddk(nm, k) = ddk(nm, k) +   rtur0(nm, k, 2)
                   bbk(nm, k) = bbk(nm, k) + 2*rtur0(nm, k, 2)/max(rtur0(nm, k, 1), epsd)
                endif
             enddo
          enddo
       else
          do nm = 1, nmmax
             do k = kfsmin(nm), kfsmax(nm) - 1
                if (kfs(nm) == 1) then
                   ddk(nm, k) = ddk(nm, k) +   cep2*(rtur0(nm, k, 2)**2) &
                              & /max(rtur0(nm, k, 1), epsd)
                   bbk(nm, k) = bbk(nm, k) + 2*cep2* rtur0(nm, k, 2)     &
                              & /max(rtur0(nm, k, 1), epsd)
                endif
             enddo
          enddo
       endif
       !
       ! vertical boundary conditions
       !
       if (l == 1) then
          !
          ! tke at free surface and bottom
          !
          do nm = 1, nmmax
             nmd = nm - icx
             ndm = nm - icy
             !
             ! at free surface
             !
             if (kfs(nm) == 1) then
                tkewin = sqrt((windsu(nm)**2 + windsv(nm)**2)/cmukep)/rhow
                aak(nm, kfsmax(nm)) = 0.0
                bbk(nm, kfsmax(nm)) = 1.0
                cck(nm, kfsmax(nm)) = 0.0
                ddk(nm, kfsmax(nm)) = tkewin
                !
                ! Added production due to waves
                !
                if (wave) then
                   ! 
                   ! Flux boundary type
                   !
                   h0     = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                   !
                   epswav = 4.0_fp * (dis(nm,2)+dis(nm,3)) / (rhow*max(hrms(nm),0.01_fp))
                   !
                   ! Hrms/2 is the characteristic length scale for the wave dissipation
                   ! which can be compared to a Nikuradse length scale near the bottom 
                   ! The roughness height due to wave breaking, needed in the boundary 
                   ! condition for k is assumed to scale in the same fashion as z0 = ks/30:
                   ! (zw = 0.5_fp*hrms(nm)/30.0_fp)
                   !
                   tkewav = (epswav*vonkar*0.5*hrms(nm)/(30.0*cde))**(2.0/3.0)
                   !
                   ddk(nm, kfsmax(nm)) = ddk(nm, kfsmax(nm)) + tkewav
                endif
                !
                ! at bottom
                !
                aak(nm, kfsmin(nm) - 1) = 0.0
                bbk(nm, kfsmin(nm) - 1) = 1.0
                cck(nm, kfsmin(nm) - 1) = 0.0
                !
                ! ustar determined 1.5 grid space above bed
                !
                kmin = kfsmin(nm)
                if (kfsmin(nm) /= kfsmax(nm)) then
                   uuu = (ueul(nmd, kmin + 1) + ueul(nm, kmin + 1))                 &
                       & /max(1, kfuz1(nmd, kmin + 1) + kfuz1(nm, kmin + 1))
                   vvv = (veul(ndm, kmin + 1) + veul(nm, kmin + 1))                 &
                       & /max(1, kfvz1(ndm, kmin + 1) + kfvz1(nm, kmin + 1))
                   dz  = dzs1(nm, kmin) + 0.5*dzs1(nm, kmin + 1)
                else
                   uuu = (ueul(nmd, kmin) + ueul(nm, kmin))                         &
                       & /max(1, kfuz1(nmd, kmin) + kfuz1(nm, kmin))
                   vvv = (veul(ndm, kmin) + veul(nm, kmin))                         &
                       & /max(1, kfvz1(ndm, kmin) + kfvz1(nm, kmin))
                   dz  = dzs1(nm, kmin)/ee
                endif
                utot = sqrt(uuu*uuu + vvv*vvv)
                h0   = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                ken  = kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm)
                if (ken /= 0) then
                   z0s = ( kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) + kfv(ndm)*z0vrou(ndm)  &
                       & + kfv(nm )*z0vrou(nm ) ) / ken
                   !
                   rz                      = 1.0 + 1.0/z0s*dz
                   s                       = log(rz)/vonkar
                   ddk(nm, kfsmin(nm) - 1) = utot*utot/(s*s*sqrt(cmukep))
                   if (kfsmax(nm)/=kfsmin(nm)) then
                      aak(nm, kmin) = 0.0
                      bbk(nm, kmin) = 1.0
                      cck(nm, kmin) = 0.0
                      ddk(nm, kmin) = (1.0 - dzs1(nm, kmin)/h0)*ddk(nm, kmin - 1)
                   endif
                endif
             endif
          enddo
       endif
       !
       ! dissipation at free surface and bottom
       !
       if (l == 2) then
          do nm = 1, nmmax
             nmd = nm - icx
             ndm = nm - icy
             if (kfs(nm) /= 0 .and. kfsmax(nm) >= kfsmin(nm)) then
                !
                ! at free surface
                !
                h0                  = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                zw                  = 0.5 * dzs1(nm, kfsmax(nm))
                tkewin              = sqrt((windsu(nm)**2 + windsv(nm)**2)/cmukep) / rhow
                epswin              = cewall * (tkewin**1.5) / zw
                aak(nm, kfsmax(nm)) = 0.0
                bbk(nm, kfsmax(nm)) = 1.0
                cck(nm, kfsmax(nm)) = 0.0
                ddk(nm, kfsmax(nm)) = epswin
                !
                ! Added production due to waves
                !
                if (wave) then
                   h0 = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                   !
                   ! need to calculate tkewav in same manner as above
                   ! (note, for this pransm should = 1)
                   ! and then adjust for differences between k flux and epsilon flux
                   ! this should include the difference in pransm
                   !
                   epswav = 4.0_fp * (dis(nm,2)+dis(nm,3)) /(rhow*max(hrms(nm),0.01_fp))
                   !
                   ddk(nm, kfsmax(nm)) = ddk(nm, kfsmax(nm)) + epswav
                endif
                !
                ! at bottom
                !
                ! Neumann BC for Epsilon
                !
                kmin = kfsmin(nm)
                if (kfsmin(nm) /= kfsmax(nm)) then
                   uuu = (ueul(nmd, kmin + 1) + ueul(nm, kmin + 1))                 &
                       & /max(1, kfuz1(nmd, kmin + 1) + kfuz1(nm, kmin + 1))
                   vvv = (veul(ndm, kmin + 1) + veul(nm, kmin + 1))                 &
                       & /max(1, kfvz1(ndm, kmin + 1) + kfvz1(nm, kmin + 1))
                   dz = dzs1(nm, kmin) + 0.5*dzs1(nm, kmin + 1)
                else
                   uuu = (ueul(nmd, kmin) + ueul(nm, kmin))                         &
                       & /max(1, kfuz1(nmd, kmin) + kfuz1(nm, kmin))
                   vvv = (veul(ndm, kmin) + veul(nm, kmin))                         &
                       & /max(1, kfvz1(ndm, kmin) + kfvz1(nm, kmin))
                   dz = dzs1(nm, kmin) / ee
                endif
                utot  = sqrt(uuu*uuu + vvv*vvv)
                h0    = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                ken   = kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm)
                if (ken /= 0) then
                   z0s    = ( kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) + kfv(ndm)*z0vrou(ndm)  &
                          & + kfv(nm )*z0vrou(nm ) ) / ken
                   rz     = 1.0 + dz/z0s
                   s      = log(rz) / vonkar
                   tkebot = utot*utot/(s*s*sqrt(cmukep))
                   aak(nm, kmin - 1) = 0.0
                   bbk(nm, kmin - 1) = 1.0
                   cck(nm, kmin - 1) = 0.0
                   ddk(nm, kmin - 1) = cewall*tkebot**1.5/z0s
                   !
                   ! Only perform additional specification of boundary flux for epsilon
                   ! when more than 1 layer is active
                   !
                   if (kfsmax(nm) > kmin) then
                      aak(nm, kmin)     = 0.0
                      bbk(nm, kmin)     = 1.0
                      cck(nm, kmin)     = 0.0
                      ddk(nm, kmin)     = z0s * ddk(nm, kmin - 1) / (dzs1(nm, kmin)+z0s)
                   endif
                else
                   aak(nm, kmin - 1) = 0.0
                   bbk(nm, kmin - 1) = 1.0
                   cck(nm, kmin - 1) = 0.0
                   ddk(nm, kmin - 1) = 0.0
                endif
             endif
          enddo
       endif
       !
       ! Reset the system of equations in the Open Boundary Points
       ! with User Defined Boundary Conditions
       !
       if (nubnd /= 0) then
          call usrbcc(j         ,nmmaxj    ,kmax      ,l         , &
                    & icx       ,icy       ,nto       ,ltur      , &
                    & mnbnd     ,ubnd      ,aak       ,bbk       ,cck       , &
                    & ddk       ,rtur0     ,rtur1    , gdp       )
       endif  
       !
       ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       do nm = 1, nmmax
          if (kfs(nm) /= 0) then
             maxk          = kfsmin(nm) - 1
             bi            = 1. / bbk(nm, maxk)
             bbk(nm, maxk) = bi
             cck(nm, maxk) = cck(nm, maxk) * bi
             ddk(nm, maxk) = ddk(nm, maxk) * bi
          endif
       enddo
       do nm = 1, nmmax
          do k = kfsmin(nm), kfsmax(nm)
             if (kfs(nm) == 1) then
                bi         = 1./(bbk(nm, k) - aak(nm, k)*cck(nm, k - 1))
                bbk(nm, k) = bi
                cck(nm, k) = cck(nm, k) * bi
                ddk(nm, k) = (ddk(nm, k) - aak(nm, k)*ddk(nm, k - 1)) * bi
             endif
          enddo
       enddo
       do nm = 1, nmmax
          if (kfsmax(nm) >= kfsmin(nm) .and. kfs(nm) == 1) then
             rtur1(nm, kfsmax(nm), l) = ddk(nm, kfsmax(nm))
          endif
       enddo
       do nm = 1, nmmax
          do k = kfsmax(nm) - 1, kfsmin(nm) - 1, -1
             if (kfs(nm) == 1) then
                rtur1(nm, k, l) = ddk(nm, k) - cck(nm, k)*rtur1(nm, k + 1, l)
             endif
          enddo
       enddo
       !
       ! Delft3D-16494: NOT NECESSARY? no implicit solve took place
       !
       ! exchange rtur1 with neighbours for parallel runs
       !
       call dfexchg ( rtur1(:, :, l), 0, kmax, dfloat, nm_pos, gdp )
    enddo
end subroutine z_tratur
