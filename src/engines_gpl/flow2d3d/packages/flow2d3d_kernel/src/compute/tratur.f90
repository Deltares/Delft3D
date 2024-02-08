subroutine tratur(dischy    ,nubnd     ,j         ,nmmaxj    ,nmmax     , &
                & nmax      ,mmax      ,kmax      ,ltur      ,nto       , &
                & icx       ,icy       ,kfs       ,kfu       ,kfv       , &
                & kcs       ,mnbnd     ,s1        ,dps       ,u1        , &
                & v1        ,w1        ,rtur0     ,rtur1     ,thick     , &
                & sig       ,guu       ,gvv       ,guv       ,gvu       , &
                & vicww     ,dicww     ,cfurou    ,cfvrou    ,z0urou    , &
                & z0vrou    ,windsu    ,windsv    ,bruvai    ,dudz      , &
                & dvdz      ,tkepro    ,tkedis    ,deltau    ,deltav    , &
                & dfu       , &
                & dfv       ,dis       ,hrms      ,uorb      ,tp        , &
                & aak       ,bbk       ,cck       ,ddk       ,bdx       , &
                & bux       ,bdy       ,buy       ,umea      ,vmea      , &
                & ubnd      ,kdismx    , &
                & hsurft    ,pkwav     ,diapl     ,rnpl      ,ueul      , &
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
! The production of turbulence due to horizontal
! shear and wall friction (slip) on vertical
! walls is included. This term may be switched on
! or off with the parameter IROV.
!
! - Horizontal Advection in U-direction : implicit, first order upwind
! - Horizontal Advection in V-direction : implicit, first order upwind
! - Horizontal Diffusion                : not implemented,
!                                         dominated by numerical diffusion
! - Vertical Advection                  : implicit, first order upwind
! - Vertical Diffusion                  : implicit
! - Sources are integrated explicitly
! - Sinks are integrated implicitly
! - Production term contains only vertical gradients (IROV=0)
! - Production term contains both vertical and horizontal gradients (IROV=1)
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
    real(fp)                            , pointer :: eps
    real(fp)                            , pointer :: clplant
    integer                             , pointer :: lundia
    real(fp)                            , pointer :: hdt
    real(fp)                            , pointer :: rhow
    real(fp)                            , pointer :: z0
    real(fp)                            , pointer :: z0v
    real(fp)                            , pointer :: vonkar
    real(fp)                            , pointer :: vicmol
    integer                             , pointer :: iro
    integer                             , pointer :: irov
    logical                             , pointer :: wind
    logical                             , pointer :: const
    logical                             , pointer :: wave
    logical                             , pointer :: veg3d
    real(fp)                            , pointer :: cde
    real(fp)                            , pointer :: cmukep
    real(fp)                            , pointer :: cep1
    real(fp)                            , pointer :: cep2
    real(fp)                            , pointer :: sigrho
    real(fp)                            , pointer :: cewall
    real(fp)                            , pointer :: zwi
    real(fp)                            , pointer :: ck
    integer                             , pointer :: inpzw
!
! Global variables
!
    integer                                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                                 :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                                 :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                 :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer                                                                 :: nto    !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: nubnd  !  Description and declaration in trisol.igs
    integer   , dimension(7, nto)                                           :: mnbnd  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: kdismx
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(2, ltur, 0:kmax, 2, nto)                          :: ubnd   !  Description and declaration in trisol.igs
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,3)            , intent(in)  :: cfurou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,3)            , intent(in)  :: cfvrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: deltau !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: deltav !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dfu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dfv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,4)            , intent(in)  :: dis    !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: hsurft !!  Turbulent length scal at surface (.5*hrms)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                            :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsv !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: aak    !!  Internal work array
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bbk    !!  Internal work array
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: cck    !!  Internal work array
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: pkwav  !!  temp variable turbulent tke prod due to breaking waves
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in)  :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)              :: rtur1  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdx    !!  Internal work array, implicit coupling of concentration in (N,M,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdy
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bux    !!  Internal work array, implicit coupling of concentration in (N,M,K) with layer concentration in (N,M+1,K)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: buy
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: diapl  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: rnpl   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: tkedis !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: tkepro !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: ueul   !!  Eulerian velocity in X-direction (including Stokes drift)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: umea   !!  Mean horizontal velocity
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: veul   !! Eulerian velocity in Y-direction (including Stokes drift)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: vmea   !!  Mean horizontal velocity
    real(fp)  , dimension(kmax)                               , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    character(8)                                              , intent(in)  :: dischy !  Description and declaration in tricom.igs
!
! Local variables
!
    integer :: icxy
    integer :: ij
    integer :: iter
    integer :: k
    integer :: kd
    integer :: ken
    integer :: knd
    integer :: knu
    integer :: ku
    integer :: kz
    integer :: l
    integer :: m
    integer :: maskval
    integer :: mmaxddb
    integer :: n
    integer :: ndm
    integer :: ndmd
    integer :: nm
    integer :: nmaxddb
    integer :: nmd
    integer :: nmu
    integer :: num
    real(fp):: alfa
    real(fp):: ap1
    real(fp):: ap2
    real(fp):: apd
    real(fp):: apu
    real(fp):: bi
    real(fp):: buoflu
    real(fp):: deltas
    real(fp):: dfdis
    real(fp):: dfus
    real(fp):: dfvs
    real(fp):: dia
    real(fp):: difd
    real(fp):: difu
    real(fp):: du
    real(fp):: dv
    real(fp):: dx
    real(fp):: dy
    real(fp):: dz
    real(fp):: dz1
    real(fp):: dz2
    real(fp):: epsd    ! Underbound denominator 
    real(fp):: epswav
    real(fp):: epswin
    real(fp):: fac
    real(fp):: fact
    real(fp):: fplant
    real(fp):: h0
    real(fp):: hsurf
    real(fp):: pransm
    real(fp):: r3
    real(fp):: rn
    real(fp):: s
    real(fp):: tauinv
    real(fp):: tauifl
    real(fp):: tflow
    real(fp):: timest
    real(fp):: tkebot
    real(fp):: tkepl
    real(fp):: tkewav
    real(fp):: tkewin
    real(fp):: udo
    real(fp):: um2
    real(fp):: umk2
    real(fp):: umku2
    real(fp):: us
    real(fp):: utot
    real(fp):: uu
    real(fp):: uup
    real(fp):: uuu
    real(fp):: vcc
    real(fp):: vdo
    real(fp):: viww
    real(fp):: vs
    real(fp):: vup
    real(fp):: vv
    real(fp):: vvv
    real(fp):: wdo
    real(fp):: wk
    real(fp):: wrkplt
    real(fp):: wu
    real(fp):: wup
    real(fp):: www
    real(fp):: xlveg
    real(fp):: z00
    real(fp):: zw
    real(fp):: zwc
    integer :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
    data epsd/1.0e-20/
!
!! executable statements -------------------------------------------------------
!
    !
    !  Von Karman constant set in subroutine TEKCOF
    !
    eps         => gdp%gdconst%eps
    clplant     => gdp%gdveg3d%clplant
    lundia      => gdp%gdinout%lundia
    hdt         => gdp%gdnumeco%hdt
    rhow        => gdp%gdphysco%rhow
    z0          => gdp%gdphysco%z0
    z0v         => gdp%gdphysco%z0v
    vonkar      => gdp%gdphysco%vonkar
    vicmol      => gdp%gdphysco%vicmol
    iro         => gdp%gdphysco%iro
    irov        => gdp%gdphysco%irov
    wind        => gdp%gdprocs%wind
    const       => gdp%gdprocs%const
    wave        => gdp%gdprocs%wave
    veg3d       => gdp%gdprocs%veg3d
    cde         => gdp%gdturcoe%cde
    cmukep      => gdp%gdturcoe%cmukep
    cep1        => gdp%gdturcoe%cep1
    cep2        => gdp%gdturcoe%cep2
    sigrho      => gdp%gdturcoe%sigrho
    cewall      => gdp%gdturcoe%cewall
    zwi         => gdp%gdturcoe%zwi
    ck          => gdp%gdturcoe%ck
    inpzw       => gdp%gdturcoe%inpzw
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    mmaxddb = mmax + 2*gdp%d%ddbound
    nm_pos  = 1
    !
    icxy = max(icx, icy)
    if (dischy=='cn') then
       timest = hdt
    else
       timest = 2 * hdt
    endif
    !
    !  mean horizontal velocities at layer interfaces
    !
    do k = 1, kmax - 1
       do nm = 1, nmmax
          nmd = nm - icx
          ndm = nm - icy
          if (kfs(nm) == 1) then
             umea(nm, k) = 0.5 * (u1(nm, k) + u1(nmd, k) + u1(nm, k + 1)          &
                         & + u1(nmd, k + 1)) / (1 + kfu(nm)*kfu(nmd))
             vmea(nm, k) = 0.5 * (v1(nm, k) + v1(ndm, k) + v1(nm, k + 1)          &
                         & + v1(ndm, k + 1)) / (1 + kfv(nm)*kfv(ndm))
          endif
       enddo
    enddo
    !
    !  initialisation coefficients internal layers
    !  arrays bux, bdy, buy and bdy are initialised for all (nm,k)
    !
    bux = 0.0
    bdx = 0.0
    buy = 0.0
    bdy = 0.0
    !
    do k = 1, kmax - 1
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             bbk(nm, k) = 1.0 / timest
          endif
       enddo
    enddo
    !
    ! advection in horizontal u direction
    !
    do k = 1, kmax - 1
       nmd = -icx
       do nm = 1, nmmax
          nmd = nmd + 1
          if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
             uuu = umea(nm, k)
             udo = 0.5 * (uuu + abs(uuu)) * kfu(nmd) / (gvu(nmd) + 1.0 - kfu(nmd))
             uup = 0.5 * (uuu - abs(uuu)) * kfu(nm ) / (gvu(nm ) + 1.0 - kfu(nm ))
             bux(nm, k) = bux(nm, k) + uup
             bbk(nm, k) = bbk(nm, k) - uup + udo
             bdx(nm, k) = bdx(nm, k) - udo
          endif
       enddo
    enddo
    !
    ! advection in horizontal v direction
    !
    do k = 1, kmax - 1
       ndm = -icy
       do nm = 1, nmmax
          ndm = ndm + 1
          if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
             vvv = vmea(nm, k)
             vdo = 0.5 * (vvv + abs(vvv)) * kfv(ndm) / (guv(ndm) + 1.0 - kfv(ndm))
             vup = 0.5 * (vvv - abs(vvv)) * kfv(nm ) / (guv(nm ) + 1.0 - kfv(nm ))
             buy(nm, k) = buy(nm, k) + vup
             bbk(nm, k) = bbk(nm, k) - vup + vdo
             bdy(nm, k) = bdy(nm, k) - vdo
          endif
       enddo
    enddo
    !
    do l = 1, ltur
       do k = 1, kmax - 1
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                ddk(nm, k) = rtur0(nm, k, l) / timest
             endif
          enddo
       enddo
       !
       ! user defined boundary conditions if nubnd <> 0
       ! in two directions
       !
       if (nubnd /= 0) then
          call usrbcc(j         ,nmmaxj    ,kmax      ,l         , &
                    & icx       ,icy       ,nto       ,ltur      , &
                    & mnbnd     ,ubnd      ,aak       ,bbk       ,cck       , &
                    & ddk       ,rtur0     ,rtur1     ,gdp       )
       endif
       !
       ! iteration loop
       !
       do k = 0, kmax
          do nm = 1, nmmax
             rtur1(nm, k, l) = rtur0(nm, k, l)
          enddo
       enddo
       do k = 1, kmax - 1
          !
          ! layers are decoupled
          !
          do iter = 1, 2
             !
             ! iterative solution method using Gauss Seidel
             ! in horizontal direction
             !
             do ij = 2, nmaxddb + mmaxddb
                nm  = ij*icx - icxy       + (max(1, ij - mmaxddb) - 1)*(icy - icx)
                nmd = ij*icx - icxy - icx + (max(1, ij - mmaxddb) - 1)*(icy - icx)
                ndm = ij*icx - icxy - icy + (max(1, ij - mmaxddb) - 1)*(icy - icx)
                !VDIR NODEP
                do n = max(1, ij - (mmaxddb)), min(nmaxddb, ij - 1)
                   nm  = nm  + (icy - icx)
                   nmd = nmd + (icy - icx)
                   ndm = ndm + (icy - icx)
                   if (kfs(nm) == 1) then
                      if (umea(nm, k) >= 0.0 .and. vmea(nm, k) >= 0.0) then
                         rtur1(nm, k, l) = (ddk(nm, k) - bdx(nm, k)*rtur1(nmd, k , l)    & 
                                         & - bdy(nm, k)*rtur1(ndm, k, l)) / bbk(nm, k)
                      endif
                   endif
                enddo
             enddo
             do ij = 1 - (mmaxddb), nmaxddb - 1
                nm  = -ij*icx - icxy       + (max(1, 1 + ij) - 1)*(icy + icx)
                nmu = -ij*icx - icxy + icx + (max(1, 1 + ij) - 1)*(icy + icx)
                ndm = -ij*icx - icxy - icy + (max(1, 1 + ij) - 1)*(icy + icx)
                !VDIR NODEP
                do n = max(1, 1 + ij), min(nmaxddb, mmaxddb + ij)
                   nm  = nm  + (icy + icx)
                   nmu = nmu + (icy + icx)
                   ndm = ndm + (icy + icx)
                   if (kfs(nm) == 1) then
                      if (umea(nm, k) < 0.0 .and. vmea(nm, k) >= 0.0) then
                         rtur1(nm, k, l) = (ddk(nm, k) - bux(nm, k)*rtur1(nmu, k , l)    &
                                         & - bdy(nm, k)*rtur1(ndm, k, l))/bbk(nm, k)
                      endif
                   endif
                enddo
             enddo
             do ij = nmaxddb - 1, 1 - (mmaxddb), -1
                nm  = -ij*icx - icxy       + (min(nmaxddb, mmaxddb + ij) + 1)*(icy + icx)
                nmd = -ij*icx - icxy - icx + (min(nmaxddb, mmaxddb + ij) + 1)*(icy + icx)
                num = -ij*icx - icxy + icy + (min(nmaxddb, mmaxddb + ij) + 1)*(icy + icx)
                !VDIR NODEP
                do n = min(nmaxddb, mmaxddb + ij), max(1, 1 + ij), -1
                   nm  = nm  - (icy + icx)
                   nmd = nmd - (icy + icx)
                   num = num - (icy + icx)
                   if (kfs(nm) == 1) then
                      if (umea(nm, k) >= 0.0 .and. vmea(nm, k) < 0.0) then
                         rtur1(nm, k, l) = (ddk(nm, k) - bdx(nm, k)*rtur1(nmd, k , l)    &
                                         & - buy(nm, k)*rtur1(num, k, l))/bbk(nm, k)
                      endif
                   endif
                enddo
             enddo
             do ij = nmaxddb + mmaxddb, 2, -1
                nm  = ij*icx - icxy       + (min(nmaxddb, ij - 1) + 1)*(icy - icx)
                nmu = ij*icx - icxy + icx + (min(nmaxddb, ij - 1) + 1)*(icy - icx)
                num = ij*icx - icxy + icy + (min(nmaxddb, ij - 1) + 1)*(icy - icx)
                !VDIR NODEP
                do n = min(nmaxddb, ij - 1), max(1, ij - (mmaxddb)), -1
                   nm  = nm  - (icy - icx)
                   nmu = nmu - (icy - icx)
                   num = num - (icy - icx)
                   if (kfs(nm) == 1) then
                      if (umea(nm, k) < 0.0 .and. vmea(nm, k) < 0.0) then
                         rtur1(nm, k, l) = (ddk(nm, k) - bux(nm, k)*rtur1(nmu, k , l)    &
                                         & - buy(nm, k)*rtur1(num, k, l))/bbk(nm, k)
                      endif
                   endif
                enddo
             enddo
          enddo
          !
          ! Delft3D-16494: NOT NECESSARY?
          ! Only 2 iterations are performed and the halo is 3 wide,
          ! so no communication should be necessary
          !
          ! Exchange rtur1 with neighbours for parallel runs
          !
          call dfexchg ( rtur1(:, :, l), 1, kmax-1, dfloat, nm_pos, gdp )
       enddo
    enddo
    !
    ! production term (only vertical gradients)
    !
    do nm = 1, nmmax
       if (kfs(nm) == 1) then
          nmd = nm - icx
          ndm = nm - icy
          do k = 1, kmax - 1
             if (kcs(nm) == 3) then
                maskval = kcs(nm) - 2
             else
                maskval = kcs(nm)
             endif
             bdy(nm, k) = 0.25 * maskval * (dudz(nm, k)**2 + dudz(nmd, k)**2           &
                        & + dvdz(nm, k)**2 + dvdz(ndm, k)**2)
          enddo
       endif
    enddo
    !
    ! production term (only horizontal gradients if IROV=1)
    !
    if (irov > 0) then
       do k = 1, kmax
          do nm = 1, nmmax
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                nmu = nm + icx
                num = nm + icy
                nmd = nm - icx
                ndm = nm - icy
                dx = 0.5*(gvv(nm) + gvv(ndm))
                dy = 0.5*(guu(nm) + guu(nmd))
                bdx(nm, k) =   ((u1(nm, k) - u1(nmd, k))/dx)**2                   &
                           & + ((v1(nm, k) - v1(ndm, k))/dy)**2
             else
                bdx(nm, k) = 0.0
             endif
          enddo
          do nm = 1, nmmax
             nmu = nm + icx
             num = nm + icy
             nmd = nm - icx
             ndm = nm - icy
             kz = kfv(nm) * kfv(nmu) * kfu(nm) * kfu(num)
             if (kz == 1) then
                dx = 0.5 * (gvv(nm) + gvv(nmu))
                dy = 0.5 * (guu(nm) + guu(num))
                bux(nm, k) = ((u1(num, k) - u1(nm, k))/dy + (v1(nmu, k) - v1(nm,k))/dx)**2
             else
                bux(nm, k) = 0.0
             endif
          enddo
       enddo
       do k = 1, kmax - 1
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                nmd  = nm - icx
                ndm  = nm - icy
                ndmd = nm - icx - icy
                bdy(nm, k) = bdy(nm, k) + 0.5*(bdx(nm, k) + bdx(nm, k + 1))            &
                           & + ((bux(nm  , k    ) + bux(nmd , k    ) + bux(ndm, k)     &
                           & +   bux(ndmd, k    ) + bux(nm  , k + 1) + bux(nmd, k + 1) &
                           & +   bux(ndm , k + 1) + bux(ndmd, k + 1))/16.0)
             endif
          enddo
       enddo
    endif
    !
    ! Second step : vertical advection, diffusion, production and diffusion
    !
    do l = 1, ltur
       if (l == 1) pransm = 1.0
       if (l == 2) pransm = 1.3
       do k = 0, kmax
          do nm = 1, nmmax
             aak(nm, k) = 0.0
             bbk(nm, k) = 1.0 / timest
             cck(nm, k) = 0.0
          enddo
       enddo
       do k = 1, kmax - 1
          do nm = 1, nmmax
             ddk(nm, k) = rtur1(nm, k, l) / timest
          enddo
       enddo
       do k = 1, kmax - 1
          ku = k + 1
          do nm = 1, nmmax
             !
             ! ADVECTION IN VERTICAL DIRECTION
             !
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                h0 = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                dz1 = thick(k ) * h0
                dz2 = thick(ku) * h0
                www = w1(nm, k)
                wdo = 0.5 * (www - abs(www)) / dz1
                wup = 0.5 * (www + abs(www)) / dz2
                if (k == kmax - 1) wup = 0.0
                aak(nm, k) = aak(nm, k) + wdo
                bbk(nm, k) = bbk(nm, k) - wdo + wup
                cck(nm, k) = cck(nm, k) - wup
             endif
          enddo
       enddo
       do k = 1, kmax - 1
          ku = k + 1
          kd = k - 1
          if (l == 2 .and. k == 1) then
             knd = kd
          else
             knd = k
          endif
          if (l == 2 .and. k == kmax - 1) then
             knu = ku
             vcc = 0.
          else
             knu = k
             vcc = vicmol
          endif
          do nm = 1, nmmax
             !
             ! DIFFUSION IN VERTICAL DIRECTION
             !
             if (kfs(nm) == 1) then
                h0   = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                dz1  = thick(k    ) * h0
                dz2  = thick(k + 1) * h0
                dz   = 0.5 * (thick(k) + thick(k + 1)) * h0
                if (.not. veg3d) then
                   apd = 1.0
                   apu = 1.0
                else
                   !
                   ! (Rigid) 3D Vegetation Model
                   !
                   apd = 1.0 - diapl(nm,k )*diapl(nm,k )*rnpl(nm,k )*pi*0.25
                   apu = 1.0 - diapl(nm,ku)*diapl(nm,ku)*rnpl(nm,ku)*pi*0.25
                endif
                difu = (vcc    + 0.5*(vicww(nm, knu) + vicww(nm, ku ))/pransm) * apu / (dz2*dz)
                difd = (vicmol + 0.5*(vicww(nm, kd ) + vicww(nm, knd))/pransm) * apd / (dz1*dz)
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
          do k = 1, kmax - 1
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   buoflu     = vicww(nm, k) * bruvai(nm, k) / sigrho
                   ddk(nm, k) = ddk(nm, k) - min(0.0_fp, buoflu) + tkepro(nm, k)
                   bbk(nm, k) = bbk(nm, k) + (max(0.0_fp, buoflu) + tkedis(nm, k)) &
                              & /max(rtur0(nm, k, 1), epsd)
                endif
             enddo
          enddo
          !
          ! (Rigid) 3D Vegetation Model
          !
          if (veg3d) then
             do nm = 1, nmmax
                h0 = max(0.01_fp,s1(nm)+real(dps(nm),fp))
                if (kfs(nm) == 1) then
                   do k = 1, kmax-1
                      ku  = k+1
                      wu  = thick(k) / (thick(k)+thick(ku))
                      wk  = 1.0-wu
                      rn  = wk*rnpl (nm,k) + wu*rnpl (nm,ku)
                      dia = wk*diapl(nm,k) + wu*diapl(nm,ku)
                      if (rn > 0) then
                         ap1       = 1.0 - dia*dia*rn*pi*0.25
                         umk2      = u1(nm,k) *u1(nm,k)  + v1(nm,k) *v1(nm,k)
                         umku2     = u1(nm,ku)*u1(nm,ku) + v1(nm,ku)*v1(nm,ku)
                         um2       = wk*umk2 + wu*umku2
                         dz        = h0 * (thick(k)+thick(ku)) / 2.0
                         fplant    = 0.5 * dia * rn * um2
                         tkepl     = fplant * sqrt(um2)
                         ddk(nm,k) = ddk(nm,k) + tkepl
                      endif
                   enddo
                endif
             enddo
          endif
          !
          ! production term horizontal and vertical gradients
          !
          do k = 1, kmax - 1
             do nm = 1, nmmax
                !
                ! Reset pkwav tke sources due to waves
                !
                pkwav(nm, k   ) = 0.
                pkwav(nm, kmax) = 0.
                pkwav(nm, 0   ) = 0.
                if (kfs(nm) == 1) then
                   ddk(nm, k) = ddk(nm, k) + 2*(vicmol + vicww(nm, k))*bdy(nm, k)
                endif
             enddo
          enddo
          !
          ! production term due to breaking of waves
          !
          ! first: determine length (vertical) of distribution of
          !        turbulent energy and fill production term in right hand side
          !
          ! The extent of wave dissipation due to breaking of waves
          ! is HRMS from the free surface downwards.
          !
          if (wave) then
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   h0           = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                   pkwav(nm, 0) =  2.0_fp*(dis(nm,2)+dis(nm,3))/(rhow*max(hrms(nm),0.01_fp))
                   zw = 0.0_fp
                   do k = 1, kmax
                      zw = zw+thick(k)*h0
                      if (hrms(nm) > zw) then
                         pkwav(nm,k) = pkwav(nm,0)*(1.0_fp - zw/(hrms(nm)))
                         ddk(nm,k)   = ddk(nm,k) + pkwav(nm, k)
                      else
                         kdismx(nm) = k-1
                         !
                         exit
                      endif
                   enddo
                endif
             enddo
          endif
          !
          ! addition of wall production due to slip on vertical walls
          !
          if (irov == 1) then
             do k = 1, kmax - 1
                ku = k + 1
                do nm = 1, nmmax
                   if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                      nmd = nm - icx
                      ndm = nm - icy
                      nmu = nm + icx
                      num = nm + icy
                      if (kcs(num) == 0) then
                         uu = 0.25 * (u1(nm, k) + u1(nmd, k) + u1(nm, ku) + u1(nmd, ku))
                         us = vonkar * uu / log((0.25*(guu(nm) + guu(nmd)) + z0v)/z0v)
                         du = abs(us) / (0.25*(guu(nm) + guu(nmd)) + z0v)
                         ddk(nm, k) = ddk(nm, k) + 0.5*du*us*us
                         bdy(nm, k) = bdy(nm, k) + 0.5*du*du
                      endif
                      if (kcs(ndm) == 0) then
                         uu = 0.25 * (u1(nm, k) + u1(nmd, k) + u1(nm, ku) + u1(nmd, ku))
                         us = vonkar * uu / log((0.25*(guu(nm) + guu(nmd)) + z0v)/z0v)
                         du = abs(us) / (0.25*(guu(nm) + guu(nmd)) + z0v)
                         ddk(nm, k) = ddk(nm, k) + 0.5*du*us*us
                         bdy(nm, k) = bdy(nm, k) + 0.5*du*du
                      endif
                      if (kcs(nmu) == 0) then
                         vv = 0.25 * (v1(nm, k) + v1(ndm, k) + v1(nm, ku) + v1(ndm, ku))
                         vs = vonkar * vv / log((0.25*(gvv(nm) + gvv(nmd)) + z0v)/z0v)
                         dv = abs(vs) / (0.25*(guu(nm) + guu(nmd)) + z0v)
                         ddk(nm, k) = ddk(nm, k) + 0.5*dv*vs*vs
                         bdy(nm, k) = bdy(nm, k) + 0.5*dv*dv
                      endif
                      if (kcs(nmd) == 0) then
                         vv = 0.25 * (v1(nm, k) + v1(ndm, k) + v1(nm, ku) + v1(ndm, ku))
                         vs = vonkar * vv / log((0.25*(gvv(nm) + gvv(nmd)) + z0v)/z0v)
                         dv = abs(vs) / (0.25*(guu(nm) + guu(nmd)) + z0v)
                         ddk(nm, k) = ddk(nm, k) + 0.5*dv*vs*vs
                         bdy(nm, k) = bdy(nm, k) + 0.5*dv*dv
                      endif
                   endif
                enddo
             enddo
          endif
       else
          !
          ! EPSILON equation
          ! buoyancy term
          !
          do k = 1, kmax - 1
             do nm = 1, nmmax
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
                   !
                   ! In a previous approach the TKE was taken implicitly (RTUR1(NM,K,1))
                   !
                   ddk(nm, k) = ddk(nm, k)                                       &
                              & + cep1*(buoflu - tkepro(nm, k))*rtur0(nm, k, 2)  &
                              & /max(rtur0(nm, k, 1), epsd)
                   bbk(nm, k) = bbk(nm, k)                                       &
                              & + cep1*tkedis(nm, k)/max(rtur0(nm, k, 1), epsd)
                endif
             enddo
          enddo
          !
          ! plants eps
          !
          if (veg3d) then
             do nm = 1,nmmax
                h0 = max (0.01_fp , s1(nm)+real(dps(nm),fp))
                if (kfs(nm) == 1) then
                   do k = 1, kmax-1
                      fac = cep2 * sqrt(cmukep)
                      r3  = 1.0 / 3.0
                      ku  = k + 1
                      wu  = thick(k) / (thick(k)+thick(ku))
                      wk  = 1.0 - wu
                      rn  = wk*rnpl (nm,k) + wu*rnpl (nm,ku)
                      dia = wk*diapl(nm,k) + wu*diapl(nm,ku)
                      if (rn > 0) then
                         ap1 = 1.0 - dia*dia*rn*pi*0.25
                         !
                         ! typical length between plants
                         !
                         xlveg = clplant * sqrt(ap1/rn)
                         umk2  = u1(nm,k )*u1(nm,k ) + v1(nm,k )*v1(nm,k )
                         umku2 = u1(nm,ku)*u1(nm,ku) + v1(nm,ku)*v1(nm,ku)
                         um2   = wk*umk2 + wu*umku2
                         if (um2 > 0) then
                            fplant = 0.5 * dia * rn * um2
                            wrkplt = fplant * sqrt(um2)
                            tauinv = fac * (wrkplt/xlveg**2)**r3
                            if (rtur0(nm,k,1) > 0) then
                               tauifl = rtur0(nm,k,2)*cep2/rtur0(nm,k,1)
                               tauinv = max(tauinv,tauifl)
                            endif
                            ddk(nm,k) = ddk(nm,k) + wrkplt*tauinv
                         endif
                      endif
                   enddo
                endif
             enddo
          endif
          !
          ! production term horizontal and vertical gradients
          !
          do k = 1, kmax - 1
             nmd = -icx
             ndm = -icy
             do nm = 1, nmmax
                nmd = nmd + 1
                ndm = ndm + 1
                if (kfs(nm) == 1) then
                   ddk(nm, k) = ddk(nm, k) + 2*cmukep*cep1*rtur0(nm, k, 1)*bdy(nm, k)
                endif
             enddo
          enddo
          !
          if (wave) then
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   !
                   ! production term breaking waves in epsilon
                   !
                   do k = 1, kdismx(nm)
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
          do k = 1, kmax - 1
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   viww       = vicmol + vicww(nm, k)
                   ddk(nm, k) = ddk(nm, k) +    cmukep*(rtur0(nm, k, 1)**2)/viww
                   bbk(nm, k) = bbk(nm, k) + 2.*cmukep* rtur0(nm, k, 1)    /viww
                endif
             enddo
          enddo
       !
       ! dissipation k-epsilon model (Newton linearisation)
       !
       elseif (l == 1) then
          do k = 1, kmax - 1
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   ddk(nm, k) = ddk(nm, k) +    rtur0(nm, k, 2)
                   bbk(nm, k) = bbk(nm, k) + 2.*rtur0(nm, k, 2)/max(rtur0(nm, k, 1), epsd)
                endif
             enddo
          enddo
       else
          !
          ! dissipation in epsilon equation (Newton linearisation)
          ! In a previous approach the TKE was taken implicitly (RTUR1(NM,K,1))
          !
          do k = 1, kmax - 1
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   ddk(nm, k) = ddk(nm, k) +    cep2*(rtur0(nm, k, 2)**2)/max(rtur0(nm, k, 1), epsd)
                   bbk(nm, k) = bbk(nm, k) + 2.*cep2* rtur0(nm, k, 2)    /max(rtur0(nm, k, 1), epsd)
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
          nmd = -icx
          ndm = -icy
          do nm = 1, nmmax
             nmd = nmd + 1
             ndm = ndm + 1
             !
             ! at free surface
             !
             if (kfs(nm) == 1) then
                if (kcs(nm) == 3) then
                   maskval = kcs(nm) - 2
                else
                   maskval = kcs(nm)
                endif
                !
                tkewin     = sqrt((windsu(nm)**2 + windsv(nm)**2)/cmukep) / rhow
                aak(nm, 0) = 0.0
                bbk(nm, 0) = 1.0
                cck(nm, 0) = 0.0
                ddk(nm, 0) = tkewin
                !
                ! Added production due to waves
                !
                if (wave) then
                   !
                   ! Inserted flux boundary type
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
                   tkewav = (epswav*vonkar*0.5_fp*hrms(nm)/(30.0_fp*cde))**(2.0_fp/3.0_fp)
                   !
                   ddk(nm, 0) = ddk(nm, 0) + tkewav
                endif
                !
                ! at bottom, using Eulerian velocities (corrected for Stokes drift)
                !
                aak(nm, kmax) = 0.0
                bbk(nm, kmax) = 1.0
                cck(nm, kmax) = 0.0
                uuu  = 0.5_fp*(ueul(nmd, kmax) + ueul(nm, kmax))*maskval
                vvv  = 0.5_fp*(veul(ndm, kmax) + veul(nm, kmax))*maskval
                utot = sqrt(uuu*uuu + vvv*vvv)
                h0   = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                dz   = 0.5 * thick(kmax) * h0
                ken  = kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm)
                s    =  (kfu(nmd)*cfurou(nmd,1) + kfu(nm)*cfurou(nm,1) + kfv(ndm)*cfvrou(ndm,1)    &
                     & + kfv(nm )*cfvrou(nm ,1)) / ken
                ddk(nm, kmax) = utot * utot / (s*s*sqrt(cmukep))
             endif
          enddo
       endif
       !
       ! dissipation at free surface and bottom
       !
       if (l == 2) then
          nmd = -icx
          ndm = -icy
          do nm = 1, nmmax
             nmd = nmd + 1
             ndm = ndm + 1
             !
             ! at free surface
             ! zw is input or calculated value, depending on inpzw
             !
             if (kfs(nm) == 1) then
                if (kcs(nm) == 3) then
                   maskval = kcs(nm) - 2
                else
                   maskval = kcs(nm)
                endif
                !
                h0         = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                zwc        = 0.5 * thick(1) * h0
                zw         = inpzw*zwi + (1 - inpzw)*zwc
                tkewin     = sqrt((windsu(nm)**2 + windsv(nm)**2)/cmukep) / rhow
                epswin     = cewall * (tkewin**1.5) / zw
                aak(nm, 0) = 0.0
                bbk(nm, 0) = 1.0
                cck(nm, 0) = 0.0
                ddk(nm, 0) = epswin
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
                   ddk(nm, 0) = ddk(nm, 0) + epswav
                endif
                !
                ! at bottom, using Eulerian velocities (corrected for Stokes drift)
                !
                uuu  = 0.5_fp * (ueul(nmd, kmax) + ueul(nm, kmax)) * maskval
                vvv  = 0.5_fp * (veul(ndm, kmax) + veul(nm, kmax)) * maskval
                utot = sqrt(uuu*uuu + vvv*vvv)
                h0   = max(0.01_fp, s1(nm) + real(dps(nm),fp))
                dz   = 0.5 * thick(kmax) * h0
                ken  = kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm)
                s    = ( kfu(nmd)*cfurou(nmd,1) + kfu(nm)*cfurou(nm,1) + kfv(ndm)*cfvrou(ndm,1)    &
                     & + kfv(nm )*cfvrou(nm ,1) ) / ken
                z00  = ( kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) + kfv(ndm)*z0vrou(ndm)     &
                     & + kfv(nm )*z0vrou(nm ) ) / ken
                tkebot = utot * utot / (s*s*sqrt(cmukep))
                aak(nm, kmax) = 0.0
                bbk(nm, kmax) = 1.0
                cck(nm, kmax) = 0.0
                ddk(nm, kmax) = cewall * (tkebot**1.5) / z00
             endif
          enddo
       endif
       !
       ! user defined boundary conditions if nubnd <> 0
       ! in two directions
       !
       if (nubnd /= 0) then
          call usrbcc(j         ,nmmaxj    ,kmax      ,l         , &
                    & icx       ,icy       ,nto       ,ltur      , &
                    & mnbnd     ,ubnd      ,aak       ,bbk       ,cck       , &
                    & ddk       ,rtur0     ,rtur1     ,gdp       )
       endif
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       do k = 1, kmax
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                bi          = 1.0_fp / bbk(nm, k)
                aak (nm, k) = aak (nm, k) * bi
                bbk (nm, k) = 1.0_fp
                cck (nm, k) = cck (nm, k) * bi
                ddk (nm, k) = ddk (nm, k) * bi
             endif
          enddo
       enddo
       !
       !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       !
       ! Division by the pivot for k=0 is not needed anymore
       ! because of row scaling
       !
       !
       do k = 1, kmax
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                bbk(nm, k) = 1.0 / (bbk(nm, k) - aak(nm, k)*cck(nm, k - 1))
                cck(nm, k) = cck(nm, k) * bbk(nm, k)
                ddk(nm, k) = (ddk(nm, k) - aak(nm, k)*ddk(nm, k - 1)) * bbk(nm, k)
             endif
          enddo
       enddo
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             rtur1(nm, kmax, l) = ddk(nm, kmax)
          endif
       enddo
       do k = kmax - 1, 0, -1
          do nm = 1, nmmax
             if (kfs(nm) == 1) then
                rtur1(nm, k, l) = ddk(nm, k) - cck(nm, k)*rtur1(nm, k + 1, l)
             endif
          enddo
       enddo
       !
       ! Delft3D-16494: NOT NECESSARY? no implicit solve took place
       !
       ! Exchange rtur1 with neighbours for parallel runs
       !
       call dfexchg ( rtur1(:, :, l), 0, kmax, dfloat, nm_pos, gdp )
    enddo
end subroutine tratur
