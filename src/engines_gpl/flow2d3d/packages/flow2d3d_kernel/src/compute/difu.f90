subroutine difu(icreep    ,timest    ,lundia    ,nst       ,icx       , &
              & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
              & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
              & lsec      ,lsed      ,lsts      ,norow     ,irocol    , &
              & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
              & kadu      ,kadv      ,s0        ,s1        ,hu        , &
              & hv        ,dps       ,qxk       ,qyk       ,qzk       , &
              & guu       ,gvv       ,guv       ,gvu       ,gsqs      , &
              & rbnd      ,sigdif    ,sigmol    ,r0        ,r1        , &
              & sour      ,sink      ,ws        ,sedtyp    ,thick     , &
              & sig       ,dicuv     ,vicww     ,dsdksi    ,dsdeta    , &
              & dtdksi    ,dtdeta    ,aak       ,bbk       ,cck       , &
              & bdddx     ,bddx      ,bdx       ,bux       ,buux      , &
              & buuux     ,uvdwk     ,vvdwk     ,areau     ,areav     , &
              & aakl      ,bbkl      ,cckl      ,ddkl      , &
              & eqmbcsand ,eqmbcmud  ,seddif    ,volum0    ,volum1    , &
              & rscale    ,bruvai    ,gdp       )
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
!    Function: Computes transport in the u, v and w-direction.
!              Implicit in the u- and w-direction, explicit in
!              v-direction.
!              Sinks are treated implicitly and sources explicit-
!              y. A special approach is used for the hori-
!              ontal diffusion to avoid artificial creeping.
! Method used: Reference : On the approximation of horizontal
!              gradients in sigma co-ordinates for bathymetry
!              with steep bottom slopes (G.S. Stelling and J.
!              van Kester - International Journal for Methods
!              in Fluids, Vol. 18 1994)
!              - Horizontal Advection in U-direction :
!                implicit, higher order upwind
!              - Horizontal Advection in V-direction :
!                explicit, central scheme
!              - Horizontal Diffusion :
!                3D: explicit, along Z-planes
!                2D: implicit in U-direction
!                    explicit in V-direction
!              - Option: horizontal diffusion strictly horizontal
!                using special filter
!              - Vertical Advection :
!                implicit, central scheme
!              - Vertical Diffusion : implicit
!              - Sources are integrated explicitly.
!              - Sinks are integrated implicitly.
!     Comment: For the Thatcher Harleman boundaries the boundary
!              points for outflow are reflected from the inner
!              points; for inflow the boundary conditions are
!              used (see also thahbc.for).
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use flow2d3d_timers
    use globaldata
    use dfparall
    use sediment_basics_module
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    include 'flow_steps_f.inc'
    integer                , pointer :: ad_itrmax
    integer                , pointer :: iro
    integer                , pointer :: mfg
    integer                , pointer :: nfg
    integer                , pointer :: nudge
    real(fp)               , pointer :: ad_epsabs
    real(fp)               , pointer :: ad_epsrel
    real(fp)               , pointer :: dryflc
    real(fp)               , pointer :: ck
    real(fp)               , pointer :: dicoww
    real(fp)               , pointer :: eps
    real(fp)               , pointer :: hdt

    real(fp)               , pointer :: vicmol
    real(fp)               , pointer :: xlo
    integer                , pointer :: max_mud_sedtyp
!
! Global variables
!
integer                                                 , intent(in)  :: icreep    !  Description and declaration in tricom.igs
integer                                                               :: icx       !!  Increment in the X-dir., if ICX= NMAX
                                                                                   !!  then computation proceeds in the X-
                                                                                   !!  dir. If icx=1 then computation pro-
                                                                                   !!  ceeds in the Y-dir.
integer                                                               :: icy       !!  Increment in the Y-dir. (see ICX)
integer                                                               :: j         !!  Begin pointer for arrays which have
                                                                                   !!  been transformed into 1D arrays.
                                                                                   !!  Due to the shift in the 2nd (M-)
                                                                                   !!  index, J = -2*NMAX + 1
integer                                                               :: kmax      !  Description and declaration in esm_alloc_int.f90
integer                                                               :: lsal      !  Description and declaration in dimens.igs
integer                                                 , intent(in)  :: lsec      !  Description and declaration in dimens.igs
integer                                                               :: lsecfl    !  Description and declaration in dimens.igs
integer                                                               :: lsed      !  Description and declaration in esm_alloc_int.f90
integer                                                 , intent(in)  :: lsts      !  Description and declaration in dimens.igs
integer                                                 , intent(in)  :: lstsc     !  Description and declaration in dimens.igs
integer                                                               :: lstsci    !  Description and declaration in esm_alloc_int.f90
integer                                                               :: ltem      !  Description and declaration in dimens.igs
integer                                                               :: lundia    !  Description and declaration in inout.igs
integer                                                               :: nmmax     !  Description and declaration in dimens.igs
integer                                                               :: nmmaxj    !  Description and declaration in dimens.igs
integer                                                               :: norow     !  Description and declaration in esm_alloc_int.f90
integer                                                 , intent(in)  :: nst
integer, dimension(5, norow)                                          :: irocol    !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kcs       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcu       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfs       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfu       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub)                             :: kfv       !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: kadu      !  Description and declaration in esm_alloc_int.f90
integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: kadv      !  Description and declaration in esm_alloc_int.f90
integer, dimension(lsed)                                , intent(in)  :: sedtyp    !! sediment type: 0=total/1=noncoh/2=coh
logical                                                 , intent(in)  :: eqmbcsand !  Description and declaration in morpar.igs
logical                                                 , intent(in)  :: eqmbcmud  !  Description and declaration in morpar.igs
real(fp)                                                , intent(in)  :: timest    !!  Half Integration time step [sec.]
real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                          :: dps       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gsqs      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guu       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: guv       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvu       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: gvv       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hu        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hv        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s0        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                            :: s1        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bruvai    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: vicww     !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)      , intent(in)  :: qzk       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed), intent(in)  :: seddif    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)              :: ws        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: aak       !!  Internal work array (in CUCNP & UZD)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bbk       !!  Internal work array (in CUCNP & UZD)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdddx     !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M-3,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bddx      !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M-2,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bdx       !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M-1,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: buuux     !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M+3,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: buux      !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M+2,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: bux       !!  Internal work array, implicit coup-
                                                                                   !!  ling of concentration in (N,M,K)
                                                                                   !!  with layer concentration in (N,M+1,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: cck       !!  Internal work array (in CUCNP & UZD)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)                    :: dicuv     !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dsdeta    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dsdksi    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dtdeta    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: dtdksi    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areau
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: areav
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qxk       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: qyk       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: rscale    !  Internal work array, row scaling parameter in difu
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: volum0
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in) :: volum1
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: uvdwk     !!  Internal work array for Jac.iteration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                      :: vvdwk     !!  Internal work array for Jac.iteration
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: aakl      !!  Internal work array, lower diagonal
                                                                                   !!  tridiagonal matrix, implicit coupling
                                                                                   !!  of concentration in (N,M,K) with con-
                                                                                   !!  centration in (N,M,K-1)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: bbkl      !!  Internal work array, main diagonal
                                                                                   !!  tridiagonal matrix, implicit coupling
                                                                                   !!  of concentration in (N,M,K)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: cckl      !!  Internal work array, upper diagonal
                                                                                   !!  tridiagonal matrix, implicit coupling
                                                                                   !!  of concentration in (N,M,K) with con-
                                                                                   !!  centration in (N,M,K+1)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: ddkl      !!  Internal work array, diagonal space
                                                                                   !!  at (N,M,K,L)
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r0        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: r1        !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sink      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)              :: sour      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(kmax)                                             :: sig       !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(kmax)                                             :: thick     !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(kmax, max(lstsc, 1), 2, norow)      , intent(in)  :: rbnd      !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(lstsci)                                           :: sigdif    !  Description and declaration in esm_alloc_real.f90
real(fp), dimension(lstsci)                             , intent(in)  :: sigmol    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
integer                 :: nmsta
integer                 :: ddb
integer                 :: iad1
integer                 :: iad2
integer                 :: iad3
integer                 :: ic
integer                 :: icxy
integer                 :: iter
integer                 :: itr
integer                 :: j1
integer                 :: j2
integer                 :: j3
integer                 :: jj
integer                 :: k
integer                 :: kfw
integer                 :: l
integer                 :: ll
integer                 :: ls
integer                 :: lst
integer                 :: maskval
integer                 :: mf
integer                 :: ml
integer                 :: n
integer                 :: ndm
integer                 :: nhystp
integer                 :: nm
integer                 :: nmd
integer                 :: nmdd
integer                 :: nmf
integer                 :: nmfu
integer                 :: nml
integer                 :: nmlu
integer, dimension(10)  :: nms
integer                 :: nmu
integer                 :: nmuu
integer                 :: nnudge
integer                 :: num
real(fp)                :: adza
real(fp)                :: adzc
real(fp)                :: bi
real(fp)                :: cl
real(fp)                :: cr
real(fp)                :: d0k    ! Internal work array
real(fp)                :: ddzc
real(fp)                :: difiwe
real(fp)                :: difl
real(fp)                :: difr
real(fp)                :: diz1
real(fp)                :: epsitr ! Maximum value of relative error and absolute error of iteration process
real(fp)                :: fac
real(fp)                :: flux
real(fp)                :: h0
real(fp)                :: h0i
real(fp)                :: h0new
real(fp)                :: h0old
real(fp)                :: hmin
real(fp), dimension(10) :: mu
real(fp)                :: nudgefac
real(fp)                :: qxu
real(fp)                :: qyv
real(fp)                :: qzw
real(fp)                :: rb
real(fp), external      :: reddic
real(fp)                :: rp
real(fp)                :: sqrtbv
real(fp)                :: timesti ! inverse of time step
real(fp)                :: tnudge
real(fp)                :: tsg
character(20)           :: errtxt
integer                 :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    eps         => gdp%gdconst%eps
    vicmol      => gdp%gdphysco%vicmol
    dicoww      => gdp%gdphysco%dicoww
    iro         => gdp%gdphysco%iro
    xlo         => gdp%gdturcoe%xlo
    ck          => gdp%gdturcoe%ck
    mfg         => gdp%gdparall%mfg
    nfg         => gdp%gdparall%nfg
    nudge       => gdp%gdnumeco%nudge
    hdt         => gdp%gdnumeco%hdt
    ad_itrmax   => gdp%gdnumeco%ad_itrmax
    ad_epsabs   => gdp%gdnumeco%ad_epsabs
    ad_epsrel   => gdp%gdnumeco%ad_epsrel
    dryflc      => gdp%gdnumeco%dryflc
    max_mud_sedtyp => gdp%gdsedpar%max_mud_sedtyp
    !
    ! INITIALISATION
    !
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    nm_pos = 1
    !
    !  INITIALIZE
    !
    call timer_start(timer_difu_ini, gdp)
    !
    ! Initialise arrays aak - cck for all (nm,k)
    !
    aak   = 0.0_fp
    buuux = 0.0_fp
    buux  = 0.0_fp
    bux   = 0.0_fp
    bdx   = 0.0_fp
    bddx  = 0.0_fp
    bdddx = 0.0_fp
    cck   = 0.0_fp
    !
    timesti = 1.0_fp / timest
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             bbk(nm, k) = volum1(nm, k) * timesti
          else
             bbk(nm, k) = 1.0_fp
             if (lsec > 0) r0(nm, k, lsecfl) = 0.0_fp
          endif
       enddo
    enddo
    do l = 1, lstsci
       if (lsec==2 .and. l==lsecfl) then
          cycle
       endif
       do k = 1, kmax
          do nm = 1, nmmax
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) * timesti
             else
                ddkl(nm, k, l) = r0(nm, k, l)
             endif
          enddo
       enddo
    enddo
    call timer_stop(timer_difu_ini, gdp)
    !
    ! CONTRIBUTION OF ADVECTION IN X-DIRECTION
    !
    call timer_start(timer_difu_horadv, gdp)
    do k = 1, kmax
       !
       ! CONTRIBUTION TO VOLUME NM AND NMU
       !
       nmd  = -icx
       nmdd = -icx - icx
       nmu  =  icx
       nmuu =  icx + icx
       do nm = 1, nmmax
          nmd  = nmd  + 1
          nmdd = nmdd + 1
          nmu  = nmu  + 1
          nmuu = nmuu + 1
          qxu  = qxk(nm, k)/6.0_fp
          if (qxu > 0.0) then
             iad1 =      kfu(nm)  *kadu(nm, k)
             iad2 = iad1*kfu(nmd) *kadu(nmd, k)
             iad3 = iad2*kfu(nmdd)*kadu(nmdd, k)
             !
             j1 = 6*iad1 + 3*iad2 +   iad3
             j2 =        - 3*iad2 - 2*iad3
             j3 =                     iad3
             !
             bbk  (nm , k) = bbk  (nm , k) + qxu*j1
             bdx  (nm , k) = bdx  (nm , k) + qxu*j2
             bddx (nm , k) = bddx (nm , k) + qxu*j3
             bdx  (nmu, k) = bdx  (nmu, k) - qxu*j1
             bddx (nmu, k) = bddx (nmu, k) - qxu*j2
             bdddx(nmu, k) = bdddx(nmu, k) - qxu*j3
          else
             iad1 =        kfu(nm)   * kadu(nm  , k)
             iad2 = iad1 * kfu(nmu)  * kadu(nmu , k)
             iad3 = iad2 * kfu(nmuu) * kadu(nmuu, k)
             !
             j1 = 6*iad1 + 3*iad2 +   iad3
             j2 =        - 3*iad2 - 2*iad3
             j3 =                     iad3
             !
             bux  (nm , k) = bux  (nm , k) + qxu*j1
             buux (nm , k) = buux (nm , k) + qxu*j2
             buuux(nm , k) = buuux(nm , k) + qxu*j3
             bbk  (nmu, k) = bbk  (nmu, k) - qxu*j1
             bux  (nmu, k) = bux  (nmu, k) - qxu*j2
             buux (nmu, k) = buux (nmu, k) - qxu*j3
          endif
       enddo
    enddo
    !
    ! CONTRIBUTION OF ADVECTION IN Y-DIRECTION
    !
    do l = 1, lstsci
       if (lsec==2 .and. l==lsecfl) then
          cycle
       endif
       do k = 1, kmax
          !
          ! CONTRIBUTION TO VOLUME NM AND NUM
          !
          ndm = -icy
          num =  icy
          do nm = 1, nmmax
             ndm = ndm + 1
             num = num + 1
             qyv  = qyk(nm, k)
             iad1 = kfv(nm)*kadv(nm, k)
             iad2 = iad1*kfv(num)*kadv(num, k)*kfv(ndm)*kadv(ndm, k)
             if (qyv > 0.0_fp) then
                d0k = 0.5_fp*qyv*( (2*iad1 - iad2)*r0(nm , k, l) &
                    &          +          iad2 *r0(num, k, l))
             else
                d0k = 0.5_fp*qyv*( (2*iad1 - iad2)*r0(num, k, l) &
                    &          +          iad2 *r0(nm , k, l))
             endif
             if (kcs(nm)  == 1) ddkl(nm , k, l) = ddkl(nm , k, l) - d0k
             if (kcs(num) == 1) ddkl(num, k, l) = ddkl(num, k, l) + d0k
          enddo
       enddo
    enddo
    call timer_stop(timer_difu_horadv, gdp)
    !
    !
    ! Explicit algoritm (call DIFACR) leads to extra stablity criterium
    ! DT <= (DX**2)/(2*DICUV)
    !
    ! This diffusion part (loop 410) is constituent independent.
    ! The value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
    call timer_start(timer_difu_hordiff, gdp)
    if (icreep==0 .or. kmax==1) then
       !
       ! HORIZONTAL DIFFUSION IN X-DIRECTION ALONG SIGMA PLANES
       !
       do k = 1, kmax
          !
          ! CONTRIBUTION TO VOLUME NM AND NMU
          !
          nmu = icx
          do nm = 1, nmmax
             nmu = nmu + 1
             hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
             if (kfu(nm)*kadu(nm, k) /= 0 .and. hmin > dryflc) then
                difl    = dicuv(nm, k)
                difr    = dicuv(nmu, k)
                flux    = 0.5_fp*(difl + difr)/(0.7_fp*gvu(nm))
                maskval = max(0, 2 - abs(kcs(nm)))
                bbk(nm, k) = bbk(nm, k) + areau(nm, k)*flux*maskval
                bux(nm, k) = bux(nm, k) - areau(nm, k)*flux*maskval
                maskval    = max(0, 2 - abs(kcs(nmu)))
                bbk(nmu, k) = bbk(nmu, k) + areau(nm, k)*flux*maskval
                bdx(nmu, k) = bdx(nmu, k) - areau(nm, k)*flux*maskval
             endif
          enddo
       enddo
       !
       ! HORIZONTAL DIFFUSION IN Y-DIRECTION ALONG SIGMA PLANES
       !
       do l = 1, lstsci
          if (lsec==2 .and. l==lsecfl) then
             cycle
          endif
          do k = 1, kmax
             !
             ! CONTRIBUTION TO VOLUME NM AND NUM
             !
             num = icy
             do nm = 1, nmmax
                num = num + 1
                hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
                if (kfv(nm)*kadv(nm, k) /= 0 .and. hmin > dryflc) then
                   cl      = r0(nm, k, l)
                   difl    = dicuv(nm, k)
                   cr      = r0(num, k, l)
                   difr    = dicuv(num, k)
                   flux    = 0.5_fp*(cr - cl)*(difl + difr)/(0.7_fp*guv(nm))
                   maskval = max(0, 2 - abs(kcs(nm)))
                   ddkl(nm, k, l)  = ddkl(nm, k, l) + areav(nm, k)*flux*maskval
                   maskval         = max(0, 2 - abs(kcs(num)))
                   ddkl(num, k, l) = ddkl(num, k, l) - areav(nm, k)*flux*maskval
                endif
             enddo
          enddo
       enddo
    else
       !
       ! Explicit algoritm (call DIFACR) leads to extra stablity criterium
       ! dt <= (dx**2)/(2*dicuv)
       !
       ! HORIZONTAL DIFFUSION ALONG Z-PLANES (only if KMAX > 1 and Anti Creep)
       !
       call difacr(icx       ,icy       ,j         ,nmmaxj    ,nmmax     , &
                 & kmax      ,lstsci    ,lsal      ,ltem      ,kcs       , &
                 & kfu       ,kfv       ,kadu      ,kadv      ,s0        , &
                 & dps       ,r0        ,ddkl      ,guu       ,gvv       , &
                 & guv       ,gvu       ,thick     ,sig       ,dicuv     , &
                 & sigdif    ,dsdksi    ,dtdksi    ,dsdeta    ,dtdeta    , &
                 & gdp       )
    endif
    call timer_stop(timer_difu_hordiff, gdp)
    call timer_start(timer_difu_vertadv, gdp)
    if (kmax > 1) then
       do k = 1, kmax - 1
          if (k==1 .or. k==kmax - 1) then
             kfw = 1
          else
             kfw = 0
          endif
          do nm = 1, nmmax
             !
             ! ADVECTION IN VERTICAL DIRECTION; W*DC/DZ
             !
             if (kfs(nm) == 1) then
                qzw = qzk(nm, k)
                if (qzw > 0.0) then
                   adza = 0.5_fp*qzw*(1 - kfw)
                   adzc = 0.5_fp*qzw*(1 + kfw)
                else
                   adza = 0.5_fp*qzw*(1 + kfw)
                   adzc = 0.5_fp*qzw*(1 - kfw)
                endif
                aak(nm, k + 1) = aak(nm, k + 1) + adza
                bbk(nm, k + 1) = bbk(nm, k + 1) + adzc
                bbk(nm, k    ) = bbk(nm, k    ) - adza
                cck(nm, k    ) = cck(nm, k    ) - adzc
             endif
          enddo
       enddo
    endif
    do l = 1, lstsci
       if (lsec==2 .and. l==lsecfl) then
          cycle
       endif
       do k = 1, kmax
          do nm = 1, nmmax
                aakl(nm, k, l) = aak(nm, k)
                bbkl(nm, k, l) = bbk(nm, k)
                cckl(nm, k, l) = cck(nm, k)
          enddo
       enddo
    enddo
    call timer_stop(timer_difu_vertadv, gdp)
    !
    ! DIFFUSION IN VERTICAL DIRECTION
    !
    call timer_start(timer_difu_vertdiff, gdp)
    if (kmax > 1) then
       do l = 1, lstsci
          !
          ! l = sediment: ls > 0
          ! else        : ls = 0
          !
          ls = 0
          if ((l>max(lsal, ltem)) .and. (l<=lsts)) ls = l - max(lsal, ltem)
          do k = 1, kmax - 1
             tsg = 0.5_fp * (thick(k) + thick(k+1))
             do nm = 1, nmmax
                if (kfs(nm) == 1) then
                   h0  = max(0.1_fp, s0(nm) + real(dps(nm),fp))
                   h0i = 1.0_fp / h0
                   !
                   ! Internal wave contribution
                   !
                   sqrtbv = max(0.0_fp, bruvai(nm, k))
                   sqrtbv = sqrt(sqrtbv)
                   difiwe = 0.2_fp * sqrtbv * xlo**2
                   if (ls > 0) then
                      !
                      ! sediment constituent:
                      ! No dicoww-restriction in reddic
                      !
                      diz1 = vicmol/sigmol(l) + difiwe + seddif(nm, k, ls)/sigdif(l)
                   else
                      !
                      ! all other constituents:
                      ! dicoww-restriction is moved from TURCLO to here (in reddic)
                      ! vicww is used instead of dicww
                      !
                      diz1 = vicmol/sigmol(l) + reddic(difiwe + vicww(nm,k)/sigdif(l), gdp)
                   endif
                   ddzc             = gsqs(nm) * diz1 * h0i / tsg
                   aakl(nm, k+1, l) = aakl(nm, k+1, l) - ddzc
                   bbkl(nm, k+1, l) = bbkl(nm, k+1, l) + ddzc
                   bbkl(nm, k  , l) = bbkl(nm, k  , l) + ddzc
                   cckl(nm, k  , l) = cckl(nm, k  , l) - ddzc
                endif
             enddo
          enddo
       enddo
    endif
    call timer_stop(timer_difu_vertdiff, gdp)
    !
    ! Include settling velocities and Dirichlet BC for sediments in
    ! matrices AAKL/BBKL/CCKL/DDKL
    !
    if (lsed > 0) then
       call timer_start(timer_difu_difws, gdp)
       call dif_ws(j         ,nmmaxj    ,nmmax     ,kmax      ,lsal      , &
                 & ltem      ,lstsci    ,lsed      ,kcs       ,kfs       , &
                 & gsqs      ,ws        ,aakl      ,bbkl      ,cckl      , &
                 & gdp       )
       call timer_stop(timer_difu_difws, gdp)
    endif
    !
    ! SET VALUES IN OPEN BOUNDARY POINTS (IN PART. FOR Y-DIRECTION)
    !     On open boundary no seconday flow (=> loop over LSTSC)
    !
    call timer_start(timer_difu_bounopen, gdp)
    do nm = 1, nmmax
       if (kcs(nm) == 2) then
          do l = 1, lstsc
             do k = 1, kmax
                ddkl(nm, k, l) = r0(nm, k, l)
                aakl(nm, k, l) = 0.0_fp
                bbkl(nm, k, l) = 1.0_fp
                cckl(nm, k, l) = 0.0_fp
             enddo
          enddo
       endif
    enddo
    !
    ! BOUNDARY CONDITIONS
    !     On open boundary no secondary flow (=> loop over LSTSC)
    !
    do ic = 1, norow
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1
       ml   = irocol(3, ic)
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmfu = nmf + icx
       nmlu = nml + icx
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS
       !
       if (kcu(nmf) == 1) then
          do k = 1, kmax
             do l = 1, lstsc
                ddkl(nmf, k, l) = rbnd(k, l, 1, ic)
             enddo
          enddo
       endif
       if (kcu(nml) == 1) then
          do k = 1, kmax
             do l = 1, lstsc
                ddkl(nmlu, k, l) = rbnd(k, l, 2, ic)
             enddo
          enddo
       endif
       !
       ! optional Neumann boundary condition for suspended sediment fractions
       !
       lst = max(lsal, ltem)
       do l = 1, lsed
          ll = lst + l
          if ((eqmbcsand .and. sedtyp(l) > max_mud_sedtyp) .or. &
            & (eqmbcmud  .and. sedtyp(l) <= max_mud_sedtyp)) then
             if (kcu(nmf) == 1) then
                do k = 1, kmax
                   ddkl(nmf, k, ll) = max(0.0_fp, r0(nmfu, k, ll))
                enddo
             endif
             if (kcu(nml) == 1) then
                do k = 1, kmax
                   ddkl(nmlu, k, ll) = max(0.0_fp, r0(nml, k, ll))
                enddo
             endif
          endif
       enddo
    enddo
    call timer_stop(timer_difu_bounopen, gdp)
    do l = 1, lstsci
       !
       ! SOURCES AND SINK TERMS
       !
       ! SINKS ARE TREATED IMPLICITLY
       !
       call timer_start(timer_difu_sourcesink, gdp)
       if (lsec==2 .and. l==lsecfl) then
          !
          ! secondary flow (equilibrium equals to new intensity)
          !       start-up problems SINK might be 0.0 when
          !       UMOD = 0.0 in SECRHS
          !
          do k = 1, kmax
             do nm = 1, nmmax
                if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                   h0new = s1(nm) + real(dps(nm),fp)
                   if (abs(sink(nm,k,l)*h0new) > eps) then
                      h0old = s0(nm) + real(dps(nm),fp)
                      r1(nm, k, l) = sour(nm, k, l)*h0old/(sink(nm, k, l)*h0new)
                   else
                      r1(nm, k, l) = 0.0_fp
                   endif
                endif
             enddo
          enddo
       else
          do k = 1, kmax
             do nm = 1, nmmax
                if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                   bbkl(nm, k, l) = bbkl(nm, k, l) + sink(nm, k, l)*volum1(nm, k)
                   ddkl(nm, k, l) = ddkl(nm, k, l) + sour(nm, k, l)*volum0(nm, k)
                endif
             enddo
          enddo
          !
          ! set concentrations in dry points and in open boundary points
          !
          do k = 1, kmax
             do nm = 1, nmmax
                if ((kfs(nm)==0 .and. kcs(nm)==1) .or. kcs(nm)==2) then
                   r1(nm, k, l) = ddkl(nm, k, l)
                endif
             enddo
          enddo
       endif
       call timer_stop(timer_difu_sourcesink, gdp)
       !
       if (l == lsecfl) then
          !
          ! boundary conditions secondary flow (spiral motion intensity)
          !
          call timer_start(timer_difu_secbou, gdp)
          call secbou(j         ,nmmaxj    ,kmax      ,icx       ,icy       , &
                    & lstsci    ,lsecfl    ,kfu       ,irocol    ,norow     , &
                    & s0        ,s1        ,dps       ,r1        ,sour      , &
                    & sink      ,gdp       )
          call timer_stop(timer_difu_secbou, gdp)
          if (lsec == 2) then
             !
             ! exchange r1 with neighbours for parallel runs
             !
             call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
             !
             cycle
          endif
       endif
       !
       ! DD code added:
       !
       ! left hand-side is now set by Delft3D-FLOW instead of the mapper
       !
       call timer_start(timer_difu_lhs, gdp)
       do nm = 1, nmmax
          if (kcs(nm) == 3 ) then
             do k = 1, kmax
                aakl(nm,k,l) = 0.0_fp
                bbkl(nm,k,l) = 1.0_fp
                cckl(nm,k,l) = 0.0_fp
                ddkl(nm,k,l) = r0(nm,k,l)
             enddo
          endif
       enddo
       call timer_stop(timer_difu_lhs, gdp)
       !
       !
       !        D3dFlow_Build_ADI_Conc: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_adi_conc, gdp)
       !
       ! DD code added end
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       !   Store scale factor in array rscale
       !   They are used for the constituent independent flux arrays b[d/u][d/u][d/u]x
       !
       call timer_start(timer_difu_rowsc, gdp)
       do k = 1, kmax
          do nm = 1, nmmax
             if (kfs(nm)==1) then
                rscale(nm, k)    = 1.0_fp / bbkl(nm, k, l)
                aakl  (nm, k, l) = aakl(nm, k, l) * rscale(nm, k)
                bbkl  (nm, k, l) = 1.0_fp
                cckl  (nm, k, l) = cckl(nm, k, l) * rscale(nm, k)
                ddkl  (nm, k, l) = ddkl(nm, k, l) * rscale(nm, k)
             endif
          enddo
       enddo
       call timer_stop(timer_difu_rowsc, gdp)
       !
       !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       ! Division by the pivot for k=1 is not needed anymore
       ! because of row scaling
       !
       call timer_start(timer_difu_solve1, gdp)
       do nm = 1, nmmax
          if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
             do k = 2, kmax
                bi             = 1.0_fp/(bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
                bbkl(nm, k, l) = bi
                cckl(nm, k, l) = cckl(nm, k, l)*bi
             enddo
          endif
       enddo
       call timer_stop(timer_difu_solve1, gdp)
       !
       ! ITERATION LOOP
       !
       call timer_start(timer_difu_solve2, gdp)
       iter = 0
       do k = 1, kmax
          do nm = 1, nmmax
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                r1(nm, k, l) = r0(nm, k, l)
                uvdwk(nm, k) = r0(nm, k, l)
             endif
          enddo
       enddo
       call timer_stop(timer_difu_solve2, gdp)
       !
       ! exchange r1 with neighbours for parallel runs
       !
       call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
       !
       ! assure that loop starts at point of correct color in own subdomain
       !
       if (mod(mfg+nfg,2) == 1) then
          ! red points
          nmsta = 1
       else
          ! black points
          nmsta = 2
       endif
       !
       ! DD code added:
       !
       !
       !       (re)solve system of equations
       !
  111  continue
       gdp%dd%difuiter = gdp%dd%difuiter + 1
       !
       ! DD code added end
       !
 1100  continue
       iter = iter + 1
       !
       ! ITERATIVE SOLUTION METHOD USING CHECKERBOARD JACOBI
       ! IN HORIZONTAL DIRECTION
       !
       itr = 0
       !
       ! set concentrations in coupling points
       !
       call timer_start(timer_difu_solve3, gdp)
       do k = 1, kmax
          do nm = 1, nmmax
             if (kcs(nm) == 3) then
                r1(nm, k, l) = ddkl(nm, k, l)
             endif
          enddo
       enddo
       call timer_stop(timer_difu_solve3, gdp)
       if(icx == 1) then
         call timer_start(timer_difu_solve4u, gdp)
       else
         call timer_start(timer_difu_solve6v, gdp)
       end if
       !
       ! loop starts at red or black point depending on own subdomain
       !
       nmsta = 3 - nmsta
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             ! ( CHECK FOR KCS TO AVOID AN ARRAY INDEX OUT OF BOUNDS )
             !
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                uvdwk(nm, k) =   bdddx(nm, k) * r1(nm - icx - icx - icx, k, l) &
                             & + bddx (nm, k) * r1(nm - icx - icx, k, l)       &
                             & + bdx  (nm, k) * r1(nm - icx, k, l)             &
                             & + bux  (nm, k) * r1(nm + icx, k, l)             &
                             & + buux (nm, k) * r1(nm + icx + icx, k, l)       &
                             & + buuux(nm, k) * r1(nm + icx + icx + icx, k, l)
                uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm, k)
             endif
          enddo
       enddo
       if(icx == 1) then
         call timer_stop(timer_difu_solve4u, gdp)
         call timer_start(timer_difu_solve5u, gdp)
       else
         call timer_stop(timer_difu_solve6v, gdp)
         call timer_start(timer_difu_solve7v, gdp)
       end if
       do nm = nmsta, nmmax, 2
          if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
             vvdwk(nm, 1) = uvdwk(nm, 1)*bbkl(nm, 1, l)
          endif
       enddo
       do k = 2, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                vvdwk(nm,k) = (uvdwk(nm,k) - aakl(nm,k,l)*vvdwk(nm,k-1)) * bbkl(nm,k,l)
             endif
          enddo
       enddo
       do k = kmax - 1, 1, -1
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                 vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l) *vvdwk(nm, k + 1)
             endif
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                epsitr = max(ad_epsabs, ad_epsrel*abs(r1(nm, k, l)))
                if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) itr = 1
                r1(nm, k, l) = vvdwk(nm, k)
             endif
          enddo
       enddo
       if(icx == 1) then
         call timer_stop(timer_difu_solve5u, gdp)
         call timer_start(timer_difu_solve4u, gdp)
       else
         call timer_stop(timer_difu_solve7v, gdp)
         call timer_start(timer_difu_solve6v, gdp)
       end if
       !
       call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
       !
       ! loop starts at point of other color now (black respectively red)
       !
       nmsta = 3 - nmsta
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                uvdwk(nm, k) =   bdddx(nm, k) * r1(nm - icx - icx - icx, k, l) &
                             & + bddx (nm, k) * r1(nm - icx - icx, k, l)       &
                             & + bdx  (nm, k) * r1(nm - icx, k, l)             &
                             & + bux  (nm, k) * r1(nm + icx, k, l)             &
                             & + buux (nm, k) * r1(nm + icx + icx, k, l)       &
                             & + buuux(nm, k) * r1(nm + icx + icx + icx, k, l)
                uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm, k)
             endif
          enddo
       enddo
       if(icx == 1) then
         call timer_stop(timer_difu_solve4u, gdp)
         call timer_start(timer_difu_solve5u, gdp)
       else
         call timer_stop(timer_difu_solve6v, gdp)
         call timer_start(timer_difu_solve7v, gdp)
       end if
       do nm = nmsta, nmmax, 2
          if (kfs(nm)==1 .and. kcs(nm) == 1) then
             vvdwk(nm, 1) = uvdwk(nm, 1)*bbkl(nm, 1, l)
          endif
       enddo
       do k = 2, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                vvdwk(nm, k) = (uvdwk(nm,k) -  aakl(nm,k,l)*vvdwk(nm,k-1)) * bbkl(nm,k,l)
             endif 
          enddo
       enddo
       do k = kmax - 1, 1, -1
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l) *vvdwk(nm, k + 1)
             endif 
          enddo
       enddo
       !
       ! CHECK FOR CONVERGENCE
       !
       do k = 1, kmax
          do nm = nmsta, nmmax, 2
             if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then
                epsitr = max(ad_epsabs, ad_epsrel*abs(r1(nm, k, l)))
                if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) itr = 1
                r1(nm, k, l) = vvdwk(nm, k)
             endif
          enddo
       enddo
       if(icx == 1) then
         call timer_stop(timer_difu_solve5u, gdp)
       else
         call timer_stop(timer_difu_solve7v, gdp)
       end if
       call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
       !
       ! determine global maximum of 'itr' over all nodes
       ! Note: this enables to synchronize the iteration process
       !
       call dfreduce_gdp( itr, 1, dfint, dfmax, gdp )
       !
       if (itr>0 .and. iter<ad_itrmax) goto 1100
       !
       if (gdp%gdflwpar%flwoutput%iteroutputsteps >= gdp%gdinttim%ntstep) then
          write (lundia, '(3(a,i0))') 'difu(ntstep,l,iter):',gdp%gdinttim%ntstep, ' ', l, ' ',iter
       endif
       if (iter >= ad_itrmax) then
          write (errtxt, '(i0,a,i0)') l, ' ', nst
          call prterr(lundia    ,'S206'    ,trim(errtxt)    )
       endif
       !
       ! DD code added:
       !
       !
       !       D3dFlow_Solve_ADI_Conc: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_adi_conc, gdp)
       if (nhystp == d3dflow_solve_adi_conc) goto 111
       !
       ! DD code added end
       !
       ! Nudging of constituents at open boundaries
       !
       if (nudge==1) then
          ! Nudging layer
          nnudge    = 4
          nudgefac  = 10.0_fp
          tnudge    = hdt
          mu(1)     = max(hdt / tnudge, 1.0_fp)
          do jj = 2, nnudge
             mu(jj) = mu(jj-1) / nudgefac
          enddo
          !
          do nm = 1, nmmax
             nmu = nm + icx
             nmd = nm - icx
             if (kcs(nmd) == 2 .and. kcs(nm) == 1 ) then
                nms(1) = nm
                do jj = 2, nnudge
                   nms(jj) = nms(jj-1) + icx
                enddo
                do jj = 1, nnudge
                   do k = 1, kmax
                      if (r1(nmd, k, l )>1.0e-6) then
                         rb = r1(nmd, k, l )
                         rp = r1(nms(jj), k, l)
                         r1(nms(jj), k, l) = rp + mu(jj)*(rb-rp)
                         r0(nms(jj), k, l) = r1(nms(jj), k, l)
                      endif
                   enddo
                enddo
             endif
             if (kcs(nmu) == 2 .and. kcs(nm) == 1) then
                nms(1) = nm
                do jj = 2, nnudge
                   nms(jj) = nms(jj-1) - icx
                enddo
                do jj = 1, nnudge
                   do k = 1, kmax
                      if (r1(nmu, k, l )>1.0e-6) then
                         rb = r1(nmu, k, l )
                         rp = r1(nms(jj), k, l)
                         r1(nms(jj), k, l) = rp + mu(jj)*(rb-rp)
                         r0(nms(jj), k, l) = r1(nms(jj), k, l)
                      endif
                   enddo
                enddo
             endif
          enddo
       endif
       !
    enddo
end subroutine difu
