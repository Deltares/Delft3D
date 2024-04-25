subroutine z_difu(lundia    ,nst       ,icx       ,icy       ,j         , &
                & nmmaxj    ,nmmax     ,kmax      ,lstsci    ,lstsc     , &
                & norow     ,nocol     ,irocol    ,kcs       ,kcu       , &
                & kcv       ,kfs       ,kfsmin    ,kfsmax    ,kfsmn0    , &
                & kfsmx0    ,kfumin    ,kfumax    ,kfumn0    ,kfumx0    , &
                & kfvmin    ,kfvmax    ,kfvmn0    ,kfvmx0    ,kfsz0     , &
                & kfuz0     ,kfvz0     ,kfu       ,kfv       ,kfsz1     , &
                & kfuz1     ,kfvz1     ,qxk       ,qyk       ,qzk       , &
                & u         ,v         ,guv       ,gvu       ,gsqs      , &
                & rbnd      ,sigdif    ,sigmol    ,dicuv     ,vicww     , &
                & r0        ,r1        ,sour      ,sink      ,aak       , &
                & bbk       ,cck       ,bdx       ,bux       ,bdy       , &
                & buy       ,uvdwk     ,vvdwk     ,rscale    ,dzu1      , &
                & dzv1      ,aakl      ,bbkl      ,cckl      ,ddkl      , &
                & dzs1      ,areau     ,areav     ,volum0    ,volum1    , &
                & guu       ,gvv       ,bruvai    ,sedtyp    ,seddif    , &
                & ws        ,lsed      ,lsal      ,ltem      ,eqmbcsand , &
                & eqmbcmud  ,lsts      ,s1        ,dps       ,gdp       )
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
! Computes transport in the u, v and w-direction.
! Implicit w-direction, explicit in u- and v-direction.
! Sinks are treated implicitly and sources explicitly
! A special approach is used for the horizontal diffusion to avoid artificial
! creeping.
! - Special approach for top layer.
! - Horizontal Advection in U-direction :
!      Van Leer 2, non linear approach (explicit)
! - Horizontal Advection in V-direction :
!      Van Leer 2, non linear approach (explicit)
! - Vertical Advection, first order upwind (impl)
! - Horizontal Diffusion :
!     2D and 3D: explicit, along Z-planes
! - Option: horizontal diffusion strictly horizontal using special filter
! - Vertical Diffusion : implicitly
! - Sources are integrated explicitly.
! - Sinks are integrated implicitly.
!
! COMMENT:
! For the Thatcher Harleman boundaries the boundary points for outflow
! are reflected from the inner points; for inflow the boundary conditions are
! used (see also thahbc.for).
!
! NOTE:
! AREAU/V were set using DZU0/V0 in Z_SUD as required by Z_DIFHOR

!
!!--pseudo code and references--------------------------------------------------
!
! "A comparison of two 3D shallow water models using sigma coordinates and
!  z-coordinates in the vertical direction."
! Bijvelds, Van Kester and Stelling.
!
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use flow2d3d_timers
    use sediment_basics_module
    !
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
    include 'flow_steps_f.inc'
    real(fp)                            , pointer :: hdt
    real(fp)                            , pointer :: ag
    real(fp)                            , pointer :: dryflc
    real(fp)                            , pointer :: dzmin
    real(fp)                            , pointer :: vicmol
    real(fp)                            , pointer :: dicoww
    integer                             , pointer :: iro
    character(13)                       , pointer :: trasol
    real(fp)                            , pointer :: xlo
    real(fp)                            , pointer :: ck
    logical                             , pointer :: nonhyd
    integer                             , pointer :: nh_level
    integer                             , pointer :: nudge
    integer                             , pointer :: max_mud_sedtyp
!
! Global variables
!
    integer                                                                :: icx       ! Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                                :: icy       ! Increment in the Y-dir. (see ICX)
    integer                                                                :: j         ! Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                                :: kmax      ! Description and declaration in esm_alloc_int.f90
    integer                                                                :: lstsci    ! Description and declaration in esm_alloc_int.f90
    integer                                                  , intent(in)  :: lsed
    integer                                                  , intent(in)  :: lstsc     ! Description and declaration in dimens.igs
    integer                                                                :: lsts      ! Description and declaration in esm_alloc_int.f90
    integer                                                                :: lsal      ! Description and declaration in iidim.f90 
    integer, dimension(lsed)                                 , intent(in)  :: sedtyp    ! sediment type: 0=total/1=noncoh/2=coh    
    integer                                                                :: lundia    ! Description and declaration in inout.igs
    integer                                                                :: nmmax     ! Description and declaration in dimens.igs
    integer                                                                :: nmmaxj    ! Description and declaration in dimens.igs
    integer                                                  , intent(in)  :: nocol     ! Description and declaration in esm_alloc_int.f90
    integer                                                  , intent(in)  :: norow     ! Description and declaration in esm_alloc_int.f90
    integer                                                                :: nst
    integer , dimension(5, norow+nocol)                      , intent(in)  :: irocol    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcs       ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcu       ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kcv       ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfs       ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfu       ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfv       ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmax    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmin    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmx0    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfsmn0    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfumin    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfumn0    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfumx0    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfumax    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfvmin    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfvmx0    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfvmax    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: kfvmn0    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfsz0     ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfsz1     ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfuz0     ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfuz1     ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfvz0     ! Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: kfvz1     ! Description and declaration in esm_alloc_int.f90
    integer                                                  , intent(in)  :: ltem      ! Description and declaration in dimens.igs
    logical                                                  , intent(in)  :: eqmbcsand ! Description and declaration in morpar.igs
    logical                                                  , intent(in)  :: eqmbcmud  ! Description and declaration in morpar.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: gsqs      ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: guu       ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: guv       ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: gvu       ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)               , intent(in)  :: gvv       ! Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                           :: dps       ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                             :: s1        ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                     :: bruvai    ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       , intent(in)  :: vicww     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       , intent(in)  :: qzk       ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: aak       ! Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: bbk       ! Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: cck       ! Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: bdx       ! Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: bux       ! Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: bdy       ! Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: buy       ! Internal work array (in Z_CUCNP & Z_UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: uvdwk     ! Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: vvdwk     ! Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                       :: rscale    ! Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)                     :: dicuv     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: areau     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: areav     ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: dzs1      ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: volum0    ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: volum1    ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: dzu1      ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: dzv1      ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: qxk       ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: qyk       ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: u
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         , intent(in)  :: v
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: aakl      ! Internal work array, lower diagonal tridiagonal matrix, implicit coupling of concentration in (N,M,K) with concentration in (N,M,K-1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: bbkl      ! Internal work array, main diagonal tridiagonal matrix, implicit coupling of concentration in (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: cckl      ! Internal work array, upper diagonal tridiagonal matrix, implicit coupling of concentration in (N,M,K) with concentration in (N,M,K+1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: ddkl      ! Internal work array, diagonal space at (N,M,K,L)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: r0        ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)               :: r1        ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in)  :: sink      ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci) , intent(in)  :: sour      ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, max(lstsci, 1), 2, norow+nocol), intent(in)  :: rbnd      ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                                            :: sigdif    ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                              , intent(in)  :: sigmol    ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed) , intent(in)  :: ws        ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed) , intent(in)  :: seddif    ! Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer                 :: ddb
    integer                 :: ic
    integer                 :: iter
    integer                 :: itr
    integer                 :: icxy
    integer                 :: istat
    integer                 :: jj
    integer                 :: k
    integer                 :: kd
    integer                 :: kenu
    integer                 :: kenv
    integer                 :: kfw
    integer                 :: kfsum
    integer                 :: kmin
    integer                 :: kkmax
    integer                 :: kr
    integer                 :: ksm
    integer                 :: ku
    integer                 :: l
    integer                 :: ll
    integer                 :: ls
    integer                 :: lst
    integer                 :: m
    integer                 :: mf
    integer                 :: mink
    integer                 :: mink2
    integer                 :: mink_old
    integer                 :: mink_new
    integer                 :: ml
    integer                 :: n
    integer                 :: ndm
    integer                 :: nhystp
    integer                 :: nf
    integer                 :: nfm
    integer                 :: nfum
    integer                 :: nl
    integer                 :: nlm
    integer                 :: nlum
    integer                 :: nm
    integer                 :: nmd
    integer                 :: nmf
    integer                 :: nmfu
    integer                 :: nml
    integer                 :: nmlu
    integer, dimension(10)  :: nms
    integer                 :: nmu
    integer                 :: nmuu
    integer                 :: num
    integer                 :: nuum
    integer                 :: nm_pos    ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
    integer                 :: nnudge
    real(fp)                :: adza
    real(fp)                :: adzc
    real(fp)                :: bi
    real(fp)                :: cfl
    real(fp)                :: ddzc
    real(fp)                :: delz
    real(fp)                :: difiwe
    real(fp)                :: diz1
    real(fp)                :: epsitr
    real(fp)                :: flux
    real(fp), dimension(10) :: mu
    real(fp)                :: nudgefac
    real(fp)                :: qzw
    real(fp)                :: r00
    real(fp), external      :: reddic
    real(fp)                :: rscal
    real(fp)                :: rb
    real(fp)                :: rp
    real(fp)                :: sqrtbv
    real(fp)                :: timest
    real(fp)                :: tnudge
    real(fp)                :: volu
    real(fp)                :: z1
    character(256)          :: errtxt
!
!! executable statements -------------------------------------------------------
!
    hdt         => gdp%gdnumeco%hdt
    ag          => gdp%gdphysco%ag
    vicmol      => gdp%gdphysco%vicmol
    dicoww      => gdp%gdphysco%dicoww
    iro         => gdp%gdphysco%iro
    trasol      => gdp%gdtricom%trasol
    xlo         => gdp%gdturcoe%xlo
    ck          => gdp%gdturcoe%ck
    nonhyd      => gdp%gdprocs%nonhyd
    nh_level    => gdp%gdnonhyd%nh_level
    dryflc      => gdp%gdnumeco%dryflc
    nudge       => gdp%gdnumeco%nudge
    dzmin       => gdp%gdzmodel%dzmin
    max_mud_sedtyp => gdp%gdsedpar%max_mud_sedtyp
    !
    if (lstsci == 0) goto 9999
    !
    ddb    = gdp%d%ddbound
    icxy   = max(icx, icy)
    nm_pos = 1
    !
    if (nonhyd .and. nh_level==nh_full) then
       timest = 2.0_fp * hdt
    else
       timest = hdt
    endif
    !
    call timer_start(timer_difu_ini, gdp)
    !
    ! Initialise work arrays
    !
    aakl   = 0.0_fp
    bbkl   = 1.0_fp
    cckl   = 0.0_fp
    ddkl   = 0.0_fp
    bdx    = 0.0_fp
    bux    = 0.0_fp
    bdy    = 0.0_fp
    buy    = 0.0_fp
    !
    do l = 1, lstsci
       !
       ! system of difference equations
       !
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             !
             ! Set concentrations in bottom layer that may have become (de)activated
             !
             if (kfsmin(nm) < kfsmn0(nm)) then
                do k = kfsmn0(nm)-1, kfsmin(nm), -1
                   r0(nm, k, l) = r0(nm, kfsmn0(nm), l)
                enddo
             elseif (kfsmin(nm) > kfsmn0(nm)) then
                do k = kfsmn0(nm), kfsmin(nm)-1
                   r0(nm, k, l) = 0.0_fp
                enddo
             endif
             !
             ! Dynamics of the top layer
             !
             kmin = min(kfsmax(nm), kfsmx0(nm))
             if (kfsmax(nm) == kfsmx0(nm) .or. kmax == 1) then
                do k = kfsmin(nm), kfsmax(nm)
                   bbkl(nm, k, l) = volum1(nm, k) / timest
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) / timest
                enddo
             elseif (kfsmax(nm) > kfsmx0(nm)) then
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = volum1(nm, k) / timest
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) / timest
                enddo
                do k = kmin + 1, kfsmax(nm)
                   bbkl(nm, kmin, l) = bbkl(nm, kmin, l) + volum1(nm, k)/timest
                   !
                   ! also volume at old time level should be taken into account 
                   ! Noted that for the concentrations the value at KMIN
                   ! is used, because at the other layers the concentration
                   ! is zero (see routine Z_CHKDRY)
                   !
                   ddkl(nm, kmin, l) = ddkl(nm, kmin, l) &
                                   & + volum0(nm, k)*r0(nm, kmin, l)/timest
                   aakl(nm, k, l) = 0.0_fp
                   bbkl(nm, k, l) = 1.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = 0.0_fp
                enddo
             else
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = volum1(nm, k) / timest
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) / timest
                enddo
                do k = kmin + 1, kfsmx0(nm)
                   bbkl(nm, k, l) = 1.0_fp
                   aakl(nm, k, l) = 0.0_fp
                   cckl(nm, k, l) = 0.0_fp
                   ddkl(nm, k, l) = 0.0_fp
                   ddkl(nm, kmin, l) = ddkl(nm, kmin, l) + volum0(nm, k)*r0(nm, k, l)/timest
                enddo
             endif
          endif
       enddo
    enddo
    call timer_stop(timer_difu_ini, gdp) 
    !
    ! Horizontal advection in U and V direction
    !
    call timer_start(timer_difu_horadv, gdp)
    if     (trasol == 'van leer-2   ') then
       !
       ! Explicit Van Leer-2 method
       !
       call z_difu_horadv_expl()
       !
    elseif (trasol == 'iupw         ') then
       !
       ! Implicit upwind method
       !
       call z_difu_horadv_impl()
       !
    endif
    call timer_stop(timer_difu_horadv, gdp)
    !
    ! Diffusion in horizontal direction
    !
    call timer_start(timer_difu_hordiff, gdp)
    if     (trasol == 'van leer-2   ') then
       !
       ! Explicit method
       !
       call z_difu_difhor_expl()
       !
    elseif (trasol == 'iupw         ') then
       !
       ! Implicit method
       !
       call z_difu_difhor_impl()
       !
    endif
    call timer_stop(timer_difu_hordiff, gdp)    
    !
    do l = 1, lstsci
       !
       uvdwk  = 0.0_fp
       vvdwk  = 0.0_fp
       rscale = 0.0_fp
       !
       ! SOURCES AND SINK TERMS
       !
       call timer_start(timer_difu_sourcesink, gdp) 
       !
       ! SINKS ARE TREATED IMPLICITLY
       !
       do nm = 1, nmmax
          if (kfs(nm)*kcs(nm) == 1) then
             if (kfsmax(nm) == kfsmx0(nm) .or. kmax == 1) then
                do k = kfsmin(nm), kfsmax(nm)
                   bbkl(nm, k, l) = bbkl(nm, k, l) + sink(nm, k, l)
                   ddkl(nm, k, l) = ddkl(nm, k, l) + sour(nm, k, l)
                enddo
             else
                kmin  = min(kfsmax(nm), kfsmx0(nm))
                kkmax = max(kfsmax(nm), kfsmx0(nm))
                do k = kfsmin(nm), kmin
                   bbkl(nm, k, l) = bbkl(nm, k, l) + sink(nm, k, l)
                   ddkl(nm, k, l) = ddkl(nm, k, l) + sour(nm, k, l)
                enddo
                do k = kmin + 1, kkmax
                   bbkl(nm, kmin, l) = bbkl(nm, kmin, l) + sink(nm, k, l)
                   ddkl(nm, kmin, l) = ddkl(nm, kmin, l) + sour(nm, k, l)
                enddo
             endif
          endif
       enddo
       call timer_stop(timer_difu_sourcesink, gdp) 
       !
       call timer_start(timer_difu_rest, gdp)
       !
       ! Summation of fluxes for cells which do not have a horizontal neighbour cell
       !
       do nm = 1, nmmax
          if (kfs(nm) /= 0) then
             kmin = min(kfsmax(nm), kfsmx0(nm))
             do k = kmin + 1, kmax
                ddkl(nm, kmin, l) = ddkl(nm, kmin, l) + ddkl(nm, k, l)
             enddo
          endif
       enddo
       call timer_stop(timer_difu_rest, gdp)
       !
       call timer_start(timer_difu_vertadv, gdp)  
       !
       do nm = 1, nmmax
          ksm = min(kfsmx0(nm), kfsmax(nm))
          if (kfs(nm) == 1) then
             do k = kfsmin(nm), ksm - 1
                if (k == kfsmin(nm) .or. k == ksm - 1) then
                   kfw = 1
                else
                   kfw = 0
                endif
                !
                ! ADVECTION IN VERTICAL DIRECTION; W*DC/DZ
                !
                ! second order central
                !
                qzw = qzk(nm, k)
                if (qzw < 0.0_fp) then
                   adza = 0.5_fp * qzw * (1 - kfw)
                   adzc = 0.5_fp * qzw * (1 + kfw)
                elseif (qzw > 0.0_fp) then
                   adza = 0.5_fp * qzw * (1 + kfw)
                   adzc = 0.5_fp * qzw * (1 - kfw)
                else 
                   adza = 0.0_fp
                   adzc = 0.0_fp
                endif
                aakl(nm, k + 1, l) = aakl(nm, k + 1, l) - adza
                bbkl(nm, k + 1, l) = bbkl(nm, k + 1, l) - adzc
                bbkl(nm, k    , l) = bbkl(nm, k    , l) + adza
                cckl(nm, k    , l) = cckl(nm, k    , l) + adzc
             enddo
          endif
       enddo
       call timer_stop(timer_difu_vertadv, gdp)
       !
       ! DIFFUSION IN VERTICAL DIRECTION
       !
       call timer_start(timer_difu_vertdiff, gdp)
       !
       ! l = sediment: ls > 0
       ! else        : ls = 0
       !
       ls = 0
       if ((l>max(lsal, ltem)) .and. (l<=lsts)) then
          ls = l - max(lsal, ltem)
       endif
       do nm = 1, nmmax
          if (kfs(nm) == 1) then
             ksm = min(kfsmx0(nm), kfsmax(nm))
             do k = kfsmin(nm), ksm - 1
                delz = max(0.0001_fp, 0.5_fp*(dzs1(nm, k) + dzs1(nm, k + 1)))
                !
                ! Internal wave contribution
                !
                sqrtbv = max(0.0_fp, bruvai(nm, k))
                sqrtbv = sqrt(sqrtbv)
                difiwe = 0.2_fp * sqrtbv * xlo**2
                !
                ! dicoww-restriction is moved from TURCLO to here (in reddic)
                ! vicww is used instead of dicww
                !
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
                ddzc = gsqs(nm) * diz1 / delz
                aakl(nm, k+1, l) = aakl(nm, k+1, l) - ddzc
                bbkl(nm, k+1, l) = bbkl(nm, k+1, l) + ddzc
                bbkl(nm, k  , l) = bbkl(nm, k  , l) + ddzc
                cckl(nm, k  , l) = cckl(nm, k  , l) - ddzc
             enddo
          endif
       enddo
       call timer_stop(timer_difu_vertdiff, gdp)  
       !
       ! Include settling velocities and Dirichlet BC for sediments in
       ! matrices AAKL/BBKL/CCKL/DDKL
       !
       if (lsed > 0) then
          call timer_start(timer_difu_difws, gdp)
          call z_dif_ws(j         ,nmmaxj    ,nmmax     ,kmax      ,lsal      , &
                    & ltem      ,lstsci    ,lsed      ,kcs       ,kfs       , &
                    & gsqs      ,ws        ,aakl      ,bbkl      ,cckl      , &
                    & kfsmx0    ,gdp       )
          call timer_stop(timer_difu_difws, gdp)
       endif         
       !
       ! set values in open boundary points (in part. for y-direction)
       !
       call timer_start(timer_difu_bounopen, gdp)
       do nm = 1, nmmax
          if (kcs(nm) == 2) then
             do k = kfsmin(nm), kmax
                ddkl(nm, k, l) = r0(nm, k, l)
                aakl(nm, k, l) = 0.0_fp
                bbkl(nm, k, l) = 1.0_fp
                cckl(nm, k, l) = 0.0_fp
             enddo
          endif
       enddo
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS IN X-DIRECTION
       !
       do ic = 1, norow
          n    = irocol(1, ic)
          mf   = irocol(2, ic) - 1
          ml   = irocol(3, ic)
          nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
          nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
          nmfu = nmf + icx
          nmlu = nml + icx
          if (kcu(nmf) == 1) then
             do k = kfsmin(nmf), kmax
                aakl(nmf, k, l) = 0.0_fp
                bbkl(nmf, k, l) = 1.0_fp
                cckl(nmf, k, l) = 0.0_fp
                ddkl(nmf, k, l) = rbnd(k, l, 1, ic)
             enddo
          endif
          if (kcu(nml) == 1) then
             do k = kfsmin(nmlu), kmax
                aakl(nmlu, k, l) = 0.0_fp
                bbkl(nmlu, k, l) = 1.0_fp
                cckl(nmlu, k, l) = 0.0_fp
                ddkl(nmlu, k, l) = rbnd(k, l, 2, ic)
             enddo
          endif
          !
          ! optional Neumann boundary condition for suspended sediment fractions
          !
          if (ls>0) then
             !
             ! for sediment fraction
             !
             if ((eqmbcsand .and. sedtyp(ls) > max_mud_sedtyp) .or. &
               & (eqmbcmud  .and. sedtyp(ls) <= max_mud_sedtyp)) then
                if (kcu(nmf) == 1) then
                   do k = 1, kmax
                      ddkl(nmf, k, l) = max(0.0_fp, r0(nmfu, k, l))
                   enddo
                endif
                if (kcu(nml) == 1) then
                   do k = 1, kmax
                      ddkl(nmlu, k, l) = max(0.0_fp, r0(nml, k, l))
                   enddo
                endif
             endif
          endif
       enddo
       call timer_stop(timer_difu_bounopen, gdp) 
       !
       ! For the fully non-hydrostatic module, transport is done in both directions in one step.
       ! Include also boundary conditions in Y-direction
       ! In hydrostatic mode (or weakly non-hydrostatic) the transport in X- and Y-direction are done in two separate calls to z_difu. 
       !
       if (nonhyd .and. nh_level==nh_full) then
          !
          ! IMPLEMENTATION OF BOUNDARY CONDITIONS IN Y-DIRECTION
          !
          do ic = norow+1, norow+nocol
             m    = irocol(1,ic)
             nf   = irocol(2,ic) - 1
             nl   = irocol(3,ic)
             !
             ! WATCH OUT: icx and icy are swapped
             !
             nfm  = (m+ddb)*icx + (nf+ddb)*icy - icxy
             nfum = nfm + icy
             nlm  = (m+ddb)*icx + (nl+ddb)*icy - icxy
             nlum = nlm + icy
             if (kcv(nfm) == 1) then
                do k = kfsmin(nfm), kfsmax(nfm)
                   ddkl(nfm,k,l) = rbnd(k,l,1,ic)
                   aakl(nfm,k,l) = 0.0_fp
                   bbkl(nfm,k,l) = 1.0_fp
                   cckl(nfm,k,l) = 0.0_fp
                enddo
             endif
             if (kcv(nlm) == 1) then
                do k = kfsmin(nlum), kfsmax(nlum)
                   ddkl(nlum,k,l) = rbnd(k,l,2,ic)
                   aakl(nlum,k,l) = 0.0_fp
                   bbkl(nlum,k,l) = 1.0_fp
                   cckl(nlum,k,l) = 0.0_fp
                enddo
             endif
             !
             ! optional Neumann boundary condition for suspended sediment fractions
             !
             if (ls>0) then
                !
                ! for sediment fraction
                !
                if ((eqmbcsand .and. sedtyp(ls) > max_mud_sedtyp) .or. &
                  & (eqmbcmud  .and. sedtyp(ls) <= max_mud_sedtyp)) then
                   if (kcv(nfm) == 1) then
                      do k = 1, kmax
                         ddkl(nfm, k, l) = r0(nfum, k, l)
                      enddo
                   endif
                   if (kcv(nlm) == 1) then
                      do k = 1, kmax
                         ddkl(nlum, k, l) = r0(nlm, k, l)
                      enddo
                   endif
                endif
             endif
          enddo
       endif
       !
       !   set concentrations in temporary dry points and in open boundary points
       !
       call timer_start(timer_difu_rest, gdp)
       do nm = 1, nmmax
          if (kfs(nm) == 0 .and. kcs(nm) == 1) then
             do k = kfsmin(nm), kmax
                r1(nm, k, l) = r0(nm, k, l)
             enddo
          endif
          if (kcs(nm) == 2) then
             do k = kfsmin(nm), kmax
                r1(nm, k, l) = ddkl(nm, k, l)
             enddo
          endif
       enddo
       !
       ! Check volume in order to circumvent very small volumes
       !
       do nm = 1, nmmax
          if (kfs(nm)*kcs(nm) == 1) then
             kmin = min(kfsmax(nm), kfsmx0(nm))
             do k = kfsmin(nm), kmin
                if (bbkl(nm,k,l) .lt. dzmin) then
                   bbkl(nm,k,l) = bbkl(nm,k,l) + dzmin
                   ddkl(nm,k,l) = ddkl(nm,k,l) + dzmin * r0(nm,k,l)
                endif
             enddo
          endif
       enddo
       call timer_stop(timer_difu_rest, gdp)
       call timer_start(timer_difu_lhs, gdp)
       do nm = 1, nmmax
          if (kcs(nm) == 3) then
             !
             ! left hand-side is now set by Delft3D-FLOW instead of the mapper
             !
             do k = 1, kmax
                aakl(nm, k, l) = 0.0_fp
                bbkl(nm, k, l) = 1.0_fp
                cckl(nm, k, l) = 0.0_fp
                ddkl(nm, k, l) = r0(nm, k, l)
             enddo
          else 
             if (kfs(nm) /= 0) then
                !
                !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
                !
                mink = min(kfsmx0(nm), kfsmax(nm))
                do k = kfsmin(nm), mink
                   rscale(nm, k)  = 1.0_fp / bbkl(nm, k, l)
                   aakl(nm, k, l) = aakl(nm, k, l) * rscale(nm, k)
                   bbkl(nm, k, l) = 1.0_fp
                   cckl(nm, k, l) = cckl(nm, k, l) * rscale(nm, k)
                   ddkl(nm, k, l) = ddkl(nm, k, l) * rscale(nm, k)
                enddo
             endif
          endif
       enddo
       call timer_stop(timer_difu_lhs, gdp)
       !
       ! D3dFlow_Build_ADI_Conc: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_adi_conc, gdp)
       !
       ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       if     (trasol == 'van leer-2   ') then
          !
          call z_difu_solv_expl()
          !
       elseif (trasol == 'iupw         ') then
          !
          call z_difu_solv_impl()
          !
       endif
       !
       ! Nudging (very experimental)
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
!                call nm_to_n_and_m(nm, n, m, gdp)
                nms(1) = nm
                do jj = 2, nnudge
                   nms(jj) = nms(jj-1) + icx
                enddo
                do jj = 1, nnudge
                   do k = kfsmin(nm), kmax
                      if (r1(nmd, k, l )>1.0e-6) then
                         rb = r1(nmd, k, l )
                         rp = r1(nms(jj), k, l)
                         r1(nms(jj), k, l) = rp + mu(jj)*(rb-rp)
                         r0(nms(jj), k, l) = r1(nms(jj), k, l)
!                        if (n==94 .and. l==2) then
!                           write(*,'(6i5,10e14.4)')lstsci,l,k,jj,m,n,rb,rp,mu(jj),r1(nms(jj), k, l)
!                        endif
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
                   do k = kfsmin(nm), kmax
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
 9999 continue    
!
!
!
contains
!
!
!===============================================================================
!
! 
!
subroutine z_difu_horadv_expl()
    !
    ! horizontal advection using Van Leer-2 method (explicit)
    !
    ! local variables
    !
    real(fp)    :: rr1 ! Numerator of the Van Leer limiter
    real(fp)    :: rr2 ! Denominator of the Van Leer limiter
    !
    do nm = 1, nmmax
       !
       ! HORIZONTAL ADVECTION IN X-DIRECTION
       !
       nmd  = nm  - icx
       nmu  = nm  + icx
       nmuu = nmu + icx
       if (kfu(nm) /= 0) then
          kmin = max(kfumin(nm), 1)
          do k = kmin, kmax
             if (kfuz0(nm,k) == 1) then
                cfl = u(nm, k) * timest / gvu(nm)
                if (qxk(nm, k) > 0.0_fp) then
                   do l = 1, lstsci
                      rr1 = abs(r0(nmd, k, l) - 2.0_fp*r0(nm, k, l) + r0(nmu, k, l))
                      rr2 = abs(r0(nmd, k, l) - r0(nmu, k, l))
                      if (kfsz0(nmd, k)*kfsz0(nmu, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(nmu)==3) then 
                         r00 = r0(nm, k, l)
                      else
                         r00 = r0(nm , k, l)                                     &
                             & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(nmd, k, l))  &
                             &                 *(r0(nmu, k, l) - r0(nm , k, l))  &
                             &                 /(r0(nmu, k, l) - r0(nmd, k, l))
                      endif
                      flux = qxk(nm, k) * r00
                      if (kcs(nm) == 1) then
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, k, l) = ddkl(nmu, k , l) + flux
                      endif
                   enddo
                elseif (qxk(nm, k) < 0.0_fp) then
                   do l = 1, lstsci
                      rr1 = abs(r0(nmuu, k, l) - 2.0_fp*r0(nmu, k, l) + r0(nm, k, l))
                      rr2 = abs(r0(nmuu, k, l) - r0(nm, k, l))
                      if (kfsz0(nm, k)*kfsz0(nmuu, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(nmu)==3) then
                         r00 = r0(nmu, k, l)
                      else
                         r00 = r0(nmu, k, l)                                      &
                             & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(nmu , k, l))  &
                             &                 *(r0(nmu, k, l) - r0(nmuu, k, l))  &
                             &                 /(r0(nm , k, l) - r0(nmuu, k, l))
                      endif
                      flux = qxk(nm, k) * r00
                      if (kcs(nm) == 1) then
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, k, l) = ddkl(nmu, k , l) + flux
                      endif
                   enddo
                endif
             endif
          enddo
       endif
       !
       ! HORIZONTAL ADVECTION IN Y-DIRECTION
       !
       ndm  = nm  - icy
       num  = nm  + icy
       nuum = num + icy
       if (kfv(nm) /= 0) then
          kmin = max(kfvmin(nm), 1)
          do k = kmin, kmax
             if (kfvz0(nm,k) == 1) then
                cfl = v(nm, k) * timest / guv(nm)
                if (qyk(nm, k) > 0.0_fp) then
                   do l = 1, lstsci
                      rr1 = abs(r0(ndm, k, l) - 2.0_fp*r0(nm, k, l) + r0(num, k, l))
                      rr2 = abs(r0(ndm, k, l) - r0(num, k, l))
                      if (kfsz0(ndm, k)*kfsz0(num, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(num)==3) then
                         r00 = r0(nm, k, l)
                      else
                         r00 = r0(nm , k, l)                                     &
                             & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(ndm, k, l))  &
                             &                 *(r0(num, k, l) - r0(nm , k, l))  &
                             &                 /(r0(num, k, l) - r0(ndm, k, l))
                      endif
                      flux = qyk(nm, k) * r00
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(num) == 1 ) then
                         ddkl(num, k, l) = ddkl(num, k , l) + flux
                      endif
                   enddo
                elseif (qyk(nm, k) < 0.0_fp) then
                   do l = 1, lstsci
                      rr1 = abs(r0(nuum, k, l) - 2.0_fp*r0(num, k, l) + r0(nm, k, l))
                      rr2 = abs(r0(nuum, k, l) - r0(nm, k, l))
                      if (kfsz0(nm, k)*kfsz0(nuum, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp .or. kcs(nm)==3 .or. kcs(num)==3) then
                         r00 = r0(num, k, l)
                      else
                         r00 = r0(num, k, l)                                      &
                             & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(num , k, l))  &
                             &                 *(r0(num, k, l) - r0(nuum, k, l))  &
                             &                 /(r0(nm , k, l) - r0(nuum, k, l))
                      endif
                      flux = qyk(nm, k) * r00
                      if (kcs(nm) == 1) then
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(num) == 1) then
                         ddkl(num, k, l) = ddkl(num, k , l) + flux
                      endif
                   enddo
                endif
             endif
          enddo
       endif
    enddo
    !
end subroutine z_difu_horadv_expl
!
!
!===============================================================================
subroutine z_difu_horadv_impl()
    !
    ! local variables
    !
    integer     :: kkmin
    integer     :: kkminu
    !
    ! Horizontal advection using first order upwind method (implicit)
    !
    do nm = 1, nmmax
       !
       ! HORIZONTAL ADVECTION IN X-DIRECTION
       !
       nmu  = nm + icx
       if (kfu(nm) /= 0 .and. kcs(nm)*kcs(nmu) /= 0) then
          kmin = max(kfumin(nm), 1)
          kkmin  = min(kfsmx0(nm), kfsmax(nm))
          kkminu = min(kfsmx0(nmu), kfsmax(nmu))
          do k = kmin, min(kkmin, kkminu)
             if (qxk(nm, k) > 0.0_fp) then
                if (kfuz0(nm, k) == 1) then
                   if (kcs(nmu) == 1) then
                      bdx(nmu, k) = - qxk(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(nm, k, l) = bbkl(nm, k, l) + qxk(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      flux = qxk(nm, k) * r0(nm, k, l)
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, k, l) = ddkl(nmu, k , l) + flux
                      endif
                   enddo
                endif
             elseif (qxk(nm,k) < 0.0_fp) then
                if (kfuz0(nmu, k) == 1) then
                   if (kcs(nm) == 1) then
                      bux(nm, k) = qxk(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(nmu, k, l) = bbkl(nmu, k, l) - qxk(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      flux = qxk(nm, k) * r0(nmu, k, l)
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, k, l) = ddkl(nmu, k , l) + flux
                      endif
                   enddo
                endif
             endif
          enddo
          if (kfumx0(nm) > min(kkmin,kkminu)) then
             !
             ! Top layer has decreased
             ! Make sure that advection fluxes in layer(s) above the new top layer (KFUMAX(NM))
             ! are added to this new top layer
             ! Switch to first order upwind explicit method
             !
             do k = min(kkmin,kkminu)+1, kfumx0(nm)
                if (qxk(nm, k) > 0.0_fp) then
                   do l = 1, lstsci
                      flux = qxk(nm, k) * r0(nm , k, l)
                      if (kcs(nm) == 1) then
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, kkminu, l) = ddkl(nmu, kkminu, l) + flux
                      endif
                   enddo
                elseif (qxk(nm, k) < 0.0_fp) then
                   do l = 1, lstsci
                      flux = qxk(nm, k) * r0(nmu, k, l)
                      if (kcs(nm) == 1) then
                         ddkl(nm, kkmin, l) = ddkl(nm, kkmin, l) - flux
                      endif
                      if (kcs(nmu) == 1) then
                         ddkl(nmu, k, l) = ddkl(nmu, k , l) + flux
                      endif
                   enddo
                endif
             enddo
          endif
       endif
       !
       ! HORIZONTAL ADVECTION IN Y-DIRECTION
       !
       num  = nm + icy
       if (kfv(nm) /= 0  .and. kcs(nm)*kcs(num) /= 0) then
          kmin = max(kfvmin(nm), 1)
          kkmin  = min(kfsmx0(nm), kfsmax(nm))
          kkminu = min(kfsmx0(num), kfsmax(num))
          do k = kmin, min(kkmin,kkminu)
             if (qyk(nm, k) > 0.0_fp) then
                if (kfvz0(nm, k) == 1) then
                   if (kcs(num) == 1) then
                      bdy(num, k) = - qyk(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(nm, k, l) = bbkl(nm, k, l) + qyk(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      flux = qyk(nm, k) * r0(nm, k, l)
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(num) == 1) then
                         ddkl(num, k, l) = ddkl(num, k , l) + flux
                      endif
                   enddo
                endif
             elseif (qyk(nm, k) < 0.0_fp) then
                if (kfvz0(num, k) == 1) then
                   if (kcs(nm) == 1) then
                      buy(nm, k) = qyk(nm, k)
                   endif
                   do l = 1, lstsci
                      bbkl(num, k, l) = bbkl(num, k, l) - qyk(nm, k)
                   enddo
                else
                   do l = 1, lstsci
                      flux = qyk(nm, k) * r0(num, k, l)
                      if (kcs(nm) == 1) then 
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(num) == 1) then
                         ddkl(num, k, l) = ddkl(num, k , l) + flux
                      endif
                   enddo
                endif
             endif
          enddo
          if (kfvmx0(nm) > min(kkmin,kkminu)) then
             !
             ! Top layer has decreased
             ! Make sure that advection fluxes in layer(s) above the new top layer (KFVMAX(NM))
             ! are added to this new top layer
             ! Switch to first order upwind explicit method
             !
             do k = min(kkmin,kkminu)+1, kfvmx0(nm)
                if (qyk(nm, k) > 0.0_fp) then
                   do l = 1, lstsci
                      flux = qyk(nm, k) * r0(nm , k, l)
                      if (kcs(nm) == 1) then
                         ddkl(nm, k, l) = ddkl(nm, k, l) - flux
                      endif
                      if (kcs(num) == 1) then
                         ddkl(num, kkminu, l) = ddkl(num, kkminu, l) + flux
                      endif
                   enddo
                elseif (qyk(nm, k) < 0.0_fp) then
                   do l = 1, lstsci
                      flux = qyk(nm, k) * r0(num, k, l)
                      if (kcs(nm) == 1) then
                         ddkl(nm, kkmin, l) = ddkl(nm, kkmin, l) - flux
                      endif
                      if (kcs(num) == 1) then
                         ddkl(num, k, l) = ddkl(num, k, l) + flux
                      endif
                   enddo
                endif
             enddo
          endif
       endif
    enddo
    !
end subroutine z_difu_horadv_impl
!
!
!===============================================================================
subroutine z_difu_difhor_expl( )
    !
    ! EXPLICIT TIME INTEGRATION FOR HORIZONTAL DIFFUSION
    !
    ! Noted that the value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
    ! Local variables
    !
    integer  :: maskval
    real(fp) :: cl
    real(fp) :: cr
    real(fp) :: difl
    real(fp) :: difr
    real(fp) :: hmin
    !
    ! IN X-DIRECTION
    !
    do l = 1, lstsci
       do nm = 1, nmmax
          nmu = nm + icx
          hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
          if (kfs(nm)*kfs(nmu) == 1 .and. hmin > dryflc) then
             do k = kfumin(nm), kmax
                if (kfuz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(nmu, k)==1) then
                   cl               = r0   (nm , k, l)
                   difl             = dicuv(nm , k)
                   cr               = r0   (nmu, k, l)
                   difr             = dicuv(nmu, k)
                   flux             = 0.5_fp*(cr - cl)*(difl + difr)/(0.7_fp*gvu(nm))
                   maskval          = max(0, abs(2 - abs(kcs(nm))))
                   ddkl (nm , k, l) = ddkl (nm , k, l) + areau(nm, k)*flux*real(maskval,fp)
                   maskval          = max(0, abs(2 - abs(kcs(nmu))))
                   ddkl (nmu, k, l) = ddkl (nmu, k, l) - areau(nm, k)*flux*real(maskval,fp)
                endif
             enddo
          endif
       enddo
    enddo
    !
    ! IN Y-DIRECTION
    !
    do l = 1, lstsci
       do nm = 1, nmmax
          num = nm + icy
          hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
          if (kfs(nm)*kfs(num) == 1 .and. hmin > dryflc) then
             do k = kfvmin(nm), kmax
                if (kfvz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(num, k)==1) then
                   cl               = r0   (nm , k, l)
                   difl             = dicuv(nm , k)
                   cr               = r0   (num, k, l)
                   difr             = dicuv(num, k)
                   flux             = 0.5_fp*(cr - cl)*(difl + difr)/(0.7_fp*guv(nm))
                   maskval          = max(0, abs(2 - abs(kcs(nm))))
                   ddkl (nm , k, l) = ddkl (nm , k, l) + areav(nm, k)*flux*real(maskval,fp)
                   maskval          = max(0, abs(2 - abs(kcs(num))))
                   ddkl (num, k, l) = ddkl (num, k, l) - areav(nm, k)*flux*real(maskval,fp)
                endif
             enddo
          endif
       enddo
    enddo
    !
end subroutine z_difu_difhor_expl
!
!
!===============================================================================
subroutine z_difu_difhor_impl( )
    !
    ! IMPLICIT TIME INTEGRATION FOR HORIZONTAL DIFFUSION 
    !
    ! Noted that the value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
    ! Local variables
    !
    integer  :: maskval1
    integer  :: maskval2
    real(fp) :: areau1
    real(fp) :: areav1
    real(fp) :: difl
    real(fp) :: difr
    real(fp) :: hmin
    !
    ! IN X-DIRECTION
    !
    do nm = 1, nmmax
       nmu      = nm + icx
       hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
       if (kfu(nm) == 1 .and. hmin > dryflc) then
          mink_old = min(kfsmx0(nm),kfsmx0(nmu))
          mink_new = min(kfsmax(nm),kfsmax(nmu))
          mink     = min(mink_old  ,mink_new)
          do k = kfumin(nm), mink   !_old
             if (kfuz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(nmu, k)==1) then
                difl        = dicuv(nm, k)
                difr        = dicuv(nmu, k)
                flux        = 0.5_fp * (difl + difr) / (0.7_fp*gvu(nm))
                maskval1    = max(0, abs(2 - abs(kcs(nm ))))
                maskval2    = max(0, abs(2 - abs(kcs(nmu))))
                bux(nm , k) = bux(nm , k) - areau(nm, k)*flux*real(maskval1,fp)
                bdx(nmu, k) = bdx(nmu, k) - areau(nm, k)*flux*real(maskval2,fp)
                do l = 1, lstsci
                   bbkl(nm , k, l) = bbkl(nm , k, l) + areau(nm, k)*flux*real(maskval1,fp)
                   bbkl(nmu, k, l) = bbkl(nmu, k, l) + areau(nm, k)*flux*real(maskval2,fp)
                enddo
             endif
          enddo
       endif
    enddo
    !
    ! IN Y-DIRECTION
    !
    do nm = 1, nmmax
       num      = nm + icy
       hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
       if (kfv(nm) == 1 .and. hmin > dryflc) then
          mink_old = min(kfsmx0(nm),kfsmx0(num))
          mink_new = min(kfsmax(nm),kfsmax(num))
          mink     = min(mink_old  ,mink_new)
          do k = kfvmin(nm), mink      !_old
             if (kfvz0(nm, k)==1 .and. kfsz0(nm, k)*kfsz0(num, k)==1) then
                difl        = dicuv(nm, k)
                difr        = dicuv(num, k)
                flux        = 0.5_fp * (difl + difr) / (0.7_fp*guv(nm))
                maskval1    = max(0, abs(2 - abs(kcs(nm))))
                maskval2    = max(0, abs(2 - abs(kcs(num))))
                buy(nm , k) = buy(nm , k) - areav(nm, k)*flux*real(maskval1,fp)
                bdy(num, k) = bdy(num, k) - areav(nm, k)*flux*real(maskval2,fp)
                do l = 1, lstsci
                   bbkl(nm , k, l) = bbkl(nm , k, l) + areav(nm, k)*flux*real(maskval1,fp)
                   bbkl(num, k, l) = bbkl(num, k, l) + areav(nm, k)*flux*real(maskval2,fp)
                enddo
             endif
          enddo
       endif
    enddo
    !
end subroutine z_difu_difhor_impl
!
!
!===============================================================================
subroutine z_difu_solv_expl( )
    !
    ! Solution procedure for the transport equation when advection was included explicitly
    !
    call timer_start(timer_difu_solve1, gdp) 
    do nm = 1, nmmax
       if (kfs(nm)/=0) then
          mink              = kfsmin(nm)
          kmin              = min(kfsmx0(nm), kfsmax(nm))
          bi                = 1./bbkl(nm, mink, l)
          cckl(nm, mink, l) = cckl(nm, mink, l)*bi
          ddkl(nm, mink, l) = ddkl(nm, mink, l)*bi
          do k = mink + 1, kmin
             bi             = 1./(bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
             cckl(nm, k, l) = cckl(nm, k, l) * bi
             ddkl(nm, k, l) = (ddkl(nm, k, l) - aakl(nm, k, l)*ddkl(nm, k - 1, l)) * bi
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve1, gdp)
    !
    ! DD code added:
    !
    ! (re)solve system of equations
    !
  111  continue
    gdp%dd%difuiter = gdp%dd%difuiter + 1
    !
    ! DD code added end
    !
    call timer_start(timer_difu_solve2, gdp)  
    !
    ! back sweep
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - 1
       if (kfs(nm)/=0) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          r1(nm, kmin, l) = ddkl(nm, kmin, l)
          do k = kmin - 1, mink, -1
             r1(nm, k, l) = ddkl(nm, k, l) - cckl(nm, k, l)*r1(nm, k + 1, l)
          enddo
          if (kfsmx0(nm)>kfsmax(nm)) then
             do k = kmin + 1, kmax
                r1(nm, k, l) = r0(nm, k, l)
             enddo
          elseif (kfsmx0(nm)<kfsmax(nm)) then
             do k = kmin + 1, kmax
                r1(nm, k, l) = r1(nm, kmin, l)
                if (k > kfsmax(nm)) then
                   r1(nm, k, l) = 0.0_fp
                endif
             enddo
          else
          endif
       endif
    enddo
    call timer_stop(timer_difu_solve2, gdp)  
    !
    ! Parallel communication of R1
    !
    call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
    !
    ! D3dFlow_Solve_ADI_Conc: Check for convergence
    !
    nhystp = nxtstp(d3dflow_solve_adi_conc, gdp)
    if (nhystp==d3dflow_solve_adi_conc) goto 111
    !
end subroutine z_difu_solv_expl
!
!
!===============================================================================
subroutine z_difu_solv_impl( )
    !
    ! Solution procedure for the transport equation when advection was included implicitly
    !
    call timer_start(timer_difu_solve1, gdp) 
    !
    ! Division by the pivot for k=1 is not needed anymore
    ! because of row scaling
    !
    do nm = 1, nmmax
       if (kfs(nm) /= 0) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink + 1, kmin
             bi             = 1.0_fp/(bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
             bbkl(nm, k, l) = bi
             cckl(nm, k, l) = cckl(nm, k, l) * bi
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve1, gdp)
    !
    ! ITERATION LOOP
    !
    call timer_start(timer_difu_solve2, gdp)
    iter = 0
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink, kmin
             r1(nm, k, l) = r0(nm, k, l)
             uvdwk(nm, k) = r0(nm, k, l)
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve2, gdp)
    !
    ! Parallel communication of R1
    !
    call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
    !
    ! DD code added:
    !
    ! (re)solve system of equations
    !
  222  continue
    gdp%dd%difuiter = gdp%dd%difuiter + 1
    !
    ! DD code added end
    !
  333  continue
    iter = iter + 1
    !
    ! ITERATIVE SOLUTION METHOD (JACOBI ITERATION)
    ! IN HORIZONTAL DIRECTION
    !
    itr = 0
    !
    !   set concentrations in coupling points
    !
    call timer_start(timer_difu_solve2, gdp)
    do nm = 1, nmmax
       if (kcs(nm) == 3) then
          mink = kfsmin(nm)
          do k = mink, kmax
             r1(nm, k, l) = ddkl(nm, k, l)
          enddo
       endif
    enddo
    !
    do nm = 1, nmmax
       if (kcs(nm) /= 3 .and. kfs(nm) /= 0) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink, kmin
             !
             ! COMPUTE RIGHT HAND SIDE
             !
             uvdwk(nm, k) = bdx (nm,k) * r1(nm-icx  ,k,l) &
                        & + bdy (nm,k) * r1(nm-icy  ,k,l) &
                        & + buy (nm,k) * r1(nm+icy  ,k,l) &
                        & + bux (nm,k) * r1(nm+icx  ,k,l)
             uvdwk(nm, k) = ddkl(nm, k, l) - rscale(nm, k)*uvdwk(nm,k)
          enddo
       endif
    enddo
    !
    ! forward sweep
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          vvdwk(nm, mink) = uvdwk(nm, mink)*bbkl(nm, mink, l)
          do k = mink + 1, kmin
             vvdwk(nm, k) = (uvdwk(nm, k) - aakl(nm, k, l)*vvdwk(nm, k - 1)) * bbkl(nm, k, l)
          enddo
       endif
    enddo
    !
    ! back sweep
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - 1
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = kmin - 1, mink, -1
             vvdwk(nm, k) = vvdwk(nm, k) - cckl(nm, k, l)*vvdwk(nm, k + 1)
          enddo
          !
          ! Check other layers
          !
          if (kfsmx0(nm) > kfsmax(nm)) then
             do k = kmin + 1, kmax
                r1(nm, k, l) = r0(nm, k, l)
                vvdwk(nm, k) = 0.0_fp
             enddo
          elseif (kfsmx0(nm) < kfsmax(nm)) then
             do k = kmin + 1, kmax
                r1(nm, k, l) = r1(nm, kmin, l)
                if (k > kfsmax(nm)) then
                   r1(nm, k, l) = 0.0_fp
                   vvdwk(nm, k) = 0.0_fp
                endif
             enddo
          else
          endif
       endif
    enddo
    !
    ! CHECK FOR CONVERGENCE
    !
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          mink = kfsmin(nm)
          kmin = min(kfsmx0(nm), kfsmax(nm))
          do k = mink, kmin
             epsitr = max(1.0e-8_fp, 0.5e-3_fp*abs(r1(nm, k, l)))
             if (abs(vvdwk(nm, k) - r1(nm, k, l)) > epsitr) then
                itr = 1
             endif
             r1(nm, k, l) = vvdwk(nm, k)
          enddo
       endif
    enddo
    call timer_stop(timer_difu_solve2, gdp)
    !
    ! Parallel communication of R1
    !
    call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp )
    !
    ! Determine global maximum of 'itr' over all nodes
    ! Note: this enables to synchronize the iteration process
    !
    call dfreduce_gdp( itr, 1, dfint, dfmax, gdp )
    !
    if (itr>0 .and. iter<50) goto 333
    !
    if (iter >= 50) then
       write (errtxt, '(i0,a,i0)') l, ' ', nst
       call prterr(lundia    ,'S206'    ,trim(errtxt)    )
    endif 
    !
    ! DD code added:
    !
    ! D3dFlow_Solve_ADI_Conc: Check for convergence
    !
    nhystp = nxtstp(d3dflow_solve_adi_conc, gdp)
    if (nhystp==d3dflow_solve_adi_conc) goto 222
    !
    ! DD code added end
    !
end subroutine z_difu_solv_impl
!
!
!===============================================================================
end subroutine z_difu
