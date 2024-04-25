subroutine difuvl(icreep    ,timest    ,lundia    ,nst       ,icx       , &
                & icy       ,j         ,nmmaxj    ,nmmax     ,kmax      , &
                & lstsci    ,lstsc     ,lsal      ,ltem      ,lsecfl    , &
                & lsec      ,lsed      ,lsts      ,norow     ,irocol    , &
                & kcs       ,kcu       ,kfs       ,kfu       ,kfv       , &
                & kadu      ,kadv      ,s0        ,s1        ,hu        , &
                & hv        ,dps       ,qxk       ,qyk       ,qzk       , &
                & guu       ,gvv       ,guv       ,gvu       ,gsqs      , &
                & rbnd      ,sigdif    ,sigmol    ,r0        ,r1        , &
                & sour      ,sink      ,ws        ,thick     ,sig       , &
                & dicuv     ,vicww     ,dsdksi    ,dsdeta    ,dtdksi    , &
                & dtdeta    ,aak       ,bbk       ,cck       ,bdddx     , &
                & bddx      ,bdx       ,bux       ,buux      ,buuux     , &
                & uvdwk     ,vvdwk     ,areau     ,areav     ,volum0    , &
                & volum1    ,aakl      ,bbkl      ,cckl      ,ddkl      , &
                & bruvai    ,stage     ,eqmbcsand ,eqmbcmud  ,sedtyp    , &
                & seddif    ,gdp       )
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
!              Explicit in the x- and y-direction, implicit in
!              sigma-direction.
!              Sinks are treated implicitly and sources explicit-
!              y. A special approach is used for the hori-
!              ontal diffusion to avoid artificial creeping.
! Method used: - Horizontal Advection in x- and y-direction :
!                explicit, Van Leer 2 scheme
!              - Horizontal Diffusion :
!                3D: explicit, along Z-planes
!                2D: explicit in U-direction
!                    explicit in V-direction
!              - Option: horizontal diffusion strictly horizontal
!                using special filter
!              - Vertical Advection :
!                implicit, central scheme
!              - Vertical Diffusion : implicit
!              - Sources are integrated explicitly.
!              - Sinks are integrated implicitly.
!              Reference: (B. Van Leer, Towards the Ultimate
!              Conservative Difference Scheme, II Monotonicity
!              and Conservation, combined in a second-order scheme,
!              J. Computational Phys. 14, 1974, 361-370)
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
    use dfparall 
    use flow2d3d_timers
    use sediment_basics_module
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
    real(fp)                            , pointer :: ck
    real(fp)                            , pointer :: dicoww
    real(fp)                            , pointer :: eps
    real(fp), dimension(:,:,:)          , pointer :: fluxu
    real(fp), dimension(:,:,:)          , pointer :: fluxuc
    real(fp), dimension(:,:,:)          , pointer :: fluxv
    real(fp), dimension(:,:,:)          , pointer :: fluxvc
    real(fp)                            , pointer :: dryflc
    real(fp)                            , pointer :: vicmol
    real(fp)                            , pointer :: xlo
    integer                             , pointer :: iro
    type (flwoutputtype)                , pointer :: flwoutput
    integer                             , pointer :: max_mud_sedtyp
!
! Global variables
!
    integer                                                     , intent(in)  :: icreep     !  Description and declaration in tricom.igs
    integer                                                     , intent(in)  :: icx        !  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-
                                                                                            !  dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                     , intent(in)  :: icy        !  Increment in the Y-dir. (see ICX)
    integer                                                     , intent(in)  :: j          !  Begin pointer for arrays which have been transformed into 1D arrays.
                                                                                            !  Due to the shift in the 2nd (M-)index, J = -2*NMAX + 1
    integer                                                     , intent(in)  :: kmax       !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: lsal       !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lsec       !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lsecfl     !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lsed       !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: lsts       !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lstsc      !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lstsci     !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: ltem       !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: lundia     !  Description and declaration in inout.igs
    integer                                                     , intent(in)  :: nmmax      !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: nmmaxj     !  Description and declaration in dimens.igs
    integer                                                     , intent(in)  :: norow      !  Description and declaration in esm_alloc_int.f90
    integer                                                     , intent(in)  :: nst
    integer, dimension(5, norow)                                , intent(in)  :: irocol     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   , intent(in)  :: kcs        !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   , intent(in)  :: kcu        !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   , intent(in)  :: kfs        !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   , intent(in)  :: kfu        !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                   , intent(in)  :: kfv        !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             , intent(in)  :: kadu       !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)             , intent(in)  :: kadv       !  Description and declaration in esm_alloc_int.f90
    integer , dimension(lsed)                                   , intent(in)  :: sedtyp     !  sediment type: 0=total/1=noncoh/2=coh
    logical                                                     , intent(in)  :: eqmbcsand  !  Description and declaration in morpar.igs
    logical                                                     , intent(in)  :: eqmbcmud   !  Description and declaration in morpar.igs
    real(fp)                                                    , intent(in)  :: timest     !  Half Integration time step [sec.]
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                , intent(in)  :: dps        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: gsqs       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: guu        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: guv        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: gvu        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: gvv        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: hu         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: hv         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: s0         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                  , intent(in)  :: s1         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)          , intent(in)  :: bruvai     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)          , intent(in)  :: vicww      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)          , intent(in)  :: qzk        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)    , intent(in)  :: seddif     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, lsed)    , intent(in)  :: ws         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: aak        !  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: bbk        !  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: bdddx      !  Internal work array, implicit coupling of concentration in (N,M,K)
                                                                                            !  with layer concentration in (N,M-3,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: bddx       !  Internal work array, implicit coup-ing of concentration in (N,M,K)
                                                                                            !  with layer concentration in (N,M-2,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: bdx        !  Internal work array, implicit coupling of concentration in (N,M,K)
                                                                                            !  with layer concentration in (N,M-1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: buuux      !  Internal work array, implicit coupling of concentration in (N,M,K)
                                                                                            !  with layer concentration in (N,M+3,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: buux       !  Internal work array, implicit coupling of concentration in (N,M,K)
                                                                                            !  with layer concentration in (N,M+2,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: bux        !  Internal work array, implicit coupling of concentration in (N,M,K)
                                                                                            !  with layer concentration in (N,M+1,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: cck        !  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)          , intent(in)  :: dicuv      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: dsdeta     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: dsdksi     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: dtdeta     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: dtdksi     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: areau
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: areav
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: qxk        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: qyk        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: volum0
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)            , intent(in)  :: volum1
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: uvdwk      !  Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                          :: vvdwk      !  Internal work array for Jac.iteration
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                  :: aakl       !  Internal work array, lower diagonal tridiagonal matrix, implicit coupling
                                                                                            !  of concentration in (N,M,K) with concentration in (N,M,K-1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                  :: bbkl       !  Internal work array, main diagonal tridiagonal matrix, implicit coupling
                                                                                            !  of concentration in (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                  :: cckl       !  Internal work array, upper diagonal tridiagonal matrix, implicit coupling
                                                                                            !  of concentration in (N,M,K) with concentration in (N,M,K+1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                  :: ddkl       !  Internal work array, diagonal space at (N,M,K,L)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                  :: r0         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)                  :: r1         !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)    , intent(in)  :: sink       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)    , intent(in)  :: sour       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                   , intent(in)  :: sig        !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                   , intent(in)  :: thick      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax, max(lstsc, 1), 2, norow)          , intent(in)  :: rbnd       !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                                 , intent(in)  :: sigdif     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(lstsci)                                 , intent(in)  :: sigmol     !  Description and declaration in esm_alloc_real.f90
    character(8)                                                , intent(in)  :: stage      !  First or second half time step
!
! Local variables
!
    integer                                                         :: ddb
    integer                                                         :: ic
    integer                                                         :: icxy
    integer                                                         :: istat
    integer                                                         :: k
    integer                                                         :: kfw
    integer                                                         :: l
    integer                                                         :: ll
    integer                                                         :: ls
    integer                                                         :: lst
    integer                                                         :: maskval
    integer                                                         :: mf
    integer                                                         :: ml
    integer                                                         :: n
    integer                                                         :: ndm
    integer                                                         :: nhystp
    integer                                                         :: nm
    integer                                                         :: nmd
    integer                                                         :: nmf
    integer                                                         :: nmfu
    integer                                                         :: nml
    integer                                                         :: nmlu
    integer                                                         :: nmu
    integer                                                         :: nmuu
    integer                                                         :: num
    integer                                                         :: nuum
    real(fp)                                                        :: adza
    real(fp)                                                        :: adzc
    real(fp)                                                        :: bi
    real(fp)                                                        :: cfl
    real(fp)                                                        :: cl
    real(fp)                                                        :: cr
    real(fp)                                                        :: ddzc
    real(fp)                                                        :: difiwe
    real(fp)                                                        :: difl
    real(fp)                                                        :: difr
    real(fp)                                                        :: diz1
    real(fp)                                                        :: fac
    real(fp)                                                        :: flux
    real(fp)                                                        :: h0
    real(fp)                                                        :: h0i
    real(fp)                                                        :: h0new
    real(fp)                                                        :: h0old
    real(fp)                                                        :: qzw
    real(fp)                                                        :: r00
    real(fp), external                                              :: reddic
    real(fp)                                                        :: rr1
    real(fp)                                                        :: rr2
    real(fp)                                                        :: sqrtbv
    real(fp)                                                        :: tsg
    real(fp)                                                        :: uuu
    real(fp)                                                        :: vvv
    real(fp)                                                        :: hmin
    integer                                                         :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    eps         => gdp%gdconst%eps
    fluxu       => gdp%gdflwpar%fluxu
    fluxuc      => gdp%gdflwpar%fluxuc
    fluxv       => gdp%gdflwpar%fluxv
    fluxvc      => gdp%gdflwpar%fluxvc
    flwoutput   => gdp%gdflwpar%flwoutput
    dryflc      => gdp%gdnumeco%dryflc
    vicmol      => gdp%gdphysco%vicmol
    dicoww      => gdp%gdphysco%dicoww
    iro         => gdp%gdphysco%iro
    xlo         => gdp%gdturcoe%xlo
    ck          => gdp%gdturcoe%ck
    max_mud_sedtyp => gdp%gdsedpar%max_mud_sedtyp
    !
    ddb = gdp%d%ddbound
    icxy = max(icx, icy) 
    !
    istat = 0
    nm_pos= 1
    if (.not. associated(gdp%gdflwpar%fluxu)) then
        if (istat==0) allocate (gdp%gdflwpar%fluxu(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
        if (istat==0) allocate (gdp%gdflwpar%fluxv(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
        if (flwoutput%cumdifuflux) then
            if (istat==0) allocate (gdp%gdflwpar%fluxuc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
            if (istat==0) allocate (gdp%gdflwpar%fluxvc(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci), stat = istat)
        endif
        !
        if (istat /= 0) then
            call prterr(lundia, 'U021', 'DIFUFVL: memory alloc error')
            call d3stop(1, gdp)
        endif
        !
        ! update local pointers
        !
        fluxu          => gdp%gdflwpar%fluxu
        fluxuc         => gdp%gdflwpar%fluxuc
        fluxv          => gdp%gdflwpar%fluxv
        fluxvc         => gdp%gdflwpar%fluxvc
        flwoutput      => gdp%gdflwpar%flwoutput
        !
        if (flwoutput%cumdifuflux) then
            fluxuc = 0.0_fp
            fluxvc = 0.0_fp
        endif
    endif
    !
    ! Initialization
    !
    fluxu = 0.0_fp
    fluxv = 0.0_fp
    !
    !  INITIALIZE FOR ALL (nm, k)
    !
    aak = 0.0_fp
    cck = 0.0_fp
    !
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfs(nm)==1) then
             bbk(nm, k) = volum1(nm, k)/timest
          else
             bbk(nm, k) = 1.0_fp
             if (lsec>0) r0(nm, k, lsecfl) = 0.0_fp
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
                ddkl(nm, k, l) = volum0(nm, k)*r0(nm, k, l)/timest
             else
                ddkl(nm, k, l) = r0(nm, k, l)
             endif
          enddo
       enddo
    enddo
    !
    nmd  = -icx
    nmu  = +icx
    nmuu = +icx + icx
    do nm = 1, nmmax
       !
       ! CONTRIBUTION OF ADVECTION IN X-DIRECTION
       !
       nmd  = nmd  + 1
       nmu  = nmu  + 1
       nmuu = nmuu + 1
       do k = 1, kmax
          if (kfu(nm)*kadu(nm, k) == 1) then
             uuu = qxk(nm, k)/(guu(nm)*hu(nm)*thick(k))
             cfl = uuu*timest/gvu(nm)
             if (qxk(nm, k) > 0.0_fp) then
                do l = 1, lstsci
                   rr1 = abs(r0(nmd, k, l) - 2.0_fp*r0(nm, k, l) + r0(nmu, k, l))
                   rr2 = abs(r0(nmd, k, l) - r0(nmu, k, l))
                   if (kfu(nmd)*kadu(nmd, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then
                      r00 = r0(nm, k, l)
                   else
                      r00 = r0(nm, k, l)                                     &
                          & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(nmd, k, l)) &
                          &                 *(r0(nmu, k, l) - r0(nm , k, l)) &
                          &                 /(r0(nmu, k, l) - r0(nmd, k, l))
                   endif
                   flux = qxk(nm, k)*r00
                   if (kcs(nm)  == 1) then                   
                        fluxu(nm,k,l) = fluxu(nm,k,l) + flux
                        ddkl (nm,k,l) = ddkl (nm,k,l) - flux
                   endif
                   if (kcs(nmu) == 1) ddkl(nmu, k, l) = ddkl(nmu, k, l) + flux
                enddo
             else
                do l = 1, lstsci
                   rr1 = abs(r0(nmuu, k, l) - 2.0_fp*r0(nmu, k, l) + r0(nm, k, l))
                   rr2 = abs(r0(nmuu, k, l) - r0(nm, k, l))
                   if (kfu(nmu)*kadu(nmu, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then
                      r00 = r0(nmu, k, l)
                   else
                      r00 = r0(nmu, k, l)                                     &
                          & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(nmu , k, l)) &
                          &                 *(r0(nmu, k, l) - r0(nmuu, k, l)) &
                          &                 /(r0(nm , k, l) - r0(nmuu, k, l))
                   endif
                   flux = qxk(nm, k)*r00
                   if (kcs(nm)  == 1) then
                        fluxu(nm,k,l) = fluxu(nm,k,l) + flux
                        ddkl (nm,k,l) = ddkl (nm,k,l) - flux
                   endif
                   if (kcs(nmu) == 1) ddkl(nmu, k, l) = ddkl(nmu, k, l) + flux
                enddo
             endif
          endif
       enddo
    enddo
    !
    ! CONTRIBUTION OF ADVECTION IN Y-DIRECTION
    !
    ndm  = -icy
    num  = +icy
    nuum = +icy + icy
    do nm = 1, nmmax
       ndm  = ndm  + 1
       num  = num  + 1
       nuum = nuum + 1
       do k = 1, kmax
          if (kfv(nm)*kadv(nm, k) == 1) then
             vvv = qyk(nm, k)/(gvv(nm)*hv(nm)*thick(k))
             cfl = vvv*timest/guv(nm)
             if (qyk(nm, k) > 0.0_fp) then
                do l = 1, lstsci
                   rr1 = abs(r0(ndm, k, l) - 2.0_fp*r0(nm, k, l) + r0(num, k, l))
                   rr2 = abs(r0(ndm, k, l) - r0(num, k, l))
                   if (kfv(ndm)*kadv(ndm, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then
                      r00 = r0(nm, k, l)
                   else
                      r00 = r0(nm, k, l)                                     &
                          & + (1.0_fp - cfl)*(r0(nm , k, l) - r0(ndm, k, l)) &
                          &                 *(r0(num, k, l) - r0(nm , k, l)) &
                          &                 /(r0(num, k, l) - r0(ndm, k, l))
                   endif
                   flux = qyk(nm, k) * r00
                   if (kcs(nm)  == 1) then
                        fluxv(nm,k,l) = fluxv(nm,k,l) + flux 
                        ddkl (nm,k,l) = ddkl (nm,k,l) - flux
                   endif
                   if (kcs(num) == 1) ddkl(num, k, l) = ddkl(num, k, l) + flux
                enddo
             else
                do l = 1, lstsci
                   rr1 = abs(r0(nuum, k, l) - 2.0_fp*r0(num, k, l) + r0(nm, k, l))
                   rr2 = abs(r0(nuum, k, l) - r0(nm, k, l))
                   if (kfv(num)*kadv(num, k) == 0 .or. rr1 >= rr2 .or. rr2 < eps_fp) then
                      r00 = r0(num, k, l)
                   else
                      r00 = r0(num, k, l)                                     &
                          & + (1.0_fp + cfl)*(r0(nm , k, l) - r0(num , k, l)) &
                          &                 *(r0(num, k, l) - r0(nuum, k, l)) &
                          &                 /(r0(nm , k, l) - r0(nuum, k, l))
                   endif
                   flux = qyk(nm, k) * r00
                   if (kcs(nm)  == 1) then 
                        fluxv(nm,k,l) = fluxv(nm,k,l) + flux
                        ddkl (nm,k,l) = ddkl (nm,k,l) - flux
                   endif
                   if (kcs(num) == 1) ddkl(num, k, l) = ddkl(num, k, l) + flux
                enddo
             endif
          endif
       enddo
    enddo
    !
    !
    ! Explicit algoritm (call DIFACR) leads to extra stablity criterium
    ! dt <= (dx**2)/(2*dicuv)
    !
    ! This diffusion part (loop 410) is constituent independent.
    ! The value of SIGDIF(L) = 0.7 (see TKECOF) for all LSTSCI
    !
    if (icreep==0 .or. kmax==1) then
       !
       ! HORIZONTAL DIFFUSION IN X-DIRECTION ALONG SIGMA PLANES
       !
       do l = 1, lstsci
          if (lsec==2 .and. l==lsecfl) then
             cycle
          endif
          do k = 1, kmax
             !
             ! CONTRIBUTION TO VOLUMES NM AND NMU
             !
             nmu = icx
             do nm = 1, nmmax
                nmu = nmu + 1
                hmin = min(s1(nm) + real(dps(nm),fp), s1(nmu) + real(dps(nmu),fp))
                if (kfu(nm)*kadu(nm, k) /= 0 .and. hmin > dryflc) then
                   cl             = r0   (nm ,k,l)
                   difl           = dicuv(nm ,k)
                   cr             = r0   (nmu,k,l)
                   difr           = dicuv(nmu,k)
                   flux           = 0.5_fp* (cr-cl) * (difl+difr) / (0.7_fp*gvu(nm))
                   maskval        = max(0, 2 - abs(kcs(nm))) 
                   fluxu(nm,k,l)  = fluxu(nm ,k,l) + areau(nm,k)*flux*maskval
                   ddkl (nm,k,l)  = ddkl (nm ,k,l) + areau(nm,k)*flux*maskval
                   maskval        = max(0, 2 - abs(kcs(nmu))) 
                   ddkl (nmu,k,l) = ddkl (nmu,k,l) - areau(nm,k)*flux*maskval
                endif
             enddo
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
             ! CONTRIBUTION TO VOLUMES NM AND NUM
             !
             num = icy
             do nm = 1, nmmax
                num = num + 1
                hmin = min(s1(nm) + real(dps(nm),fp), s1(num) + real(dps(num),fp))
                if (kfv(nm)*kadv(nm, k) /= 0 .and. hmin > dryflc) then
                   cl             = r0   (nm ,k,l)
                   difl           = dicuv(nm ,k)
                   cr             = r0   (num,k,l)
                   difr           = dicuv(num,k)
                   flux           = 0.5_fp * (cr-cl) * (difl+difr) / (0.7_fp*guv(nm))
                   maskval        = max(0, 2 - abs(kcs(nm))) 
                   fluxv(nm ,k,l) = fluxv(nm ,k,l) + areav(nm,k)*flux*maskval
                   ddkl (nm ,k,l) = ddkl (nm ,k,l) + areav(nm,k)*flux*maskval
                   maskval        = max(0, 2 - abs(kcs(num))) 
                   ddkl (num,k,l) = ddkl (num,k,l) - areav(nm,k)*flux*maskval
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
    !
    ! Swap fluxes when in stage 2, because then the U- and V- direction are interchanged
    !
    if (stage == 'stage2  ') then
       fluxu       => gdp%gdflwpar%fluxv
       fluxv       => gdp%gdflwpar%fluxu
    endif
    !
    ! Cumulative flux
    !
    if (flwoutput%cumdifuflux) then
       fluxuc = fluxuc + fluxu * timest
       fluxvc = fluxvc + fluxv * timest
    endif
    !
    if (kmax>1) then
       do k = 1, kmax - 1
          if (k==1 .or. k==kmax - 1) then
             kfw = 1
          else
             kfw = 0
          endif
          do nm = 1, nmmax
             !
             ! ADVECTION IN VERTICAL DIRECTION; W*DU/DZ
             !
             if (kfs(nm)==1) then
                qzw = qzk(nm, k)
                if (qzw>0.0_fp) then
                   adza = 0.5_fp*qzw*(1 - kfw)
                   adzc = 0.5_fp*qzw*(1 + kfw)
                else
                   adza = 0.5_fp*qzw*(1 + kfw)
                   adzc = 0.5_fp*qzw*(1 - kfw)
                endif
                aak(nm, k + 1) = aak(nm, k + 1) + adza
                bbk(nm, k + 1) = bbk(nm, k + 1) + adzc
                bbk(nm, k) = bbk(nm, k) - adza
                cck(nm, k) = cck(nm, k) - adzc
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
             if (kfs(nm)==1) then
                aakl(nm, k, l) = aak(nm, k)
                bbkl(nm, k, l) = bbk(nm, k)
                cckl(nm, k, l) = cck(nm, k)
             endif
          enddo
       enddo
    enddo
    !
    if (kmax>1) then
       do l = 1, lstsci
          !
          ! l = sediment: ls > 0
          ! else        : ls = 0
          !
          ls = 0
          if ((l>max(lsal, ltem)) .and. (l<=lsts)) then
             ls = l - max(lsal, ltem)
          endif
          do k = 1, kmax - 1
             tsg = 0.5_fp*(thick(k) + thick(k + 1))
             do nm = 1, nmmax
                !
                ! DIFFUSION IN VERTICAL DIRECTION
                !
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
                   ddzc = gsqs(nm) * diz1 * h0i / tsg
                   aakl(nm, k+1, l) = aakl(nm, k+1, l) - ddzc
                   bbkl(nm, k+1, l) = bbkl(nm, k+1, l) + ddzc
                   bbkl(nm, k  , l) = bbkl(nm, k  , l) + ddzc
                   cckl(nm, k  , l) = cckl(nm, k  , l) - ddzc
                endif
             enddo
          enddo
       enddo
    endif
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
    !     On open boundary no secondary flow (=> loop over LSTSC)
    !
    do nm = 1, nmmax
       if (kcs(nm)==2) then
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
       n = irocol(1, ic)
       mf = irocol(2, ic) - 1
       ml = irocol(3, ic)
       nmf = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nml = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmfu = nmf + icx
       nmlu = nml + icx
       !
       ! IMPLEMENTATION OF BOUNDARY CONDITIONS
       !
       if (kcu(nmf)==1) then
          do k = 1, kmax
             do l = 1, lstsc
                ddkl(nmf, k, l) = rbnd(k, l, 1, ic)
             enddo
          enddo
       endif
       if (kcu(nml)==1) then
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
    do l = 1, lstsci
       !
       ! SOURCES AND SINK TERMS
       !
       ! SINKS ARE TREATED IMPLICITLY
       !
       if (lsec==2 .and. l==lsecfl) then
          !
          ! secondary flow (equilibrium equals to new intensity)
          !
          do k = 1, kmax
             do nm = 1, nmmax
                if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then 
                   h0new = s1(nm) + real(dps(nm),fp)
                   if (abs(sink(nm, k, l))*h0new>eps) then
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
       !
       if (l==lsecfl) then
          !
          ! boundary conditions secondary flow (spiral motion intensity)
          !
          call secbou(j         ,nmmaxj    ,kmax      ,icx       ,icy       , &
                    & lstsci    ,lsecfl    ,kfu       ,irocol    ,norow     , &
                    & s0        ,s1        ,dps       ,r1        ,sour      , &
                    & sink      ,gdp       )
          if (lsec==2) then
             ! 
             ! exchange r1 with neighbours for parallel runs 
             ! 
             call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp ) 
             cycle
          endif
       endif
       !
       ! DD code added:
       !
       ! left-hand side is now set by Delft3D-FLOW instead of the mapper
       !
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
       !
       !
       !        D3dFlow_Build_ADI_Conc: poke the coupling equations into system
       !
       nhystp = nxtstp(d3dflow_build_adi_conc, gdp)
       !
       ! DD code added end
       !
       !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
       !
       !
       ! DD code added:
       !
       !
       !       (re)solve system of equations
       !
  111  continue
       gdp%dd%difuiter = gdp%dd%difuiter + 1
       !
       ! set concentrations in coupling points
       !
       do k = 1, kmax
          do nm = 1, nmmax
             if (kcs(nm)==3) then
                r1(nm, k, l) = ddkl(nm, k, l)
             endif
          enddo
       enddo
       !
       ! DD code added end
       !
       do nm = 1, nmmax
          if ( (kfs(nm)==1) .and. (kcs(nm)==1) ) then 
             !
             !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
             !
             do k = 1, kmax
                bi             = 1.0_fp / bbkl(nm, k, l)
                aakl(nm, k, l) = aakl(nm, k, l) * bi
                bbkl(nm, k, l) = 1.0_fp
                cckl(nm, k, l) = cckl(nm, k, l) * bi
                ddkl(nm, k, l) = ddkl(nm, k, l) * bi
             enddo
             r1(nm, 1, l) = ddkl(nm, 1, l)
             !
             !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
             !
             !
             ! Division by the pivot for k=1 is not needed anymore
             ! because of row scaling
             !
             do k = 2, kmax
                bi = 1.0_fp / (bbkl(nm, k, l) - aakl(nm, k, l)*cckl(nm, k - 1, l))
                cckl(nm, k, l) = cckl(nm, k, l) * bi
                r1(nm, k, l) = (ddkl(nm, k, l) - aakl(nm, k, l)*r1(nm, k - 1, l)) * bi
             enddo
             do k = kmax - 1, 1, -1
                r1(nm, k, l) = r1(nm, k, l) - cckl(nm, k, l)*r1(nm, k + 1, l)
             enddo
          endif
       enddo
       !
       ! exchange r1 with neighbours for parallel runs 
       ! 
       call dfexchg ( r1(:,:,l), 1, kmax, dfloat, nm_pos, gdp ) 
       ! 
       ! DD code added:
       !
       !
       !       D3dFlow_Solve_ADI_Conc: Check for convergence
       !
       nhystp = nxtstp(d3dflow_solve_adi_conc, gdp)
       if (nhystp==d3dflow_solve_adi_conc) goto 111
       !
       ! DD code added end
       !
    enddo
    !
    ! Deallocate possible local array
    !
end subroutine difuvl
