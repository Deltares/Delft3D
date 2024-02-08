subroutine z_momcor_nhfull(nmmax     ,kmax      ,icx       ,icy       ,s1        , &
                         & u1        ,v1        ,w1        ,umean     ,vmean     , &
                         & qxk       ,qyk       ,qzk       ,guu       ,gvv       , &
                         & guv       ,gvu       ,gsqs      ,kcs       ,kcshyd    , &
                         & kfumin    ,kfumx0    ,kfuz0     ,kfvz0     , &
                         & kfvmin    ,kfvmx0    ,kfsmin    ,kfsmx0    ,kfsz0     , &
                         & dzs0      ,dzu0      ,dzv0      ,p1        ,p0        , &
                         & u0        ,v0        ,w0        ,s0        ,disch     , &
                         & evap      ,mnksrc    ,nsrc      ,d0k       ,dps       , &
                         & norow     ,nocol     ,irocol    ,zk        ,            &
                         & kfs       ,kfu       ,kfv       ,nst       ,precip    , &
                         & gdp )
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
    real(fp)               , pointer :: hdt
    integer                , pointer :: maseva
    integer                , pointer :: lundia
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    logical                , pointer :: culvert
    real(fp)               , pointer :: tetaq
    real(fp)               , pointer :: tetaz
    character(8)           , pointer :: updwl
!
! Global variables
!
    integer                                            , intent(in) :: icx
    integer                                            , intent(in) :: icy
    integer                                            , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kcshyd !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfvmx0 !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)          , intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)    , intent(in) :: kfvz0  !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: nst    !!  Time step number
    integer                                            , intent(in) :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: norow  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow + nocol)               , intent(in) :: irocol !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                                     :: mnksrc !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: d0k    !!  Internal work array
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: qzk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzs0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                       :: disch  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: evap   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: p0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: precip !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                        , intent(in) :: zk     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer       :: ddb
    integer       :: icxy
    integer       :: ic
    integer       :: i
    integer       :: ibf
    integer       :: ibl
    integer       :: k
    integer       :: kenm
    integer       :: kk
    integer       :: ku
    integer       :: kd
    integer       :: n
    integer       :: m
    integer       :: mf
    integer       :: ml
    integer       :: ndm
    integer       :: nm
    integer       :: nmd
    integer       :: nmst
    integer       :: nmu
    integer       :: num
    integer       :: nf
    integer       :: nfm
    integer       :: nfu
    integer       :: nfum
    integer       :: nfdm
    integer       :: nl
    integer       :: nlm
    integer       :: nlum
    integer       :: nmf
    integer       :: nmfu
    integer       :: nml
    integer       :: nmlu
    real(fp)      :: dt
    real(fp)      :: dy
    real(fp)      :: dz
    real(fp)      :: wdummy,rmass,rmassd,rmax
    character(26) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    culvert    => gdp%gdprocs%culvert
    rhow       => gdp%gdphysco%rhow
    ag         => gdp%gdphysco%ag
    maseva     => gdp%gdheat%maseva
    lundia     => gdp%gdinout%lundia
    hdt        => gdp%gdnumeco%hdt
    tetaq      => gdp%gdnonhyd%tetaq
    tetaz      => gdp%gdnonhyd%tetaz
    updwl      => gdp%gdnonhyd%updwl
    !
    ddb = gdp%d%ddbound
    !
    icxy = max(icx, icy)
    dt = 2*hdt
    !
    ! full array initialization
    !
    d0k = 0.0_fp
    qxk = 0.0_fp
    qyk = 0.0_fp
    !
    ! compute non hydrostatic correction
    !
    do nm = 1, nmmax
       num = nm + icy
       nmu = nm + icx
       if( kcs(nm)*kcs(nmu)*kfu(nm) == 1) then
          do k = kfumin(nm), kfumx0(nm)
             if (kfuz0(nm,k) == 1) then
                !u1(nm,k) = u1(nm,k) - dt*(p1(nmu,k)-p1(nm,k))/(gvu(nm)*rhow)
                u1(nm,k) = u1(nm,k) - dt*(p1(nmu,k) - p1(nm ,k))*kfsz0(nmu,k)*kfsz0(nm ,k)/(gvu(nm)*rhow)
             endif
          enddo
          umean(nm) = u1(nm,kfumx0(nm))
       endif
      if (kcs(nm)*kcs(num)*kfv(nm) == 1) then
          do k = kfvmin(nm), kfvmx0(nm)
             if (kfvz0(nm,k) == 1) then
                !v1(nm,k) = v1(nm,k) - dt*(p1(num,k)-p1(nm,k))/(guv(nm)*rhow)
                v1(nm,k) = v1(nm,k) - dt*(p1(num,k) - p1(nm ,k))*kfsz0(num,k)*kfsz0(nm ,k)/(guv(nm)*rhow)
             endif
          enddo
          vmean(nm) = v1(nm,kfvmx0(nm))
       endif
    enddo
    !
    ! Open boundary conditions
    ! At water level boundaries, a correction must be applied.  For
    ! all other (= velocity) boundaries, the velocity is set (in CUCBP_NHFULL), i.e.,
    ! do not apply a correction (would overwrite value!).
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
          do k = kfumin(nmf), kfumx0(nmf)
             u1(nmf,k) = u1(nmf,k) - dt*(p1(nmfu,k)-p1(nmf,k))/(gvu(nmf)*rhow)
          enddo
       endif
       if (ibl == 2) then
          do k = kfumin(nml), kfumx0(nml)
             u1(nml,k) = u1(nml,k) - dt*(p1(nmlu,k)-p1(nml,k))/(gvu(nml)*rhow)
          enddo
       endif
    enddo
    do ic = norow+1, norow+nocol
       m    = irocol(1, ic)
       nf   = irocol(2, ic) - 1
       nl   = irocol(3, ic)
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nfm  = (nf+ddb)*icy + (m+ddb)*icx - icxy
       nlm  = (nl+ddb)*icy + (m+ddb)*icx - icxy
       nlum = nlm + icy
       nfum = nfm + icy
       if (ibf == 2) then
          do k = kfvmin(nfm), kfvmx0(nfm)
             v1(nfm,k) = v1(nfm,k) - dt*(p1(nfum,k)-p1(nfm,k))/(guv(nfm)*rhow)
          enddo
       endif
       if (ibl == 2) then
          do k = kfvmin(nlm), kfvmx0(nlm)
             v1(nlm,k) = v1(nlm,k) - dt*(p1(nlum,k)-p1(nlm,k))/(guv(nlm)*rhow)
          enddo
       endif
    enddo
    !
    ! Computation of the new fluxes in u- and v-points
    !
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          do k = kfumin(nm), kfumx0(nm)
             qxk(nm,k) = dzu0(nm,k) * u1(nm,k) * guu(nm)
          enddo
       endif
       if (kfv(nm) == 1) then
          do k = kfvmin(nm), kfvmx0(nm)
             qyk(nm,k) = dzv0(nm,k) * v1(nm,k) * gvv(nm)
          enddo
       endif
    enddo
    if (maseva > 0) then
       do nm = 1, nmmax
          if (kcs(nm) == 1) then
             k         = kfsmx0(nm)
             d0k(nm,k) = d0k(nm,k) + precip(nm)
             if (kfs(nm) == 1) then
                d0k(nm,k) = d0k(nm,k) - evap(nm)/rhow
             endif
          endif
       enddo
    endif
    !
    ! ADDITION OF DISCHARGES (suction only permitted if the point isn't dry)
    !
    !
    ! k=0 : discharge divided over total column
    ! k<>0: discharge in cell k
    !
    do i = 1, nsrc
       nm   = (mnksrc(5,i)+ddb) + ((mnksrc(4,i)-1)+ddb)*icxy
       k    = mnksrc(6, i)
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
             d0k(nm,k) = d0k(nm,k) + disch(i)
          else
             do kk = 1, kmax
                !
                ! Source term addition is zero when dzs0 = 0.0
                ! Do not add source term in that case: dps+s1 may be 0.0!
                !
                if (dzs0(nm,kk) > 0.0_fp) then
                   d0k(nm,kk) = d0k(nm, kk) + disch(i)*dzs0(nm,kk)               &
                              &               /(real(dps(nm),fp)+s0(nm))
                endif
             enddo
          endif
       else
          write (errtxt, '(i0,i3)') nst, i
          call prterr(lundia    ,'S208'    ,trim(errtxt))
       endif
       !
       ! in case of an intake for an intake/outfall combination:
       !
       if (mnksrc(7,i) >= 2) then
          nm   = (mnksrc(2,i)+ddb) + ((mnksrc(1,i)-1)+ddb)*icxy
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
                d0k(nm,k) = d0k(nm,k) - disch(i)
             else
                do kk = 1, kmax
                   !
                   ! Source term addition is zero when dzs0 = 0.0
                   ! Do not add source term in that case: dps+s1 may be 0.0!
                   !
                   if (dzs0(nm,kk) > 0.0_fp) then
                      d0k(nm,kk) = d0k(nm,kk) - disch(i)*dzs0(nm,kk)            &
                                 &              /(real(dps(nm),fp)+s0(nm))
                   endif
                enddo
             endif
          elseif (mnksrc(7,i) /= 3) then
             !
             ! in case of a culvert no warning generated
             !
             write (errtxt, '(i0,i3)') nst, i
             call prterr(lundia    ,'S208'    ,trim(errtxt))
          else
          endif
       endif
    enddo
    !
    !  Correction for vertical tiles of water
    !
    do nm= 1, nmmax
        nmd = nm - icx
        ndm = nm - icy
        if (kcs(nm)*kfs(nm) == 1) then
          do k = kfsmx0(nm)+1, kmax
             d0k(nm,kfsmx0(nm)) = d0k(nm,kfsmx0(nm)) + (  qxk(nmd,k) - qxk(nm,k) &
                                &                       + qyk(ndm,k) - qyk(nm,k))
          enddo
       endif  
    enddo
    rmax = -1.1_fp
    do nm = 1, nmmax
       if (kcs(nm)*kfs(nm) == 1) then
          nmd = nm - icx
          ndm = nm - icy
          w1 (nm,kfsmin(nm)-1) = 0.0_fp
          qzk(nm,kfsmin(nm)-1) = 0.0_fp
          do k = kfsmin(nm), kfsmx0(nm) 
             if (updwl == 'momcor') then
                !
                ! Everywhere except the surface from the momentum correction equation
                !
                if (k == kfsmx0(nm)) then
                   w1(nm,k) = w1(nm,k-1) + (  qxk(nmd,k) - qxk(nm,k)                    &
                            &               + qyk(ndm,k) - qyk(nm,k)+d0k(nm,k))/gsqs(nm)
                else
                  dz       = 0.5_fp * (zk(k+1)-zk(k-1))
                  w1(nm,k) = w1(nm,k) - dt * ((p1(nm,k+1)-p1(nm,k))/(dz*rhow))
                endif
             else
                ! 
                ! (alternative approach, which guarantees conservation of mass)
                ! Everywhere from the continuity equation at time n+1
                !
                w1(nm,k) = w1(nm,k-1) + (  qxk(nmd,k) - qxk(nm,k)                    &
                         &               + qyk(ndm,k) - qyk(nm,k)+d0k(nm,k))/gsqs(nm)
             endif
             qzk(nm,k) = w1(nm,k) * gsqs(nm)
          enddo
          !
          ! Update water levels
          !
          s1(nm) = s0(nm) + dt * (tetaz*w1(nm,kfsmx0(nm))+(1.0_fp-tetaz)*w0(nm,kfsmx0(nm)))
          !
          ! Check conservation of mass
          !
          do k = kfsmin(nm), kfsmx0(nm)-1
             rmassd = gsqs(nm)*(w1(nm,k)-w1(nm,k-1)) - (  qxk(nmd,k) - qxk(nm,k)           &
                    &                                   + qyk(ndm,k) - qyk(nm,k)+d0k(nm,k))
             rmassd = abs(rmassd)
             if (rmassd > rmax) then
                rmax = rmassd
             endif
          enddo
          k     = kfsmx0(nm)
          rmass =   gsqs(nm)*(s1(nm)-s0(nm))/dt                                   &
                & - gsqs(nm)* w1(nm,k-1) - (  qxk(nmd,k) - qxk(nm,k)              &
                &                           + qyk(ndm,k) - qyk(nm,k))-d0k(nm,k)
          rmass = abs(rmass)
          if (rmass > rmax) then
             rmax = rmass
          endif
       endif
       if (kcs(nm)*kfs(nm)>0) then
         do k = kfsmin(nm), kfsmx0(nm)
             p1(nm, k) = p1(nm, k)/tetaq + p0(nm, k)
             p1(nm, k) = p1(nm, k) + ag*rhow*(s0(nm) - s1(nm))
          enddo
       endif
    enddo
    !
    ! Open boundary conditions
    ! For (= velocity/discharge) boundaries, the velocity is set (in CUCBP), i.e.,
    ! we apply the Neumann BC for the pressure correction and the waterlevels.
    !
    do ic = 1, norow
       n    = irocol(1,ic)
       mf   = irocol(2,ic) - 1
       ml   = irocol(3,ic)
       ibf  = irocol(4,ic)
       ibl  = irocol(5,ic)
       nmf  = (n+ddb)*icy + (mf+ddb)*icx - icxy
       nmfu = nmf + icx
       nml  = (n+ddb)*icy + (ml+ddb)*icx - icxy
       nmlu = nml + icx
       if (ibf > 2) then
          s1(nmf) = s1(nmfu) 
          do k = kfumin(nmf), kmax
             p1(nmf,k) = p1(nmfu,k) 
             p0(nmf,k) = p1(nmfu,k)
          enddo
       endif
       if (ibl > 2) then
          s1(nmlu) = s1(nml)
          do k = kfumin(nml), kmax
             p1(nmlu,k)= p1(nml,k)
             p0(nmlu,k)= p1(nml,k)
          enddo
       endif
    enddo
    do ic = norow + 1, norow + nocol
       m    = irocol(1,ic)
       nf   = irocol(2,ic) - 1
       nl   = irocol(3,ic)
       ibf  = irocol(4,ic)
       ibl  = irocol(5,ic)
       nfm  = (nf+ddb)*icy + (m+ddb)*icx - icxy
       nlm  = (nl+ddb)*icy + (m+ddb)*icx - icxy
       nlum = nlm + icy
       nfum = nfm + icy
       if (ibf > 2) then
          s1(nfm) = s1(nfum)
          do k = kfvmin(nfm), kmax
             p1(nfm,k) = p1(nfum,k)
             p0(nfm,k) = p1(nfum,k)
          enddo
       endif
       if (ibl > 2) then
          s1 (nlum) = s1(nlm)
          do k = kfvmin(nlm), kmax
             p1(nlum,k) = p1(nlm,k)
             p0(nlum,k) = p1(nlm,k)
          enddo
       endif
    enddo
end subroutine z_momcor_nhfull
