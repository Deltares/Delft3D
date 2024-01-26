subroutine z_momcor(nmmax     ,kmax      ,icx       ,icy       ,s1        , &
                  & u1        ,v1        ,w1        ,umean     ,vmean     , &
                  & qxk       ,qyk       ,qzk       ,guu       ,gvv       , &
                  & guv       ,gvu       ,gsqs      ,kcs       ,kcshyd    , &
                  & kfuz1     ,kfvz1     ,kfsz1     ,kfumin    ,kfumax    , &
                  & kfvmin    ,kfvmax    ,kfsmin    ,kfsmax    ,dzs1      , &
                  & dzu0      ,dzv0      ,p1        ,p0        ,s00       , &
                  & w0        ,s0        ,disch     , &
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
!
! Global variables
!
    integer                                            , intent(in) :: icx
    integer                                            , intent(in) :: icy
    integer                                            , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcshyd !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: nst    !!  Time step number
    integer                                            , intent(in) :: nocol  !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: norow  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(5, norow + nocol)              , intent(in) :: irocol !  Description and declaration in esm_alloc_int.f90
    integer , dimension(7, nsrc)                                    :: mnksrc !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: d0k    !!  Internal work array
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)       , intent(in) :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s00    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: qzk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                       :: disch  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: evap   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: p0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: p1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: precip !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                        , intent(in) :: zk    !  Description and declaration in esm_alloc_real.f90
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
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    dt   = 2.0_fp * hdt
    !
    d0k  = 0.0_fp
    !
    ! compute non hydrostatic correction
    !
    do nm = 1, nmmax
       if (kcshyd(nm)==1) then
          if (kfsmax(nm)>=kfsmin(nm)) then
             s00(nm) = s1(nm)
             s1 (nm) = s1(nm) + p1(nm, kfsmax(nm))/(rhow*ag)
          endif
          nmu = nm + icx
          num = nm + icy
          do k = kfumin(nm), kfumax(nm)
             if (kfuz1(nm, k)==1 .and. kfsz1(nmu, k)==1) then
                u1 (nm, k) = u1(nm, k) - dt*(p1(nmu, k) - p1(nm, k))/(gvu(nm)*rhow)
                qxk(nm, k) = dzu0(nm, k)*u1(nm, k)*guu(nm)
             endif
          enddo
          do k = kfvmin(nm), kfvmax(nm)
             if (kfvz1(nm, k)==1 .and. kfsz1(num, k)==1) then
                v1 (nm, k) = v1(nm, k) - dt*(p1(num, k) - p1(nm, k))/(guv(nm)*rhow)
                qyk(nm, k) = dzv0(nm, k)*v1(nm, k)*gvv(nm)
             endif
          enddo
       endif
    enddo
    !
    if (maseva > 0) then
       do nm = 1, nmmax
          if (kcs(nm)==1) then
             k = kfsmax(nm)
             d0k(nm, k) = d0k(nm, k) + precip(nm)
             if (kfs(nm)==1) then
                d0k(nm, k) = d0k(nm, k) - evap(nm)/rhow
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
       nm   = (mnksrc(5, i) + ddb) + ((mnksrc(4, i) - 1) + ddb)*icxy
       k    = mnksrc(6, i)
       kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
       if (kenm/=0 .or. disch(i)>=0.0_fp) then
          if (k/=0) then
             !
             ! The order is important at dry points (kfsmax=-1)
             !
             if (k>kfsmax(nm)) then
                k = kfsmax(nm)
             endif
             if (k<kfsmin(nm)) then
                k = kfsmin(nm)
             endif
             d0k(nm, k) = d0k(nm, k) + disch(i)
          else
             do kk = 1, kmax
                !
                ! Source term addition is zero when dzs1 = 0.0
                ! Do not add source term in that case: dps+s1 may be 0.0!
                !
                if (dzs1(nm, kk) > 0.0_fp) then
                   d0k(nm, kk) = d0k(nm, kk) + disch(i)*dzs1(nm, kk)               &
                               & /(real(dps(nm),fp) + s0(nm))
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
       if (mnksrc(7, i) >= 2) then
          nm   = (mnksrc(2, i) + ddb) + ((mnksrc(1, i) - 1) + ddb)*icxy
          k    = mnksrc(3, i)
          kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
          if (kenm/=0 .or. -disch(i)>=0.0_fp) then
             if (k /= 0) then
                !
                ! The order is important at dry points (kfsmax=-1)
                !
                if (k>kfsmax(nm)) then
                   k = kfsmax(nm)
                endif
                if (k<kfsmin(nm)) then
                   k = kfsmin(nm)
                endif
                d0k(nm, k) = d0k(nm, k) - disch(i)
             else
                do kk = 1, kmax
                   !
                   ! Source term addition is zero when dzs1 = 0.0
                   ! Do not add source term in that case: dps+s1 may be 0.0!
                   !
                   if (dzs1(nm, kk) > 0.0_fp) then
                      d0k(nm, kk) = d0k(nm, kk) - disch(i)*dzs1(nm, kk)            &
                                  & /(real(dps(nm),fp) + s0(nm))
                   endif
                enddo
             endif
          elseif (mnksrc(7, i) /= 3) then
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
    do nm = 1, nmmax
       if (kcshyd(nm) == 1) then
          nmu = nm + icx
          nmd = nm - icx
          ndm = nm - icy
          do k = kfsmin(nm), kfsmax(nm)
             if (k == kfsmax(nm)) then
                qzk(nm, k) = qzk(nm, k-1) + (qxk(nmd, k) - qxk(nm, k)  &
                                        & +  qyk(ndm, k) - qyk(nm, k)) &
                                        & + d0k(nm,k)
                w1 (nm, k) = qzk(nm, k)/gsqs(nm)
             else
                dz         = 0.5_fp*(dzs1(nm, k) + dzs1(nm, k+1))
                w1(nm, k)  = w1(nm, k) - dt*(p1(nm, k + 1) - p1(nm, k))/(dz*rhow)
                qzk(nm, k) = w1(nm, k)*gsqs(nm)
             endif
          enddo
       endif
    enddo
    do nm = 1, nmmax
       if (kcshyd(nm) == 1) then
          do k = kfsmin(nm), kfsmax(nm)
             p1(nm, k) = p1(nm, k) + p0(nm, k)
          enddo
       endif
    enddo
    !
    do nm = 1, nmmax
       if (kcshyd(nm) == 1) then
          do k = kfsmin(nm), kfsmax(nm)
             p1(nm, k) = p1(nm, k) + ag*rhow*(s00(nm) - s1(nm))
             w0(nm, k) = w1(nm, k)
          enddo
          s00(nm) = s1(nm)
       endif
    enddo
end subroutine z_momcor
