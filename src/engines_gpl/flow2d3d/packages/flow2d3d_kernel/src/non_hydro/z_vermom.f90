subroutine z_vermom(nmmax     ,kmax      ,icx       ,icy       ,u0        , &
                  & v0        ,w0        ,vicww     ,rxz       ,ryz       , &
                  & guu       ,gvv       ,guv       ,gvu       ,kfs       , &
                  & kcs       ,aak       ,bbk       ,cck       ,ddk       , &
                  & kfuz1     ,kfvz1     ,kfsz1     ,kfsmin    ,kfsmax    , &
                  & kcshyd    ,dzs1      ,dzu0      ,dzv0      ,w1        , &
                  & p0        ,gdp       )
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
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: vicmol
    integer                , pointer :: m1_nhy
    integer                , pointer :: m2_nhy
    integer                , pointer :: n1_nhy
    integer                , pointer :: n2_nhy
!
! Global variables
!
    integer                                            , intent(in) :: icx
    integer                                            , intent(in) :: icy
    integer                                            , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                            , intent(in) :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: kcshyd !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfsz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfuz1  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: kfvz1  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: w0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzs1   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzv0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: p0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ryz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: v0     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: ddb
    integer  :: icxy
    integer  :: ifkx
    integer  :: ifky
    integer  :: ikenx
    integer  :: ikeny
    integer  :: k
    integer  :: kfad
    integer  :: ku
    integer  :: kup
    integer  :: m
    integer  :: maxk
    integer  :: ndelta
    integer  :: ndm
    integer  :: ndmd
    integer  :: ndmu
    integer  :: nm
    integer  :: nmd
    integer  :: nmst
    integer  :: nmstart
    integer  :: nmu
    integer  :: num
    integer  :: numd
    integer  :: numu
    real(fp) :: advecx
    real(fp) :: advecy
    real(fp) :: adza
    real(fp) :: adzc
    real(fp) :: bi
    real(fp) :: cuu
    real(fp) :: cvv
    real(fp) :: ddza
    real(fp) :: ddzb
    real(fp) :: ddzc
    real(fp) :: dt
    real(fp) :: dz
    real(fp) :: dzdo
    real(fp) :: dzu
    real(fp) :: dzup
    real(fp) :: dzv
    real(fp) :: geta
    real(fp) :: gksi
    real(fp) :: uuu
    real(fp) :: viscow
    real(fp) :: visk
    real(fp) :: viskup
    real(fp) :: vix
    real(fp) :: viy
    real(fp) :: vvv
    real(fp) :: wdo
    real(fp) :: wup
    real(fp) :: www
!
!! executable statements -------------------------------------------------------
!
    m1_nhy   => gdp%gdnonhyd%m1_nhy
    m2_nhy   => gdp%gdnonhyd%m2_nhy
    n1_nhy   => gdp%gdnonhyd%n1_nhy
    n2_nhy   => gdp%gdnonhyd%n2_nhy
    rhow     => gdp%gdphysco%rhow
    vicmol   => gdp%gdphysco%vicmol
    hdt      => gdp%gdnumeco%hdt
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    dt   = 2.0_fp*hdt
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    ! initialisatie
    !
    do nm = 1, nmmax
       do k = 1, kmax
          aak(nm, k) = 0.0_fp
          bbk(nm, k) = 1.0_fp
          cck(nm, k) = 0.0_fp
          ddk(nm, k) = 0.0_fp
       enddo
    enddo
    !
    ! turbulent stresses rxz, ryz
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          nmu = nm + icx
          num = nm + icy
          do k = 1, kmax
             rxz(nm, k) = 0.0_fp
             ryz(nm, k) = 0.0_fp
             kup = min(kmax, k + 1)
             if (k<kmax) then
                ifkx = kfuz1(nm, k)*kfuz1(nm, kup)*kfsz1(nm, k)*kfsz1(nmu, k)
                ifky = kfvz1(nm, k)*kfvz1(nm, kup)*kfsz1(nm, k)*kfsz1(num, k)
             else
                ifkx = 0
                ifky = 0
             endif
             vix = 0.5_fp*(vicww(nm, k) + vicww(nmu, k))
             viy = 0.5_fp*(vicww(nm, k) + vicww(num, k))
             dzu = 0.5_fp*(dzu0 (nm, k) + dzu0 (nm , kup))
             dzv = 0.5_fp*(dzv0 (nm, k) + dzv0 (nm , kup))
             if (ifkx==1) then
                rxz(nm, k) = vix*((w1(nmu, k)   - w1(nm, k))/gvu(nm) +  &
                           &      (u0(nm , kup) - u0(nm, k))/dzu)
             endif
             if (ifky==1) then
                ryz(nm, k) = viy*((w1(num, k)   - w1(nm, k))/guv(nm) +  &
                           &      (v0(nm , kup) - v0(nm, k))/dzv)
             endif
          enddo
       enddo
    enddo
    !
    ! horizontal advection: u dw/dx + v dw/dy
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
             do k = kfsmin(nm), kfsmax(nm)
                ku     = min(kfsmax(nm), k + 1)
                vvv    = 0.25_fp*(v0(ndm, k) + v0(nm, k) + v0(ndm, ku) + v0(nm, ku))
                uuu    = 0.25_fp*(u0(nmd, k) + u0(nm, k) + u0(nmd, ku) + u0(nm, ku))
                cvv    = vvv / geta
                cuu    = uuu / gksi
                advecx = 0.0_fp
                advecy = 0.0_fp
                if (uuu>=0.0_fp .and. vvv>=0.0_fp) then
                   if (cuu>cvv) then
                      ikenx = kfuz1(nmd ,k)*kfuz1(nmd ,ku)*kfsz1(nm ,k)*kfsz1(nmd ,k)
                      ikeny = kfvz1(ndmd,k)*kfvz1(ndmd,ku)*kfsz1(nmd,k)*kfsz1(ndmd,k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(nm, k) - w1(nmd, k))/gvu(nmd)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(nmd, k) - w1(ndmd, k))/guv(ndmd)
                      endif
                   else
                      ikenx = kfuz1(ndmd,k)*kfuz1(ndmd,ku)*kfsz1(ndm,k)*kfsz1(ndmd, k)
                      ikeny = kfvz1(ndm ,k)*kfvz1(ndm ,ku)*kfsz1(nm ,k)*kfsz1(ndm , k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(ndm, k) - w1(ndmd, k))/gvu(ndmd)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(nm, k) - w1(ndm, k))/guv(ndm)
                      endif
                   endif
                elseif (uuu<0.0_fp .and. vvv>=0.0_fp) then
                   if ( - cuu>cvv) then
                      ikenx = kfuz1(nm  ,k)*kfuz1(nm  ,ku)*kfsz1(nmu,k)*kfsz1(nm  ,k)
                      ikeny = kfvz1(ndmu,k)*kfvz1(ndmu,ku)*kfsz1(nmu,k)*kfsz1(ndmu,k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(nmu, k) - w1(nm, k))/gvu(nm)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(nmu, k) - w1(ndmu, k))/guv(ndmu)
                      endif
                   else
                      ikenx = kfuz1(ndm,k)*kfuz1(ndm,ku)*kfsz1(ndmu,k)*kfsz1(ndm,k)
                      ikeny = kfvz1(ndm,k)*kfvz1(ndm,ku)*kfsz1(nm  ,k)*kfsz1(ndm,k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(ndmu, k) - w1(ndm, k))/gvu(ndm)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(nm, k) - w1(ndm, k))/guv(ndm)
                      endif
                   endif
                elseif (uuu>=0.0_fp .and. vvv<0.0_fp) then
                   if (cuu> - cvv) then
                      ikenx = kfuz1(nmd,k)*kfuz1(nmd,ku)*kfsz1(nm  ,k)*kfsz1(nmd,k)
                      ikeny = kfvz1(nmd,k)*kfvz1(nmd,ku)*kfsz1(numd,k)*kfsz1(nmd,k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(nm, k) - w1(nmd, k))/gvu(nmd)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(numd, k) - w1(nmd, k))/guv(nmd)
                      endif
                   else
                      ikenx = kfuz1(numd,k)*kfuz1(numd,ku)*kfsz1(num,k)*kfsz1(numd,k)
                      ikeny = kfvz1(nm  ,k)*kfvz1(nm  ,ku)*kfsz1(nm ,k)*kfsz1(num ,k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(num, k) - w1(numd, k))/gvu(numd)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(num, k) - w1(nm, k))/guv(nm)
                      endif
                   endif
                elseif (uuu<0.0_fp .and. vvv<0.0_fp) then
                   if ( - cuu> - cvv) then
                      ikenx = kfuz1(nm ,k)*kfuz1(nm ,ku)*kfsz1(nmu,k)*kfsz1(nm  ,k)
                      ikeny = kfvz1(nmu,k)*kfvz1(nmu,ku)*kfsz1(nmu,k)*kfsz1(numu,k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(nmu, k) - w1(nm, k))/gvu(nm)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(numu, k) - w1(nmu, k))/guv(nmu)
                      endif
                   else
                      ikenx = kfuz1(numd,k)*kfuz1(numd,ku)*kfsz1(num,k)*kfsz1(numd,k)
                      ikeny = kfvz1(nm  ,k)*kfvz1(nm  ,ku)*kfsz1(nm ,k)*kfsz1(num ,k)
                      if (ikenx/=0) then
                         advecx = ikenx*uuu*(w1(numu, k) - w1(num, k))/gvu(num)
                      endif
                      if (ikeny/=0) then
                         advecy = ikeny*vvv*(w1(num, k) - w1(nm, k))/guv(nm)
                      endif
                   endif
                else
                endif
                !
                ! vertical advection
                !
                dz   = 0.5_fp*(dzs1(nm, k) + dzs1(nm, ku))
                kfad = 0
                kup  = k + 1
                if (k == kfsmin(nm)) then
                   kfad = 1
                endif
                if (k == kfsmax(nm)) then
                   kfad = -1
                   kup  = k
                endif
                dzup = dzs1(nm, kup)
                dzdo = dzs1(nm, k)
                if (k <= kfsmax(nm)-2) then
                   if (kfs(nm)*kcs(nm) == 1) then
                      www = w1(nm, k)
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
                   aak(nm, k) = adza
                   bbk(nm, k) = 1.0_fp/dt - adza - adzc
                   cck(nm, k) = adzc
                !
                ! advection first order upwind for two upper most grid cells to avoid
                ! downwinding when dzs1(nm,kfsmax) approaches zero
                !
                else
                   ku = min(kfsmax(nm), k + 1)
                   if (kfs(nm)*kcs(nm)==1) then
                      www = w1(nm, k)
                      wdo = 0.5_fp*(www + abs(www))/dzs1(nm, k)
                      wup = 0.5_fp*(www - abs(www))/dzs1(nm, ku)
                      if (k==kfsmin(nm)) then
                         wdo = 0.0_fp
                      endif
                      if (k==kfsmax(nm)) then
                         wup = 0.0_fp
                      endif
                      aak(nm, k) = -wdo
                      bbk(nm, k) = 1.0_fp/dt + wdo - wup
                      cck(nm, k) = wup
                   endif
                endif
                !
                ! vertical viscosity (rzz)
                !
                viskup = 0.125_fp*(2 + kfad*(1 - kfad))                             &
                       & *(2*vicmol + vicww(nm, k) + vicww(nm, kup))
                visk = 0.5_fp*(2.0_fp*vicmol + vicww(nm, k) + vicww(nm, k - 1))
                dzup = dzs1(nm, kup)
                dzdo = dzs1(nm, k)
                dz   = 0.5_fp*(dzup + dzdo)
                ddza = visk  /(dzdo*dz)
                ddzc = viskup/(dzup*dz)
                ddzb = -ddza - ddzc
                aak(nm, k) = aak(nm, k) - ddza
                bbk(nm, k) = bbk(nm, k) - ddzb
                cck(nm, k) = cck(nm, k) - ddzc
                !
                viscow = (rxz(nm, k) - rxz(nmd, k))/(0.5_fp*(gvv(nm) + gvv(ndm))) + &
                       & (ryz(nm, k) - ryz(ndm, k))/(0.5_fp*(guu(nm) + guu(nmd)))
                if (k/=kfsmax(nm)) then
                   ddk(nm, k) = w0(nm, k)/dt - (advecx + advecy) + viscow -     &
                              & (p0(nm, ku) - p0(nm, k))/(dz*rhow)
                else
                   ddk(nm, k) = w0(nm, k)/dt - (advecx + advecy) + viscow
                endif
                if (k == kfsmin(nm)) then
                   aak(nm, k - 1) = 0.0_fp
                   bbk(nm, k - 1) = 1.0_fp
                   cck(nm, k - 1) = 0.0_fp
                   ddk(nm, k - 1) = 0.0_fp
                endif
             enddo
          endif
       enddo
    enddo
    !
    !***SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfsmax(nm) - kfsmin(nm)>0 .and. kcs(nm)==1) then
             maxk          = kfsmin(nm) - 1
             bi            = 1.0_fp/bbk(nm, maxk)
             bbk(nm, maxk) = 1.0_fp
             cck(nm, maxk) = cck(nm, maxk)*bi
             ddk(nm, maxk) = ddk(nm, maxk)*bi
          endif
       enddo
    enddo
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kfs(nm)*kcs(nm)==1) then
             do k = kfsmin(nm), kfsmax(nm)
                bi         = 1.0_fp/(bbk(nm, k) - aak(nm, k)*cck(nm, k - 1))
                cck(nm, k) = cck(nm, k)*bi
                ddk(nm, k) = (ddk(nm, k) - aak(nm, k)*ddk(nm, k - 1))*bi
             enddo
          endif
       enddo
    enddo
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kcs(nm) == 1) then
              if (kfsmax(nm) >= kfsmin(nm)) then
                 w1(nm, kfsmax(nm)) = ddk(nm, kfsmax(nm))
              endif
          endif
       enddo
    enddo
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          if (kcs(nm) == 1) then
             do k = kfsmax(nm) - 1, kfsmin(nm) - 1, -1
                ddk(nm, k) = ddk(nm, k) - cck(nm, k)*ddk(nm, k + 1)
                w1 (nm, k) = ddk(nm, k)
             enddo
          endif
       enddo
    enddo
end subroutine z_vermom
