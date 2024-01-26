subroutine z_vermom_finvol(nmmax     ,kmax      ,icx       ,icy       ,u0        , &
                         & v0        ,w0        ,vicww     ,rxz       ,ryz       , &
                         & guu       ,gvv       ,guv       ,gvu       ,guz       , &
                         & gvz       ,gsqs      ,kfs       ,kcs       ,aak       ,bbk     , &
                         & cck       ,ddk       ,kfuz0     ,kfvz0     ,kfsz0     , &
                         & kfsmin    ,kfsmx0    ,kcshyd    ,dzs0      ,dzu0      , &
                         & dzv0      ,w1        ,p0        ,zk        ,gdp       )
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
    ! They replace the  include igd / include igp lines
    !
    real(fp), pointer :: hdt
    real(fp), pointer :: rhow
    real(fp), pointer :: vicmol
    integer , pointer :: m1_nhy
    integer , pointer :: m2_nhy
    integer , pointer :: n1_nhy
    integer , pointer :: n2_nhy
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
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: guz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gvz    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in) :: gsqs   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: vicww !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax) , intent(in) :: w0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: w1    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzs0  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzu0  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: dzv0  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: p0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxz   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ryz   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: u0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in) :: v0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                        , intent(in) :: zk    !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer            :: ddb
    integer            :: icxy
    integer            :: ifkx
    integer            :: ifky
    integer            :: ikenx
    integer            :: ikeny
    integer            :: k
    integer            :: kfad
    integer            :: ku
    integer            :: kuu
    integer            :: kd
    integer            :: m
    integer            :: maxk
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
    real(fp)           :: ddkadx = 0.0_fp
    real(fp)           :: ddkady = 0.0_fp
    real(fp)           :: ddkadz = 0.0_fp
    integer            :: kadx = 1
    integer            :: kady = 1
    integer            :: kadz = 1
    real(fp)           :: area       ! area of flux interface 
    real(fp)           :: uavg0      ! transport velocity at interface, east
    real(fp)           :: vavg0      ! transport velocity at interface, north
    real(fp)           :: wavg0      ! transport velocity at interface, top
    real(fp)           :: wzeta      ! conservation correction
    real(fp)           :: thvert     ! theta coefficient for vertical terms
    real(fp)           :: pcoef      ! temporary value for coefficient pressure derivative
    real(fp)           :: voltemp    ! work variable
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
    ddb    = gdp%d%ddbound
    icxy   = max(icx,icy)
    dt     = 2.0_fp * hdt
    thvert = 0.0_fp
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
    w1  = 0.0_fp
    !
    ! Turbulent stresses rxz, ryz
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
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
       enddo
    enddo
    !
    ! Set up the complete system for the vertical velocities
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
                ku   = k + 1
                kuu  = k + 2
                kd   = k - 1
                dz   = 0.5_fp * (zk(ku)-zk(kd))
                !
                ! Initialize system
                !
                bbk(nm,k) = 1.0_fp/dt
                ddk(nm,k) = w0(nm,k)/dt
                !
                ! Horizontal viscosity
                !
                viscow     =   (rxz(nm,k)-rxz(nmd,k)) / (0.5_fp*(gvv(nm)+gvv(ndm)))   &
                           & + (ryz(nm,k)-ryz(ndm,k)) / (0.5_fp*(guu(nm)+guu(nmd)))
                !                
                ddk(nm,k)  = ddk(nm,k) + viscow
                !
                ! Vertical viscosity (rzz)
                !
                viskup    = 0.5_fp * (2.0_fp*vicmol + vicww(nm, k) + vicww(nm, ku))
                visk      = 0.5_fp * (2.0_fp*vicmol + vicww(nm, k) + vicww(nm, kd))
                dzup      = zk(ku) - zk(k)
                dzdo      = zk(k ) - zk(kd)
                dz        = 0.5_fp * (dzup+dzdo)
                ddza      = visk   / (dzdo*dz)
                ddzc      = viskup / (dzup*dz)
                ddzb      = -ddza - ddzc
                aak(nm,k) = aak(nm,k) - ddza
                bbk(nm,k) = bbk(nm,k) - ddzb
                cck(nm,k) = cck(nm,k) - ddzc
                !
                ! Eq. for velocity under bottom (w=0)
                !
                if (k == kfsmin(nm)) then 
                   aak(nm,kd) = 0.0_fp
                   bbk(nm,kd) = 1.0_fp
                   cck(nm,kd) = 0.0_fp
                   ddk(nm,kd) = 0.0_fp
                endif
                !
                ! Eq. for velocity above free surface (const. extrapolation)
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
    ! Finite volume approach for horizontal advection: u dw/dx + v dw/dy
    !
    do nm = 1, nmmax
       nmd  = nm - icx
       ndm  = nm - icy
       ndmd = nm - icx - icy
       nmu  = nm + icx
       num  = nm + icy
       numu = nm + icx + icy
       ndmu = nm + icx - icy
       numd = nm - icx + icy
       !
       ! For all flooded w points, excluding water level boundaries
       !
       if (kcs(nm)*kfs(nm) == 1) then 
          do k = kfsmin(nm), kfsmx0(nm)-1
             ku   = k + 1
             kd   = k - 1
             dz   = 0.5_fp * (zk(ku)-zk(kd))
             !
             ! Note: system coefficients already initialized above
             !
             ! Advective fluxes
             !
             ddkadx = 0.0_fp
             ddkady = 0.0_fp
             ddkadz = 0.0_fp
             kadx   = 1
             kady   = 1
             kadz   = 1
             !
             if (kfsz0(nmd,k)*kfsz0(nmd,ku) == 1) then
                !
                ! Internal / water level boundary (west)
                !
                uavg0 = 0.5_fp * (u0(nmd,k)+u0(nmd,ku))
                area  = guu(nmd) * 0.5_fp*(dzu0(nmd,k)+dzu0(nmd,ku)) 
                !
                ! Water level boundary 
                !
                if (kcs(nmd) == 2) then 
                   !
                   ! Constant extrapolation outwards 
                   !
                   if ( uavg0 < 0.0_fp ) then
                      ddkadx = ddkadx + area*uavg0*w0(nm,k)
                   else
                      ! 
                      ! No advection if inward flow 
                      !
                      kadx = 0
                   endif   
                else
                   !
                   ! Internal flux
                   !
                   ddkadx = ddkadx + area*( 0.5_fp*(uavg0+abs(uavg0))*w0(nmd,k) + 0.5_fp*(uavg0-abs(uavg0))*w0(nm,k) )
                endif
             elseif( k >= kfsmx0(nmd) ) then ! .and. k < kfsmx0(nmu) ) then
                !
                ! Vertical free surface section (west) 
                !
                ! REMARK: is the if part necessary?
                !
                ! The horizontal velocities are always available, dzu0 not
                !
                area  = guu(nmd) * 0.5_fp*(dzs0(nm,k)+dzs0(nm,k+1))   !(dzu0(nmd,k)+dzu0(nmd,k+1)) 
                uavg0 = (u0(nmd,k)+u0(nmd,ku)) !/ (max (1, kfuz0(nmd,k)+kfuz0(nmd,ku)) )
                !
                ! Constant extrapolation outwards 
                !
                if( uavg0 < 0.0_fp ) then
                    ddkadx = ddkadx + area*uavg0*w0(nm,k) 
                else
                    ! 
                    ! No advection so flux west => flux east
                    !
                    kadx = 0
                endif
             endif
             !
             if (kfsz0(nmu,k)*kfsz0(nmu,ku) == 1) then
                !
                ! East
                !
                uavg0 = 0.5_fp * (u0(nm,k)+u0(nm,ku))
                area  = guu(nm) * 0.5_fp*(dzu0(nm,k)+dzu0(nm,ku))
                if (kcs(nmu) == 2) then
                   !
                   ! Constant extrapolation outwards 
                   !
                   if (uavg0 > 0.0_fp) then
                      ddkadx = ddkadx - area*uavg0*w0(nm,k) 
                   elseif ( kfuz0(nmd,k)*kfuz0(nmd,ku) == 1 ) then
                      ! 
                      ! No advection if inwards flow 
                      !
                      kadx = 0
                   endif
                else
                   ddkadx = ddkadx - area*( 0.5_fp*(uavg0+abs(uavg0))*w0(nm,k) + 0.5_fp*(uavg0-abs(uavg0))*w0(nmu,k) )
                endif
             elseif( k >= kfsmx0(nmu) ) then !  .and. k < kfsmx0(nmd) ) then
                !
                ! East vertical section
                ! Note: in case of a single column of water no horizontal advection (???)
                !
                ! REMARK: is the if part necessary?
                !
                ! The horizontal velocities are always available, dzu0 not
                !
                area  = guu(nm) * 0.5_fp*(dzs0(nm,k)+dzs0(nm,k+1)) !(dzu0(nm,k)+dzu0(nm,k+1)) 
                uavg0 = (u0(nm,k)+u0(nm,ku)) !/ (max (1, kfuz0(nm,k)+kfuz0(nm,ku)) )
                !
                ! Constant extrapolation outwards 
                !
                if( uavg0 > 0.0_fp ) then
                    ddkadx = ddkadx - area*uavg0*w0(nm,k)
                else
                    ! 
                    ! No advection so flux east => flux west
                    !
                    kadx = 0
                endif
             endif 
             if (k == kfsmx0(nm)-1) then
                !
                ! Vertical (top)
                !
                ! Obtain surface velocity from continuity equation
                !
                wzeta = w0(nm,k) + (u0(nmd,k+1)*dzu0(nmd,k+1)*guu(nmd)-u0(nm,k+1)*dzu0(nm,k+1)*guu(nm))/gsqs(nm)
                wavg0 = 0.5_fp*(wzeta+w0(nm,k))
!                if( wavg0 > 0.0 ) then 
                   ddkadz = ddkadz - (1.0_fp-thvert)*gsqs(nm)*( 0.5_fp*(wavg0+abs(wavg0))*w0(nm,k) + 0.5_fp*(wavg0-abs(wavg0))*wzeta )
!                else 
!                   !
!                   ! No advection in case of inwards flow
!                   !
!                   kadz = 0
!                endif
             else
                wavg0  = 0.5_fp*(w0(nm,k)+w0(nm,ku))
                ddkadz = ddkadz - gsqs(nm)*(1.0_fp-thvert)*( 0.5_fp*(wavg0+abs(wavg0))*w0(nm,k) + 0.5_fp*(wavg0-abs(wavg0))*w0(nm,ku) )
             endif
             !
             ! Vertical (bottom)
             !
             if (k > kfsmin(nm)) then
                wavg0  = 0.5_fp*(w0(nm,k)+w0(nm,kd))
                ddkadz = ddkadz + gsqs(nm)*(1.0_fp-thvert)*( 0.5_fp*(wavg0+abs(wavg0))*w0(nm,kd) + 0.5_fp*(wavg0-abs(wavg0))*w0(nm,k) )
             else
                ! 
                ! In the bottom layer the lower volume face is located at the center of the mass volume
                !
                wavg0  = 0.5_fp*(w0(nm,k)) ! w=0 at bottom
                ddkadz = ddkadz + gsqs(nm)*(1.0_fp-thvert)*( 0.5_fp*(wavg0+abs(wavg0))*w0(nm,kd) + 0.5_fp*(wavg0-abs(wavg0))*w0(nm,k) )
             endif
             !
             ! Update system coefficients
             !
             ddk(nm,k) = ddk(nm,k) + ( ddkadx*real(kadx,fp) + ddkady*real(kady,fp) + ddkadz*real(kadz,fp) ) / ( gsqs(nm)*0.5_fp*(dzs0(nm,ku)+dzs0(nm,k)) )
             !
             ! Pressure
             !
             pcoef      = 1.0_fp / dz
             ddk(nm, k) = ddk(nm,k) - pcoef*(p0(nm,ku)-p0(nm,k))/ rhow
          enddo
       endif
    enddo
    !
    ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfsmx0(nm)-kfsmin(nm)>0 .and. kcs(nm)==1) then
             maxk         = kfsmin(nm) - 1
             bi           = 1.0_fp / bbk(nm, maxk)
             bbk(nm,maxk) = 1.0_fp
             cck(nm,maxk) = cck(nm,maxk) * bi
             ddk(nm,maxk) = ddk(nm,maxk) * bi
          endif
       enddo
    enddo
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfs(nm)*kcs(nm) == 1) then
             do k = kfsmin(nm), kfsmx0(nm)
                bi        = 1.0_fp / (bbk(nm,k)-aak(nm,k)*cck(nm,k-1))
                cck(nm,k) = cck(nm,k) * bi
                ddk(nm,k) = (ddk(nm,k)-aak(nm,k)*ddk(nm,k-1)) * bi
             enddo
          endif
       enddo
    enddo
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kcs(nm) == 1) then
              if (kfsmx0(nm) >= kfsmin(nm)) then
                 w1(nm,kfsmx0(nm)) = ddk(nm,kfsmx0(nm))
              endif
          endif
       enddo
    enddo
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kcs(nm) == 1) then
             do k = kfsmx0(nm)-1, kfsmin(nm)-1, -1
                ddk(nm,k) = ddk(nm,k) - cck(nm,k)*ddk(nm,k+1)
                w1(nm ,k) = ddk(nm,k)
             enddo
          endif
       enddo
    enddo
end subroutine z_vermom_finvol
