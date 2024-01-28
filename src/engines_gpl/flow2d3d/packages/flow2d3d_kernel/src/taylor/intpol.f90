subroutine intpol(kmax      ,kmxdt     ,kmxt      ,h0        ,zeta      , &
                & dps       ,siglim    ,barcli    ,riav      ,rilz      , &
                & xk        ,u0        ,dudz      ,d2u0      ,ustbx     , &
                & ustwix    ,v0        ,dvdz      ,d2v0      ,ustby     , &
                & ustwiy    ,sig       ,thick     ,rich      ,bruvai    , &
                & rtur0     ,vicww     ,scale     ,bvav      ,bvmx      , &
                & xkh       ,kbed      ,ktop      ,umean     ,vmean     , &
                & utg       ,vtg       ,d2u       ,d2v       ,bv2       , &
                & tke       ,eps       ,edvis     ,zlw       ,tlw       , &
                & zlo       ,useref    ,gdp       )
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
!    Function: Interpolation on vertical profiles of:
!             -horizontal velocity,
!             -bouyancy frequency and
!             -turbulence parameters.
!              Estimate vertical curvature of horizontal
!              velocity profile.
! Method used: linear interpolation and matching to
!              logarithmic layers.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
   !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: ricrit
    real(fp)               , pointer :: bvmin
    real(fp)               , pointer :: epsuko
    real(fp)               , pointer :: epsbv2
    real(fp)               , pointer :: clw
    real(fp)               , pointer :: ckw
    real(fp)               , pointer :: viscof
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: vonkar
!
! Global variables
!
    integer                                     :: kbed   !!  KTG-value of upper level stratified
                                                          !!  layer
    integer                       , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                       , intent(in)  :: kmxdt  !  Description and declaration in dimens.igs
    integer                       , intent(in)  :: kmxt   !  Description and declaration in dimens.igs
    integer                                     :: ktop   !!  KTG-value of lower level stratified
                                                          !!  layer
    logical                       , intent(out) :: barcli !!  Switch for applying IWE model
    logical                       , intent(in)  :: useref
    real(fp)                                    :: bvav   !!  Mean bouyancy frequency
    real(fp)                      , intent(out) :: bvmx   !!  Maximal buoyancy frequency
    real(prec)                    , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)                                    :: h0     !!  Water depth
    real(fp)                                    :: riav   !!  Mean value of all RICH with RICH >
                                                          !!  RICRIT
    real(fp)                                    :: rilz   !!  Vertical interval with RICH > RICRIT
    real(fp)                                    :: scale  !!  Length scale for non-dimensionalising
    real(fp)                      , intent(in)  :: siglim !  Description and declaration in iwepar.igs
    real(fp)                                    :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)                                    :: ustbx
    real(fp)                                    :: ustby
    real(fp)                      , intent(in)  :: ustwix !!  X-component of wind-shear stress
                                                          !!  velocity
    real(fp)                      , intent(in)  :: ustwiy !!  Y-component of wind-shear stress
                                                          !!  velocity
    real(fp)                                    :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp)                      , intent(in)  :: xk     !!  Magnitude of horizontal wave number
                                                          !!  vector, in [m^-1]
    real(fp)                      , intent(out) :: xkh    !!  XK normalized by SCALE
    real(fp)                      , intent(in)  :: zeta   !!  Watrface elevation
    real(fp)                                    :: zlo    !!  Ozmidov length scale, depth-averaged
                                                          !!  over stratified layer
    real(fp), dimension(0:kmax)   , intent(in)  :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                 :: d2u0   !!  Dummay array, curvature of vertical
                                                          !!  profile of u-velocity component
                                                          !!  component on hydrodynamic grid
    real(fp), dimension(0:kmax)                 :: d2v0   !!  Dummy array, curvature of vertical
                                                          !!  profile of v-velocity component
                                                          !!  component on hydrodynamic grid
    real(fp), dimension(0:kmax)   , intent(in)  :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)   , intent(in)  :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)   , intent(in)  :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)   , intent(in)  :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax, 2), intent(in)  :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmxdt)                :: bv2    !!  Interpolated bouyancy frequency,
                                                          !!   squared
    real(fp), dimension(0:kmxdt)                :: d2u    !!  D2U0, interpolated
    real(fp), dimension(0:kmxdt)                :: d2v    !!  D2V0, interpolated
    real(fp), dimension(0:kmxdt)                :: edvis  !!  Non-dimensional eddy viscosity, on
                                                          !!  interpolated grid
    real(fp), dimension(0:kmxdt)                :: eps    !  Description and declaration in numeco.igs
    real(fp), dimension(0:kmxdt)                :: tke    !!  Non-dimensional interpolated TKE=
                                                          !!  RTUR0(K,1)
    real(fp), dimension(0:kmxdt)  , intent(out) :: tlw    !!  Non-dimensional integral time scale
                                                          !!  of vertical turbulent motions
    real(fp), dimension(0:kmxdt)                :: utg    !!  Non-dimensional interpolated u-vel.
                                                          !!  component, relative to vel. at KREF
    real(fp), dimension(0:kmxdt)                :: vtg    !!  As UTG for v-velocity component
    real(fp), dimension(0:kmxdt)                :: zlw    !!  Non-dimensional integral length scale
                                                          !!  of turbulence in z/w-direction
    real(fp), dimension(kmax)     , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)     , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)     , intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)     , intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: k
    integer  :: kd
    integer  :: kdpm   ! Sigma-level pointer 
    integer  :: kdpmd
    integer  :: kref   ! Reference level for velocity, on interpolated grid 
    integer  :: ksum
    integer  :: ktg    ! Pointer for interpolation arrays 
    real(fp) :: bv2av  ! Mean buoyancy frequency squared 
    real(fp) :: bv2lvl ! Definition of minimal buoyancy frequency, squared, in stratified layer 
    real(fp) :: bv2mx  ! Maximal buoyancy frequency squared 
    real(fp) :: diff
    real(fp) :: dsig
    real(fp) :: dz     ! Thickness of sigma layer 
    real(fp) :: dztg   ! Layer thickness of interpolation grid 
    real(fp) :: epsktg
    real(fp) :: epsscl ! Scaling factor for dissipation [m^2/s^3] 
    real(fp) :: ratio
    real(fp) :: rmsw   ! Rms of turbulent vertical velocity 
    real(fp) :: sumbv2
    real(fp) :: tkektg
    real(fp) :: tkescl ! Scaling factor for turbulent kinetic energy [m^2/s^2] 
    real(fp) :: uref   ! U-velocity component as reference level KREF 
    real(fp) :: velscl ! Scaling factor for velocity [m/s] 
    real(fp) :: vis3
    real(fp) :: vismol ! Non-dimensional kinematic viscosity of water 
    real(fp) :: visscl ! Scaling factor for viscosity [m^2/s] 
    real(fp) :: vref
    real(fp) :: w0     ! Weighting factor for linear inter- polation 
    real(fp) :: w1     ! Weighting factor for linear inter- polation 
    real(fp) :: xlam
    real(fp) :: z0min
    real(fp) :: z1
    real(fp) :: zbv2   ! Weight factor for centre of gravity determination of velocity reference level KREF 
    real(fp) :: zk     ! Vertical coordinate relative to reference plane distance ZETA below water surface 
    real(fp) :: zlk    ! Kolmogoroff length scale 
    real(fp) :: zsur   ! Distance between (u,v) velocity level and water surface 
    real(fp) :: ztg    ! Distance, scaled by water depth, from water surface to w-level in inter- polation grid 
    real(fp) :: zw     ! Distance, scaled by  water depth, from water surface to w-level in hydrodynamic grid 
!
!! executable statements -------------------------------------------------------
!
    ag       => gdp%gdphysco%ag
    z0       => gdp%gdphysco%z0
    vonkar   => gdp%gdphysco%vonkar
    ricrit   => gdp%gdiwearr%ricrit
    bvmin    => gdp%gdiwearr%bvmin
    epsuko   => gdp%gdiwearr%epsuko
    epsbv2   => gdp%gdiwearr%epsbv2
    clw      => gdp%gdiwearr%clw
    ckw      => gdp%gdiwearr%ckw
    viscof   => gdp%gdiwearr%viscof
    !
    h0 = zeta + real(dps,fp)
    rilz = 0.0
    riav = 0.0
    do k = 1, kmax - 1
       if (rich(k)>ricrit) then
          dsig = 0.5*(thick(k + 1) + thick(k))
          rilz = rilz + dsig
          riav = riav + rich(k)*dsig
       endif
    enddo
    if (riav>0.0) riav = riav/rilz
    !
    if (rilz<siglim) then
       barcli = .false.
       rilz = rilz*h0
       goto 9999
    else
       barcli = .true.
       rilz = rilz*h0
    endif
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Interpolate vertical gradients and turbulence parameters,
    ! defined in w-points of DPM, to TG-grid:
    !
    dztg = 1./real(kmxt,fp)
    kdpm = 1
    zw = 1. - thick(1)
    !
    do ktg = 0, kmxt
       ztg = 1. - ktg*dztg
       !-->
   30  continue
       if (ztg<zw .and. kdpm<kmax) then
          kdpm = kdpm + 1
          zw = zw - thick(kdpm)
          goto 30
       ! <--
       endif
       w0 = max(0.0_fp, (ztg - zw)/thick(kdpm))
       w1 = 1. - w0
       kdpmd = kdpm - 1
       bv2(ktg) = w0*bruvai(kdpmd) + w1*bruvai(kdpm)
       tke(ktg) = w0*rtur0(kdpmd, 1) + w1*rtur0(kdpm, 1)
       eps(ktg) = w0*rtur0(kdpmd, 2) + w1*rtur0(kdpm, 2)
       edvis(ktg) = w0*vicww(kdpmd) + w1*vicww(kdpm)
    enddo
    !
    ! Depth-averaged velocity and vertical curvature of (u,v),
    ! defined in (u,v)-points of DPM grid:
    !
    umean = 0.0
    vmean = 0.0
    do k = 1, kmax
       kd = k - 1
       dz = thick(k)*h0
       d2u0(k) = (dudz(kd) - dudz(k))/dz
       d2v0(k) = (dvdz(kd) - dvdz(k))/dz
       umean = umean + thick(k)*u0(k)
       vmean = vmean + thick(k)*v0(k)
    enddo
    !
    ! Interpolate velocity and its curvature of hydrodynamic module, defined
    ! at (u,v) level, to TG-grid:
    !
    dztg = h0/kmxt
    kdpm = 1
    !
    do ktg = 1, kmxt - 1
       ztg = zeta - ktg*dztg
       ! -->
   80  continue
       zk = zeta + h0*sig(kdpm)
       if (ztg<=zk .and. kdpm<kmax) then
          kdpm = kdpm + 1
          goto 80
       ! <--
       endif
       if (kdpm==1) then
          ! log layer near surface:
          zsur = zeta - ztg
          ratio = log(zsur/(zeta - zk))
          utg(ktg) = u0(1) + ratio*ustwix/vonkar
          vtg(ktg) = v0(1) + ratio*ustwiy/vonkar
          d2u(ktg) = d2u0(1)
          d2v(ktg) = d2v0(1)
       elseif (ztg<zk) then
          ! log layer near bed:
          z0min = 1.E-6
          z1 = max(z0min, z0)
          ratio = log((real(dps,fp) + ztg + z1)/z1)/log((real(dps,fp) + zk + z1)/z1)
          utg(ktg) = ratio*u0(kmax)
          vtg(ktg) = ratio*v0(kmax)
          d2u(ktg) = d2u0(kmax)
          d2v(ktg) = d2v0(kmax)
       else
          ! internal levels:
          kdpmd = kdpm - 1
          w0 = (ztg - zk)/(0.5*h0*(thick(kdpm) + thick(kdpmd)))
          w1 = 1. - w0
          utg(ktg) = w0*u0(kdpmd) + w1*u0(kdpm)
          vtg(ktg) = w0*v0(kdpmd) + w1*v0(kdpm)
          d2u(ktg) = w0*d2u0(kdpmd) + w1*d2u0(kdpm)
          d2v(ktg) = w0*d2v0(kdpmd) + w1*d2v0(kdpm)
       endif
    enddo
    ! extrapolations to surface:
    utg(0) = 2*utg(1) - utg(2)
    vtg(0) = 2*vtg(1) - vtg(2)
    d2u(0) = d2u(1)
    d2v(0) = d2v(1)
    ! extrapolations to bed:
    utg(kmxt) = 2*utg(kmxt - 1) - utg(kmxt - 2)
    vtg(kmxt) = 2*vtg(kmxt - 1) - vtg(kmxt - 2)
    d2u(kmxt) = d2u(kmxt - 1)
    d2v(kmxt) = d2v(kmxt - 1)
    !
    ! Normalisation of buoyancy and velocity profiles for
    ! application in subroutine TG.
    !
    sumbv2 = 0.
    zbv2 = 0.
    ksum = 0
    do ktg = 0, kmxt
       if (bv2(ktg)>0) then
          ksum = ksum + 1
          sumbv2 = sumbv2 + bv2(ktg)
          zbv2 = zbv2 + ktg*bv2(ktg)
       endif
    enddo
    !
    ! Reference level for velocity is ZBVV2 and it equals
    ! centre-of-gravity of stably-stratified part of water
    ! column:
    !
    sumbv2 = max(sumbv2, 1.E-12_fp)
    kref = nint(zbv2/sumbv2)
    bv2av = sumbv2/ksum
    bvav = max(bvmin, sqrt(bv2av))
    bv2av = bvav**2
    !
    ! Normalisation of buoyancy freq. profile, velocity vector, its
    ! derivatives and turbulence state parameters. Length SCALE depends
    ! on water depth or horizontal IW wave length:
    !
    bv2mx = 0.
    do ktg = 0, kmxt
       bv2(ktg) = bv2(ktg)/bv2av
       if (bv2mx<bv2(ktg)) then
          bv2mx = bv2(ktg)
       endif
    enddo
    bvmx = sqrt(bv2mx)
    !
    xlam = 1./xk
    scale = min(h0, xlam)
    xkh = xk*scale
    !
    tkescl = (bvav*scale)**2
    epsscl = (bvav**3)*(scale**2)
    visscl = bvav*(scale**2)
    velscl = bvav*scale
    !
    if (useref) then
       uref = utg(kref)
       vref = vtg(kref)
    else
       uref = 0.0
       vref = 0.0
    endif
    !
    do ktg = 0, kmxt
       d2u(ktg) = d2u(ktg)*scale/bvav
       d2v(ktg) = d2v(ktg)*scale/bvav
       utg(ktg) = (utg(ktg) - uref)/velscl
       vtg(ktg) = (vtg(ktg) - vref)/velscl
       tke(ktg) = tke(ktg)/tkescl
       eps(ktg) = eps(ktg)/epsscl
       edvis(ktg) = edvis(ktg)/visscl
    enddo
    !
    ! Check for neutrally-stratified or unstably-stratified top layer:
    !
    bv2lvl = epsbv2*bv2mx
    ktg = -1
    ! -->
  120 continue
    ktg = ktg + 1
    diff = bv2(ktg) - bv2lvl
    if (diff<0 .and. ktg<kmxt) goto 120
    ! <--
    if (ktg<kmxt) then
       ktop = ktg
    else
       ktop = 0
    endif
    !
    ! Check for neutrally-stratified or unstably-stratified bed layer:
    !
    ktg = kmxt + 1
    ! -->
  130 continue
    ktg = ktg - 1
    diff = bv2(ktg) - bv2lvl
    if (diff<0 .and. ktg>0) goto 130
    ! <--
    if (ktg>0) then
       kbed = ktg
    else
       kbed = kmxt
    endif
    ! safety first:
    ktop = min(ktop, kbed)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Determine turbulence parameters:
    !
    vismol = viscof/visscl
    vis3 = vismol**3
    zlo = 0.0
    !
    do ktg = 0, kmxt
       epsktg = max(epsuko, eps(ktg))
       tkektg = max(epsuko, tke(ktg))
       zlw(ktg) = clw*tkektg**1.5/epsktg
       !
       ! ZLW is truncated at Kolmogoroff length scale ZLK:
       !
       zlk = (vis3/epsktg)**0.25
       rmsw = ckw*sqrt(tkektg)
       tlw(ktg) = zlw(ktg)/rmsw
       zlw(ktg) = max(zlw(ktg), zlk)
       !
       ! Truncation ZLW for spuriously large ZLW without turbulence:
       !
       if (edvis(ktg)<vismol) zlw(ktg) = zlk
       if (ktop<=ktg .and. ktg<=kbed .and. bv2(ktg)>bvmin) then
          zlo = zlo + sqrt(epsktg/bv2(ktg)**1.5)
       endif
    enddo
    zlo = zlo/(kbed - ktop + 2)
    !
 9999 continue
end subroutine intpol
