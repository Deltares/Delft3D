subroutine bediwe(kmax      ,kmxdt     ,kmxt      ,h0        ,scale     , &
                & bvav      ,xkh       ,angle     ,singul    ,kbed      , &
                & ktop      ,kcrit     ,thick     ,ustbx     ,ustby     , &
                & d2u       ,d2v       ,dudz      ,dvdz      ,utg       , &
                & vtg       ,frcbed    ,edvis     ,qz        ,r1tg      , &
                & dijdij    ,ttkiw     ,tiwtk     ,az        ,zamp      , &
                & dzdz      ,luniwe    ,gdp       )
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
!      Function: Power transfer of lee waves to TKE. The latter
!                transfer is distributed TKE production according
!                to the IW-imposed shear rate.
!
!                TIWTK is the distributed TKE production in
!                                                     [m^2/s^3].
!
!                All power contributions are added to TTKIW or
!                TIWTK.
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
    real(fp) , pointer :: alfaz
    real(fp) , pointer :: viscof
    logical  , pointer :: iwedia
!
! Global variables
!
    integer         :: kbed
                                   !!  KTG-value of upper level strat. layer
    integer, intent(in)            :: kcrit
                                   !!  K-value on TG grid of the critical
                                   !!  level associated with a given IW own
                                   !!  mode (R1TG)
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: kmxdt !  Description and declaration in dimens.igs
    integer         :: kmxt !  Description and declaration in dimens.igs
    integer         :: ktop
                                   !!  KTG-value of lower level strat. layer
    integer, intent(in)            :: luniwe
                                   !!  Unit number of IWE diagnostic file
    logical, intent(in)            :: singul
                                   !!  .true. implies lee wave with critical
                                   !!  layer
    real(fp)        :: angle
                                   !!  Angle [degrees] between horizontal
                                   !!  IW wavenumber vector and x-u-axis
    real(fp)        :: az
                                   !!  Rms internal wave vertical displace-
                                   !!  ment amplitude [m]
    real(fp), intent(in)               :: bvav
                                   !!  Mean bouyancy frequency
    real(fp), intent(in)               :: frcbed !  Description and declaration in iwepar.igs
    real(fp)        :: h0
                                   !!  Water depth
    real(fp)        :: scale
                                   !!  Length scale for non-dimensionalising
    real(fp), intent(in)               :: ustbx
                                   !!  X/u-component of bed-shear stress
                                   !!  velocity (zeta point)
    real(fp), intent(in)               :: ustby
                                   !!  Y/v-component of bed-shear stress
                                   !!  velocity (zeta point)
    real(fp)        :: xkh
                                   !!  Wave number magnitude normalized by
                                   !!  SCALE
    real(fp), dimension(0:kmax), intent(in) :: dudz !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax), intent(in) :: dvdz !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmxdt) :: d2u
                                   !!  curvature of vertical profile of u-
                                   !!  velocity component
    real(fp), dimension(0:kmxdt) :: d2v
                                   !!  curvature of vertical profile of v-
                                   !!  velocity component
    real(fp), dimension(0:kmxdt) :: dijdij
                                   !!  Shear rate induced by lee waves
    real(fp), dimension(0:kmxdt) :: dzdz
                                   !!  Maximal IW-induced vertical strain
                                   !!  rate
    real(fp), dimension(0:kmxdt), intent(in) :: edvis
                                   !!  Interpolated eddy viscosity
    real(fp), dimension(0:kmxdt) :: qz
                                   !!  Vertical wave number squared, in TG
                                   !!  equation
    real(fp), dimension(0:kmxdt) :: r1tg
    real(fp), dimension(0:kmxdt) :: tiwtk
                                   !!  Transfer rate of IWE to TKE
    real(fp), dimension(0:kmxdt) :: ttkiw
                                   !!  Transfer rate of TKE to IWE
    real(fp), dimension(0:kmxdt) :: utg
    real(fp), dimension(0:kmxdt) :: vtg
    real(fp), dimension(0:kmxdt) :: zamp
                                   !!  Maximal vertical displacement amp-
                                   !!  litude of bed-induced IW
    real(fp), dimension(kmax), intent(in) :: thick !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: k
    integer                        :: kmn                  ! Lower KTG-value for analysis 
    integer                        :: kmx                  ! Upper KTG-value for analysis 
    real(fp)                       :: aw                   ! Rms internal wave vertical velocity amplitude [m/s] 
    real(fp)                       :: crad
    real(fp)                       :: d2rdz2
    real(fp)                       :: dirbed
    real(fp)                       :: dsig
    real(fp)                       :: dz
    real(fp)                       :: dztg
    real(fp)                       :: dztg2
    real(fp)                       :: epsav                ! Depth-averaged dissipation of lee waves, after amplitude reduction 
    real(fp)                       :: epsscl               ! Scaling factor for dissipation/ production terms [m^2/s^3] 
    real(fp)                       :: excit                ! Excitation rate [m^3/s^3] of lee wave energy 
    real(fp)                       :: fac
    real(fp)                       :: frccor               ! Reduced FRCBED 
    real(fp)                       :: ha2w                 ! 0.5*AW^2, calibrated variance of vertical IW velocity 
    real(fp)                       :: omeg                 ! Angular frequency of IW with respect to ground (root of TG equation) 
    real(fp)                       :: phi
    real(fp)                       :: power                ! Total power spent by lee waves working against viscous and tur- bulent flow 
    real(fp)                       :: reduc
    real(fp)                       :: sig
    real(fp)                       :: sumtit               ! Depth integral of TKE production by IW shear rates DIJDIJ 
    real(fp)                       :: tit                  ! Distributed TKE production by IW shear rates [m^2/s^3] 
    real(fp)                       :: u2bx                 ! X-component of bed-shear stress vector 
    real(fp)                       :: u2bxp
    real(fp)                       :: u2by                 ! Y-component of bed-shear stress vector 
    real(fp)                       :: u2byp
    real(fp)                       :: ubx                  ! X-component of bed-shear stress velocity 
    real(fp)                       :: uby                  ! Y-component of bed-shear stress velocity 
    real(fp)                       :: ustb2
    real(fp)                       :: velscl               ! Velocity scale for non-dimensionaling 
    real(fp)                       :: vismol               ! Non-dimensional kinematic viscosity 
    real(fp)                       :: visscl               ! Viscosity scale for non-dimensio- naling 
    real(fp)                       :: xkl2
    real(fp)                       :: xmu
    real(fp)                       :: zcrit
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    alfaz   => gdp%gdiwearr%alfaz
    viscof  => gdp%gdiwearr%viscof
    iwedia  => gdp%gdiwearr%iwedia
    !
    omeg = 0.0
    dz = h0/kmxt
    dztg = dz/scale
    dztg2 = dztg**2
    xkl2 = xkh**2
    !
    epsscl = (bvav**3)*(scale**2)
    visscl = bvav*(scale**2)
    velscl = bvav*scale
    crad = atan(1.)/45.
    !
    vismol = viscof/visscl
    ubx = ustbx/velscl
    uby = ustby/velscl
    phi = angle*crad
    !
    ! Depth-integrated power EXCIT [m^3/s^3] of shear rate,
    ! projected in IW direction, working against bed-induced
    ! stresses RXZ and RYZ, also projected in IW direction:
    !
    sig = 0.0
    u2bx = ubx*abs(ubx)
    u2by = uby*abs(uby)
    u2bxp = u2bx*cos(phi)**2
    u2byp = u2by*sin(phi)**2
    ustb2 = sqrt(u2bx**2 + u2by**2)
    dirbed = acos(u2bx/ustb2)/crad
    if (u2by<0.0) dirbed = 360. - dirbed
    !
    excit = (thick(1)**2)*(u2bxp*dudz(1) + u2byp*dvdz(1))/8.
    do k = 1, kmax - 1
       sig = sig + thick(k)
       dsig = 0.5*(thick(k) + thick(k + 1))
       excit = excit + (u2bxp*dudz(k) + u2byp*dvdz(k))*sig*dsig
    enddo
    excit = excit + thick(kmax)*(u2bxp*dudz(kmax) + u2byp*dvdz(kmax))/2.
    excit = max(0.0_fp, excit)*h0
    excit = excit/(epsscl*scale)
    !
    ! This routine is called only for top=.false.:
    !
    kmn = 0
    kmx = kmxt
    if (singul) kmn = kcrit
    !
    ! Second-order derivative of eigen-solution d^2r/dz^2 is "d2rdz2":
    !
    power = 0.0
    do k = kmn + 1, kmx - 1
       if (abs(qz(k))>1.E-6) then
          d2rdz2 = -qz(k)*r1tg(k)
       else
          d2rdz2 = (r1tg(k - 1) + r1tg(k + 1) - 2*r1tg(k))/dztg2
       endif
       dijdij(k) = d2rdz2**2/xkl2
       power = power + (vismol + edvis(k))*dijdij(k)
    enddo
    power = power*dztg
    !
    ! Fraction of the depth-integrated power spent by the flow on
    ! bed-induced stresses is converted into IWE:
    !
    sumtit = frcbed*excit
    !
    ! Calibrate IW variance 0.5*(A_w)^2 for matching the energy flux
    ! from TKE to IWE to the energy flux from TKE to IWE, estimated
    ! above:
    !
    ha2w = sumtit/power
    !
    ! A_w=aw is the rms vertical velocity amplitude in [m/s].
    ! A_z=az is the rms wave amplitude [m], to be multiplied by R1TG:
    !
    aw = sqrt(ha2w)*velscl
    az = aw/bvav
    frccor = frcbed
    !
    ! If maximal IW amplitude exceeded then power reduction:
    !
    if (.not.singul) then
       call ampiwe(angle     ,az        ,h0        ,kbed      ,ktop      , &
                 & kmxt      ,omeg      ,qz        ,r1tg      ,scale     , &
                 & utg       ,vtg       ,xkh       ,zamp      ,dzdz      , &
                 & gdp       )
       !
       !
       xmu = alfaz*sqrt(2.)
       if (dzdz(kmxdt)>xmu) then
          reduc = xmu/dzdz(kmxdt)
          dzdz(kmxdt) = xmu
          az = reduc*az
          aw = bvav*az
          ha2w = (aw/velscl)**2
          sumtit = power*ha2w
          frccor = sumtit/excit
          zamp(kmxdt) = reduc*zamp(kmxdt)
       endif
    endif
    epsav = sumtit*epsscl*scale
    !
    ! IWE to TKE transfer in [m^2/s^3]:
    !
    sumtit = sumtit*epsscl*scale
    fac = ha2w*epsscl
    !
    do k = kmn + 1, kmx - 1
       tit = fac*edvis(k)*dijdij(k)
       tiwtk(k) = tiwtk(k) + tit
    enddo
    !
    ! Wave-current forces:
    !
    !
    ! Diagnostics:
    !
    if (iwedia) then
       !
       write (luniwe, *)
       write (luniwe, '(a)') ' BEDIWE                          :'
       write (luniwe, '(a,1x,g10.3)') ' Angle of bed friction      [dgr]:',      &
                                   & dirbed
       write (luniwe, '(a,1x,g10.3)') ' Angle of lee wave          [dgr]:', angle
       write (luniwe, '(a,1x,g10.3)') ' Power transfer to TKE  [m^3/s^3]:', epsav
       write (luniwe, '(a,1x,g10.3)') ' Fraction of power of flow    [-]:',      &
                                   & frccor
       write (luniwe, '(a,1x,g10.3)') ' Near-bed orb. velocity     [m/s]:', aw
       !
       if (.not.singul) then
          write (luniwe, '(a)') ' No critical layer               :'
          write (luniwe, '(a,1x,g10.3)') ' Max dZ/dz in strat. layer    [-]:',   &
                                      & dzdz(kmxdt)
          write (luniwe, '(a,1x,g10.3)') ' Max. z-displacement          [m]:',   &
                                      & zamp(kmxdt)
       else
          zcrit = (kmxt - kcrit)*h0/kmxt
          write (luniwe, '(a,1x,g10.3)') ' Level crit. layer above bed  [m]:',   &
                                      & zcrit
       endif
    endif
end subroutine bediwe
