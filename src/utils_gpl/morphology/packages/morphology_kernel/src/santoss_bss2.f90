subroutine santoss_bss2(sw_effects, as_effects, g, d, rhow, rhos, delta, &
               & d50, d90, b, r, t, uw, aw, uwc, uwt, uc, ut, unet_delwblt, &
               & ang, delwblt, alpha, ksw, ksc, fw, fcw, tc, tt, tcu, tcd, &
               & ttu, ttd, fc, sc, st, swc, swt, scx, scy, stx, sty, fcwc, fcwt)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!   Computation of the bed shear stresses using the method of Ribberink (1998)
!   Acceleration-skewness method by Van der A (2009) 
!   Part 2: Addition of acceleration effect on friction factor, surface wave effect
!   and resulting bed shear stress
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi,degrad
    implicit none
!
! arguments
!
    integer   , intent(in)    :: sw_effects
    integer   , intent(in)    :: as_effects
    real(fp)  , intent(in)    :: g
    real(fp)  , intent(in)    :: d
    real(fp)  , intent(in)    :: rhow
    real(fp)  , intent(in)    :: rhos
    real(fp)  , intent(in)    :: delta
    real(fp)  , intent(in)    :: d50
    real(fp)  , intent(in)    :: d90
    real(fp)  , intent(in)    :: b
    real(fp)  , intent(in)    :: r
    real(fp)  , intent(in)    :: t
    real(fp)  , intent(in)    :: uw
    real(fp)  , intent(in)    :: aw
    real(fp)  , intent(in)    :: uwc
    real(fp)  , intent(in)    :: uwt
    real(fp)  , intent(in)    :: uc
    real(fp)  , intent(in)    :: ut
    real(fp)  , intent(in)    :: unet_delwblt
    real(fp)  , intent(in)    :: ang
    real(fp)  , intent(in)    :: delwblt
    real(fp)  , intent(in)    :: alpha
    real(fp)  , intent(in)    :: ksw
    real(fp)  , intent(in)    :: ksc
    real(fp)  , intent(in)    :: fw
    real(fp)  , intent(in)    :: fcw
    real(fp)  , intent(in)    :: tc
    real(fp)  , intent(in)    :: tt
    real(fp)  , intent(in)    :: tcu
    real(fp)  , intent(in)    :: tcd
    real(fp)  , intent(in)    :: ttu
    real(fp)  , intent(in)    :: ttd
!
    real(fp)  , intent(inout) :: fc
!
    real(fp)  , intent(out)   :: sc
    real(fp)  , intent(out)   :: st
    real(fp)  , intent(out)   :: swc
    real(fp)  , intent(out)   :: swt
    real(fp)  , intent(out)   :: scx
    real(fp)  , intent(out)   :: scy
    real(fp)  , intent(out)   :: stx
    real(fp)  , intent(out)   :: sty
    real(fp)  , intent(out)   :: fcwc
    real(fp)  , intent(out)   :: fcwt
!
! local variables
!
    real(fp)                :: aspc
    real(fp)                :: aspt
    real(fp)                :: awc
    real(fp)                :: awt
    real(fp)                :: fwc
    real(fp)                :: fwt
    real(fp)                :: ksi
    real(fp)                :: eta
    real(fp)                :: l
    real(fp)                :: c
    real(fp)                :: alpha_sw
    real(fp)                :: tauwre
    real(fp)                :: swre
!
!! executable statements -------------------------------------------------------
!
!   acceleration-skewness (van der A)
!
    if (comparereal(b,0.5_fp) /= 0 .and. as_effects == 1) then
!     - wave friction coefficient for crest and trough 
!     - combined wave-current friction coefficent for crest and trough
        if (comparereal(tc,0.0_fp) == 0) then
            aspc=1.0_fp
        else
            aspc=(2.0_fp*tcu/tc)    ! alternative acceleration skewness paramater crest [-]
        endif
        if(comparereal(tt,0.0_fp) == 0)then
            aspt=1.0_fp
        else
            aspt=(2.0_fp*ttu/tt)    ! alternative acceleration skewness paramater trough [-]
        endif
        awc=(aspc**2.6_fp)*aw       ! equivalent excursion amplitude for crest [m]
        awt=(aspt**2.6_fp)*aw       ! equivalent excursion amplitude for trough [m]

!       crest friction factor
        if (awc <= 0.0_fp) then
            fwc = 0.3_fp
        else  
            if (ksw/awc < 0.63_fp) then
                fwc = exp(5.213_fp*(ksw/awc)**0.194_fp - 5.977_fp)
            else
                fwc = 0.3_fp
            endif
        endif

!       trough friction factor
        if (awt <= 0.0_fp) then
            fwt = 0.3_fp
        else  
            if (ksw/awt < 0.63_fp) then
                fwt = exp(5.213_fp*(ksw/awt)**0.194_fp - 5.977_fp)
            else
                fwt = 0.3_fp
            endif
        endif 

        if (comparereal(unet_delwblt,0.0_fp) == 0) then
            fc   = 0.0_fp
            fcwc = fwc    ! combined wave-current friction factor crest [-]
            fcwt = fwt    ! combined wave-current friction factor trough [-]
        else
            ! combined wave-current friction coefficient at crest fcwc and trough fcwt
            ! using formula Madsen & Grant (1976)
            fcwc = alpha*fc+(1.0_fp-alpha)*fwc
            fcwt = alpha*fc+(1.0_fp-alpha)*fwt
        endif

        ! bed shear stress for acceleration-skewed waves with/without 
        ! a  current tc, tt and x,y components stx, sty
        ! bed shear stresses due to wave alone!
        swc = (0.5_fp*fwc*uwc**2/(delta*g*d50))
        swt = (0.5_fp*fwt*uwt**2/(delta*g*d50))
        ! total bed shear stresses!
        sc = (0.5_fp*fcwc*uc**2/(delta*g*d50))
        st = (0.5_fp*fcwt*ut**2/(delta*g*d50))

        if(uwc+unet_delwblt*cos(ang*degrad) <= 0.0_fp) then
            scx = sc * 0.001_fp/uc
            scy = sc * unet_delwblt*sin(ang*degrad)/uc
        else            
            scx = sc * (uwc + unet_delwblt*cos(ang*degrad))/uc
            scy = sc * unet_delwblt*sin(ang*degrad)/uc
        endif
        if (unet_delwblt*cos(ang*degrad)-uwt >= 0.0_fp) then
            stx = st * -0.001_fp/ut
            sty = unet_delwblt*sin(ang*degrad)/ut 
        else
            stx = st  * (-uwt + unet_delwblt*cos(ang*degrad))/ut
            sty = st  * unet_delwblt*sin(ang*degrad)/ut 
        endif

    elseif (comparereal(b,0.5_fp) == 0 .or. as_effects == 0) then
!
!       (iv) bed shear stress for velocity-skewed or sine waves with/without a current tc, tt and x,y components stx, sty
!
        ! bed shear stresses due to wave alone!
        swc = (0.5_fp*fw*uwc**2/(delta*g*d50))
        swt = (0.5_fp*fw*uwt**2/(delta*g*d50))
        ! total bed shear stresses!
        sc  = (0.5_fp*fcw*uc**2/(delta*g*d50))
        st  = (0.5_fp*fcw*ut**2/(delta*g*d50))
        if(uwc+unet_delwblt*cos(ang*degrad) <= 0.0_fp) then
            scx = sc * 0.001_fp/uc
            scy = sc * unet_delwblt*sin(ang*degrad)/uc
        else
            scx = sc * (uwc + unet_delwblt*cos(ang*degrad))/uc
            scy = sc * unet_delwblt*sin(ang*degrad)/uc
        endif
        if (unet_delwblt*cos(ang*degrad)-uwt >= 0.) then
            stx = st * -0.001_fp/ut
            sty = unet_delwblt*sin(ang*degrad)/ut 
        else
            stx = st * (-uwt + unet_delwblt*cos(ang*degrad))/ut
            sty = st * unet_delwblt*sin(ang*degrad)/ut 
        endif
     endif

    ! surface wave effects
    if (sw_effects == 1) then
!
!       (v) wave Reynolds stress
!
        ! calculation wave propagation speed c
        ksi = 4.0_fp*pi**2*d/(g*t**2)  ! (d=depth, t=wave period)
        if  (ksi <= 1.0_fp) then
            eta = sqrt(ksi) * (1.0_fp + 0.2_fp * ksi)
        elseif (ksi > 1.0_fp) then
            eta = ksi * (1.0_fp+0.2_fp*exp(2.0_fp - 2.0_fp*ksi))
        endif
        l= 2.0_fp* pi* d / eta
        c = l / t
        
        ! wave reynolds stress and shields
        !  - alpha = 0.424 (sine waves)
        !  - friction coefficient = fcw (mobile bed)
        alpha_sw = 0.424_fp
        tauwre = rhow * fcw * alpha_sw * uw**3.0_fp/(2.0_fp * c)
        swre    = tauwre / ((rhos - rhow) * g * d50)
!
!       (vi) crest period extension for horizontal particle displacement.
!
!       removed
!
!       (vii) bed shear stress for surface waves with/without a current
!
        ! tc, tt and x,y components stx, sty
        scx = scx + swre
        stx = stx + swre
        sc = sqrt(scx**2 + scy**2)
        st = sqrt(stx**2 + sty**2)
     endif
end subroutine santoss_bss2
