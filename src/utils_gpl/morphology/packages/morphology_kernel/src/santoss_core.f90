subroutine santoss_core(pl_effects, sw_effects, g, d50, d, hw, rhos, rhow, &
                      & delta, tp, r, b, tc, tt, tcu, tcd, ttu, ttd, sfltc, sfltt, &
                      & wss, rh, scr_c, scr_t, sc, st, scx, scy, stx, sty, &
                      & uc, ut, n, m, alphas, alphar, pcr, &
                      & pc, pt, oc, occ, oct, ot, ott, otc, phicx, phitx, &
                      & phicy, phity, qsx, qsy)
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
!   The SANTOSS practical sand transport model, version 2.08  
!   Computations of the orbital motions in the nearshore morphodynamical model
!   using the formula of Abreu et al. (2010)
!
!   Calculation of transport rates
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts, only: pi, eps_fp
    use ieee_arithmetic, only: ieee_is_nan

    implicit none
!
! arguments
!
    integer   , intent(in)  :: pl_effects
    integer   , intent(in)  :: sw_effects
    real(fp)  , intent(in)  :: g
    real(fp)  , intent(in)  :: d50
    real(fp)  , intent(in)  :: d
    real(fp)  , intent(in)  :: hw
    real(fp)  , intent(in)  :: rhos
    real(fp)  , intent(in)  :: rhow
    real(fp)  , intent(in)  :: delta
    real(fp)  , intent(in)  :: tp
    real(fp)  , intent(in)  :: r
    real(fp)  , intent(in)  :: b
    real(fp)  , intent(in)  :: tc
    real(fp)  , intent(in)  :: tt
    real(fp)  , intent(in)  :: tcu
    real(fp)  , intent(in)  :: tcd
    real(fp)  , intent(in)  :: ttu
    real(fp)  , intent(in)  :: ttd
    real(fp)  , intent(in)  :: sfltc
    real(fp)  , intent(in)  :: sfltt
    real(fp)  , intent(in)  :: wss
    real(fp)  , intent(in)  :: rh
    real(fp)  , intent(in)  :: scr_c
    real(fp)  , intent(in)  :: scr_t
    real(fp)  , intent(in)  :: sc
    real(fp)  , intent(in)  :: st
    real(fp)  , intent(in)  :: scx
    real(fp)  , intent(in)  :: scy
    real(fp)  , intent(in)  :: stx
    real(fp)  , intent(in)  :: sty
    real(fp)  , intent(in)  :: uc
    real(fp)  , intent(in)  :: ut
    real(fp)  , intent(in)  :: n
    real(fp)  , intent(in)  :: m
    real(fp)  , intent(in)  :: alphas
    real(fp)  , intent(in)  :: alphar
    real(fp)  , intent(in)  :: pcr
!
    real(fp)  , intent(out) :: pc
    real(fp)  , intent(out) :: pt
    real(fp)  , intent(out) :: oc
    real(fp)  , intent(out) :: occ
    real(fp)  , intent(out) :: oct
    real(fp)  , intent(out) :: ot
    real(fp)  , intent(out) :: ott
    real(fp)  , intent(out) :: otc
    real(fp)  , intent(out) :: phicx
    real(fp)  , intent(out) :: phitx
    real(fp)  , intent(out) :: phicy
    real(fp)  , intent(out) :: phity
    real(fp)  , intent(out) :: qsx
    real(fp)  , intent(out) :: qsy
!
! local variables
!
    real(fp)                :: l
    real(fp)                :: xi
    real(fp)                :: ksi
    real(fp)                :: eta
    real(fp)                :: c
    real(fp)                :: tmp
    real(fp)                :: eps_corr
    real(fp)                :: worbc
    real(fp)                :: worbt
    real(fp)                :: worbc1
    real(fp)                :: worbt1
    real(fp)                :: worbc2
    real(fp)                :: worbt2
    real(fp)                :: delta_w
    real(fp)                :: xc
    real(fp)                :: xt
    real(fp)                :: phix
    real(fp)                :: phiy
!
!! executable statements -------------------------------------------------------
!
!   calculation phase-lag parameters [-]
!
!   calculate maximum vertical orbital velocity for real waves (zero for other)
!   with 2nd order Stokes theory. tuning factor 1.5 for transport
!   measurements GWK Schretlen 2010
    if (sw_effects == 1) then
        if (comparereal(rh,0.0_fp) == 0) then
            worbc1 = pi*hw*sfltc/tp/d
            worbt1 = pi*hw*sfltt/tp/d
        else
            worbc1 = pi*hw*rh/tp/d
            worbt1 = pi*hw*rh/tp/d
        endif
        worbc2 = worbc1*2.0_fp*(2.0_fp*r-1.0_fp)
        worbt2 = worbt1*2.0_fp*(2.0_fp*r-1.0_fp)
        eps_corr = 1.0_fp ! correction factor level vertical orbital velocity
        worbc = (1.0_fp/8.0_fp)*worbc1*sqrt(64.0_fp-(-worbc1+sqrt(worbc1**2 + &
                     & 32.0_fp*worbc2**2))**2/(worbc2**2))+worbc2* &
                     & sin(2.0_fp*acos((1.0_fp/8.0_fp)* (-worbc1+ &
                     & sqrt(worbc1**2+32.0_fp*worbc2**2))/worbc2))
        worbc = worbc*eps_corr
        worbt = (1.0_fp/8.0_fp)*worbt1*sqrt(64.0_fp-(-worbt1+sqrt(worbt1**2 + &
                     & 32.0_fp*worbt2**2))**2/(worbt2**2))+ &
                     & worbt2*sin(2.0_fp*acos((1.0_fp/8.0_fp)*(-worbt1+ &
                     & sqrt(worbt1**2+32.0_fp*worbt2**2))/worbt2))
        worbt = worbt*eps_corr
        ! calculation wave propegation speed
        ksi = 4.0_fp*pi**2*d/(g*tp**2)
        if (ksi <= 1.0_fp) then
            eta = sqrt(ksi)*(1.0_fp+0.2_fp*ksi)
        else ! ksi > 1.0_fp
            eta = ksi*(1.0_fp+0.2_fp*exp(2.0_fp-2.0_fp*ksi))
        endif
        l  = 2.0_fp*pi*d/eta
        c  = l/tp
        xi = 1.7_fp !0.55*sqrt(2)
    else
        worbc = 0.0_fp
        worbt = 0.0_fp
        xi    = 0.0_fp
        c     = 1.0_fp
    endif

    if (pl_effects == 0) then
        ! if case of no phase lag effects pc=pt=0 instead of 1, 19/4/19, jjvdwerf
        pc = 0.0_fp
        pt = 0.0_fp
    else
        delta_w = max(wss-worbt,0.0_fp)    ! test to prevent negative vertical settling velocity
        ! correction (1-xi*uc/c) instead of ((1-xi*uc)/c), 19/4/19, jjvdwerf
        if (comparereal(rh,0.0_fp) == 0) then
            pc = alphas*(1.0_fp-xi*uc/c)*sfltc/((wss+worbc)*2.0_fp*(tc-tcu))
            pt = alphas*(1.0_fp+xi*ut/c)*sfltt/(delta_w*2.0_fp*(tt-ttu))
        else
            pc = alphar*(1.0_fp-xi*uc/c)*rh/((wss+worbc)*2.0_fp*(tc-tcu))
            pt = alphar*(1.0_fp+xi*ut/c)*rh/(delta_w*2.0_fp*(tt-ttu))
        endif
        if (ieee_is_nan(pc)) pc = 1.0_fp
        if (ieee_is_nan(pt)) pt = 1.0_fp
    endif

!
!   load components [-]
!
    oc = m*max((sc-scr_c),0.0_fp)**n            ! load entrained during crest period
    if( pc <= 1.0_fp) then
        occ = oc                                ! load entrained and transported during crest period
        oct = 0.0_fp
    else ! pc > 1.0_fp
        occ=min(pcr/pc,1.0_fp)*oc
        if  (abs(tc) < eps_fp) then             ! check zero value t_crest
            tmp = 1.0_fp                        ! limit goes to 1. Normal calculation goes to NaN and otc becomes 0 and not otc
        else
            tmp = (pc-pcr)/pc                   ! normal calculation
        endif
        oct = max(tmp,0.0_fp)*oc                ! load entrained during crest period and transported during trough period
    endif

    ot = m*max((abs(st)-scr_t),0.0_fp)**n       ! load entrained during trough period
    if( pt <= 1.0_fp) then
        ott=ot
        otc=0.0_fp
    else ! pt > 1.0_fp
        ott=min(pcr/pt,1.0_fp)*ot               ! load entrained and transported during trough period
        if (abs(tt) < eps_fp) then              ! check zero value t_trough
            tmp = 1.0_fp                        ! limit goes to 1. Normal calculation goes to nan and otc becomes 0 and not ot
        else
            tmp = (pt-pcr)/pt                   ! normal calculation
        endif
        otc = max(tmp,0.0_fp)*ot                ! entrained during trough period and transported during crest period
    endif

!
!   transport components [-]
!
    xc = 2.0_fp*tcu/tc                     ! alternative acceleration skewness paramater crest [-]
    xt = 2.0_fp*ttu/tt                     ! alternative acceleration skewness paramater trough [-]

    if (tc <= 0.001_fp) then               ! if there is no crest period, there is no transport in crest direction    
        phicx = 0.0_fp
        phicy = 0.0_fp
    elseif (tt <= 0.001_fp) then           ! if there is no trough period, there is no sand exchange from crest to trough period
        phicx = scx/sqrt(sc)*occ
        phicy = scy/sqrt(sc)*occ
    else
        phicx = scx/sqrt(sc)*(occ+1.0_fp/xc*otc)
        phicy = scy/sqrt(sc)*(occ+1.0_fp/xc*otc)
    endif

    if (tt <= 0.001_fp) then
        phitx = 0.0_fp
        phity = 0.0_fp
    elseif (tc <= 0.001_fp) then
        phitx = stx/sqrt(abs(st))*ott
        phity = sty/sqrt(abs(st))*ott
    else
        phitx = stx/sqrt(abs(st))*(ott+1.0_fp/xt*oct)
        phity = sty/sqrt(abs(st))*(ott+1.0_fp/xt*oct)
    endif

    phix = (tc*phicx+tt*phitx)/tp          ! dimensionless transport in x-direction
    phiy = (tc*phicy+tt*phity)/tp          ! dimensionless transport in y-direction

    qsx = phix*sqrt(delta*g*d50**3)        ! transport in x-direction [m2/s]
    qsy = phiy*sqrt(delta*g*d50**3)        ! transport in y-direction [m2/s]
end subroutine santoss_core
